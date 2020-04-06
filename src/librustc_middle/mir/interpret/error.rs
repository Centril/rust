use super::{AllocId, ConstValue, Pointer, RawConst, ScalarMaybeUndef};

use crate::ty::layout::LayoutError;
use crate::ty::query::TyCtxtAt;
use crate::ty::tls;
use crate::ty::Ty;

use backtrace::Backtrace;
use rustc_data_structures::sync::Lock;
use rustc_errors::{struct_span_err, DiagnosticBuilder};
use rustc_macros::HashStable;
use rustc_session::CtfeBacktrace;
use rustc_span::def_id::DefId;
use rustc_target::abi::{Align, Size};
use std::{any::Any, fmt, mem};

#[derive(Debug, Copy, Clone, PartialEq, Eq, HashStable, RustcEncodable, RustcDecodable)]
pub enum ErrorHandled {
    /// Already reported a lint or an error for this evaluation.
    Reported,
    /// Don't emit an error, the evaluation failed because the MIR was generic
    /// and the substs didn't fully monomorphize it.
    TooGeneric,
}

impl ErrorHandled {
    pub fn assert_reported(self) {
        match self {
            ErrorHandled::Reported => {}
            ErrorHandled::TooGeneric => bug!(
                "MIR interpretation failed without reporting an error \
                 even though it was fully monomorphized"
            ),
        }
    }
}

CloneTypeFoldableImpls! {
    ErrorHandled,
}

pub type ConstEvalRawResult<'tcx> = Result<RawConst<'tcx>, ErrorHandled>;
pub type ConstEvalResult<'tcx> = Result<ConstValue<'tcx>, ErrorHandled>;

pub fn struct_error<'tcx>(tcx: TyCtxtAt<'tcx>, msg: &str) -> DiagnosticBuilder<'tcx> {
    struct_span_err!(tcx.sess, tcx.span, E0080, "{}", msg)
}

/// Packages the kind of error we got from the const code interpreter
/// up with a Rust-level backtrace of where the error occurred.
/// Thsese should always be constructed by calling `.into()` on
/// a `InterpError`. In `librustc_mir::interpret`, we have `throw_err_*`
/// macros for this.
#[derive(Debug)]
pub struct ErrorAndBacktrace<Error> {
    pub kind: Error,
    backtrace: Option<Box<Backtrace>>,
}

impl<Error: fmt::Display> fmt::Display for ErrorAndBacktrace<Error> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl<T> ErrorAndBacktrace<T> {
    pub fn print_backtrace(&mut self) {
        if let Some(ref mut backtrace) = self.backtrace {
            print_backtrace(&mut *backtrace);
        }
    }
}

fn print_backtrace(backtrace: &mut Backtrace) {
    backtrace.resolve();
    eprintln!("\n\nAn error occurred in miri:\n{:?}", backtrace);
}

impl From<ErrorHandled> for ErrorAndBacktrace<InterpError<'_>> {
    fn from(err: ErrorHandled) -> Self {
        match err {
            ErrorHandled::Reported => err_inval!(ReferencedConstant),
            ErrorHandled::TooGeneric => err_inval!(TooGeneric),
        }
        .into()
    }
}

impl<Error> From<Error> for ErrorAndBacktrace<Error> {
    fn from(kind: Error) -> Self {
        let capture_backtrace = tls::with_context_opt(|ctxt| {
            if let Some(ctxt) = ctxt {
                *Lock::borrow(&ctxt.tcx.sess.ctfe_backtrace)
            } else {
                CtfeBacktrace::Disabled
            }
        });

        let backtrace = match capture_backtrace {
            CtfeBacktrace::Disabled => None,
            CtfeBacktrace::Capture => Some(Box::new(Backtrace::new_unresolved())),
            CtfeBacktrace::Immediate => {
                // Print it now.
                let mut backtrace = Backtrace::new_unresolved();
                print_backtrace(&mut backtrace);
                None
            }
        };

        Self { kind, backtrace }
    }
}

/// Error information for when the program we executed turned out not to actually be a valid
/// program. This cannot happen in stand-alone Miri, but it can happen during CTFE/ConstProp
/// where we work on generic code or execution does not have all information available.
pub enum InvalidProgramInfo<'tcx> {
    /// Resolution can fail if we are in a too generic context.
    TooGeneric,
    /// Cannot compute this constant because it depends on another one
    /// which already produced an error.
    ReferencedConstant,
    /// Abort in case type errors are reached.
    TypeckError,
    /// An error occurred during layout computation.
    Layout(LayoutError<'tcx>),
    /// An invalid transmute happened.
    TransmuteSizeDiff(Ty<'tcx>, Ty<'tcx>),
}

impl fmt::Debug for InvalidProgramInfo<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use InvalidProgramInfo::*;
        match self {
            TooGeneric => write!(f, "encountered overly generic constant"),
            ReferencedConstant => write!(f, "referenced constant has errors"),
            TypeckError => write!(f, "encountered constants with type errors, stopping evaluation"),
            Layout(ref err) => write!(f, "{}", err),
            TransmuteSizeDiff(from_ty, to_ty) => write!(
                f,
                "tried to transmute from {:?} to {:?}, but their sizes differed",
                from_ty, to_ty
            ),
        }
    }
}

/// Used by `check_in_alloc` to indicate context of check
#[derive(Debug, Copy, Clone, RustcEncodable, RustcDecodable, HashStable)]
pub enum CheckInAllocMsg {
    MemoryAccessTest,
    NullPointerTest,
    PointerArithmeticTest,
    InboundsTest,
}

impl fmt::Display for CheckInAllocMsg {
    /// When this is printed as an error the context looks like this
    /// "{test name} failed: pointer must be in-bounds at offset..."
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                CheckInAllocMsg::MemoryAccessTest => "Memory access",
                CheckInAllocMsg::NullPointerTest => "Null pointer test",
                CheckInAllocMsg::PointerArithmeticTest => "Pointer arithmetic",
                CheckInAllocMsg::InboundsTest => "Inbounds test",
            }
        )
    }
}

/// Error information for when the program caused Undefined Behavior.
pub enum UndefinedBehaviorInfo {
    /// Free-form case. Only for errors that are never caught!
    Ub(String),
    /// Unreachable code was executed.
    Unreachable,
    /// An enum discriminant was set to a value which was outside the range of valid values.
    InvalidDiscriminant(ScalarMaybeUndef),
    /// A slice/array index projection went out-of-bounds.
    BoundsCheckFailed {
        len: u64,
        index: u64,
    },
    /// Something was divided by 0 (x / 0).
    DivisionByZero,
    /// Something was "remainded" by 0 (x % 0).
    RemainderByZero,
    /// Overflowing inbounds pointer arithmetic.
    PointerArithOverflow,
    /// Invalid metadata in a wide pointer (using `str` to avoid allocations).
    InvalidMeta(&'static str),
    /// Reading a C string that does not end within its allocation.
    UnterminatedCString(Pointer),
    /// Dereferencing a dangling pointer after it got freed.
    PointerUseAfterFree(AllocId),
    /// Used a pointer outside the bounds it is valid for.
    PointerOutOfBounds {
        ptr: Pointer,
        msg: CheckInAllocMsg,
        allocation_size: Size,
    },
    /// Used a pointer with bad alignment.
    AlignmentCheckFailed {
        required: Align,
        has: Align,
    },
    /// Using an integer as a pointer in the wrong way.
    InvalidIntPointerUsage(u64),
    /// Writing to read-only memory.
    WriteToReadOnly(AllocId),
    /// Using a pointer-not-to-a-function as function pointer.
    InvalidFunctionPointer(Pointer),
    // Trying to access the data behind a function pointer.
    DerefFunctionPointer(AllocId),
    /// The value validity check found a problem.
    /// Should only be thrown by `validity.rs` and always point out which part of the value
    /// is the problem.
    ValidationFailure(String),
    /// Using a non-boolean `u8` as bool.
    InvalidBool(u8),
    /// Using a non-character `u32` as character.
    InvalidChar(u32),
    /// Using uninitialized data where it is not allowed.
    InvalidUndefBytes(Option<Pointer>),
    /// Working with a local that is not currently live.
    DeadLocal,
    /// Trying to read from the return place of a function.
    ReadFromReturnPlace,
}

impl fmt::Debug for UndefinedBehaviorInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use UndefinedBehaviorInfo::*;
        match self {
            Ub(msg) => write!(f, "{}", msg),
            Unreachable => write!(f, "entering unreachable code"),
            InvalidDiscriminant(val) => write!(f, "encountering invalid enum discriminant {}", val),
            BoundsCheckFailed { ref len, ref index } => write!(
                f,
                "indexing out of bounds: the len is {:?} but the index is {:?}",
                len, index
            ),
            DivisionByZero => write!(f, "dividing by zero"),
            RemainderByZero => write!(f, "calculating the remainder with a divisor of zero"),
            PointerArithOverflow => write!(f, "overflowing in-bounds pointer arithmetic"),
            InvalidMeta(msg) => write!(f, "invalid metadata in wide pointer: {}", msg),
            UnterminatedCString(p) => write!(
                f,
                "reading a null-terminated string starting at {:?} with no null found before end of allocation",
                p,
            ),
            PointerUseAfterFree(a) => {
                write!(f, "pointer to {:?} was dereferenced after this allocation got freed", a)
            }
            PointerOutOfBounds { ptr, msg, allocation_size } => write!(
                f,
                "{} failed: pointer must be in-bounds at offset {}, \
                           but is outside bounds of {} which has size {}",
                msg,
                ptr.offset.bytes(),
                ptr.alloc_id,
                allocation_size.bytes()
            ),
            InvalidIntPointerUsage(0) => write!(f, "invalid use of NULL pointer"),
            InvalidIntPointerUsage(i) => write!(f, "invalid use of {} as a pointer", i),
            AlignmentCheckFailed { required, has } => write!(
                f,
                "accessing memory with alignment {}, but alignment {} is required",
                has.bytes(),
                required.bytes()
            ),
            WriteToReadOnly(a) => write!(f, "writing to {:?} which is read-only", a),
            InvalidFunctionPointer(p) => {
                write!(f, "using {:?} as function pointer but it does not point to a function", p)
            }
            DerefFunctionPointer(a) => write!(f, "accessing {:?} which contains a function", a),
            ValidationFailure(ref err) => write!(f, "type validation failed: {}", err),
            InvalidBool(b) => write!(f, "interpreting an invalid 8-bit value as a bool: {}", b),
            InvalidChar(c) => write!(f, "interpreting an invalid 32-bit value as a char: {}", c),
            InvalidUndefBytes(Some(p)) => write!(
                f,
                "reading uninitialized memory at {:?}, but this operation requires initialized memory",
                p
            ),
            InvalidUndefBytes(None) => write!(
                f,
                "using uninitialized data, but this operation requires initialized memory"
            ),
            DeadLocal => write!(f, "accessing a dead local variable"),
            ReadFromReturnPlace => write!(f, "tried to read from the return place"),
        }
    }
}

/// Error information for when the program did something that might (or might not) be correct
/// to do according to the Rust spec, but due to limitations in the interpreter, the
/// operation could not be carried out. These limitations can differ between CTFE and the
/// Miri engine, e.g., CTFE does not support casting pointers to "real" integers.
///
/// Currently, we also use this as fall-back error kind for errors that have not been
/// categorized yet.
pub enum UnsupportedOpInfo {
    /// Free-form case. Only for errors that are never caught!
    Unsupported(String),
    /// Accessing an unsupported foreign static.
    ReadForeignStatic(DefId),
    /// Could not find MIR for a function.
    NoMirFor(DefId),
    /// Encountered a pointer where we needed raw bytes.
    ReadPointerAsBytes,
    /// Encountered raw bytes where we needed a pointer.
    ReadBytesAsPointer,
}

impl fmt::Debug for UnsupportedOpInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use UnsupportedOpInfo::*;
        match self {
            Unsupported(ref msg) => write!(f, "{}", msg),
            ReadForeignStatic(did) => {
                write!(f, "tried to read from foreign (extern) static {:?}", did)
            }
            NoMirFor(did) => write!(f, "could not load MIR for {:?}", did),
            ReadPointerAsBytes => write!(f, "unable to turn pointer into raw bytes",),
            ReadBytesAsPointer => write!(f, "unable to turn bytes into a pointer"),
        }
    }
}

/// Error information for when the program exhausted the resources granted to it
/// by the interpreter.
pub enum ResourceExhaustionInfo {
    /// The stack grew too big.
    StackFrameLimitReached,
    /// The program ran for too long.
    ///
    /// The exact limit is set by the `const_eval_limit` attribute.
    StepLimitReached,
}

impl fmt::Debug for ResourceExhaustionInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ResourceExhaustionInfo::*;
        match self {
            StackFrameLimitReached => {
                write!(f, "reached the configured maximum number of stack frames")
            }
            StepLimitReached => {
                write!(f, "exceeded interpreter step limit (see `#[const_eval_limit]`)")
            }
        }
    }
}

/// A trait to work around not having trait object upcasting.
pub trait AsAny: Any {
    fn as_any(&self) -> &dyn Any;
}

impl<T: Any> AsAny for T {
    #[inline(always)]
    fn as_any(&self) -> &dyn Any {
        self
    }
}

/// A trait for machine-specific errors (or other "machine stop" conditions).
pub trait MachineStopType: AsAny + fmt::Debug + Send {}
impl MachineStopType for String {}

impl dyn MachineStopType {
    #[inline(always)]
    pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
        self.as_any().downcast_ref()
    }
}

pub enum InterpError<'tcx> {
    /// The program caused undefined behavior.
    UndefinedBehavior(UndefinedBehaviorInfo),
    /// The program did something the interpreter does not support (some of these *might* be UB
    /// but the interpreter is not sure).
    Unsupported(UnsupportedOpInfo),
    /// The program was invalid (ill-typed, bad MIR, not sufficiently monomorphized, ...).
    InvalidProgram(InvalidProgramInfo<'tcx>),
    /// The program exhausted the interpreter's resources (stack/heap too big,
    /// execution takes too long, ...).
    ResourceExhaustion(ResourceExhaustionInfo),
    /// Stop execution for a machine-controlled reason. This is never raised by
    /// the core engine itself.
    MachineStop(Box<dyn MachineStopType>),
}

pub type InterpResult<'tcx, T = ()> = Result<T, ErrorAndBacktrace<InterpError<'tcx>>>;

impl fmt::Display for InterpError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Forward `Display` to `Debug`.
        fmt::Debug::fmt(self, f)
    }
}

impl fmt::Debug for InterpError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use InterpError::*;
        match *self {
            Unsupported(ref msg) => write!(f, "{:?}", msg),
            InvalidProgram(ref msg) => write!(f, "{:?}", msg),
            UndefinedBehavior(ref msg) => write!(f, "{:?}", msg),
            ResourceExhaustion(ref msg) => write!(f, "{:?}", msg),
            MachineStop(ref msg) => write!(f, "{:?}", msg),
        }
    }
}

impl InterpError<'_> {
    /// Some errors allocate to be created as they contain free-form strings.
    /// And sometimes we want to be sure that did not happen as it is a
    /// waste of resources.
    pub fn allocates(&self) -> bool {
        match self {
            // Zero-sized boxes do not allocate.
            InterpError::MachineStop(b) => mem::size_of_val::<dyn MachineStopType>(&**b) > 0,
            InterpError::Unsupported(UnsupportedOpInfo::Unsupported(_))
            | InterpError::UndefinedBehavior(UndefinedBehaviorInfo::ValidationFailure(_))
            | InterpError::UndefinedBehavior(UndefinedBehaviorInfo::Ub(_)) => true,
            _ => false,
        }
    }
}
