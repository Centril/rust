use rustc_errors::DiagnosticBuilder;
use rustc_hir::definitions::DefPathData;
use rustc_hir::HirId;
use rustc_middle::mir::interpret::{struct_error, ErrorHandled, InterpError};
use rustc_middle::ty::layout::LayoutError;
use rustc_middle::ty::{self, query::TyCtxtAt};
use rustc_span::{Pos, Span};

use std::fmt;

#[derive(Debug)]
pub struct ConstEvalErr<'tcx> {
    pub span: Span,
    pub error: InterpError<'tcx>,
    pub stacktrace: Vec<FrameInfo<'tcx>>,
}

#[derive(Debug)]
pub struct FrameInfo<'tcx> {
    pub instance: ty::Instance<'tcx>,
    pub span: Span,
    pub lint_root: Option<HirId>,
}

impl<'tcx> fmt::Display for FrameInfo<'tcx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ty::tls::with(|tcx| {
            if tcx.def_key(self.instance.def_id()).disambiguated_data.data
                == DefPathData::ClosureExpr
            {
                write!(f, "inside closure")?;
            } else {
                write!(f, "inside `{}`", self.instance)?;
            }
            if !self.span.is_dummy() {
                let lo = tcx.sess.source_map().lookup_char_pos(self.span.lo());
                write!(f, " at {}:{}:{}", lo.file.name, lo.line, lo.col.to_usize() + 1)?;
            }
            Ok(())
        })
    }
}

impl<'tcx> ConstEvalErr<'tcx> {
    pub fn struct_error(
        &self,
        tcx: TyCtxtAt<'tcx>,
        message: &str,
        emit: impl FnOnce(DiagnosticBuilder<'_>),
    ) -> Result<(), ErrorHandled> {
        self.struct_generic(tcx, message, emit, None)
    }

    pub fn report_as_error(&self, tcx: TyCtxtAt<'tcx>, message: &str) -> ErrorHandled {
        match self.struct_error(tcx, message, |mut e| e.emit()) {
            Ok(_) => ErrorHandled::Reported,
            Err(x) => x,
        }
    }

    pub fn report_as_lint(
        &self,
        tcx: TyCtxtAt<'tcx>,
        message: &str,
        lint_root: HirId,
        span: Option<Span>,
    ) -> ErrorHandled {
        match self.struct_generic(
            tcx,
            message,
            |mut lint: DiagnosticBuilder<'_>| {
                // Apply the span.
                if let Some(span) = span {
                    let primary_spans = lint.span.primary_spans().to_vec();
                    // point at the actual error as the primary span
                    lint.replace_span_with(span);
                    // point to the `const` statement as a secondary span
                    // they don't have any label
                    for sp in primary_spans {
                        if sp != span {
                            lint.span_label(sp, "");
                        }
                    }
                }
                lint.emit();
            },
            Some(lint_root),
        ) {
            Ok(_) => ErrorHandled::Reported,
            Err(err) => err,
        }
    }

    /// Create a diagnostic for this const eval error.
    ///
    /// Sets the message passed in via `message` and adds span labels with detailed error
    /// information before handing control back to `emit` to do any final processing.
    /// It's the caller's responsibility to call emit(), stash(), etc. within the `emit`
    /// function to dispose of the diagnostic properly.
    ///
    /// If `lint_root.is_some()` report it as a lint, else report it as a hard error.
    /// (Except that for some errors, we ignore all that -- see `must_error` below.)
    fn struct_generic(
        &self,
        tcx: TyCtxtAt<'tcx>,
        message: &str,
        emit: impl FnOnce(DiagnosticBuilder<'_>),
        lint_root: Option<HirId>,
    ) -> Result<(), ErrorHandled> {
        let must_error = match self.error {
            err_inval!(Layout(LayoutError::Unknown(_))) | err_inval!(TooGeneric) => {
                return Err(ErrorHandled::TooGeneric);
            }
            err_inval!(TypeckError) => return Err(ErrorHandled::Reported),
            // We must *always* hard error on these, even if the caller wants just a lint.
            err_inval!(Layout(LayoutError::SizeOverflow(_))) => true,
            _ => false,
        };
        trace!("reporting const eval failure at {:?}", self.span);

        let err_msg = match &self.error {
            InterpError::MachineStop(msg) => {
                // A custom error (`ConstEvalErrKind` in `librustc_mir/interp/const_eval/error.rs`).
                // Should be turned into a string by now.
                msg.downcast_ref::<String>().expect("invalid MachineStop payload").clone()
            }
            err => err.to_string(),
        };

        let finish = |mut err: DiagnosticBuilder<'_>, span_msg: Option<String>| {
            if let Some(span_msg) = span_msg {
                err.span_label(self.span, span_msg);
            }
            // Add spans for the stacktrace. Don't print a single-line backtrace though.
            if self.stacktrace.len() > 1 {
                for frame_info in &self.stacktrace {
                    err.span_label(frame_info.span, frame_info.to_string());
                }
            }
            // Let the caller finish the job.
            emit(err)
        };

        if must_error {
            // The `message` makes little sense here, this is a more serious error than the
            // caller thinks anyway.
            // See <https://github.com/rust-lang/rust/pull/63152>.
            finish(struct_error(tcx, &err_msg), None);
        } else {
            // Regular case.
            if let Some(lint_root) = lint_root {
                // Report as lint.
                let hir_id = self
                    .stacktrace
                    .iter()
                    .rev()
                    .filter_map(|frame| frame.lint_root)
                    .next()
                    .unwrap_or(lint_root);
                tcx.struct_span_lint_hir(
                    rustc_session::lint::builtin::CONST_ERR,
                    hir_id,
                    tcx.span,
                    |lint| finish(lint.build(message), Some(err_msg)),
                );
            } else {
                // Report as hard error.
                finish(struct_error(tcx, message), Some(err_msg));
            }
        }
        Ok(())
    }
}
