use crate::llvm;
use crate::context::CodegenCx;

use rustc::hir;
use rustc_codegen_ssa::traits::*;

use std::ffi::CString;

impl AsmMethods for CodegenCx<'ll, 'tcx> {
    fn codegen_global_asm(&self, ga: &hir::GlobalAsm) {
        let asm = CString::new(ga.asm.as_str().as_bytes()).unwrap();
        unsafe {
            llvm::LLVMRustAppendModuleInlineAsm(self.llmod, asm.as_ptr());
        }
    }
}
