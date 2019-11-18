use rustc::hir::GlobalAsm;

pub trait AsmMethods {
    fn codegen_global_asm(&self, ga: &GlobalAsm);
}
