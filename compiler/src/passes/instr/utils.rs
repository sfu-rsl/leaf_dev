use rustc_middle::ty::TyCtxt;
use rustc_span::def_id::DefId;

pub(crate) trait TyCtxtExt<'tcx> {
    fn is_llvm_intrinsic(self, def_id: DefId) -> bool;
}

impl<'tcx> TyCtxtExt<'tcx> for TyCtxt<'tcx> {
    fn is_llvm_intrinsic(self, def_id: DefId) -> bool {
        // Grabbed from `is_call_from_compiler_builtins_to_upstream_monomorphization`#5aea140.
        if let Some(name) = self.codegen_fn_attrs(def_id).link_name {
            name.as_str().starts_with("llvm.")
        } else {
            false
        }
    }
}
