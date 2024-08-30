#[derive(Default)]
pub(crate) struct Chain<A, B> {
    pub first: A,
    pub second: B,
}

/* NOTE: The trailing comma is mandatory for this macro.
 * Also note that a kind of expression is path, so it is important to
 * distinguish them in the pattern.
 */
macro_rules! chain {
    (<$head:path>, $($tail:tt)*) => {
        crate::utils::chain!((<$head>::default()), $($tail)*)
    };
    ($single:expr,) => {
        $single
    };
    ($head:expr, $($tail:tt)+) => {
        crate::utils::Chain {
            first: $head,
            second: crate::utils::chain!($($tail)+)
        }
    };
}

pub(crate) use chain;

pub(crate) mod mir {
    use rustc_hir::definitions::DisambiguatedDefPathData;
    use rustc_middle::ty::TyCtxt;
    use rustc_span::def_id::DefId;

    pub(crate) trait TyCtxtExt<'tcx> {
        fn is_llvm_intrinsic(self, def_id: DefId) -> bool;
        fn module_of(self, def_id: DefId) -> impl Iterator<Item = DisambiguatedDefPathData>;
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

        fn module_of(self, def_id: DefId) -> impl Iterator<Item = DisambiguatedDefPathData> {
            use rustc_hir::definitions::DefPathData;
            self.def_path(def_id)
                .data
                .into_iter()
                .take_while(|p| matches!(p.data, DefPathData::TypeNs(_)))
        }
    }
}
