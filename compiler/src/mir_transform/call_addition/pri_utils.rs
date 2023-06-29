use std::collections::HashMap;

use rustc_hir::{def::DefKind, def_id::DefId, definitions::DisambiguatedDefPathData};
use rustc_middle::ty::{Ty, TyCtxt};

use super::context::{FunctionInfo, HelperFunctions, SpecialTypes};

pub(super) fn find_pri_exported_symbols(tcx: TyCtxt) -> Vec<DefId> {
    use rustc_middle::middle::exported_symbols::ExportedSymbol;

    fn def_id<'a>(symbol: &'a ExportedSymbol) -> Option<&'a DefId> {
        match symbol {
            ExportedSymbol::NonGeneric(def_id) | ExportedSymbol::Generic(def_id, _) => Some(def_id),
            _ => None,
        }
    }

    // Finding the runtime crate.
    let crate_num = *tcx
        .crates(())
        .iter()
        .find(|cnum| tcx.crate_name(**cnum).as_str() == stringify!(runtime))
        .unwrap_or_else(|| {
            panic!(
                "{} crate is not added as a dependency.",
                stringify!(runtime)
            )
        });

    let runtime_symbols = tcx
        .exported_symbols(crate_num)
        .iter()
        .filter_map(|(s, _)| def_id(s))
        .cloned()
        .filter(|def_id| def_id.krate == crate_num)
        .collect::<Vec<_>>();

    runtime_symbols.filter_by_marker(tcx, stringify!(runtime::pri::MODULE_MARKER), true)
}

pub(super) fn find_pri_funcs<'tcx>(
    pri_symbols: &[DefId],
    tcx: TyCtxt<'tcx>,
) -> HashMap<String, FunctionInfo<'tcx>> {
    pri_symbols
        .filter_by_marker(tcx, stringify!(runtime::pri::MODULE_MARKER), false)
        .into_iter()
        .filter(|def_id| matches!(tcx.def_kind(def_id), DefKind::Fn | DefKind::AssocFn))
        .map(|def_id| {
            (
                tcx.def_path_str(def_id),
                FunctionInfo {
                    def_id,
                    ret_ty: tcx
                        .fn_sig(def_id)
                        .skip_binder()
                        .output()
                        .no_bound_vars()
                        .expect("PRI functions are not expected to have bound vars (generics)."),
                },
            )
        })
        .collect()
}

macro_rules! helper_item_name {
    ($name:ident) => {
        &stringify!(runtime::pri::compiler_helpers::$name).replace(' ', "")
    };
}

pub(super) fn find_special_types<'tcx>(
    pri_symbols: &[DefId],
    tcx: TyCtxt<'tcx>,
) -> SpecialTypes<'tcx> {
    /*
     * FIXME: The desired enums and type aliases don't show up in the exported symbols.
     * It may be because of the MIR phases that clean up/optimize/unify things,
     * the way that the library is added (using the compiled file), or
     * that enums and type aliases are not included at all in the exported_symbols.
     * As a workaround, we have defined some static variables having those desired
     * types and are accessible.
     * However, there should be some functions in TyCtxt that will list these items for us.
     */
    let def_ids: HashMap<String, DefId> = pri_symbols
        .filter_by_marker(
            tcx,
            stringify!(runtime::pri::compiler_helpers::MODULE_MARKER),
            false,
        )
        .into_iter()
        .filter(|def_id| matches!(tcx.def_kind(def_id), DefKind::Static(_)))
        .map(|def_id| (tcx.def_path_str(def_id), def_id))
        .collect();

    let get_ty = |name: &str| -> Ty {
        tcx.type_of(def_ids.get(&name.replace(' ', "")).unwrap())
            .no_bound_vars()
            .expect("PRI special types are not expected to have bound vars (generics).")
    };

    SpecialTypes {
        place_ref: get_ty(helper_item_name!(PLACE_REF_TYPE_HOLDER)),
        operand_ref: get_ty(helper_item_name!(OPERAND_REF_TYPE_HOLDER)),
        binary_op: get_ty(helper_item_name!(BINARY_OP_TYPE_HOLDER)),
        unary_op: get_ty(helper_item_name!(UNARY_OP_TYPE_HOLDER)),
    }
}

pub(super) fn find_helper_funcs<'tcx>(pri_symbols: &[DefId], tcx: TyCtxt<'tcx>) -> HelperFunctions {
    let def_ids: HashMap<String, DefId> = pri_symbols
        .filter_by_marker(
            tcx,
            stringify!(runtime::pri::compiler_helpers::MODULE_MARKER),
            false,
        )
        .into_iter()
        .filter(|def_id| matches!(tcx.def_kind(def_id), DefKind::Fn | DefKind::AssocFn))
        .map(|def_id| (tcx.def_path_str(def_id), def_id))
        .collect();

    HelperFunctions {
        f32_to_bits: *def_ids.get(helper_item_name!(f32_to_bits)).unwrap(),
        f64_to_bits: *def_ids.get(helper_item_name!(f64_to_bits)).unwrap(),
    }
}

fn get_module_of_marker(
    mut symbols: impl Iterator<Item = DefId>,
    tcx: TyCtxt,
    marker_name: &str,
) -> impl Iterator<Item = DisambiguatedDefPathData> {
    symbols
        .find(|def_id| tcx.def_path_str(def_id) == marker_name.replace(' ', ""))
        .map(|marker_id| module_of(tcx, &marker_id))
        .unwrap_or_else(|| panic!("Could not find the marker symbol. {marker_name}"))
}

fn module_of(tcx: TyCtxt, def_id: &DefId) -> impl Iterator<Item = DisambiguatedDefPathData> {
    use rustc_hir::definitions::DefPathData;
    tcx.def_path(*def_id)
        .data
        .into_iter()
        .take_while(|p| matches!(p.data, DefPathData::TypeNs(_)))
}

trait DefIdSliceExt {
    fn filter_by_marker(self, tcx: TyCtxt, marker_name: &str, submodules: bool) -> Vec<DefId>;
}

impl DefIdSliceExt for &[DefId] {
    fn filter_by_marker(self, tcx: TyCtxt, marker_name: &str, submodules: bool) -> Vec<DefId> {
        let marker_module =
            get_module_of_marker(self.iter().cloned(), tcx, marker_name).collect::<Vec<_>>();
        self.iter()
            .filter(|def_id| {
                let module = module_of(tcx, def_id);
                if submodules {
                    module
                        .take(marker_module.len())
                        .eq_by(&marker_module, |a, b| &a == b)
                } else {
                    module.eq_by(&marker_module, |a, b| &a == b)
                }
            })
            .cloned()
            .collect()
    }
}
