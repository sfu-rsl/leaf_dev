use std::collections::HashMap;

use rustc_hir::{def::DefKind, def_id::DefId, definitions::DisambiguatedDefPathData};
use rustc_middle::ty::{Ty, TyCtxt};

pub(crate) struct FunctionInfo<'tcx> {
    pub def_id: DefId,
    pub ret_ty: Ty<'tcx>,
}

/// Contains types that are used in PRI functions along with primitive types.
pub(crate) struct PriTypes<'tcx> {
    pub place_ref: Ty<'tcx>,
    pub operand_ref: Ty<'tcx>,
    pub binary_op: Ty<'tcx>,
    pub unary_op: Ty<'tcx>,
    pub raw_ptr: Ty<'tcx>,
}

pub(crate) struct PriHelperFunctions<'tcx> {
    pub f32_to_bits: DefId,
    pub f64_to_bits: DefId,
    #[cfg(place_addr)]
    pub type_id_of: FunctionInfo<'tcx>,
    _phantom: std::marker::PhantomData<&'tcx ()>,
}

pub(crate) fn find_pri_exported_symbols(tcx: TyCtxt) -> Vec<DefId> {
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

pub(crate) fn find_pri_funcs<'tcx>(
    pri_symbols: &[DefId],
    tcx: TyCtxt<'tcx>,
) -> HashMap<String, FunctionInfo<'tcx>> {
    pri_symbols
        .filter_by_marker(tcx, stringify!(runtime::pri::MODULE_MARKER), false)
        .into_iter()
        .filter(|def_id| matches!(tcx.def_kind(def_id), DefKind::Fn | DefKind::AssocFn))
        .map(|def_id| (tcx.def_path_str(def_id), func_info_from(tcx, def_id)))
        .collect()
}

macro_rules! helper_item_name {
    ($name:ident) => {
        &crate::pri_utils::normalize_str_path(&stringify!(runtime::pri::compiler_helpers::$name))
    };
}
pub(crate) use helper_item_name;

#[inline]
pub(crate) fn normalize_str_path(name: &str) -> String {
    name.replace(' ', "")
}

pub(crate) fn find_pri_types<'tcx>(pri_symbols: &[DefId], tcx: TyCtxt<'tcx>) -> PriTypes<'tcx> {
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
        tcx.type_of(def_ids.get(&normalize_str_path(name)).unwrap())
            .no_bound_vars()
            .expect("PRI types are not expected to have bound vars (generics).")
    };

    PriTypes {
        place_ref: get_ty(helper_item_name!(PLACE_REF_TYPE_HOLDER)),
        operand_ref: get_ty(helper_item_name!(OPERAND_REF_TYPE_HOLDER)),
        binary_op: get_ty(helper_item_name!(BINARY_OP_TYPE_HOLDER)),
        unary_op: get_ty(helper_item_name!(UNARY_OP_TYPE_HOLDER)),
        raw_ptr: get_ty(helper_item_name!(RAW_PTR_TYPE_HOLDER)),
    }
}

pub(crate) fn find_helper_funcs<'tcx>(
    pri_symbols: &[DefId],
    tcx: TyCtxt<'tcx>,
) -> PriHelperFunctions<'tcx> {
    let def_ids: HashMap<String, DefId> = pri_symbols
        .filter_by_marker(tcx, helper_item_name!(MODULE_MARKER), false)
        .into_iter()
        .filter(|def_id| matches!(tcx.def_kind(def_id), DefKind::Fn | DefKind::AssocFn))
        .map(|def_id| (tcx.def_path_str(def_id), def_id))
        .collect();

    PriHelperFunctions {
        f32_to_bits: *def_ids.get(helper_item_name!(f32_to_bits)).unwrap(),
        f64_to_bits: *def_ids.get(helper_item_name!(f64_to_bits)).unwrap(),
        #[cfg(place_addr)]
        type_id_of: func_info_from(
            tcx,
            *def_ids
                .get(helper_item_name!(type_id_of))
                .expect("`type_id_of` is not exported (probably erased by compiler)."),
        ),
        _phantom: std::marker::PhantomData,
    }
}

fn func_info_from(tcx: TyCtxt, def_id: DefId) -> FunctionInfo {
    FunctionInfo {
        def_id,
        ret_ty: tcx
            .fn_sig(def_id)
            .skip_binder()
            .output()
            .no_bound_vars()
            .expect(
                "PRI functions are not expected to have bound vars (generics) for the return type.",
            ),
    }
}

pub(crate) fn eq_def_path_str(tcx: TyCtxt, def_id: &DefId, def_path_str: &str) -> bool {
    tcx.def_path_str(def_id) == normalize_str_path(def_path_str)
}

fn get_module_of_marker(
    mut symbols: impl Iterator<Item = DefId>,
    tcx: TyCtxt,
    marker_name: &str,
) -> impl Iterator<Item = DisambiguatedDefPathData> {
    symbols
        .find(|def_id| eq_def_path_str(tcx, def_id, marker_name))
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
