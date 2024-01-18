use std::collections::HashMap;

use rustc_hir::{def::DefKind, def_id::DefId, definitions::DisambiguatedDefPathData};
use rustc_middle::ty::{Ty, TyCtxt};

pub mod sym {
    #![allow(non_upper_case_globals)]

    #[derive(Clone, Copy, derive_more::Deref, Debug)]
    #[repr(transparent)]
    pub struct LeafSymbol(&'static str);

    use const_format::concatcp;
    use LeafSymbol as LS;

    pub const RUNTIME_LIB_CRATE: LS = LS(crate::constants::CRATE_RUNTIME);

    macro_rules! in_lib {
        ($name: ident) => {
            concatcp!(RUNTIME_LIB_CRATE.0, "::", stringify!($name))
        };
    }
    macro_rules! symbols_in_lib {
        ($($name: ident),* $(,)?) => {
            $(pub const $name: LS = LS(in_lib!($name));)*
        };
    }

    symbols_in_lib! {
        pri,
    }

    macro_rules! in_pri {
        ($name: ident) => {
            concatcp!(pri.0, "::", stringify!($name))
        };
    }
    macro_rules! symbols_in_pri {
        ($($name: ident),* $(,)?) => {
            #[allow(dead_code)]
            $(pub const $name: LS = LS(in_pri!($name));)*
        };
    }

    symbols_in_pri! {
        MODULE_MARKER,
        compiler_helpers,
    }

    macro_rules! fn_name {
        ($(#[$($attr: meta)*])* fn $name:ident ($($(#[$($arg_attr: meta)*])* $arg:ident : $arg_type:ty),* $(,)?) $(-> $ret_ty:ty)?;) => {
            symbols_in_pri!($name);
        };
    }
    common::pri::list_func_decls!(modifier: fn_name);

    macro_rules! in_compiler_helpers {
        ($name: ident) => {
            concatcp!(compiler_helpers.0, "::", stringify!($name))
        };
    }
    macro_rules! symbols_in_compiler_helpers {
        ($($name: ident),* $(,)?) => {
            $(pub const $name: LS = LS(in_compiler_helpers!($name));)*
        };
    }

    symbols_in_compiler_helpers! {
        CH_MODULE_MARKER,

        PLACE_REF_TYPE_HOLDER,
        OPERAND_REF_TYPE_HOLDER,
        BINARY_OP_TYPE_HOLDER,
        UNARY_OP_TYPE_HOLDER,
        RAW_PTR_TYPE_HOLDER,
        FUNC_ID_TYPE_HOLDER,

        f32_to_bits,
        f64_to_bits,

        mark_as_nctfe,

        set_place_address_typed,
        type_id_of,
        size_of,

        const_binary_op_of,
        const_unary_op_of,
    }
}

#[derive(Clone, Copy, Debug)]
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
    pub func_id: Ty<'tcx>,
}

pub(crate) struct PriHelperFunctions<'tcx> {
    pub f32_to_bits: DefId,
    pub f64_to_bits: DefId,
    pub set_place_address_typed: FunctionInfo<'tcx>,
    pub type_id_of: FunctionInfo<'tcx>,
    pub size_of: FunctionInfo<'tcx>,
    pub const_binary_op_of: FunctionInfo<'tcx>,
    pub const_unary_op_of: FunctionInfo<'tcx>,
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
        .find(|cnum| tcx.crate_name(**cnum).as_str() == *sym::RUNTIME_LIB_CRATE)
        .unwrap_or_else(|| {
            panic!(
                "{} crate is not added as a dependency.",
                *sym::RUNTIME_LIB_CRATE
            )
        });

    let runtime_symbols = tcx
        .exported_symbols(crate_num)
        .iter()
        .filter_map(|(s, _)| def_id(s))
        .cloned()
        .filter(|def_id| def_id.krate == crate_num)
        .collect::<Vec<_>>();

    runtime_symbols.filter_by_marker(tcx, *sym::MODULE_MARKER, true)
}

pub(crate) fn find_pri_funcs<'tcx>(
    pri_symbols: &[DefId],
    tcx: TyCtxt<'tcx>,
) -> HashMap<String, FunctionInfo<'tcx>> {
    pri_symbols
        .filter_by_marker(tcx, *sym::MODULE_MARKER, false)
        .into_iter()
        .filter(|def_id| matches!(tcx.def_kind(def_id), DefKind::Fn | DefKind::AssocFn))
        .map(|def_id| (tcx.def_path_str(def_id), func_info_from(tcx, def_id)))
        .collect()
}

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
        .filter_by_marker(tcx, *sym::CH_MODULE_MARKER, false)
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
        place_ref: get_ty(*sym::PLACE_REF_TYPE_HOLDER),
        operand_ref: get_ty(*sym::OPERAND_REF_TYPE_HOLDER),
        binary_op: get_ty(*sym::BINARY_OP_TYPE_HOLDER),
        unary_op: get_ty(*sym::UNARY_OP_TYPE_HOLDER),
        raw_ptr: get_ty(*sym::RAW_PTR_TYPE_HOLDER),
        func_id: get_ty(*sym::FUNC_ID_TYPE_HOLDER),
    }
}

pub(crate) fn find_helper_funcs<'tcx>(
    pri_symbols: &[DefId],
    tcx: TyCtxt<'tcx>,
) -> PriHelperFunctions<'tcx> {
    let def_ids: HashMap<String, DefId> = pri_symbols
        .filter_by_marker(tcx, *sym::CH_MODULE_MARKER, false)
        .into_iter()
        .filter(|def_id| matches!(tcx.def_kind(def_id), DefKind::Fn | DefKind::AssocFn))
        .map(|def_id| (tcx.def_path_str(def_id), def_id))
        .collect();

    let get_func_info = |name: &str| {
        func_info_from(
            tcx,
            *def_ids.get(name).unwrap_or_else(|| {
                panic!("`{}` is not exported (probably erased by compiler).", name);
            }),
        )
    };

    PriHelperFunctions {
        f32_to_bits: *def_ids.get(*sym::f32_to_bits).unwrap(),
        f64_to_bits: *def_ids.get(*sym::f64_to_bits).unwrap(),
        set_place_address_typed: get_func_info(*sym::set_place_address_typed),
        type_id_of: get_func_info(*sym::type_id_of),
        size_of: get_func_info(*sym::size_of),
        const_binary_op_of: get_func_info(*sym::const_binary_op_of),
        const_unary_op_of: get_func_info(*sym::const_unary_op_of),
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
