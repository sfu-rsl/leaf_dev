use std::collections::{HashMap, HashSet};

use rustc_hir::{
    def::DefKind,
    def_id::{CrateNum, DefId, LOCAL_CRATE},
};
use rustc_middle::ty::{Ty, TyCtxt};

pub(super) const TAG_DISCOVERY: &str = "pri_discovery";

pub mod sym {
    #![allow(non_upper_case_globals)]

    use derive_more as dm;

    #[derive(Clone, Copy, dm::Deref, Debug, dm::Display, Hash, PartialEq, Eq, PartialOrd, Ord)]
    #[repr(transparent)]
    pub struct LeafSymbol(&'static str);

    use const_format::concatcp;
    use LeafSymbol as LS;

    pub const CORE_LIB_CRATE: LS = LS("core");
    pub const RUNTIME_LIB_CRATE: LS = LS(crate::constants::CRATE_RUNTIME);

    macro_rules! in_lib {
        ($name: ident) => {
            concatcp!(RUNTIME_LIB_CRATE.0, "::", stringify!($name))
        };
    }
    macro_rules! symbols_in_lib {
        ($($name: ident),* $(,)?) => {
            $(pub(crate) const $name: LS = LS(in_lib!($name));)*
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
            $(pub(crate) const $name: LS = LS(in_pri!($name));)*
        };
    }

    symbols_in_pri! {
        MODULE_MARKER,
        compiler_helpers,
    }

    mod mains {
        use super::*;

        common::pri::pass_func_names_to!(symbols_in_pri, all_comma_separated);

        macro_rules! bracket {
            ($($name: ident),*) => {
                [
                    $($name),*
                ]
            };
        }
        pub(crate) const ALL_MAINS: [LeafSymbol; 85] =
            common::pri::pass_func_names_to!(bracket, all_comma_separated);
    }
    pub(crate) use mains::*;

    mod compiler_helpers {
        use super::*;

        macro_rules! in_compiler_helpers {
            ($name: ident) => {
                concatcp!(compiler_helpers.0, "::", stringify!($name))
            };
        }
        macro_rules! symbols_in_compiler_helpers {
            ($($name: ident),* $(,)?) => {
                $(pub(crate) const $name: LS = LS(in_compiler_helpers!($name));)*
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

        pub(crate) const ALL_HELPERS: [LS; 15] = [
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
        ];
    }
    pub(crate) use compiler_helpers::*;

    impl TryFrom<String> for LeafSymbol {
        type Error = String;

        fn try_from(s: String) -> Result<Self, Self::Error> {
            ALL_MAINS
                .iter()
                .chain(ALL_HELPERS.iter())
                .find(|sym| sym.0 == s)
                .copied()
                .ok_or(s)
        }
    }
}

use sym::LeafSymbol;

#[derive(Clone, Copy, Debug, derive_more::From, derive_more::Deref)]
pub(crate) struct FunctionInfo {
    pub def_id: DefId,
}

impl FunctionInfo {
    pub(crate) fn ret_ty<'tcx>(&self, tcx: TyCtxt<'tcx>) -> Ty<'tcx> {
        // FIXME: Check if additional caching can be beneficial
        tcx.fn_sig(self.def_id)
            .skip_binder()
            .output()
            .no_bound_vars()
            .expect(
                "PRI functions are not expected to have bound vars (generics) for the return type.",
            )
    }
}

struct TypeHolder(DefId);

impl TypeHolder {
    pub(crate) fn ty<'tcx>(&self, tcx: TyCtxt<'tcx>) -> Ty<'tcx> {
        tcx.type_of(self.0)
            .no_bound_vars()
            .expect("PRI types are not expected to have bound vars.")
    }
}

/// Provides types that are used in PRI functions along with primitive types.
pub(crate) struct PriTypes {
    place_ref: TypeHolder,
    operand_ref: TypeHolder,
    binary_op: TypeHolder,
    unary_op: TypeHolder,
    raw_ptr: TypeHolder,
    func_id: TypeHolder,
}

impl PriTypes {
    pub(crate) fn operand_ref<'tcx>(&self, tcx: TyCtxt<'tcx>) -> Ty<'tcx> {
        // FIXME: Check if additional caching can be beneficial
        self.operand_ref.ty(tcx)
    }

    pub(crate) fn func_id<'tcx>(&self, tcx: TyCtxt<'tcx>) -> Ty<'tcx> {
        self.func_id.ty(tcx)
    }
}

pub(crate) struct PriHelperFunctions {
    pub f32_to_bits: DefId,
    pub f64_to_bits: DefId,
    pub set_place_address_typed: FunctionInfo,
    pub type_id_of: FunctionInfo,
    pub size_of: FunctionInfo,
    pub const_binary_op_of: FunctionInfo,
    pub const_unary_op_of: FunctionInfo,
}

/// Lists all the PRI items, including the compiler helper items.
pub(crate) fn all_pri_items(tcx: TyCtxt) -> Vec<DefId> {
    let crate_num = find_pri_host_crate(tcx);
    let marker_id = find_pri_marker(tcx, crate_num);
    let result = collect_all_pri_items(tcx, marker_id);
    log::debug!(
        target: TAG_DISCOVERY,
        "Found {} PRI items.",
        result.len(),
    );
    result
}

/// Finds the crate that holds the PRI items.
/// # Remarks
/// Currently, it is possible to have access to the PRI either as a part of
/// the core library or as an external crate linked to the target program.
/// This returns either crate that should be searched for the PRI items.
/// When building the core library, the current crate is returned.
fn find_pri_host_crate(tcx: TyCtxt) -> CrateNum {
    let building_core = tcx.crate_name(LOCAL_CRATE).as_str() == *sym::CORE_LIB_CRATE;

    tcx.crate_by_name(*sym::RUNTIME_LIB_CRATE)
        .or_else(|| tcx.crate_by_name(*sym::CORE_LIB_CRATE))
        .or_else(|| {
            if building_core {
                Some(LOCAL_CRATE)
            } else {
                None
            }
        })
        .inspect(|cnum| {
            log::info!(
                target: TAG_DISCOVERY,
                "Selected crate {}, to search for PRI symbols.",
                tcx.crate_name(*cnum).as_str().to_string()
            );
        })
        .expect("Could not find the expected crates hosting PRI symbols.")
}

/// Finds `DefId` of the PRI module's marker.
/// # Remarks
/// The marker is a dummy static variable residing in the PRI module.
/// As we usually search through a set of definitions (and not modules), to find
/// the PRI module/items, we use this workaround rather than looking for the
/// module directly.
fn find_pri_marker(tcx: TyCtxt, crate_num: CrateNum) -> DefId {
    log::debug!(
        target: TAG_DISCOVERY,
        "Searching for the PRI module marker in crate {}.",
        tcx.crate_name(crate_num).as_str(),
    );
    let search_space: Box<dyn Iterator<Item = DefId>> = if crate_num == LOCAL_CRATE {
        Box::new(tcx.mir_keys(()).iter().map(|id| id.to_def_id()))
    } else {
        use rustc_middle::middle::exported_symbols::ExportedSymbol;
        Box::new(
            tcx.exported_symbols(crate_num)
                .iter()
                .filter_map(|(s, _)| match s {
                    ExportedSymbol::NonGeneric(def_id) | ExportedSymbol::Generic(def_id, _) => {
                        Some(*def_id)
                    }
                    _ => None,
                }),
        )
    };

    search_space
        .find_by_name(tcx, *sym::MODULE_MARKER)
        .unwrap_or_else(|| {
            panic!(
                "Could not find the PRI module marker in the given crate: {}:{}.",
                crate_num,
                tcx.crate_name(crate_num).as_str(),
            )
        })
}

/// Collects all the PRI items with respect to the marker.
/// # Returns
/// The list of `DefId`s existing in the same module or submodules of the parent
/// module of the marker.
fn collect_all_pri_items<'tcx>(tcx: TyCtxt<'tcx>, module_marker_id: DefId) -> Vec<DefId> {
    log::debug!(
        target: TAG_DISCOVERY,
        "Collecting all PRI items in the same module or sibling modules of {:?}.",
        module_marker_id,
    );
    if module_marker_id.krate == LOCAL_CRATE {
        /* NOTE: `module_children` panics for local ids,
         * and `module_children_local` doesn't return items. */
        tcx.mir_keys(())
            .iter()
            .map(|id| id.to_def_id())
            .filter_by_module_marker(tcx, module_marker_id, true)
            .collect()
    } else {
        tcx.module_children_rec(tcx.parent(module_marker_id), true)
    }
}

/// Filters out the top-level functions out of the list of all PRI items.
/// # Remarks
/// The functions are the main ones that record the program behavior,
/// i.e. the ones in [`common::pri::ProgramRuntimeInterface`].
/// These items are recognized as being on the same hierarchical level as the marker item.
/// At the moment, the items that are not selected are compiler helpers.
pub(crate) fn filter_main_funcs<'tcx>(
    tcx: TyCtxt<'tcx>,
    all_pri_items: &[DefId],
) -> HashMap<LeafSymbol, FunctionInfo> {
    let items = filter_pri_items(tcx, all_pri_items, sym::MODULE_MARKER)
        .filter(|def_id| matches!(tcx.def_kind(def_id), DefKind::Fn | DefKind::AssocFn))
        .inspect(|def_id| {
            log::debug!(
                target: TAG_DISCOVERY,
                "Found PRI function: {:?}",
                def_id,
            );
        })
        .filter_associate_with_symbol(tcx)
        .map(|(s, id)| (s, id.into()))
        .collect::<HashMap<_, _>>();
    debug_assert_eq!(
        items.len(),
        sym::ALL_MAINS.len(),
        "Some main functions are missing: {:?}",
        &HashSet::from(sym::ALL_MAINS) - &HashSet::from_iter(items.into_keys())
    );
    items
}

/// Filters out the compiler helper items out of the list of all PRI items.
pub(crate) fn filter_helper_items<'tcx>(
    tcx: TyCtxt<'tcx>,
    all_pri_items: &[DefId],
) -> HashMap<LeafSymbol, DefId> {
    filter_pri_items(tcx, all_pri_items, sym::CH_MODULE_MARKER)
        .filter_associate_with_symbol(tcx)
        .collect()
}

/// Collects the helper type holders out of the list of compiler helper PRI items.
pub(crate) fn collect_helper_types<'tcx>(helper_def_ids: &HashMap<LeafSymbol, DefId>) -> PriTypes {
    /* FIXME: The desired enums and type aliases don't show up in the exported symbols.
     * It may be because of the MIR phases that clean up/optimize/unify things,
     * the way that the library is added (using the compiled file), or
     * that enums and type aliases are not included at all in the exported_symbols.
     * As a workaround, we have defined some static variables having those desired
     * types and are accessible.
     * However, there should be some functions in TyCtxt that will list these items for us.
     * Update: Check if the problem still exists with the introduction of `module_children`.
     */

    let get_type_holder =
        |name: LeafSymbol| -> TypeHolder { TypeHolder(*helper_def_ids.get(&name).unwrap()) };

    PriTypes {
        place_ref: get_type_holder(sym::PLACE_REF_TYPE_HOLDER),
        operand_ref: get_type_holder(sym::OPERAND_REF_TYPE_HOLDER),
        binary_op: get_type_holder(sym::BINARY_OP_TYPE_HOLDER),
        unary_op: get_type_holder(sym::UNARY_OP_TYPE_HOLDER),
        raw_ptr: get_type_holder(sym::RAW_PTR_TYPE_HOLDER),
        func_id: get_type_holder(sym::FUNC_ID_TYPE_HOLDER),
    }
}

/// Collects the helper functions out of the list of compiler helper PRI items.
pub(crate) fn collect_helper_funcs<'tcx>(
    helper_def_ids: &HashMap<LeafSymbol, DefId>,
) -> PriHelperFunctions {
    let get_func_info = |name: LeafSymbol| {
        helper_def_ids
            .get(&name)
            .copied()
            .unwrap_or_else(|| {
                panic!("`{}` is not exported (probably erased by compiler).", name);
            })
            .into()
    };

    PriHelperFunctions {
        f32_to_bits: *helper_def_ids.get(&sym::f32_to_bits).unwrap(),
        f64_to_bits: *helper_def_ids.get(&sym::f64_to_bits).unwrap(),
        set_place_address_typed: get_func_info(sym::set_place_address_typed),
        type_id_of: get_func_info(sym::type_id_of),
        size_of: get_func_info(sym::size_of),
        const_binary_op_of: get_func_info(sym::const_binary_op_of),
        const_unary_op_of: get_func_info(sym::const_unary_op_of),
    }
}

fn filter_pri_items<'a, 'tcx: 'a>(
    tcx: TyCtxt<'tcx>,
    all_pri_items: &'a [DefId],
    module_marker: LeafSymbol,
) -> impl Iterator<Item = DefId> + 'a {
    all_pri_items
        .iter()
        .copied()
        .filter_by_sibling_module_marker_name(tcx, module_marker)
}

fn to_map_key(tcx: TyCtxt, def_id: DefId) -> String {
    let mut full_path = tcx.def_path_str(def_id);
    if let Some(start) = full_path.find(*sym::RUNTIME_LIB_CRATE) {
        full_path.split_off(start)
    } else {
        full_path
    }
}

trait DefIdIterExt<'tcx> {
    fn find_by_name(self, tcx: TyCtxt<'tcx>, name: &str) -> Option<DefId>;

    fn filter_by_sibling_module_marker_name<'a>(
        self,
        tcx: TyCtxt<'tcx>,
        marker_name: LeafSymbol,
    ) -> Box<dyn Iterator<Item = DefId> + 'a>
    where
        Self: 'a + Clone,
        'tcx: 'a;

    fn filter_by_module_marker<'a>(
        self,
        tcx: TyCtxt<'tcx>,
        marker: DefId,
        submodules: bool,
    ) -> Box<dyn Iterator<Item = DefId> + 'a>
    where
        Self: 'a,
        'tcx: 'a;

    fn filter_associate_with_symbol<'a>(
        self,
        tcx: TyCtxt<'tcx>,
    ) -> Box<dyn Iterator<Item = (LeafSymbol, DefId)> + 'a>
    where
        Self: 'a,
        'tcx: 'a;
}

impl<'tcx, I: Iterator<Item = DefId>> DefIdIterExt<'tcx> for I {
    fn find_by_name(mut self, tcx: TyCtxt<'tcx>, name: &str) -> Option<DefId> {
        self.find(|def_id| to_map_key(tcx, *def_id) == name)
    }

    fn filter_by_sibling_module_marker_name<'a>(
        self,
        tcx: TyCtxt<'tcx>,
        marker_name: LeafSymbol,
    ) -> Box<dyn Iterator<Item = DefId> + 'a>
    where
        Self: 'a + Clone,
        'tcx: 'a,
    {
        let marker_id = self.clone().find_by_name(tcx, &marker_name).unwrap();
        self.filter_by_module_marker(tcx, marker_id, false)
    }

    fn filter_by_module_marker<'a>(
        self,
        tcx: TyCtxt<'tcx>,
        marker: DefId,
        submodules: bool,
    ) -> Box<dyn Iterator<Item = DefId> + 'a>
    where
        Self: 'a,
        'tcx: 'a,
    {
        let module_of = move |def_id| {
            use rustc_hir::definitions::DefPathData;
            tcx.def_path(def_id)
                .data
                .into_iter()
                .take_while(|p| matches!(p.data, DefPathData::TypeNs(_)))
        };

        let marker_module = module_of(marker).collect::<Vec<_>>();
        let result = self.filter(move |def_id| {
            let module = module_of(*def_id);
            if submodules {
                module
                    .take(marker_module.len())
                    .eq_by(&marker_module, |a, b| &a == b)
            } else {
                module.eq_by(&marker_module, |a, b| &a == b)
            }
        });
        Box::new(result)
    }

    fn filter_associate_with_symbol<'a>(
        self,
        tcx: TyCtxt<'tcx>,
    ) -> Box<dyn Iterator<Item = (LeafSymbol, DefId)> + 'a>
    where
        Self: 'a,
        'tcx: 'a,
    {
        let result = self.filter_map(move |def_id| {
            (LeafSymbol::try_from(to_map_key(tcx, def_id)).map(|s| (s, def_id))).ok()
        });
        Box::new(result)
    }
}

trait TyCtxtExt<'tcx> {
    fn crate_by_name(self, name: &str) -> Option<CrateNum>;
    fn module_children_rec(self, module_id: DefId, submodules: bool) -> Vec<DefId>;
}

impl<'tcx> TyCtxtExt<'tcx> for TyCtxt<'tcx> {
    fn crate_by_name(self, name: &str) -> Option<CrateNum> {
        self.crates(())
            .iter()
            .find(|cnum| self.crate_name(**cnum).as_str() == name)
            .copied()
    }

    fn module_children_rec(self, module_id: DefId, submodules: bool) -> Vec<DefId> {
        let children = if let Some(module_id) = module_id.as_local() {
            self.module_children_local(module_id)
        } else {
            self.module_children(module_id)
        };

        children
            .iter()
            .map(|child| child.res)
            .filter(|res| res.opt_def_id().is_some_and(|id| id != module_id))
            .filter(|res| {
                // Skipping imports
                self.opt_parent(res.def_id())
                    .is_some_and(|parent| parent == module_id)
            })
            .flat_map(|res| {
                let mut result = vec![res.def_id()];
                if submodules {
                    if let Some(submodule_id) = res.mod_def_id() {
                        result.extend(self.module_children_rec(submodule_id, true));
                    }
                }
                result
            })
            .collect()
    }
}
