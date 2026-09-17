mod discovery;

use std::collections::{HashMap, HashSet};

use rustc_hir::def_id::DefId;
use rustc_middle::ty::{Ty, TyCtxt};

use crate::passes::{Leak, Storage, StorageExt};

use super::{
    FunctionInfo,
    sym::{self, LeafSymbol},
};

pub(crate) use discovery::TAG_DISCOVERY;
use discovery::{all_pri_items, filter_helper_items, filter_main_funcs};

const KEY_PRI_ITEMS: &str = "pri_items";

pub(crate) struct PriItems {
    /// Maps from leaf symbols to their corresponding function information.
    /// Of main functions (not including helpers)
    pub funcs: HashMap<LeafSymbol, FunctionInfo>,
    pub types: PriTypes,
    pub helper_funcs: PriHelperFunctions,
    pub all_items: HashSet<DefId>,
}

pub(crate) fn get_pri_items<'tcx>(
    tcx: TyCtxt<'tcx>,
    storage: &mut dyn Storage,
) -> <dyn Storage as StorageExt>::Leaked<PriItems> {
    fn make_pri_items(tcx: TyCtxt) -> PriItems {
        let all_items = all_pri_items(tcx);
        let main_funcs = filter_main_funcs(tcx, &all_items);
        let helper_items = filter_helper_items(tcx, &all_items);
        PriItems {
            funcs: main_funcs,
            types: collect_helper_types(&helper_items),
            helper_funcs: collect_helper_funcs(helper_items),
            all_items: all_items.into_iter().collect(),
        }
    }

    storage
        .get_or_insert_with(KEY_PRI_ITEMS.to_owned(), || make_pri_items(tcx))
        .leak()
}

struct TypeHolder(DefId);

impl TypeHolder {
    fn ty<'tcx>(&self, tcx: TyCtxt<'tcx>) -> Ty<'tcx> {
        tcx.type_of(self.0)
            .no_bound_vars()
            .expect("PRI types are not expected to have bound vars.")
    }
}

/// Provides types that are used in PRI functions along with primitive types.
pub(crate) struct PriTypes {
    place_ref: TypeHolder,
    operand_ref: TypeHolder,
}

impl PriTypes {
    pub(crate) fn place_ref<'tcx>(&self, tcx: TyCtxt<'tcx>) -> Ty<'tcx> {
        self.place_ref.ty(tcx)
    }

    pub(crate) fn operand_ref<'tcx>(&self, tcx: TyCtxt<'tcx>) -> Ty<'tcx> {
        // FIXME: Check if additional caching can be beneficial
        self.operand_ref.ty(tcx)
    }
}

macro_rules! define_pri_helper_funcs {
    ($($name: ident),*$(,)?) => {
        pub(crate) struct PriHelperFunctions {
            $(
                pub $name: FunctionInfo,
            )*
            pub all_helpers: HashMap<LeafSymbol, DefId>,
        }
    };
}

sym::pass_compiler_helpers_to!(define_pri_helper_funcs, just_funcs);

/// Collects the helper type holders out of the list of compiler helper PRI items.
fn collect_helper_types(helper_def_ids: &HashMap<LeafSymbol, DefId>) -> PriTypes {
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
        place_ref: get_type_holder(sym::helpers::PLACE_REF_TYPE_HOLDER),
        operand_ref: get_type_holder(sym::helpers::OPERAND_REF_TYPE_HOLDER),
    }
}

/// Collects the helper functions out of the list of compiler helper PRI items.
fn collect_helper_funcs(helper_def_ids: HashMap<LeafSymbol, DefId>) -> PriHelperFunctions {
    let get_func_info = |name: LeafSymbol| {
        helper_def_ids
            .get(&name)
            .copied()
            .unwrap_or_else(|| {
                panic!("`{}` is not exported (probably erased by compiler).", name);
            })
            .into()
    };

    macro_rules! create {
        ($($name: ident),*$(,)?) => {
            PriHelperFunctions {
                $($name: get_func_info(sym::helpers::$name)),*,
                all_helpers: helper_def_ids,
            }
        };
    }

    sym::pass_compiler_helpers_to!(create, just_funcs)
}
