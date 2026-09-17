pub(crate) mod pri_utils;

use rustc_middle::ty::TyCtxt;

use crate::passes::{Leak, Storage, StorageExt};

use super::call::context::PriItems;

const KEY_PRI_ITEMS: &str = "pri_items";

pub(super) fn get_pri_items<'tcx>(
    tcx: TyCtxt<'tcx>,
    storage: &mut dyn Storage,
) -> <dyn Storage as StorageExt>::Leaked<PriItems> {
    storage
        .get_or_insert_with(KEY_PRI_ITEMS.to_owned(), || make_pri_items(tcx))
        .leak()
}

fn make_pri_items(tcx: TyCtxt) -> PriItems {
    use pri_utils::*;
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
