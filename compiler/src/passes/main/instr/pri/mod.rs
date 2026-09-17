mod catalog;
pub(crate) mod sym;
mod utils;

use rustc_hir::def_id::DefId;

pub(crate) use catalog::{PriHelperFunctions, PriItems, PriTypes, TAG_DISCOVERY, get_pri_items};
pub(crate) use utils::called_pri_func;

#[derive(Clone, Copy, Debug, derive_more::From, derive_more::Deref)]
pub(crate) struct FunctionInfo {
    pub def_id: DefId,
}
