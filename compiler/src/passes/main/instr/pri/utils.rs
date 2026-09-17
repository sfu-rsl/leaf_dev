use std::collections::HashSet;

use rustc_hir::def_id::DefId;
use rustc_middle::mir::TerminatorKind;

/// Returns the DefId of the called function if it is a PRI function, otherwise returns None.
#[inline]
pub(crate) fn called_pri_func(
    terminator: &TerminatorKind,
    all_pri_funcs: &HashSet<DefId>,
) -> Option<DefId> {
    let (TerminatorKind::Call { func, .. } | TerminatorKind::TailCall { func, .. }) = terminator
    else {
        return None;
    };
    func.const_fn_def()
        .filter(|(def_id, _)| all_pri_funcs.contains(&def_id))
        .map(|(def_id, _)| def_id)
}
