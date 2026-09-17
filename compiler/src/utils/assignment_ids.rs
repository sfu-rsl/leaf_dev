use std::borrow::Cow;

use derive_more as dm;

use rustc_middle::{
    mir::{Body, Location, NonDivergingIntrinsic, Operand, Place, StatementKind, TerminatorKind},
    ty::TyCtxt,
};

use common::pri::AssignmentId;

use super::mir::BodyExt;

#[derive(dm::From)]
pub(crate) enum AssignmentDestination<'a, 'tcx> {
    Place(Cow<'a, Place<'tcx>>),
    PtrDeref,
}

pub(crate) fn assignment_ids_split_agnostic<'a, 'tcx>(
    tcx: TyCtxt<'tcx>,
    body: &'a Body<'tcx>,
) -> impl Iterator<Item = (Location, AssignmentDestination<'a, 'tcx>, AssignmentId)> {
    // Basically, the order index of which we see an assignment is used as its id.
    body.collect_map_ordered(
        |stmt| match stmt.kind {
            StatementKind::Assign(box (ref dest, _)) => Some(Cow::Borrowed(dest).into()),
            StatementKind::SetDiscriminant { ref place, .. } => {
                Some(Cow::Borrowed(place.as_ref()).into())
            }
            StatementKind::Intrinsic(box NonDivergingIntrinsic::CopyNonOverlapping(..)) => {
                Some(AssignmentDestination::PtrDeref)
            }
            _ => None,
        },
        |terminator| match terminator.kind {
            TerminatorKind::Call {
                ref func,
                ref destination,
                ..
            } => {
                if is_writing_intrinsic(tcx, func) {
                    Some(AssignmentDestination::PtrDeref)
                } else {
                    Some(Cow::Borrowed(destination).into())
                }
            }
            TerminatorKind::TailCall { .. } => Some(AssignmentDestination::from(Cow::Owned(
                Place::return_place(),
            ))),
            _ => None,
        },
    )
    .into_iter()
    .enumerate()
    .map(|(i, (loc, dest))| (loc, dest, i.try_into().expect("Too many assignments")))
}

fn is_writing_intrinsic<'tcx>(tcx: TyCtxt<'tcx>, func: &Operand<'tcx>) -> bool {
    let Some((def_id, _)) = func.const_fn_def() else {
        return false;
    };

    let Some(intrinsic) = tcx.intrinsic(def_id) else {
        return false;
    };

    {
        use rustc_span::symbol::sym::*;
        #[allow(non_upper_case_globals)]
        match intrinsic.name {
            write_bytes | write_via_move | nontemporal_store => true,
            copy | copy_nonoverlapping | typed_swap_nonoverlapping => true,
            volatile_copy_memory
            | volatile_copy_nonoverlapping_memory
            | volatile_store
            | unaligned_volatile_store
            | volatile_set_memory => true,
            simd_shuffle | simd_scatter | simd_masked_store | simd_insert => true,
            n if n.as_str().starts_with("atomic_store") => true,
            _ => false,
        }
    }
}
