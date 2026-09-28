use rustc_middle::mir::Operand;
use rustc_span::Spanned;

use super::super::{
    decision::{self},
    insertion::{
        MemoryIntrinsicHandler, ProbeInserter,
        context::{ConfigProvider, SourceInfoProvider},
        ctxt_reqs as cr,
    },
};
use super::AssignmentId;

pub(super) fn instrument_memory_intrinsic_call<'tcx, 'a, C>(
    inserter: &mut ProbeInserter<C>,
    args: &'a [Spanned<Operand<'tcx>>],
    kind: decision::MemoryIntrinsicKind,
    is_volatile: bool,
) where
    C: cr::ForAssignment<'tcx>,
{
    use decision::MemoryIntrinsicKind::*;
    use decision::rules::EventDecision::*;

    let filter = (&inserter.config().assignment_filter).intrinsic_memory_op;
    match filter {
        Omit => return,
        Opaque | Detailed => {
            if matches!(filter, Opaque) {
                inserter.add_opaque_assignment();
                return;
            }

            let ptr_arg = match (&kind, is_volatile) {
                // `volatile_copy_memory`, `volatile_copy_nonoverlapping_memory` have dst first!
                (Copy { .. }, true) => args.get(1),
                _ => args.get(0),
            };
            let mut inserter = inserter.perform_memory_op(is_volatile, ptr_arg.cloned());

            match kind {
                Load { is_ptr_aligned } => inserter.load(is_ptr_aligned),
                Store { is_ptr_aligned } => inserter.store(&args[1], is_ptr_aligned),
                Copy { is_overlapping } => {
                    let dest: &Spanned<Operand<'tcx>> =
                        if is_volatile { &args[0] } else { &args[1] };
                    inserter.copy(dest, &args[2], is_overlapping)
                }
                Set => {
                    inserter.set(&args[1], &args[2]);
                }
                Swap => {
                    inserter.swap(&args[1]);
                }
                RawEq => {
                    inserter.raw_eq(&args[1]);
                }
                CompareBytes => {
                    inserter.compare_bytes(&args[1], &args[2]);
                }
            }
        }
    }
}

pub(super) fn instrument_memory_intrinsic_copy_non_overlapping<'tcx, 'a, C>(
    inserter: &mut ProbeInserter<C>,
    src: &Operand<'tcx>,
    dst: &Operand<'tcx>,
    count: &Operand<'tcx>,
    assignment_id: AssignmentId,
) where
    C: cr::ForInsertion<'tcx>,
{
    use decision::rules::EventDecision::*;

    match inserter.config().assignment_filter.intrinsic_memory_op {
        Omit | Opaque => return,
        Detailed => (),
    }

    let [src, dst, count] = {
        let span = inserter.source_info().span;
        [src, dst, count].map(|op| Spanned {
            node: op.clone(),
            span,
        })
    };

    let mut inserter = inserter.before();
    let mut inserter = inserter.memory_write(assignment_id);
    let mut inserter = inserter.perform_memory_op(false, Some(src.clone()));
    inserter.copy(&dst, &count, false);
}
