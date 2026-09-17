use rustc_hir::def_id::DefId;
use rustc_middle::mir::{
    self, BasicBlock, BasicBlockData, Body, Operand, Place, Statement, TerminatorKind, UnwindAction,
};
use rustc_span::{Span, Spanned};

use std::collections::{HashMap, HashSet};

use crate::{mir_transform, passes::StorageExt, visit::TerminatorKindVisitor};

mod orig_index {
    use crate::passes::Storage;

    use super::*;

    const KEY_SWITCH_ORIG_INDICES: &str = "instr_switch_indices";

    pub(crate) fn record_original_indices(body: &Body, storage: &mut dyn Storage) {
        let mut entry =
            storage.get_or_default::<Vec<BasicBlock>>(KEY_SWITCH_ORIG_INDICES.to_owned());
        *entry = TerminatorLocationRecorder::default().visit_body(body);
    }

    pub(crate) fn make_orig_index_map(
        split_body: &Body,
        storage: &mut dyn Storage,
    ) -> HashMap<BasicBlock, BasicBlock> {
        let split_indices = TerminatorLocationRecorder::default().visit_body(split_body);
        let mut orig_indices =
            storage.get_or_default::<Vec<BasicBlock>>(KEY_SWITCH_ORIG_INDICES.to_owned());
        // Consume the saved indices.
        let orig_indices = core::mem::replace(orig_indices.as_mut(), Default::default());
        assert_eq!(
            split_indices.len(),
            orig_indices.len(),
            "Change in the number of terminators during the split is not expected"
        );
        split_indices
            .into_iter()
            .zip(orig_indices)
            .collect::<HashMap<_, _>>()
    }

    /// Records the basic block in which the terminators of interest are located.
    /// # Remark
    /// As the traversal order is the same, we can use the result list
    /// for mapping the indices to their original ones after transformations that
    /// do not introduce terminator of interest, e.g. block splitting.
    struct TerminatorLocationRecorder {
        select_bbs: Vec<BasicBlock>,
        current_bb: BasicBlock,
    }

    impl Default for TerminatorLocationRecorder {
        fn default() -> Self {
            Self {
                select_bbs: Default::default(),
                current_bb: BasicBlock::ZERO,
            }
        }
    }

    impl TerminatorLocationRecorder {
        fn visit_body<'tcx>(&mut self, body: &Body<'tcx>) -> Vec<BasicBlock> {
            for (index, block) in body.basic_blocks.iter_enumerated() {
                self.current_bb = index;
                self.visit_terminator_kind(&block.terminator().kind);
            }
            self.select_bbs.drain(..).collect()
        }

        fn record(&mut self) {
            self.select_bbs.push(self.current_bb);
        }
    }

    impl<'tcx> TerminatorKindVisitor<'tcx, ()> for TerminatorLocationRecorder {
        fn visit_switch_int(&mut self, _discr: &Operand<'tcx>, _targets: &mir::SwitchTargets) {
            self.record();
        }

        fn visit_assert(
            &mut self,
            _cond: &Operand<'tcx>,
            _expected: &bool,
            _msg: &mir::AssertMessage<'tcx>,
            _target: &BasicBlock,
            _unwind: &UnwindAction,
        ) {
            self.record();
        }

        fn visit_call(
            &mut self,
            _func: &Operand<'tcx>,
            _args: &[Spanned<Operand<'tcx>>],
            _destination: &Place<'tcx>,
            _target: &Option<BasicBlock>,
            _unwind: &UnwindAction,
            _call_source: &mir::CallSource,
            _fn_span: Span,
        ) {
            self.record();
        }

        fn visit_tail_call(
            &mut self,
            _func: &Operand<'tcx>,
            _args: &[Spanned<Operand<'tcx>>],
            _fn_span: Span,
        ) {
            self.record();
        }

        fn visit_return(&mut self) {
            self.record();
        }

        fn visit_drop(
            &mut self,
            _place: &Place<'tcx>,
            _target: &BasicBlock,
            _unwind: &UnwindAction,
            _replace: &bool,
        ) {
            self.record();
        }
    }
}

mod cleanup {
    use super::*;

    pub(crate) fn clear_existing_instrumentation(
        body: &mut Body<'_>,
        pri_funcs: &HashSet<DefId>,
    ) -> bool {
        // Avoid clearing compiler helpers
        if pri_funcs
            .iter()
            .next()
            .is_some_and(|id| id.krate == body.source.def_id().krate)
        {
            return false;
        }

        #[cfg(debug_assertions)]
        let original_body = body.clone();

        let cleared_some = mir_transform::noop_blocks_with(body, |block| {
            super::validation::is_instrumentation_block(block, pri_funcs)
        }) > 0;

        // Ensure that the control flow is not modified.
        #[cfg(debug_assertions)]
        if cleared_some {
            assert_eq!(
                original_body.basic_blocks.len(),
                body.basic_blocks.len(),
                "Blocks are not expected to be added or removed."
            );

            for (original_bb, bb) in original_body
                .basic_blocks
                .iter()
                .zip(body.basic_blocks.iter())
            {
                assert!(
                    original_bb
                        .terminator()
                        .successors()
                        .eq(bb.terminator().successors())
                );
            }
        }

        cleared_some
    }
}

mod validation {
    use super::*;

    pub(crate) fn sanity_check_inserted_block<'tcx>(
        bb: &BasicBlockData<'tcx>,
        expected_called_funcs: &HashSet<DefId>,
    ) {
        if !is_instrumentation_block(bb, expected_called_funcs) {
            panic!(
                "Unexpected block inserted during instrumentation: {:#?}",
                bb
            );
        }
    }

    pub(crate) fn is_instrumentation_block<'tcx>(
        bb: &BasicBlockData<'tcx>,
        all_pri_funcs: &HashSet<DefId>,
    ) -> bool {
        let terminator = bb.terminator.as_ref().unwrap();
        match &terminator.kind {
            TerminatorKind::Call { .. } | TerminatorKind::TailCall { .. } => {
                called_pri_func(&terminator.kind, all_pri_funcs).is_some()
            }
            TerminatorKind::Goto { target } => {
                bb.statements.is_empty() && *target == mir_transform::NEXT_BLOCK
            }
            _ => false,
        }
    }
}

pub(super) use cleanup::clear_existing_instrumentation;
pub(super) use orig_index::{make_orig_index_map, record_original_indices};
pub(super) use validation::sanity_check_inserted_block;

pub(super) fn requires_immediate_instr_after(stmt: &Statement) -> bool {
    use rustc_middle::mir::StatementKind::*;
    matches!(
        &stmt.kind,
        Assign(..) | SetDiscriminant { .. } | StorageLive(..)
    )
}

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
