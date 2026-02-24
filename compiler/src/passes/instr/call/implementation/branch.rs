use crate::mir_transform::JumpModificationConstraint;

use super::{
    BodyProvider, BranchingHandler, BranchingReferencer, OperandReferencer, SwitchInfo,
    context::{BaseContext, BlockIndexProvider, BlockOriginalIndexProvider, SwitchInfoProvider},
    ctxt_reqs::ForBranching,
    prelude::{mir::*, *},
};

impl<'tcx, C> BranchingReferencer<'tcx> for RuntimeCallAdder<C>
where
    Self: MirCallAdder<'tcx> + BlockInserter<'tcx> + OperandReferencer<'tcx>,
    C: ForBranching<'tcx>,
{
    fn store_branching_info(&mut self, discr: &Operand<'tcx>) -> SwitchInfo<'tcx> {
        let discr_ref = self.reference_operand(discr);

        let (info_block, info_local) = self.make_bb_for_helper_call_with_all(
            self.context.pri_helper_funcs().switch_info,
            [],
            vec![
                self.original_bb_index_as_arg(),
                operand::move_for_local(discr_ref.into()),
            ],
            Default::default(),
        );
        self.insert_blocks([info_block]);
        SwitchInfo {
            node_index: self.context.block_index(),
            discr_ty: discr.ty(self.context.local_decls(), self.tcx()),
            info_local,
        }
    }
}

impl<'tcx, C> RuntimeCallAdder<C>
where
    Self: BodyProvider<'tcx>,
    C: BaseContext<'tcx> + BlockIndexProvider + BlockOriginalIndexProvider,
{
    pub(super) fn original_bb_index_as_arg(&self) -> Operand<'tcx> {
        let tcx = self.tcx();
        let current = self.context.block_index();
        let bb_index = self
            .context
            .block_original_index(current)
            .unwrap_or_else(|| {
                panic!(
                    "Original index is not available for {:?}: {:?}",
                    current,
                    self.body().basic_blocks.get(current),
                )
            });
        operand::const_from_uint(tcx, bb_index.as_u32())
    }
}

impl<'tcx, C> BranchingHandler for RuntimeCallAdder<C>
where
    Self: MirCallAdder<'tcx> + BlockInserter<'tcx>,
    C: SwitchInfoProvider<'tcx> + ForBranching<'tcx>,
{
    fn take_by_value(&mut self, value: u128) {
        let tcx = self.context.tcx();
        let switch_info = self.context.switch_info();
        let discr_ty = switch_info.discr_ty;
        let (func_name, additional_args) = if discr_ty.is_bool() {
            const FALSE_SWITCH_VALUE: u128 = 0;
            if value == FALSE_SWITCH_VALUE {
                (sym::take_branch_false, vec![])
            } else {
                unreachable!(
                    "SwitchInts for booleans are expected to provide only the value 0 (false)."
                )
            }
        } else if discr_ty.is_integral() {
            // NOTE: Discriminants are integers.
            (
                sym::take_branch_int,
                vec![
                    operand::const_from_uint(tcx, value),
                    operand::const_from_uint(tcx, discr_ty.primitive_size(tcx).bits()),
                    operand::const_from_bool(tcx, discr_ty.is_signed()),
                ],
            )
        } else if discr_ty.is_char() {
            (
                sym::take_branch_char,
                vec![operand::const_from_char(
                    tcx,
                    char::from_u32(value.try_into().unwrap()).unwrap(),
                )],
            )
        } else {
            unreachable!(
                "Branching node discriminant is supposed to be either bool, int, char, or enum discriminant."
            )
        };

        let block = self.make_bb_for_call_with_target(
            func_name,
            [
                vec![operand::move_for_local(switch_info.info_local)],
                additional_args,
            ]
            .concat(),
            Some(self.context.block_index()),
        );
        let new_block_index = self.insert_blocks_with_stickiness([block], false)[0];
        self.context.modify_jump_target_where(
            switch_info.node_index,
            self.context.block_index(),
            new_block_index,
            JumpModificationConstraint::SwitchValue(value),
        );
    }

    fn take_otherwise<I>(&mut self, non_values: I)
    where
        I: IntoIterator<Item = u128>,
        I::IntoIter: ExactSizeIterator<Item = u128>,
    {
        let tcx = self.context.tcx();
        let switch_info = self.context.switch_info();
        let discr_ty = switch_info.discr_ty;
        let mut non_values = non_values.into_iter();
        let (additional_stmts, func_name, additional_args) = if discr_ty.is_bool() {
            debug_assert!(non_values.len() == 1);
            debug_assert!(non_values.next().unwrap() == 0);

            (vec![], sym::take_branch_ow_bool, vec![])
        } else {
            let (func_name, value_ty, non_values, additional_args) = if discr_ty.is_integral() {
                (
                    sym::take_branch_ow_int,
                    tcx.types.u128,
                    non_values
                        .map(|nv: u128| operand::const_from_uint(tcx, nv))
                        .collect(),
                    vec![
                        operand::const_from_uint(tcx, discr_ty.primitive_size(tcx).bits()),
                        operand::const_from_bool(tcx, discr_ty.is_signed()),
                    ],
                )
            } else if discr_ty.is_char() {
                (
                    sym::take_branch_ow_char,
                    tcx.types.char,
                    non_values
                        .map(|nv: u128| {
                            operand::const_from_char(
                                tcx,
                                char::from_u32(nv.try_into().unwrap()).unwrap(),
                            )
                        })
                        .collect(),
                    vec![],
                )
            } else {
                unreachable!(
                    "Branching node discriminant is supposed to be either bool, int, char, or enum discriminant."
                )
            };

            let (non_values_local, assign_stmts) =
                utils::prepare_operand_for_slice(tcx, &mut self.context, value_ty, non_values);
            (
                assign_stmts.to_vec(),
                func_name,
                [
                    vec![operand::move_for_local(non_values_local)],
                    additional_args,
                ]
                .concat(),
            )
        };

        let mut block = self.make_bb_for_call_with_target(
            func_name,
            [
                vec![operand::move_for_local(switch_info.info_local)],
                additional_args,
            ]
            .concat(),
            Some(self.context.block_index()),
        );
        block.statements.extend(additional_stmts);
        let new_block_index = self.insert_blocks_with_stickiness([block], false)[0];
        self.context.modify_jump_target_where(
            switch_info.node_index,
            self.context.block_index(),
            new_block_index,
            JumpModificationConstraint::SwitchOtherwise,
        );
    }
}

mod utils {
    pub(super) use super::super::utils::{operand, prepare_operand_for_slice};
}
use utils::*;
