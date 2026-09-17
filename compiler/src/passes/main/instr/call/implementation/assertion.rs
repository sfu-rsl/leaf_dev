use super::{
    AssertionHandler, OperandReferencer,
    ctxt_reqs::ForAssertion,
    prelude::{mir::*, *},
};

impl<'tcx, C> AssertionHandler<'tcx> for RuntimeCallAdder<C>
where
    Self: MirCallAdder<'tcx> + BlockInserter<'tcx>,
    C: ForAssertion<'tcx>,
{
    fn check_assert(
        &mut self,
        cond: OperandRef,
        expected: bool,
        msg: &rustc_middle::mir::AssertMessage<'tcx>,
    ) {
        let Some((func_name, additional_operands)) = self.reference_assert_kind(msg) else {
            return;
        };

        let (info_block, info_local) = {
            self.make_bb_for_helper_call_with_all(
                self.context.pri_helper_funcs().assertion_info,
                [],
                vec![
                    self.original_bb_index_as_arg(),
                    operand::move_for_local(cond.into()),
                    operand::const_from_bool(self.context.tcx(), expected),
                ],
                Default::default(),
            )
        };

        let block = self.make_bb_for_call(
            func_name,
            [
                vec![operand::move_for_local(info_local.into())],
                additional_operands,
            ]
            .concat(),
        );

        self.insert_blocks([info_block, block]);
    }
}
impl<'tcx, C> RuntimeCallAdder<C>
where
    C: ForAssertion<'tcx>,
{
    fn reference_assert_kind(
        &mut self,
        msg: &rustc_middle::mir::AssertMessage<'tcx>,
    ) -> Option<(LeafSymbol, Vec<Operand<'tcx>>)> {
        use rustc_middle::mir::AssertKind;
        match msg {
            AssertKind::BoundsCheck { len, index } => {
                let len_ref = self.reference_operand(len);
                let index_ref = self.reference_operand(index);
                Some((
                    sym::assert_bounds_check,
                    vec![
                        operand::copy_for_local(len_ref.into()),
                        operand::copy_for_local(index_ref.into()),
                    ],
                ))
            }
            AssertKind::Overflow(operator, op1, op2) => {
                let tcx = self.tcx();

                let operator = utils::convert_mir_binop_to_pri(operator);
                let operator_local = {
                    let (block, local) = self.make_bb_for_helper_call_with_all(
                        self.context.pri_helper_funcs().const_binary_op_of,
                        vec![],
                        vec![operand::const_from_uint(tcx, operator.to_raw())],
                        Default::default(),
                    );
                    self.insert_blocks([block]);
                    local
                };

                let op1_ref = self.reference_operand(op1);
                let op2_ref = self.reference_operand(op2);
                Some((
                    sym::assert_overflow,
                    vec![
                        // TODO: double check that moves and copies here are correct
                        operand::move_for_local(operator_local),
                        operand::copy_for_local(op1_ref.into()),
                        operand::copy_for_local(op2_ref.into()),
                    ],
                ))
            }
            AssertKind::OverflowNeg(op) => {
                let op_ref = self.reference_operand(op);
                Some((
                    sym::assert_overflow_neg,
                    vec![operand::copy_for_local(op_ref.into())],
                ))
            }
            AssertKind::DivisionByZero(op) => {
                let op_ref = self.reference_operand(op);
                Some((
                    sym::assert_div_by_zero,
                    vec![operand::copy_for_local(op_ref.into())],
                ))
            }
            AssertKind::RemainderByZero(op) => {
                let op_ref = self.reference_operand(op);
                Some((
                    sym::assert_rem_by_zero,
                    vec![operand::copy_for_local(op_ref.into())],
                ))
            }
            AssertKind::ResumedAfterReturn(..)
            | AssertKind::ResumedAfterPanic(..)
            | AssertKind::ResumedAfterDrop(..) => {
                /* NOTE: These two assertions look to be used to make sure
                 * that the state machine is correctly generated.
                 * When they are reached, an illegal has happened in the state machine.
                 * They are in fact assert(false) statements.
                 */
                None
            }
            AssertKind::MisalignedPointerDereference { required, found } => {
                let required_ref = self.reference_operand(required);
                let found_ref = self.reference_operand(found);
                Some((
                    sym::assert_misaligned_ptr_deref,
                    vec![
                        operand::copy_for_local(required_ref.into()),
                        operand::copy_for_local(found_ref.into()),
                    ],
                ))
            }
            AssertKind::NullPointerDereference => Some((sym::assert_null_ptr_deref, vec![])),
            AssertKind::InvalidEnumConstruction(discr) => {
                let discr_ref = self.reference_operand(discr);
                Some((
                    sym::assert_invalid_enum_ctn,
                    vec![operand::copy_for_local(discr_ref.into())],
                ))
            }
        }
    }
}

mod utils {
    pub(super) use super::super::utils::{convert_mir_binop_to_pri, operand};
}
use utils::*;
