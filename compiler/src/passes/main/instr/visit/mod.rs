mod call;

use rustc_abi::VariantIdx;
use rustc_middle::{
    mir::{
        self, BasicBlock, BasicBlockData, Body, Location, Operand, Place, Rvalue, UnwindAction,
        visit::Visitor,
    },
    ty as mir_ty,
};
use rustc_span::{Span, Spanned};

use std::{collections::BTreeMap, rc::Rc};

use common::{log_debug, pri::AssignmentId};

use crate::{
    utils::assignment_ids::assignment_ids_split_agnostic, utils::mir_transform::JumpTargetModifier,
    visit::*,
};

use super::{
    TAG_INSTR,
    insertion::{
        AssertionHandler, AssignmentHandler, BranchingHandler, DropHandler, EntryFunctionHandler,
        FunctionHandler,
        InsertionLocation::*,
        ProbeInserter, StorageMarker,
        context::{
            BlockIndexProvider, BlockOriginalIndexProvider, BodyProvider, TyContextProvider,
        },
        ctxt_reqs as cr,
    },
};

pub(super) fn instrument_body<'tcx, 'c, 'body, C>(
    inserter: &'c mut ProbeInserter<C>,
    body: &'body Body<'tcx>,
) where
    C: cr::Basic<'tcx> + BlockOriginalIndexProvider + JumpTargetModifier,
{
    let is_entry = inserter
        .tcx()
        .entry_fn(())
        .is_some_and(|(id, _)| id == body.source.def_id());

    if is_entry {
        handle_entry_function_pre(inserter, body);
    }

    // Insert some instrumentation at the beginning of the body.
    {
        let mut inserter = inserter.at(Before(body.basic_blocks.indices().next().unwrap()));
        let mut inserter = inserter.with_source_info(*body.source_info(Location::START));
        handle_body_pre_blocks(&mut inserter);
    }

    VisitorFactory::make_body_visitor(inserter).visit_body(body);

    if is_entry {
        handle_entry_function_post(inserter, body);
    }
}

fn handle_entry_function_pre<'tcx, C>(inserter: &mut ProbeInserter<C>, body: &Body<'tcx>)
where
    C: cr::Basic<'tcx>,
{
    let mut inserter = inserter.in_entry_fn();
    let first_block = body.basic_blocks.indices().next().unwrap();
    let mut inserter = inserter.with_source_info(*body.source_info(Location::START));
    let mut inserter = inserter.at(Before(first_block));
    inserter.init_runtime_lib();
}

fn handle_entry_function_post<'tcx, C>(inserter: &mut ProbeInserter<C>, body: &Body<'tcx>)
where
    C: cr::Basic<'tcx>,
{
    let mut inserter = inserter.in_entry_fn();
    body.basic_blocks
        .iter_enumerated()
        .filter(|(_, bb)| bb.terminator().kind == mir::TerminatorKind::Return)
        .for_each(|(index, bb)| {
            inserter
                .at(Before(index))
                .with_source_info(bb.terminator().source_info)
                .shutdown_runtime_lib();
        });
}

fn handle_body_pre_blocks<'tcx, C>(inserter: &mut ProbeInserter<C>)
where
    C: cr::ForFunctionCalling<'tcx> + cr::ForStorageMarking<'tcx>,
{
    inserter.enter_func();

    rustc_mir_dataflow::impls::always_storage_live_locals(inserter.body())
        .iter()
        .for_each(|l| match inserter.body().local_kind(l) {
            mir::LocalKind::Temp => {
                inserter.mark_live(&l.into());
            }
            mir::LocalKind::Arg => {}
            mir::LocalKind::ReturnPointer => {}
        });
}

struct VisitorFactory;

impl VisitorFactory {
    fn make_body_visitor<'tcx, 'c, C>(inserter: &'c mut ProbeInserter<C>) -> impl Visitor<'tcx> + 'c
    where
        C: cr::Basic<'tcx> + BlockOriginalIndexProvider + JumpTargetModifier,
    {
        let assignment_ids = assignment_ids_split_agnostic(inserter.tcx(), inserter.body())
            .map(|(loc, _, id)| (loc, id))
            .collect();
        LeafBodyVisitor {
            inserter: ProbeInserter::borrow_from(inserter),
            assignment_ids: Rc::new(assignment_ids),
        }
    }

    fn make_basic_block_visitor<'tcx, 'c, C>(
        inserter: &'c mut ProbeInserter<C>,
        block: BasicBlock,
        assignment_ids: Rc<AssignmentIdMap>,
    ) -> impl Visitor<'tcx> + 'c
    where
        C: cr::Basic<'tcx> + BlockOriginalIndexProvider + JumpTargetModifier,
    {
        LeafBasicBlockVisitor {
            inserter: inserter.at(Before(block)),
            assignment_ids,
        }
    }

    fn make_statement_kind_visitor<'tcx, 'b, C>(
        inserter: &'b mut ProbeInserter<C>,
        assignment_id: Option<AssignmentId>,
    ) -> impl StatementKindVisitor<'tcx, ()> + 'b
    where
        C: cr::ForPlaceRef<'tcx> + cr::ForOperandRef<'tcx>,
    {
        LeafStatementKindVisitor {
            inserter: ProbeInserter::borrow_from(inserter),
            assignment_id,
        }
    }

    fn make_terminator_kind_visitor<'tcx, 'b, C>(
        inserter: &'b mut ProbeInserter<C>,
        assignment_id: Option<AssignmentId>,
    ) -> impl TerminatorKindVisitor<'tcx, ()> + 'b
    where
        C: cr::ForPlaceRef<'tcx>
            + cr::ForOperandRef<'tcx>
            + cr::ForBranching<'tcx>
            + cr::ForReturning<'tcx>,
    {
        LeafTerminatorKindVisitor {
            inserter: ProbeInserter::borrow_from(inserter),
            assignment_id,
        }
    }
}

macro_rules! make_general_visitor {
    ($vis:vis $name:ident $({ $($field_name: ident : $field_ty: ty),* $(,)? })?) => {
        $vis struct $name<C> {
            inserter: ProbeInserter<C>,
            $($($field_name: $field_ty),*)?
        }
    };
}

type AssignmentIdMap = BTreeMap<Location, AssignmentId>;

make_general_visitor!(LeafBodyVisitor {
    assignment_ids: Rc<AssignmentIdMap>,
});

impl<'tcx, C> Visitor<'tcx> for LeafBodyVisitor<C>
where
    C: cr::Basic<'tcx> + BlockOriginalIndexProvider + JumpTargetModifier,
{
    fn visit_basic_block_data(&mut self, block: BasicBlock, data: &BasicBlockData<'tcx>) {
        if data.is_cleanup {
            // NOTE: Cleanup blocks will be investigated in #206.
            log_debug!(target: TAG_INSTR, "Skipping instrumenting cleanup block: {:?}", block);
            return;
        }

        VisitorFactory::make_basic_block_visitor(
            &mut self.inserter,
            block,
            self.assignment_ids.clone(),
        )
        .visit_basic_block_data(block, data);
    }
}

make_general_visitor!(LeafBasicBlockVisitor {
    assignment_ids: Rc<AssignmentIdMap>,
});

impl<'tcx, C> Visitor<'tcx> for LeafBasicBlockVisitor<C>
where
    C: cr::Basic<'tcx> + BlockIndexProvider + BlockOriginalIndexProvider + JumpTargetModifier,
{
    fn visit_statement(
        &mut self,
        statement: &rustc_middle::mir::Statement<'tcx>,
        location: Location,
    ) {
        log_debug!(
            target: TAG_INSTR,
            "Visiting statement: {:?} at {:?}",
            statement.kind,
            location
        );
        VisitorFactory::make_statement_kind_visitor(
            &mut self
                .inserter
                .with_source_info(statement.source_info)
                .before(),
            self.assignment_ids.get(&location).copied(),
        )
        .visit_statement_kind(&statement.kind);
    }

    fn visit_terminator(&mut self, terminator: &mir::Terminator<'tcx>, location: Location) {
        VisitorFactory::make_terminator_kind_visitor(
            &mut self
                .inserter
                .with_source_info(terminator.source_info)
                .before(),
            self.assignment_ids.get(&location).copied(),
        )
        .visit_terminator_kind(&terminator.kind);
    }
}

make_general_visitor!(LeafStatementKindVisitor {
    assignment_id: Option<AssignmentId>,
});

impl<'tcx, C> StatementKindVisitor<'tcx, ()> for LeafStatementKindVisitor<C>
where
    C: cr::ForPlaceRef<'tcx> + cr::ForOperandRef<'tcx>,
{
    fn visit_assign(&mut self, place: &Place<'tcx>, rvalue: &Rvalue<'tcx>) {
        self.inserter
            .assign(self.assignment_id.unwrap(), *place)
            .to_rvalue(rvalue)
    }

    fn visit_set_discriminant(&mut self, place: &Place<'tcx>, variant_index: &VariantIdx) {
        self.inserter
            .assign(self.assignment_id.unwrap(), *place)
            .its_discriminant_to(variant_index)
    }

    fn visit_intrinsic(&mut self, intrinsic: &mir::NonDivergingIntrinsic<'tcx>) {
        match intrinsic {
            mir::NonDivergingIntrinsic::Assume(_operand) => {
                // No plans for now
            }
            mir::NonDivergingIntrinsic::CopyNonOverlapping(mir::CopyNonOverlapping {
                src,
                dst,
                count,
            }) => {
                call::mem_intrinsics::instrument_memory_intrinsic_copy_non_overlapping(
                    &mut self.inserter,
                    src,
                    dst,
                    count,
                    self.assignment_id.unwrap(),
                );
            }
        }
    }

    fn visit_storage_live(&mut self, local: &mir::Local) {
        let mut inserter = self.inserter.after();
        inserter.mark_live(&(*local).into());
    }

    fn visit_storage_dead(&mut self, local: &mir::Local) {
        self.inserter.before().mark_dead(&(*local).into());
    }

    fn visit_nop(&mut self) {
        // Nothing to do
        Default::default()
    }

    fn visit_coverage(&mut self, _coverage: &mir::coverage::CoverageKind) {
        // Nothing to do
        Default::default()
    }

    fn visit_place_mention(&mut self, _place: &Place<'tcx>) {}

    fn visit_const_eval_counter(&mut self) {
        // Nothing to do
        Default::default()
    }

    fn visit_backward_incompatible_drop_hint(
        &mut self,
        _place: &Place<'tcx>,
        _reason: &mir::BackwardIncompatibleDropReason,
    ) {
        // Nothing to do
        Default::default()
    }

    fn visit_ascribe_user_type(
        &mut self,
        _place: &Place<'tcx>,
        _user_type_proj: &mir::UserTypeProjection,
        _variance: &mir_ty::Variance,
    ) {
        panic!("Unexpected statement kind at this stage.")
    }

    fn visit_fake_read(&mut self, _cause: &mir::FakeReadCause, _place: &Place<'tcx>) {
        panic!("Unexpected statement kind at this stage.")
    }
}

impl<C> LeafStatementKindVisitor<C> {}

make_general_visitor!(LeafTerminatorKindVisitor {
    assignment_id: Option<AssignmentId>,
});

impl<'tcx, C> TerminatorKindVisitor<'tcx, ()> for LeafTerminatorKindVisitor<C>
where
    C: cr::ForOperandRef<'tcx>
        + cr::ForPlaceRef<'tcx>
        + cr::ForBranching<'tcx>
        + cr::ForReturning<'tcx>
        + cr::ForFunctionCalling<'tcx>
        + cr::ForDropping<'tcx>,
{
    fn visit_switch_int(&mut self, discr: &Operand<'tcx>, targets: &mir::SwitchTargets) {
        self.inserter.switch(discr, targets);
    }

    fn visit_return(&mut self) {
        rustc_mir_dataflow::impls::always_storage_live_locals(self.inserter.body())
            .iter()
            .for_each(|l| {
                if self.inserter.body().local_kind(l) == mir::LocalKind::ReturnPointer {
                    return;
                }
                self.inserter.before().mark_dead(&l.into());
            });

        self.inserter.return_from_func();
    }

    fn visit_unreachable(&mut self) {
        Default::default()
    }

    fn visit_drop(
        &mut self,
        place: &Place<'tcx>,
        _target: &BasicBlock,
        _unwind: &UnwindAction,
        _replace: &bool,
    ) {
        let mut inserter = self.inserter.before();
        inserter.before_call_drop(place);

        let mut inserter = inserter.after();
        inserter.after_call_drop();
    }

    fn visit_call(
        &mut self,
        func: &Operand<'tcx>,
        args: &[Spanned<Operand<'tcx>>],
        destination: &Place<'tcx>,
        target: &Option<BasicBlock>,
        _unwind: &UnwindAction,
        _call_source: &mir::CallSource,
        _fn_span: Span,
    ) {
        call::handle_call(
            &mut self.inserter,
            self.assignment_id,
            call::CallParams {
                func,
                args,
                destination,
                is_diverging: target.is_none(),
            },
        );
    }

    fn visit_tail_call(
        &mut self,
        _func: &Operand<'tcx>,
        _args: &[Spanned<Operand<'tcx>>],
        _fn_span: Span,
    ) {
        // NOTE: https://github.com/rust-lang/rust/issues/112788
        unimplemented!(
            "This is still an experimental feature in the compiler and is not expected to appear in target projects."
        )
    }

    fn visit_assert(
        &mut self,
        cond: &Operand<'tcx>,
        expected: &bool,
        msg: &mir::AssertMessage<'tcx>,
        _target: &BasicBlock,
        _unwind: &UnwindAction,
    ) {
        // TODO: Handle the target
        self.inserter.check_assert(cond, *expected, msg);
    }

    fn visit_yield(
        &mut self,
        _value: &Operand<'tcx>,
        _resume: &BasicBlock,
        _resume_arg: &Place<'tcx>,
        _drop: &Option<BasicBlock>,
    ) {
        Default::default()
    }

    fn visit_coroutine_drop(&mut self) {
        Default::default()
    }

    fn visit_inline_asm(
        &mut self,
        _asm_macro: &mir::InlineAsmMacro,
        _template: &[rustc_ast::InlineAsmTemplatePiece],
        _operands: &[mir::InlineAsmOperand<'tcx>],
        _options: &rustc_ast::InlineAsmOptions,
        _line_spans: &'tcx [Span],
        _destination: &Box<[BasicBlock]>,
        _unwind: &UnwindAction,
    ) {
        Default::default()
    }

    fn visit_unwind_resume(&mut self) {
        // TODO
        Default::default()
    }

    fn visit_unwind_terminate(&mut self, _reason: &mir::UnwindTerminateReason) {
        // TODO
        Default::default()
    }

    fn visit_goto(&mut self, _target: &BasicBlock) {
        // Nothing to do
        Default::default()
    }

    fn visit_false_edge(&mut self, _real_target: &BasicBlock, _imaginary_target: &BasicBlock) {
        panic!("Unexpected terminator kind at this stage.")
    }

    fn visit_false_unwind(&mut self, _real_target: &BasicBlock, _unwind: &UnwindAction) {
        panic!("Unexpected terminator kind at this stage.")
    }
}
