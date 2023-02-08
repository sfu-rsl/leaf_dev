use rustc_middle::mir::visit::MutVisitor;
use rustc_middle::mir::{
    self, visit::Visitor, BasicBlock, BasicBlockData, HasLocalDecls, Local, Location, MirPass,
    Operand, Place, Rvalue,
};

use rustc_target::abi::VariantIdx;

use crate::mir_transform::call_addition::{
    context_requirements as ctxtreqs, Assigner, BranchingHandler, BranchingReferencer,
    EntryFunctionHandler, FunctionHandler, OperandRef, OperandReferencer, PlaceReferencer,
    RuntimeCallAdder,
};
use crate::mir_transform::modification::{BodyModificationUnit, JumpTargetModifier};
use crate::visit::StatementKindVisitor;
use crate::visit::{RvalueVisitor, TerminatorKindVisitor};

pub struct LeafPass;

impl<'tcx> MirPass<'tcx> for LeafPass {
    fn run_pass(
        &self,
        tcx: rustc_middle::ty::TyCtxt<'tcx>,
        body: &mut rustc_middle::mir::Body<'tcx>,
    ) {
        log::info!("Running leaf pass on body at {:#?}", body.span);

        let mut modification = BodyModificationUnit::new(body.local_decls().next_index());
        let mut call_adder = RuntimeCallAdder::new(tcx, &mut modification);
        let mut call_adder = call_adder.in_body(body);
        if tcx.entry_fn(()).expect("No entry function was found").0 == body.source.def_id() {
            Self::handle_entry_function(
                &mut call_adder
                    .at(body.basic_blocks.indices().next().unwrap())
                    .in_entry_fn(),
            );
        }
        VisitorFactory::make_body_visitor(&mut call_adder).visit_body(body);
        modification.commit(body);
    }
}

impl LeafPass {
    fn handle_entry_function(call_adder: &mut impl EntryFunctionHandler) {
        call_adder.init_runtime_lib();
    }
}

struct VisitorFactory;

impl VisitorFactory {
    fn make_body_visitor<'tcx, 'c, BC>(
        call_adder: &'c mut RuntimeCallAdder<BC>,
    ) -> impl Visitor<'tcx> + 'c
    where
        BC: ctxtreqs::Basic<'tcx> + JumpTargetModifier,
    {
        LeafBodyVisitor {
            call_adder: RuntimeCallAdder::borrow_from(call_adder),
        }
    }

    fn make_basic_block_visitor<'tcx, 'c, BC>(
        call_adder: &'c mut RuntimeCallAdder<BC>,
        block: BasicBlock,
    ) -> impl Visitor<'tcx> + 'c
    where
        BC: ctxtreqs::Basic<'tcx> + JumpTargetModifier,
    {
        LeafBasicBlockVisitor {
            call_adder: call_adder.at(block),
        }
    }

    fn make_statement_kind_visitor<'tcx, 'b, BC>(
        call_adder: &'b mut RuntimeCallAdder<BC>,
    ) -> impl StatementKindVisitor<'tcx, ()> + 'b
    where
        BC: ctxtreqs::ForPlaceRef<'tcx> + ctxtreqs::ForOperandRef<'tcx>,
    {
        LeafStatementKindVisitor {
            call_adder: RuntimeCallAdder::borrow_from(call_adder),
        }
    }

    fn make_terminator_kind_visitor<'tcx, 'b, BC>(
        call_adder: &'b mut RuntimeCallAdder<BC>,
    ) -> impl TerminatorKindVisitor<'tcx, ()> + 'b
    where
        BC: ctxtreqs::ForPlaceRef<'tcx>
            + ctxtreqs::ForOperandRef<'tcx>
            + ctxtreqs::ForBranching<'tcx>,
    {
        LeafTerminatorKindVisitor {
            call_adder: RuntimeCallAdder::borrow_from(call_adder),
        }
    }

    fn make_assignment_visitor<'tcx, 'b, BC>(
        call_adder: &'b mut RuntimeCallAdder<BC>,
        destination: &Place<'tcx>,
    ) -> impl RvalueVisitor<'tcx, ()> + 'b
    where
        BC: ctxtreqs::ForPlaceRef<'tcx> + ctxtreqs::ForOperandRef<'tcx>,
    {
        let dest_ref = call_adder.reference_place(destination);
        LeafAssignmentVisitor {
            call_adder: call_adder.assign(dest_ref),
        }
    }
}

macro_rules! make_general_visitor {
    ($name:ident) => {
        struct $name<C> {
            call_adder: RuntimeCallAdder<C>,
        }
    };
}

make_general_visitor!(LeafBodyVisitor);

impl<'tcx, C> Visitor<'tcx> for LeafBodyVisitor<C>
where
    C: ctxtreqs::Basic<'tcx> + JumpTargetModifier,
{
    fn visit_basic_block_data(&mut self, block: BasicBlock, data: &BasicBlockData<'tcx>) {
        VisitorFactory::make_basic_block_visitor(&mut self.call_adder, block)
            .visit_basic_block_data(block, data);
    }
}

make_general_visitor!(LeafBasicBlockVisitor);

impl<'tcx, C> Visitor<'tcx> for LeafBasicBlockVisitor<C>
where
    C: ctxtreqs::ForPlaceRef<'tcx> + ctxtreqs::ForOperandRef<'tcx> + JumpTargetModifier,
{
    fn visit_statement(
        &mut self,
        statement: &rustc_middle::mir::Statement<'tcx>,
        _location: Location,
    ) {
        VisitorFactory::make_statement_kind_visitor(&mut self.call_adder)
            .visit_statement_kind(&statement.kind);
    }

    fn visit_terminator(&mut self, terminator: &mir::Terminator<'tcx>, _location: Location) {
        VisitorFactory::make_terminator_kind_visitor(&mut self.call_adder)
            .visit_terminator_kind(&terminator.kind);
    }
}

make_general_visitor!(LeafStatementKindVisitor);

impl<'tcx, C> StatementKindVisitor<'tcx, ()> for LeafStatementKindVisitor<C>
where
    C: ctxtreqs::ForPlaceRef<'tcx> + ctxtreqs::ForOperandRef<'tcx>,
{
    fn visit_assign(&mut self, place: &Place<'tcx>, rvalue: &Rvalue<'tcx>) -> () {
        VisitorFactory::make_assignment_visitor(&mut self.call_adder, place).visit_rvalue(rvalue)
    }

    fn visit_set_discriminant(&mut self, place: &Place<'tcx>, variant_index: &VariantIdx) -> () {
        Default::default()
    }

    fn visit_deinit(&mut self, place: &Place<'tcx>) -> () {
        Default::default()
    }

    fn visit_intrinsic(
        &mut self,
        intrinsic: &rustc_middle::mir::NonDivergingIntrinsic<'tcx>,
    ) -> () {
        Default::default()
    }
}

make_general_visitor!(LeafTerminatorKindVisitor);

impl<'tcx, C> TerminatorKindVisitor<'tcx, ()> for LeafTerminatorKindVisitor<C>
where
    C: ctxtreqs::ForOperandRef<'tcx>
        + ctxtreqs::ForPlaceRef<'tcx>
        + ctxtreqs::ForBranching<'tcx>
        + ctxtreqs::ForFunctionCalling<'tcx>
        + ctxtreqs::ForReturning<'tcx>,
{
    fn visit_switch_int(&mut self, discr: &Operand<'tcx>, targets: &mir::SwitchTargets) -> () {
        let switch_info = self.call_adder.store_branching_info(discr);
        let mut call_adder = self.call_adder.branch(switch_info);
        for (value, target) in targets.iter() {
            call_adder.at(target).take_by_value(value);
        }
        call_adder
            .at(targets.otherwise())
            .take_otherwise(targets.iter().map(|v| v.0).into_iter());
    }

    fn visit_resume(&mut self) -> () {
        Default::default()
    }

    fn visit_abort(&mut self) -> () {
        Default::default()
    }

    fn visit_return(&mut self) -> () {
        self.call_adder.return_from_func();
    }

    fn visit_unreachable(&mut self) -> () {
        Default::default()
    }

    fn visit_drop(
        &mut self,
        place: &Place<'tcx>,
        target: &BasicBlock,
        unwind: &Option<BasicBlock>,
    ) -> () {
        Default::default()
    }

    fn visit_call(
        &mut self,
        func: &Operand<'tcx>,
        args: &Vec<Operand<'tcx>>,
        destination: &Place<'tcx>,
        target: &Option<BasicBlock>,
        cleanup: &Option<BasicBlock>,
        from_hir_call: bool,
        fn_span: rustc_span::Span,
    ) -> () {
        let func_ref = self.call_adder.reference_operand(func);
        let arg_refs = args
            .iter()
            .map(|a| self.call_adder.reference_operand(a))
            .collect::<Vec<OperandRef>>();
        let dest_ref = self.call_adder.reference_place(destination);
        self.call_adder
            .call_func(func_ref, arg_refs.iter().copied(), dest_ref);
    }

    fn visit_assert(
        &mut self,
        cond: &Operand<'tcx>,
        expected: &bool,
        msg: &mir::AssertMessage<'tcx>,
        target: &BasicBlock,
        cleanup: &Option<BasicBlock>,
    ) -> () {
        Default::default()
    }

    fn visit_yield(
        &mut self,
        value: &Operand<'tcx>,
        resume: &BasicBlock,
        resume_arg: &Place<'tcx>,
        drop: &Option<BasicBlock>,
    ) -> () {
        Default::default()
    }

    fn visit_generator_drop(&mut self) -> () {
        Default::default()
    }

    fn visit_inline_asm(
        &mut self,
        template: &&[rustc_ast::InlineAsmTemplatePiece],
        operands: &Vec<mir::InlineAsmOperand<'tcx>>,
        options: &rustc_ast::InlineAsmOptions,
        line_spans: &'tcx [rustc_span::Span],
        destination: &Option<BasicBlock>,
        cleanup: &Option<BasicBlock>,
    ) -> () {
        Default::default()
    }
}

make_general_visitor!(LeafAssignmentVisitor);

impl<'tcx, C> RvalueVisitor<'tcx, ()> for LeafAssignmentVisitor<C>
where
    C: ctxtreqs::ForPlaceRef<'tcx> + ctxtreqs::ForOperandRef<'tcx> + ctxtreqs::ForAssignment<'tcx>,
{
    fn visit_rvalue(&mut self, rvalue: &Rvalue<'tcx>) {
        log::debug!("Visiting Rvalue: {:#?}", rvalue);
        self.super_rvalue(rvalue)
    }

    fn visit_use(&mut self, operand: &Operand<'tcx>) {
        let operand_ref = self.call_adder.reference_operand(operand);
        self.call_adder.by_use(operand_ref)
    }

    fn visit_repeat(&mut self, operand: &Operand<'tcx>, count: &rustc_middle::ty::Const<'tcx>) {
        //log::debug!("VISIT REPEAT BEFORE={count:?}"); 
        let number = match count.kind() {
            //rustc_middle::ty::ConstKind::Param(param_const) => param_const.index, // nope
            //rustc_middle::ty::ConstKind::Infer(_) => panic!(),
            //rustc_middle::ty::ConstKind::Bound(_, _) => panic!(),
            //rustc_middle::ty::ConstKind::Placeholder(_) => panic!(),
            //rustc_middle::ty::ConstKind::Unevaluated(_) => panic!(),
            rustc_middle::ty::ConstKind::Value(val_tree) => match val_tree {
                rustc_middle::ty::ValTree::Leaf(scalar_int) => scalar_int.try_to_u64().unwrap(), //TODO: what if it isn't the correct size? -> match size before and pick the right "try to" function in any case
                rustc_middle::ty::ValTree::Branch(_) => return, // TODO: look into this
            },
            //rustc_middle::ty::ConstKind::Error(_) => panic!(),
            //rustc_middle::ty::ConstKind::Expr(_) => panic!(),
            _ => return,
        };
        //log::debug!("VISIT REPEAT AFTER={number:?}"); // YAY!
        let operand_ref = self.call_adder.reference_operand(operand);
        self.call_adder.by_repeat(
            operand_ref,
            number, //todo!("Convert {count} to number."), // okay, todo this
        )
    }

    fn visit_ref(
        &mut self,
        _region: &rustc_middle::ty::Region,
        borrow_kind: &rustc_middle::mir::BorrowKind,
        place: &Place<'tcx>,
    ) {
        let place_ref = self.call_adder.reference_place(place);
        self.call_adder.by_ref(
            place_ref,
            matches!(borrow_kind, rustc_middle::mir::BorrowKind::Mut { .. }),
        )
    }

    fn visit_thread_local_ref(&mut self) {
        self.call_adder.by_thread_local_ref();
    }

    fn visit_address_of(&mut self, mutability: &rustc_ast::Mutability, place: &Place<'tcx>) {
        let place_ref = self.call_adder.reference_place(place);
        self.call_adder
            .by_address_of(place_ref, mutability.is_mut());
    }

    fn visit_len(&mut self, place: &Place<'tcx>) {
        let place_ref = self.call_adder.reference_place(place);
        self.call_adder.by_len(place_ref)
    }

    fn visit_cast(
        &mut self,
        kind: &rustc_middle::mir::CastKind,
        operand: &Operand<'tcx>,
        ty: &rustc_middle::ty::Ty<'tcx>,
    ) {
        todo!()
    }

    fn visit_binary_op(&mut self, op: &mir::BinOp, operands: &Box<(Operand<'tcx>, Operand<'tcx>)>) {
        self.visit_binary_op_general(op, operands, false)
    }

    fn visit_checked_binary_op(
        &mut self,
        op: &mir::BinOp,
        operands: &Box<(Operand<'tcx>, Operand<'tcx>)>,
    ) {
        self.visit_binary_op_general(op, operands, true)
    }

    fn visit_nullary_op(
        &mut self,
        _op: &rustc_middle::mir::NullOp,
        _ty: &rustc_middle::ty::Ty<'tcx>,
    ) {
        // Nothing to do as they get computed and converted to constants before this stage.
        Default::default()
    }

    fn visit_unary_op(&mut self, op: &rustc_middle::mir::UnOp, operand: &Operand<'tcx>) {
        let operand_ref = self.call_adder.reference_operand(operand);
        self.call_adder.by_unary_op(op, operand_ref)
    }

    fn visit_discriminant(&mut self, place: &Place<'tcx>) {
        let place_ref = self.call_adder.reference_place(place);
        self.call_adder.by_discriminant(place_ref)
    }

    fn visit_aggregate(&mut self, kind: &Box<mir::AggregateKind>, operands: &Vec<Operand<'tcx>>) {
        // Based on compiler documents it is the only possible type that can reach here.
        assert!(matches!(kind, box mir::AggregateKind::Array(_)));

        let items: Vec<OperandRef> = operands
            .iter()
            .map(|o| self.call_adder.reference_operand(o))
            .collect();
        self.call_adder.by_aggregate_array(items.as_slice())
    }

    fn visit_shallow_init_box(&mut self, operand: &Operand<'tcx>, ty: &rustc_middle::ty::Ty<'tcx>) {
        todo!("Not sure yet.")
    }

    fn visit_copy_for_deref(&mut self, place: &Place<'tcx>) {
        todo!("Not sure yet.")
    }
}

impl<'tcx, C> LeafAssignmentVisitor<C>
where
    C: ctxtreqs::ForOperandRef<'tcx> + ctxtreqs::ForAssignment<'tcx>,
{
    fn visit_binary_op_general(
        &mut self,
        op: &mir::BinOp,
        operands: &Box<(Operand<'tcx>, Operand<'tcx>)>,
        checked: bool,
    ) {
        let first_ref = self.call_adder.reference_operand(&operands.0);
        let second_ref = self.call_adder.reference_operand(&operands.1);
        self.call_adder
            .by_binary_op(op, first_ref, second_ref, checked)
    }
}
