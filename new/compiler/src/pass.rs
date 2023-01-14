use rustc_middle::mir::visit::MutVisitor;
use rustc_middle::mir::{visit::Visitor, BasicBlock};
use rustc_middle::mir::{
    BasicBlockData, HasLocalDecls, Local, Location, MirPass, Operand, Place, Rvalue,
};
use rustc_target::abi::VariantIdx;

use crate::mir_transform::BodyModificationUnit;
use crate::mir_transform::RuntimeCallAdder;
use crate::mir_transform::RuntimeCallAdderForAssignment;
use crate::visit::RvalueVisitor;
use crate::visit::StatementKindVisitor;

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
        let mut visitor = BodyVisitor {
            call_adder: &mut call_adder,
        };
        visitor.visit_body(body);
        modification.commit(body);
    }
}

struct BodyVisitor<'tcx, 'c, 'm> {
    call_adder: &'c mut RuntimeCallAdder<'tcx, 'm>,
}

impl<'tcx, 'c, 'm> Visitor<'tcx> for BodyVisitor<'tcx, 'c, 'm> {
    fn visit_basic_block_data(&mut self, block: BasicBlock, data: &BasicBlockData<'tcx>) {
        let mut visitor = BasicBlockVisitor {
            call_adder: &mut self.call_adder,
            current_block: block,
        };
        visitor.visit_basic_block_data(block, data);
    }
}

struct BasicBlockVisitor<'tcx, 'c, 'm> {
    call_adder: &'c mut RuntimeCallAdder<'tcx, 'm>,
    current_block: BasicBlock,
}

impl<'tcx, 'c, 'm> Visitor<'tcx> for BasicBlockVisitor<'tcx, 'c, 'm> {
    fn visit_statement(
        &mut self,
        statement: &rustc_middle::mir::Statement<'tcx>,
        location: Location,
    ) {
        LeafStatementKindVisitor {
            call_adder: self.call_adder,
            location,
        }
        .visit_statement_kind(&statement.kind)
    }
}

struct LeafStatementKindVisitor<'tcx, 'c, 'm> {
    call_adder: &'c mut RuntimeCallAdder<'tcx, 'm>,
    location: Location,
}

impl<'tcx, 'c, 'm> StatementKindVisitor<'tcx, ()> for LeafStatementKindVisitor<'tcx, 'c, 'm> {
    fn visit_assign(&mut self, place: &Place<'tcx>, rvalue: &Rvalue<'tcx>) -> () {
        LeafAssignmentVisitor::new(self.call_adder, self.location, place).visit_rvalue(rvalue)
    }

    fn visit_fake_read(
        &mut self,
        cause: &rustc_middle::mir::FakeReadCause,
        place: &Place<'tcx>,
    ) -> () {
        Default::default()
    }

    fn visit_set_discriminant(&mut self, place: &Place<'tcx>, variant_index: &VariantIdx) -> () {
        Default::default()
    }

    fn visit_deinit(&mut self, place: &Place<'tcx>) -> () {
        Default::default()
    }

    fn visit_storage_live(&mut self, local: &Local) -> () {
        Default::default()
    }

    fn visit_storage_dead(&mut self, local: &Local) -> () {
        Default::default()
    }

    fn visit_retag(&mut self, kind: &rustc_middle::mir::RetagKind, place: &Place<'tcx>) -> () {
        Default::default()
    }

    fn visit_ascribe_user_type(
        &mut self,
        place: &Place<'tcx>,
        user_type_proj: &rustc_middle::mir::UserTypeProjection,
        variance: &rustc_type_ir::Variance,
    ) -> () {
        Default::default()
    }

    fn visit_coverage(&mut self, coverage: &rustc_middle::mir::Coverage) -> () {
        Default::default()
    }

    fn visit_intrinsic(
        &mut self,
        intrinsic: &rustc_middle::mir::NonDivergingIntrinsic<'tcx>,
    ) -> () {
        Default::default()
    }

    fn visit_nop(&mut self) -> () {
        Default::default()
    }
}

struct LeafAssignmentVisitor<'tcx, 'c, 'm> {
    call_adder: RuntimeCallAdderForAssignment<'tcx, 'm, 'c>,
    location: Location,
}
impl<'tcx, 'c, 'm> LeafAssignmentVisitor<'tcx, 'c, 'm> {
    fn new(
        call_adder: &'c mut RuntimeCallAdder<'tcx, 'm>,
        location: Location,
        destination: &Place<'tcx>,
    ) -> Self {
        let dest_ref = call_adder.reference_place(location.block, destination);
        Self {
            call_adder: call_adder.assign(location.block, dest_ref),
            location,
        }
    }
}

impl<'tcx, 'c, 'm> RvalueVisitor<'tcx, ()> for LeafAssignmentVisitor<'tcx, 'c, 'm> {
    fn visit_use(&mut self, operand: &rustc_middle::mir::Operand<'tcx>) {
        let operand_ref = self.reference_operand(operand);
        self.call_adder.by_use(operand_ref)
    }

    fn visit_repeat(
        &mut self,
        operand: &rustc_middle::mir::Operand<'tcx>,
        count: &rustc_middle::ty::Const<'tcx>,
    ) {
        self.call_adder.by_repeat(
            self.reference_operand(operand),
            todo!("Convert {count} to number."),
        )
    }

    fn visit_ref(
        &mut self,
        _region: &rustc_middle::ty::Region,
        borrow_kind: &rustc_middle::mir::BorrowKind,
        place: &Place<'tcx>,
    ) {
        let place_ref = self.reference_place(place);
        self.call_adder.by_ref(
            place_ref,
            matches!(borrow_kind, rustc_middle::mir::BorrowKind::Mut { .. }),
        )
    }

    fn visit_thread_local_ref(&mut self) {
        self.call_adder.by_thread_local_ref();
    }

    fn visit_address_of(&mut self, mutability: &rustc_ast::Mutability, place: &Place<'tcx>) {
        let place_ref = self.reference_place(place);
        self.call_adder
            .by_address_of(place_ref, mutability.is_mut());
    }

    fn visit_len(&mut self, place: &Place<'tcx>) {
        let place_ref = self.reference_place(place);
        self.call_adder.by_len(place_ref)
    }

    fn visit_cast(
        &mut self,
        kind: &rustc_middle::mir::CastKind,
        operand: &rustc_middle::mir::Operand<'tcx>,
        ty: &rustc_middle::ty::Ty<'tcx>,
    ) {
        todo!()
    }

    fn visit_binary_op(
        &mut self,
        op: &rustc_middle::mir::BinOp,
        operands: &Box<(
            rustc_middle::mir::Operand<'tcx>,
            rustc_middle::mir::Operand<'tcx>,
        )>,
    ) {
        self.visit_binary_op_general(op, operands, false)
    }

    fn visit_checked_binary_op(
        &mut self,
        op: &rustc_middle::mir::BinOp,
        operands: &Box<(
            rustc_middle::mir::Operand<'tcx>,
            rustc_middle::mir::Operand<'tcx>,
        )>,
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

    fn visit_unary_op(
        &mut self,
        op: &rustc_middle::mir::UnOp,
        operand: &rustc_middle::mir::Operand<'tcx>,
    ) {
        let operand_ref = self.reference_operand(operand);
        self.call_adder.by_unary_op(op, operand_ref)
    }

    fn visit_discriminant(&mut self, place: &Place<'tcx>) {
        let place_ref = self.reference_place(place);
        self.call_adder.by_discriminant(place_ref)
    }

    fn visit_aggregate(
        &mut self,
        kind: &Box<rustc_middle::mir::AggregateKind>,
        operands: &Vec<rustc_middle::mir::Operand<'tcx>>,
    ) {
        // Based on compiler documents it is the only possible type that can reach here.
        assert!(matches!(
            kind,
            box rustc_middle::mir::AggregateKind::Array(_)
        ));

        let items = operands
            .iter()
            .map(|o| self.reference_operand(o))
            .collect::<Vec<Local>>();
        self.call_adder.by_aggregate_array(items.as_slice())
    }

    fn visit_shallow_init_box(
        &mut self,
        operand: &rustc_middle::mir::Operand<'tcx>,
        ty: &rustc_middle::ty::Ty<'tcx>,
    ) {
        todo!("Not sure yet.")
    }

    fn visit_copy_for_deref(&mut self, place: &Place<'tcx>) {
        todo!("Not sure yet.")
    }
}

impl<'tcx, 'c, 'm> LeafAssignmentVisitor<'tcx, 'c, 'm> {
    fn reference_place(&mut self, place: &Place<'tcx>) -> Local {
        self.call_adder
            .call_adder
            .reference_place(self.location.block, place)
    }

    fn reference_operand(&mut self, operand: &Operand<'tcx>) -> Local {
        self.call_adder
            .call_adder
            .reference_operand(self.location.block, operand)
    }

    fn visit_binary_op_general(
        &mut self,
        op: &rustc_middle::mir::BinOp,
        operands: &Box<(
            rustc_middle::mir::Operand<'tcx>,
            rustc_middle::mir::Operand<'tcx>,
        )>,
        checked: bool,
    ) {
        let first_ref = self.reference_operand(&operands.0);
        let second_ref = self.reference_operand(&operands.1);
        self.call_adder
            .by_binary_op(op, first_ref, second_ref, checked)
    }
}
