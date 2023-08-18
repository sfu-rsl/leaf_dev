mod call;

use rustc_abi::{FieldIdx, VariantIdx};
use rustc_index::IndexVec;
use rustc_middle::{
    mir::{
        self, visit::Visitor, BasicBlock, BasicBlockData, Body, BorrowKind, CastKind,
        HasLocalDecls, Location, Operand, Place, Rvalue, Statement, UnwindAction,
    },
    ty::TyCtxt,
};

use crate::{
    mir_transform::{split_blocks_with, BodyInstrumentationUnit, JumpTargetModifier},
    visit::*,
};

use call::{
    context::{BodyProvider, TyContextProvider},
    ctxtreqs, AssertionHandler, Assigner, BranchingHandler, BranchingReferencer, CastAssigner,
    EntryFunctionHandler, FunctionHandler,
    InsertionLocation::*,
    OperandRef, OperandReferencer, PlaceReferencer, RuntimeCallAdder,
};

use self::call::{context::LocationProvider, ctxtreqs::Basic};

use super::{CompilationPass, Storage};

#[derive(Default)]
pub(crate) struct Instrumentor;

impl CompilationPass for Instrumentor {
    fn transform_mir_body<'tcx>(
        tcx: TyCtxt<'tcx>,
        body: &mut Body<'tcx>,
        storage: &mut dyn Storage,
    ) {
        transform(tcx, body, storage);
    }
}

fn transform<'tcx>(tcx: TyCtxt<'tcx>, body: &mut Body<'tcx>, storage: &mut dyn Storage) {
    log::info!(
        "Running instrumentation pass on body of {:#?} at {:?}",
        body.source.def_id(),
        body.span,
    );

    let mut modification = BodyInstrumentationUnit::new(body.local_decls().next_index());
    let mut call_adder = RuntimeCallAdder::new(tcx, &mut modification, storage);
    let mut call_adder = call_adder.in_body(body);
    if tcx.entry_fn(()).expect("No entry function was found").0 == body.source.def_id() {
        handle_entry_function(
            &mut call_adder
                .in_entry_fn()
                .at(Before(body.basic_blocks.indices().next().unwrap())),
        );
    }

    // TODO: determine if body will ever be a promoted block
    let _is_promoted_block = body.source.promoted.is_some();
    call_adder
        .at(Before(body.basic_blocks.indices().next().unwrap()))
        .enter_func();

    VisitorFactory::make_body_visitor(&mut call_adder).visit_body(body);
    modification.commit(body);
}

fn handle_entry_function<'tcx, C>(call_adder: &mut RuntimeCallAdder<C>)
where
    C: ctxtreqs::ForEntryFunction<'tcx> + ctxtreqs::ForFunctionCalling<'tcx>,
{
    call_adder.init_runtime_lib();
}

struct VisitorFactory;

impl VisitorFactory {
    fn make_body_visitor<'tcx, 'c, C>(
        call_adder: &'c mut RuntimeCallAdder<C>,
    ) -> impl Visitor<'tcx> + 'c
    where
        C: ctxtreqs::Basic<'tcx> + JumpTargetModifier + BodyProvider<'tcx>,
    {
        LeafBodyVisitor {
            call_adder: RuntimeCallAdder::borrow_from(call_adder),
        }
    }

    fn make_basic_block_visitor<'tcx, 'c, C>(
        call_adder: &'c mut RuntimeCallAdder<C>,
        block: BasicBlock,
    ) -> impl Visitor<'tcx> + 'c
    where
        C: Basic<'tcx> + BodyProvider<'tcx> + JumpTargetModifier,
    {
        LeafBasicBlockVisitor {
            call_adder: call_adder.at(Before(block)),
        }
    }

    fn make_statement_kind_visitor<'tcx, 'b, C>(
        call_adder: &'b mut RuntimeCallAdder<C>,
    ) -> impl StatementKindVisitor<'tcx, ()> + 'b
    where
        C: ctxtreqs::ForPlaceRef<'tcx> + ctxtreqs::ForOperandRef<'tcx>,
    {
        LeafStatementKindVisitor {
            call_adder: RuntimeCallAdder::borrow_from(call_adder),
        }
    }

    fn make_terminator_kind_visitor<'tcx, 'b, C>(
        call_adder: &'b mut RuntimeCallAdder<C>,
    ) -> impl TerminatorKindVisitor<'tcx, ()> + 'b
    where
        C: ctxtreqs::ForOperandRef<'tcx>
            + ctxtreqs::ForPlaceRef<'tcx>
            + ctxtreqs::ForBranching<'tcx>
            + ctxtreqs::ForFunctionCalling<'tcx>
            + ctxtreqs::ForReturning<'tcx>,
    {
        LeafTerminatorKindVisitor {
            call_adder: RuntimeCallAdder::borrow_from(call_adder),
        }
    }

    fn make_assignment_visitor<'tcx, 'b, C>(
        call_adder: &'b mut RuntimeCallAdder<C>,
        destination: &Place<'tcx>,
    ) -> impl RvalueVisitor<'tcx, ()> + 'b
    where
        C: ctxtreqs::ForPlaceRef<'tcx> + ctxtreqs::ForOperandRef<'tcx>,
    {
        let dest_ref = call_adder.reference_place(destination);
        LeafAssignmentVisitor {
            call_adder: call_adder.assign(dest_ref),
        }
    }
}

macro_rules! make_general_visitor {
    ($vis:vis $name:ident) => {
        $vis struct $name<C> {
            call_adder: RuntimeCallAdder<C>,
        }
    };
}

make_general_visitor!(LeafBodyVisitor);

impl<'tcx, C> Visitor<'tcx> for LeafBodyVisitor<C>
where
    C: ctxtreqs::Basic<'tcx> + JumpTargetModifier + BodyProvider<'tcx>,
{
    fn visit_basic_block_data(&mut self, block: BasicBlock, data: &BasicBlockData<'tcx>) {
        if data.is_cleanup {
            // NOTE: Cleanup blocks will be investigated in #206.
            log::debug!("Skipping instrumenting cleanup block: {:?}", block);
            return;
        }

        VisitorFactory::make_basic_block_visitor(&mut self.call_adder, block)
            .visit_basic_block_data(block, data);
    }
}

make_general_visitor!(LeafBasicBlockVisitor);

impl<'tcx, C> Visitor<'tcx> for LeafBasicBlockVisitor<C>
where
    C: Basic<'tcx> + BodyProvider<'tcx> + LocationProvider + JumpTargetModifier,
{
    fn visit_statement(
        &mut self,
        statement: &rustc_middle::mir::Statement<'tcx>,
        location: Location,
    ) {
        log::debug!("Visiting statement: {:?} at {:?}", statement.kind, location);
        VisitorFactory::make_statement_kind_visitor(&mut self.call_adder.after())
            .visit_statement_kind(&statement.kind);
    }

    fn visit_terminator(&mut self, terminator: &mir::Terminator<'tcx>, _location: Location) {
        VisitorFactory::make_terminator_kind_visitor(&mut self.call_adder.before())
            .visit_terminator_kind(&terminator.kind);
    }
}

make_general_visitor!(LeafStatementKindVisitor);

impl<'tcx, C> StatementKindVisitor<'tcx, ()> for LeafStatementKindVisitor<C>
where
    C: ctxtreqs::ForPlaceRef<'tcx> + ctxtreqs::ForOperandRef<'tcx>,
{
    fn visit_assign(&mut self, place: &Place<'tcx>, rvalue: &Rvalue<'tcx>) {
        VisitorFactory::make_assignment_visitor(&mut self.call_adder, place).visit_rvalue(rvalue)
    }

    fn visit_set_discriminant(&mut self, place: &Place<'tcx>, variant_index: &VariantIdx) {
        let destination = self.call_adder.reference_place(place);
        self.call_adder
            .assign(destination)
            .its_discriminant_to(variant_index)
    }

    fn visit_deinit(&mut self, _place: &Place<'tcx>) {
        Default::default()
    }

    fn visit_intrinsic(&mut self, _intrinsic: &rustc_middle::mir::NonDivergingIntrinsic<'tcx>) {
        Default::default()
    }
}

make_general_visitor!(pub(crate) LeafTerminatorKindVisitor);

impl<'tcx, C> TerminatorKindVisitor<'tcx, ()> for LeafTerminatorKindVisitor<C>
where
    C: ctxtreqs::ForOperandRef<'tcx>
        + ctxtreqs::ForPlaceRef<'tcx>
        + ctxtreqs::ForBranching<'tcx>
        + ctxtreqs::ForFunctionCalling<'tcx>
        + ctxtreqs::ForReturning<'tcx>,
{
    fn visit_switch_int(&mut self, discr: &Operand<'tcx>, targets: &mir::SwitchTargets) {
        let switch_info = self.call_adder.store_branching_info(discr);
        let mut call_adder = self.call_adder.branch(switch_info);
        for (value, target) in targets.iter() {
            call_adder.at(Before(target)).take_by_value(value);
        }
        call_adder
            .at(Before(targets.otherwise()))
            .take_otherwise(targets.iter().map(|v| v.0));
    }

    fn visit_return(&mut self) {
        self.call_adder.return_from_func();
    }

    fn visit_unreachable(&mut self) {
        Default::default()
    }

    fn visit_drop(
        &mut self,
        _place: &Place<'tcx>,
        _target: &BasicBlock,
        _unwind: &UnwindAction,
        _replace: &bool,
    ) {
        Default::default()
    }

    fn visit_call(
        &mut self,
        func: &Operand<'tcx>,
        args: &[Operand<'tcx>],
        destination: &Place<'tcx>,
        target: &Option<BasicBlock>,
        _unwind: &UnwindAction,
        _call_source: &mir::CallSource,
        _fn_span: rustc_span::Span,
    ) {
        let func_ref = self.call_adder.reference_operand(func);
        let arg_refs = args
            .iter()
            .map(|arg| self.call_adder.reference_operand(arg))
            .collect::<Vec<_>>();
        Self::instrument_call(
            &mut self.call_adder,
            func_ref,
            arg_refs.into_iter(),
            destination,
            target,
        );
    }

    fn visit_assert(
        &mut self,
        cond: &Operand<'tcx>,
        expected: &bool,
        msg: &mir::AssertMessage<'tcx>,
        // we ignore target because this is concolic execution, not symbolic (program execution guides location)
        _target: &BasicBlock,
        _unwind: &UnwindAction,
    ) {
        let cond_ref = self.call_adder.reference_operand(cond);
        log::debug!("looking at assert message: '{:?}'", msg);
        self.call_adder.check_assert(cond_ref, *expected, msg);
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

    fn visit_generator_drop(&mut self) {
        Default::default()
    }

    fn visit_inline_asm(
        &mut self,
        _template: &&[rustc_ast::InlineAsmTemplatePiece],
        _operands: &[mir::InlineAsmOperand<'tcx>],
        _options: &rustc_ast::InlineAsmOptions,
        _line_spans: &'tcx [rustc_span::Span],
        _destination: &Option<BasicBlock>,
        _unwind: &UnwindAction,
    ) {
        Default::default()
    }
}

impl<'tcx, C> LeafTerminatorKindVisitor<C>
where
    C: ctxtreqs::ForFunctionCalling<'tcx>,
{
    fn instrument_call(
        call_adder: &mut RuntimeCallAdder<C>,
        func: OperandRef,
        args: impl Iterator<Item = OperandRef>,
        destination: &Place<'tcx>,
        target: &Option<BasicBlock>,
    ) {
        call_adder.before().before_call_func(func, args);

        if target.is_some() {
            call_adder.after().after_call_func(destination);
        } else {
            // This branch is only triggered by hitting a divergent function:
            // https://doc.rust-lang.org/rust-by-example/fn/diverging.html
            // (this means the program will exit immediately)
            log::warn!("visit_call() had no target, so couldn't insert block");
        }
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
        let operand_ref = self.call_adder.reference_operand(operand);
        self.call_adder.by_repeat(operand_ref, count)
    }

    fn visit_ref(
        &mut self,
        _region: &rustc_middle::ty::Region,
        borrow_kind: &BorrowKind,
        place: &Place<'tcx>,
    ) {
        Self::instrument_ref(&mut self.call_adder, borrow_kind, place)
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
        kind: &CastKind,
        operand: &Operand<'tcx>,
        ty: &rustc_middle::ty::Ty<'tcx>,
    ) {
        let operand_ref = self.call_adder.reference_operand(operand);
        let call_adder = &mut self.call_adder.by_cast(operand_ref);
        use CastKind::*;
        match kind {
            IntToInt | FloatToInt => call_adder.to_int(*ty),
            IntToFloat | FloatToFloat => call_adder.to_float(*ty),
            Pointer(kind) => {
                use rustc_middle::ty::adjustment::PointerCast::*;
                match kind {
                    Unsize => call_adder.through_unsizing(),
                    ReifyFnPointer | UnsafeFnPointer | ClosureFnPointer(_) => {
                        todo!("Support FnPointer casts")
                    }
                    MutToConstPointer | ArrayToPointer => todo!("Support raw pointer casts"),
                }
            }
            PointerExposeAddress => todo!("Support PointerExposeAddress casts"),
            PointerFromExposedAddress => todo!("Support PointerFromExposedAddress casts"),
            PtrToPtr => todo!("Support PtrToPtr casts"),
            FnPtrToPtr => todo!("Support FnPtrToPtr casts"),
            DynStar => todo!("Support DynStar casts"),
            Transmute => todo!("Support transmute casts"),
        }
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

    fn visit_aggregate(
        &mut self,
        kind: &Box<mir::AggregateKind>,
        operands: &IndexVec<FieldIdx, Operand<'tcx>>,
    ) {
        let operands: Vec<OperandRef> = operands
            .iter()
            .map(|o| self.call_adder.reference_operand(o))
            .collect();

        use mir::AggregateKind::*;
        #[allow(clippy::type_complexity)]
        let mut add_call: Box<dyn FnMut(&[OperandRef])> = match kind.as_ref() {
            Array(_) => {
                Box::new(|items| self.call_adder.by_aggregate_array(items))
            }
            Tuple => Box::new(|fields| {
                self.call_adder.by_aggregate_tuple(fields);
            }),
            Adt(def_id, variant, _, _, None) => {
                use rustc_hir::def::DefKind;
                match self.call_adder.tcx().def_kind(def_id) {
                    DefKind::Enum => Box::new(|fields| {self.call_adder.by_aggregate_enum(fields, *variant)}),
                    DefKind::Struct => Box::new(|fields| {
                        self.call_adder.by_aggregate_struct(fields)
                    }),
                    _ => unreachable!("Only enums and structs are supposed to be ADT.")
                }
            }
            Adt(_, _, _, _, Some(active_field)) /* Union */ => Box::new(|fields| {
                assert_eq!(
                    fields.len(),
                    1,
                    "For a union, there should only be one field."
                );
                self.call_adder.by_aggregate_union(*active_field, fields[0])
            }),
            Closure(_, _) => todo!("Closures are not supported yet."),
            Generator(_, _, _) => todo!("Generators are not supported yet."),
        };

        add_call(operands.as_slice())
    }

    fn visit_shallow_init_box(
        &mut self,
        _operand: &Operand<'tcx>,
        _ty: &rustc_middle::ty::Ty<'tcx>,
    ) {
        todo!("Not sure yet.")
    }

    fn visit_copy_for_deref(&mut self, place: &Place<'tcx>) {
        let operand = Operand::Copy(*place);
        self.visit_use(&operand)
    }
}

impl<'tcx, C> LeafAssignmentVisitor<C>
where
    C: ctxtreqs::ForAssignment<'tcx>,
{
    fn instrument_ref(
        call_adder: &mut RuntimeCallAdder<C>,
        borrow_kind: &BorrowKind,
        place: &Place<'tcx>,
    ) where
        C: ctxtreqs::ForPlaceRef<'tcx>,
    {
        let place_ref = call_adder.reference_place(place);
        call_adder.by_ref(place_ref, matches!(borrow_kind, BorrowKind::Mut { .. }))
    }

    fn visit_binary_op_general(
        &mut self,
        op: &mir::BinOp,
        operands: &(Operand<'tcx>, Operand<'tcx>),
        checked: bool,
    ) where
        C: ctxtreqs::ForOperandRef<'tcx>,
    {
        let first_ref = self.call_adder.reference_operand(&operands.0);
        let second_ref = self.call_adder.reference_operand(&operands.1);
        self.call_adder
            .by_binary_op(op, first_ref, second_ref, checked)
    }
}
