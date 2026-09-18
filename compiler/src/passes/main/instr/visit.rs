use rustc_abi::VariantIdx;
use rustc_middle::{
    mir::{
        self, BasicBlock, BasicBlockData, Body, Location, Operand, Place, Rvalue, UnwindAction,
        visit::Visitor,
    },
    ty::{self as mir_ty, IntrinsicDef},
};
use rustc_span::{Span, Spanned, def_id::DefId};

use std::{collections::BTreeMap, rc::Rc};

use common::{
    log_debug, log_info, log_warn,
    pri::{AssignmentId, AtomicBinaryOp},
};

use crate::{
    utils::mir_transform::JumpTargetModifier,
    utils::{assignment_ids::assignment_ids_split_agnostic, mir::TyCtxtExt},
    visit::*,
};

use super::{
    TAG_INSTR,
    call::{
        AssertionHandler, AssignmentHandler, AtomicIntrinsicHandler, BranchingHandler, DropHandler,
        EntryFunctionHandler, FunctionHandler,
        InsertionLocation::*,
        IntrinsicHandler, MemoryIntrinsicHandler, RuntimeCallAdder, StorageMarker,
        context::{
            BlockIndexProvider, BlockOriginalIndexProvider, BodyProvider, ConfigProvider,
            PriItemsProvider, SourceInfoProvider, TyContextProvider,
        },
        ctxt_reqs as cr,
    },
    decision::{self, AtomicIntrinsicKind},
    pri::sym::intrinsics::LeafIntrinsicSymbol,
};

pub(super) fn instrument_body<'tcx, 'c, 'body, C>(
    call_adder: &'c mut RuntimeCallAdder<C>,
    body: &'body Body<'tcx>,
) where
    C: cr::Basic<'tcx> + BlockOriginalIndexProvider + JumpTargetModifier,
{
    let is_entry = call_adder
        .tcx()
        .entry_fn(())
        .is_some_and(|(id, _)| id == body.source.def_id());

    if is_entry {
        handle_entry_function_pre(call_adder, body);
    }

    // Insert some instrumentation at the beginning of the body.
    {
        let mut call_adder = call_adder.at(Before(body.basic_blocks.indices().next().unwrap()));
        let mut call_adder = call_adder.with_source_info(*body.source_info(Location::START));
        handle_body_pre_blocks(&mut call_adder);
    }

    VisitorFactory::make_body_visitor(call_adder).visit_body(body);

    if is_entry {
        handle_entry_function_post(call_adder, body);
    }
}

fn handle_entry_function_pre<'tcx, C>(call_adder: &mut RuntimeCallAdder<C>, body: &Body<'tcx>)
where
    C: cr::Basic<'tcx>,
{
    let mut call_adder = call_adder.in_entry_fn();
    let first_block = body.basic_blocks.indices().next().unwrap();
    let mut call_adder = call_adder.with_source_info(*body.source_info(Location::START));
    let mut call_adder = call_adder.at(Before(first_block));
    call_adder.init_runtime_lib();
}

fn handle_entry_function_post<'tcx, C>(call_adder: &mut RuntimeCallAdder<C>, body: &Body<'tcx>)
where
    C: cr::Basic<'tcx>,
{
    let mut call_adder = call_adder.in_entry_fn();
    body.basic_blocks
        .iter_enumerated()
        .filter(|(_, bb)| bb.terminator().kind == mir::TerminatorKind::Return)
        .for_each(|(index, bb)| {
            call_adder
                .at(Before(index))
                .with_source_info(bb.terminator().source_info)
                .shutdown_runtime_lib();
        });
}

fn handle_body_pre_blocks<'tcx, C>(call_adder: &mut RuntimeCallAdder<C>)
where
    C: cr::ForFunctionCalling<'tcx> + cr::ForStorageMarking<'tcx>,
{
    call_adder.enter_func();

    rustc_mir_dataflow::impls::always_storage_live_locals(call_adder.body())
        .iter()
        .for_each(|l| match call_adder.body().local_kind(l) {
            mir::LocalKind::Temp => {
                call_adder.mark_live(&l.into());
            }
            mir::LocalKind::Arg => {}
            mir::LocalKind::ReturnPointer => {}
        });
}

struct VisitorFactory;

impl VisitorFactory {
    fn make_body_visitor<'tcx, 'c, C>(
        call_adder: &'c mut RuntimeCallAdder<C>,
    ) -> impl Visitor<'tcx> + 'c
    where
        C: cr::Basic<'tcx> + BlockOriginalIndexProvider + JumpTargetModifier,
    {
        let assignment_ids = assignment_ids_split_agnostic(call_adder.tcx(), call_adder.body())
            .map(|(loc, _, id)| (loc, id))
            .collect();
        LeafBodyVisitor {
            call_adder: RuntimeCallAdder::borrow_from(call_adder),
            assignment_ids: Rc::new(assignment_ids),
        }
    }

    fn make_basic_block_visitor<'tcx, 'c, C>(
        call_adder: &'c mut RuntimeCallAdder<C>,
        block: BasicBlock,
        assignment_ids: Rc<AssignmentIdMap>,
    ) -> impl Visitor<'tcx> + 'c
    where
        C: cr::Basic<'tcx> + BlockOriginalIndexProvider + JumpTargetModifier,
    {
        LeafBasicBlockVisitor {
            call_adder: call_adder.at(Before(block)),
            assignment_ids,
        }
    }

    fn make_statement_kind_visitor<'tcx, 'b, C>(
        call_adder: &'b mut RuntimeCallAdder<C>,
        assignment_id: Option<AssignmentId>,
    ) -> impl StatementKindVisitor<'tcx, ()> + 'b
    where
        C: cr::ForPlaceRef<'tcx> + cr::ForOperandRef<'tcx>,
    {
        LeafStatementKindVisitor {
            call_adder: RuntimeCallAdder::borrow_from(call_adder),
            assignment_id,
        }
    }

    fn make_terminator_kind_visitor<'tcx, 'b, C>(
        call_adder: &'b mut RuntimeCallAdder<C>,
        assignment_id: Option<AssignmentId>,
    ) -> impl TerminatorKindVisitor<'tcx, ()> + 'b
    where
        C: cr::ForPlaceRef<'tcx>
            + cr::ForOperandRef<'tcx>
            + cr::ForBranching<'tcx>
            + cr::ForReturning<'tcx>,
    {
        LeafTerminatorKindVisitor {
            call_adder: RuntimeCallAdder::borrow_from(call_adder),
            assignment_id,
        }
    }
}

macro_rules! make_general_visitor {
    ($vis:vis $name:ident $({ $($field_name: ident : $field_ty: ty),* $(,)? })?) => {
        $vis struct $name<C> {
            call_adder: RuntimeCallAdder<C>,
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
            &mut self.call_adder,
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
                .call_adder
                .with_source_info(statement.source_info)
                .before(),
            self.assignment_ids.get(&location).copied(),
        )
        .visit_statement_kind(&statement.kind);
    }

    fn visit_terminator(&mut self, terminator: &mir::Terminator<'tcx>, location: Location) {
        VisitorFactory::make_terminator_kind_visitor(
            &mut self
                .call_adder
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
        self.call_adder
            .assign(self.assignment_id.unwrap(), *place)
            .to_rvalue(rvalue)
    }

    fn visit_set_discriminant(&mut self, place: &Place<'tcx>, variant_index: &VariantIdx) {
        self.call_adder
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
                instrument_memory_intrinsic_copy_non_overlapping(
                    &mut self.call_adder,
                    src,
                    dst,
                    count,
                    self.assignment_id.unwrap(),
                );
            }
        }
    }

    fn visit_storage_live(&mut self, local: &mir::Local) -> () {
        let mut call_adder = self.call_adder.after();
        call_adder.mark_live(&(*local).into());
    }

    fn visit_storage_dead(&mut self, local: &mir::Local) {
        self.call_adder.before().mark_dead(&(*local).into());
    }
}

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
        self.call_adder.switch(discr, targets);
    }

    fn visit_return(&mut self) {
        rustc_mir_dataflow::impls::always_storage_live_locals(self.call_adder.body())
            .iter()
            .for_each(|l| {
                if self.call_adder.body().local_kind(l) == mir::LocalKind::ReturnPointer {
                    return;
                }
                self.call_adder.before().mark_dead(&l.into());
            });

        self.call_adder.return_from_func();
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
        let mut call_adder = self.call_adder.before();
        call_adder.before_call_drop(place);

        let mut call_adder = call_adder.after();
        call_adder.after_call_drop();
    }

    fn visit_call(
        &mut self,
        func: &Operand<'tcx>,
        args: &[Spanned<Operand<'tcx>>],
        destination: &Place<'tcx>,
        target: &Option<BasicBlock>,
        _unwind: &UnwindAction,
        _call_source: &mir::CallSource,
        fn_span: Span,
    ) {
        let tcx = self.call_adder.tcx();
        let opt_def_id =
            if let mir_ty::TyKind::FnDef(def_id, ..) = func.ty(&self.call_adder, tcx).kind() {
                assert!(
                    !self.call_adder.all_pri_items().contains(def_id),
                    "Instrumenting our own instrumentation."
                );
                Some(*def_id)
            } else {
                None
            };

        let params = CallParams {
            func,
            args,
            destination,
            target,
            fn_span,
        };

        match opt_def_id {
            Some(def_id) if tcx.is_llvm_intrinsic(def_id) => {
                self.instrument_llvm_intrinsic_call(params)
            }
            Some(def_id) if let Some(intrinsic) = tcx.intrinsic(def_id) => {
                self.instrument_intrinsic_call((def_id, intrinsic), params)
            }
            Some(def_id)
                if tcx
                    .lang_items()
                    .drop_glue_fn()
                    .is_some_and(|id| id == def_id) =>
            {
                self.instrument_drop_in_place_call(params)
            }
            _ => self.instrument_regular_call(params),
        }
    }

    fn visit_tail_call(
        &mut self,
        _func: &Operand<'tcx>,
        _args: &[Spanned<Operand<'tcx>>],
        _fn_span: Span,
    ) -> () {
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
        self.call_adder.check_assert(cond, *expected, msg);
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
}

struct CallParams<'a, 'tcx> {
    func: &'a Operand<'tcx>,
    args: &'a [Spanned<Operand<'tcx>>],
    destination: &'a Place<'tcx>,
    target: &'a Option<BasicBlock>,
    fn_span: Span,
}

impl<'tcx, C> LeafTerminatorKindVisitor<C>
where
    C: cr::ForOperandRef<'tcx> + cr::ForPlaceRef<'tcx> + cr::ForFunctionCalling<'tcx>,
{
    fn instrument_intrinsic_call(
        &mut self,
        (def_id, def): (DefId, IntrinsicDef),
        params: CallParams<'_, 'tcx>,
    ) {
        use decision::IntrinsicDecision::*;
        match decision::decide_intrinsic_call(def) {
            OneToOneAssign(func_name) => {
                self.instrument_one_to_one_intrinsic_call(def_id, func_name, params);
            }
            Atomic(kind) => {
                // Source: rustc_codegen_llvm/builder/struct.GenericBuilder.html#method.codegen_intrinsic_call
                let parse_ordering = |at| {
                    params
                        .func
                        .const_fn_def()
                        .unwrap()
                        .1
                        .const_at(at)
                        .to_value()
                        .valtree
                        .to_branch()[0]
                        .to_leaf()
                        .to_atomic_ordering()
                };
                use AtomicIntrinsicKind::*;
                self.instrument_atomic_intrinsic_call(
                    &params,
                    parse_ordering(match kind {
                        Load | Store | Exchange | CompareExchange { .. } => 1,
                        BinOp(AtomicBinaryOp::MAX | AtomicBinaryOp::MIN) => 1,
                        BinOp(..) => 2,
                        Fence { .. } => 0,
                    }),
                    match kind {
                        CompareExchange { .. } => Some(parse_ordering(2)),
                        _ => None,
                    },
                    kind,
                );
            }
            Memory { kind, is_volatile } => {
                self.instrument_memory_intrinsic_call(
                    &params,
                    self.assignment_id.unwrap(),
                    kind,
                    is_volatile,
                );
            }
            NoOp => {
                self.instrument_noop_intrinsic_call(params);
            }
            Contract => {
                // Currently, no instrumentation
                Default::default()
            }
            ToDo | ConstEvaluated => {
                log_warn!(
                    target: TAG_INSTR,
                    "Intrinsic call to {:?} observed.",
                    def.name
                );
                self.instrument_unsupported_call(params);
            }
            NotPlanned => {
                log_warn!(
                    target: TAG_INSTR,
                    concat!(
                        "Intrinsic call to {:?} observed, which is not planned to be supported.",
                        "You might want to revise the target program.",
                    ),
                    def.name
                );
                self.instrument_unsupported_call(params);
            }
            Unsupported => {
                log_info!(
                    target: TAG_INSTR,
                    "Intrinsic call to {:?} observed, which is not yet supported.",
                    def.name
                );
                self.instrument_unsupported_call(params)
            }
            Unexpected => {
                panic!(
                    "Unexpected intrinsic call to {:?} observed at {:?}.",
                    def.name, params.fn_span,
                );
            }
        }
    }

    fn instrument_one_to_one_intrinsic_call(
        &mut self,
        def_id: DefId,
        func_name: LeafIntrinsicSymbol,
        params: CallParams<'_, 'tcx>,
    ) {
        use decision::rules::EventDecision::*;

        let rules = &self.call_adder.config().assignment_filter;
        let filter = match params.args.len() {
            1 => rules.intrinsic_unary_op,
            2 => rules.intrinsic_binary_op,
            3 => rules.intrinsic_ternary_op,
            _ => rules.intrinsic_misc_op,
        };

        match filter {
            Omit => return,
            Opaque | Detailed => {
                let mut call_adder = self.call_adder.before();
                let mut call_adder =
                    call_adder.assign(self.assignment_id.unwrap(), params.destination.clone());

                match filter {
                    Detailed => {
                        call_adder.intrinsic_one_to_one_by(def_id, func_name, params.args.iter());
                    }
                    Opaque => {
                        call_adder.add_opaque_assignment();
                    }
                    _ => unreachable!(),
                }
            }
        }
    }

    fn instrument_memory_intrinsic_call(
        &mut self,
        params: &CallParams<'_, 'tcx>,
        assignment_id: AssignmentId,
        kind: decision::MemoryIntrinsicKind,
        is_volatile: bool,
    ) {
        instrument_memory_intrinsic_call(
            &mut self.call_adder,
            &params.args,
            params.destination,
            assignment_id,
            kind,
            is_volatile,
        );
    }

    fn instrument_atomic_intrinsic_call(
        &mut self,
        params: &CallParams<'_, 'tcx>,
        ordering: mir_ty::AtomicOrdering,
        failure_ordering: Option<mir_ty::AtomicOrdering>,
        kind: AtomicIntrinsicKind,
    ) {
        use AtomicIntrinsicKind::*;
        use decision::rules::EventDecision::*;

        let convert_ordering = |ord: mir_ty::AtomicOrdering| match ord {
            mir_ty::AtomicOrdering::Relaxed => common::pri::AtomicOrdering::RELAXED,
            mir_ty::AtomicOrdering::Release => common::pri::AtomicOrdering::RELEASE,
            mir_ty::AtomicOrdering::Acquire => common::pri::AtomicOrdering::ACQUIRE,
            mir_ty::AtomicOrdering::AcqRel => common::pri::AtomicOrdering::ACQ_REL,
            mir_ty::AtomicOrdering::SeqCst => common::pri::AtomicOrdering::SEQ_CST,
        };
        let ordering = convert_ordering(ordering);
        let failure_ordering = failure_ordering.map(convert_ordering);

        let rules = &self.call_adder.config().assignment_filter;
        let filter = match kind {
            Load | Store | Exchange | CompareExchange { .. } => rules.atomic_memory_op,
            BinOp(..) => rules.atomic_binary_op,
            Fence { .. } => {
                // FIXME: Add config.
                Detailed
            }
        };

        match filter {
            Omit => return,
            Opaque | Detailed => {
                let mut call_adder = self.call_adder.before();

                match kind {
                    Fence { single_thread } => {
                        call_adder
                            .perform_atomic_op(ordering, None)
                            .fence(single_thread);
                    }
                    Load | Store | Exchange | CompareExchange { .. } | BinOp(..) => {
                        let mut call_adder =
                            call_adder.assign(self.assignment_id.unwrap(), *params.destination);

                        match filter {
                            Detailed => {
                                let ptr_arg = params.args.get(0).unwrap();
                                let mut call_adder =
                                    call_adder.perform_atomic_op(ordering, Some(ptr_arg.clone()));

                                match kind {
                                    Load => call_adder.load(),
                                    Store => call_adder.store(&params.args[1]),
                                    BinOp(binop) => {
                                        call_adder.binary_op(binop, &params.args[1]);
                                    }
                                    Exchange => call_adder.exchange(&params.args[1]),
                                    CompareExchange { weak } => call_adder.compare_exchange(
                                        failure_ordering.unwrap(),
                                        weak,
                                        &params.args[1],
                                        &params.args[2],
                                    ),
                                    Fence { .. } => unreachable!(),
                                }
                            }
                            Opaque => call_adder.add_opaque_assignment(),
                            _ => unreachable!(),
                        }
                    }
                };
            }
        }
    }

    fn instrument_llvm_intrinsic_call(&mut self, params: CallParams<'_, 'tcx>) {
        // Currently, we do not support for LLVM intrinsics.
        self.instrument_unsupported_call(params);
    }

    fn instrument_drop_in_place_call(
        &mut self,
        CallParams {
            func,
            args,
            destination: _,
            target,
            fn_span: _,
        }: CallParams<'_, 'tcx>,
    ) {
        let mut call_adder = self.call_adder.before();
        assert_eq!(args.len(), 1);
        call_adder.before_call_drop_in_place(func, &args[0]);

        if target.is_some() {
            let mut call_adder = call_adder.after();
            call_adder.after_call_drop();
        }
    }

    fn instrument_regular_call(&mut self, params: CallParams<'_, 'tcx>) {
        self.instrument_call_general(params, false);
    }

    fn instrument_noop_intrinsic_call(&mut self, params: CallParams<'_, 'tcx>) {
        // Although ineffective in runtime, we still report it.
        self.instrument_call_general(params, true);
    }

    fn instrument_unsupported_call(&mut self, params: CallParams<'_, 'tcx>) {
        self.instrument_call_general(params, true);
    }

    fn instrument_call_general(
        &mut self,
        CallParams {
            func,
            args,
            destination,
            target,
            fn_span: _,
        }: CallParams<'_, 'tcx>,
        no_definition: bool,
    ) {
        let mut call_adder = self.call_adder.before();

        call_adder.before_call_func(func, args, no_definition);

        if target.is_none() {
            // This branch is only triggered by hitting a divergent function:
            // https://doc.rust-lang.org/rust-by-example/fn/diverging.html
            // (this means the program will exit immediately)
            return;
        }

        let mut call_adder = call_adder.after();
        let mut call_adder = call_adder.assign(self.assignment_id.unwrap(), *destination);
        call_adder.after_call_func();
    }
}

fn instrument_memory_intrinsic_call<'tcx, 'a, C>(
    call_adder: &mut RuntimeCallAdder<C>,
    args: &'a [Spanned<Operand<'tcx>>],
    destination: &'a Place<'tcx>,
    assignment_id: AssignmentId,
    kind: decision::MemoryIntrinsicKind,
    is_volatile: bool,
) where
    C: cr::ForOperandRef<'tcx> + cr::ForPlaceRef<'tcx>,
{
    use decision::MemoryIntrinsicKind::*;
    use decision::rules::EventDecision::*;

    let filter = (&call_adder.config().assignment_filter).intrinsic_memory_op;
    match filter {
        Omit => return,
        Opaque | Detailed => {
            let mut call_adder = call_adder.before();

            let mut call_adder = call_adder.assign(assignment_id, *destination);

            if matches!(filter, Opaque) {
                call_adder.add_opaque_assignment();
                return;
            }

            let ptr_arg = match (&kind, is_volatile) {
                // `volatile_copy_memory`, `volatile_copy_nonoverlapping_memory` have dst first!
                (Copy { .. }, true) => args.get(1),
                _ => args.get(0),
            };
            let mut call_adder = call_adder.perform_memory_op(is_volatile, ptr_arg.cloned());

            match kind {
                Load { is_ptr_aligned } => call_adder.load(is_ptr_aligned),
                Store { is_ptr_aligned } => call_adder.store(&args[1], is_ptr_aligned),
                Copy { is_overlapping } => {
                    let dest: &Spanned<Operand<'tcx>> =
                        if is_volatile { &args[0] } else { &args[1] };
                    call_adder.copy(dest, &args[2], is_overlapping)
                }
                Set => {
                    call_adder.set(&args[1], &args[2]);
                }
                Swap => {
                    call_adder.swap(&args[1]);
                }
                RawEq => {
                    call_adder.raw_eq(&args[1]);
                }
                CompareBytes => {
                    call_adder.compare_bytes(&args[1], &args[2]);
                }
            }
        }
    }
}

fn instrument_memory_intrinsic_copy_non_overlapping<'tcx, 'a, C>(
    call_adder: &mut RuntimeCallAdder<C>,
    src: &Operand<'tcx>,
    dst: &Operand<'tcx>,
    count: &Operand<'tcx>,
    assignment_id: AssignmentId,
) where
    C: cr::ForOperandRef<'tcx>,
{
    use decision::rules::EventDecision::*;

    match call_adder.config().assignment_filter.intrinsic_memory_op {
        Omit | Opaque => return,
        Detailed => (),
    }

    let [src, dst, count] = {
        let span = call_adder.source_info().span;
        [src, dst, count].map(|op| Spanned {
            node: op.clone(),
            span,
        })
    };

    let mut call_adder = call_adder.before();
    let mut call_adder = call_adder.memory_write(assignment_id);
    let mut call_adder = call_adder.perform_memory_op(false, Some(src.clone()));
    call_adder.copy(&dst, &count, false);
}
