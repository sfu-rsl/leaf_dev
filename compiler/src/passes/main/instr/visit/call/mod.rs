mod intrinsic_decision;
pub(super) mod mem_intrinsics;

use rustc_hir::def_id::DefId;
use rustc_middle::{
    mir::{Operand, Place},
    ty::IntrinsicDef,
};
use rustc_span::Spanned;

use common::pri::AtomicBinaryOp;
use common::{log_info, log_warn};

use crate::utils::mir::TyCtxtExt;

use super::super::{
    TAG_INSTR,
    insertion::{
        AtomicIntrinsicHandler, DropHandler, FunctionHandler, IntrinsicHandler, ProbeInserter,
        context::{AssignmentIdProvider, PriItemsProvider, TyContextProvider},
        ctxt_reqs::{self as cr, ForAssignment},
    },
    pri::sym::intrinsics::LeafIntrinsicSymbol,
};
use super::{AssignmentId, mir_ty};

pub(super) struct CallParams<'a, 'tcx> {
    pub func: &'a Operand<'tcx>,
    pub args: &'a [Spanned<Operand<'tcx>>],
    pub destination: &'a Place<'tcx>,
    pub is_diverging: bool,
}

pub(super) fn handle_call<'tcx, 'c, C>(
    inserter: &'c mut ProbeInserter<C>,
    assignment_id: Option<AssignmentId>,
    params: CallParams<'_, 'tcx>,
) where
    C: cr::ForFunctionCalling<'tcx> + cr::ForDropping<'tcx>,
{
    let tcx = inserter.tcx();
    let def_id = if let mir_ty::TyKind::FnDef(def_id, ..) = params.func.ty(inserter, tcx).kind() {
        *def_id
    } else {
        // Example: function pointer.
        let mut inserter = inserter.assign(assignment_id.unwrap(), *params.destination);
        instrument_regular_call(&mut inserter, params);
        return;
    };

    assert!(
        !inserter.all_pri_items().contains(&def_id),
        "Instrumenting our own instrumentation."
    );

    if tcx
        .lang_items()
        .drop_glue_fn()
        .is_some_and(|id| id == def_id)
    {
        return instrument_drop_in_place_call(inserter, params);
    }

    let mut inserter = inserter.assign(assignment_id.unwrap(), *params.destination);
    if tcx.is_llvm_intrinsic(def_id) {
        instrument_llvm_intrinsic_call(&mut inserter, params)
    } else if let Some(intrinsic) = tcx.intrinsic(def_id) {
        instrument_intrinsic_call(&mut inserter, (def_id, intrinsic), params)
    } else {
        instrument_regular_call(&mut inserter, params)
    }
}

fn instrument_intrinsic_call<'tcx, 'c, C>(
    inserter: &'c mut ProbeInserter<C>,
    (def_id, def): (DefId, IntrinsicDef),
    params: CallParams<'_, 'tcx>,
) where
    C: cr::ForFunctionCallingWithResult<'tcx>,
{
    use intrinsic_decision::IntrinsicDecision::*;
    match intrinsic_decision::decide_intrinsic_call(def) {
        OneToOneAssign(func_name) => {
            instrument_one_to_one_intrinsic_call(inserter, def_id, func_name, params);
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
            use intrinsic_decision::AtomicIntrinsicKind::*;
            instrument_atomic_intrinsic_call(
                inserter,
                Some(inserter.assignment_id()),
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
            instrument_memory_intrinsic_call(inserter, &params, kind, is_volatile);
        }
        NoOp => {
            instrument_noop_intrinsic_call(inserter, params);
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
            instrument_unsupported_call(inserter, params);
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
            instrument_unsupported_call(inserter, params);
        }
        Unsupported => {
            log_info!(
                target: TAG_INSTR,
                "Intrinsic call to {:?} observed, which is not yet supported.",
                def.name
            );
            instrument_unsupported_call(inserter, params)
        }
        Unexpected => {
            panic!("Unexpected intrinsic call to {:?} observed.", def.name,);
        }
    }
}

fn instrument_one_to_one_intrinsic_call<'tcx, 'c, C>(
    inserter: &'c mut ProbeInserter<C>,
    def_id: DefId,
    func_name: LeafIntrinsicSymbol,
    params: CallParams<'_, 'tcx>,
) where
    C: cr::ForAssignment<'tcx>,
{
    let mut inserter = inserter.before();
    inserter.intrinsic_one_to_one_by(def_id, func_name, params.args.iter());
}

fn instrument_memory_intrinsic_call<'tcx, 'c, C>(
    inserter: &'c mut ProbeInserter<C>,
    params: &CallParams<'_, 'tcx>,
    kind: intrinsic_decision::MemoryIntrinsicKind,
    is_volatile: bool,
) where
    C: ForAssignment<'tcx>,
{
    let mut inserter = inserter.before();
    mem_intrinsics::instrument_memory_intrinsic_call(
        &mut inserter,
        &params.args,
        kind,
        is_volatile,
    );
}

fn instrument_atomic_intrinsic_call<'tcx, 'c, C>(
    inserter: &mut ProbeInserter<C>,
    assignment_id: Option<AssignmentId>, // Optional because of `fence`
    params: &CallParams<'_, 'tcx>,
    ordering: mir_ty::AtomicOrdering,
    failure_ordering: Option<mir_ty::AtomicOrdering>,
    kind: intrinsic_decision::AtomicIntrinsicKind,
) where
    C: cr::ForInsertion<'tcx>,
{
    let convert_ordering = |ord: mir_ty::AtomicOrdering| match ord {
        mir_ty::AtomicOrdering::Relaxed => common::pri::AtomicOrdering::RELAXED,
        mir_ty::AtomicOrdering::Release => common::pri::AtomicOrdering::RELEASE,
        mir_ty::AtomicOrdering::Acquire => common::pri::AtomicOrdering::ACQUIRE,
        mir_ty::AtomicOrdering::AcqRel => common::pri::AtomicOrdering::ACQ_REL,
        mir_ty::AtomicOrdering::SeqCst => common::pri::AtomicOrdering::SEQ_CST,
    };
    let ordering = convert_ordering(ordering);
    let failure_ordering = failure_ordering.map(convert_ordering);

    use intrinsic_decision::AtomicIntrinsicKind::*;
    match kind {
        Fence { single_thread } => {
            inserter
                .perform_atomic_op(ordering, None)
                .fence(single_thread);
        }
        Load | Store | Exchange | CompareExchange { .. } | BinOp(..) => {
            let mut inserter = inserter.assign(assignment_id.unwrap(), *params.destination);
            let ptr_arg = params.args.get(0).unwrap();
            let mut inserter = inserter.perform_atomic_op(ordering, Some(ptr_arg.clone()));

            match kind {
                Load => inserter.load(),
                Store => inserter.store(&params.args[1]),
                BinOp(binop) => {
                    inserter.binary_op(binop, &params.args[1]);
                }
                Exchange => inserter.exchange(&params.args[1]),
                CompareExchange { weak } => inserter.compare_exchange(
                    failure_ordering.unwrap(),
                    weak,
                    &params.args[1],
                    &params.args[2],
                ),
                Fence { .. } => unreachable!(),
            }
        }
    };
}

fn instrument_llvm_intrinsic_call<'tcx, 'c, C>(
    inserter: &mut ProbeInserter<C>,
    params: CallParams<'_, 'tcx>,
) where
    C: cr::ForFunctionCallingWithResult<'tcx>,
{
    // Currently, we do not support for LLVM intrinsics.
    instrument_unsupported_call(inserter, params);
}

fn instrument_drop_in_place_call<'tcx, 'c, C>(
    inserter: &mut ProbeInserter<C>,
    CallParams {
        func,
        args,
        destination: _,
        is_diverging,
    }: CallParams<'_, 'tcx>,
) where
    C: cr::ForDropping<'tcx>,
{
    let mut inserter = inserter.before();
    assert_eq!(args.len(), 1);
    inserter.before_call_drop_in_place(func, &args[0]);

    if is_diverging {
        return;
    }

    let mut inserter = inserter.after();
    inserter.after_call_drop();
}

fn instrument_regular_call<'tcx, 'c, C>(
    inserter: &mut ProbeInserter<C>,
    params: CallParams<'_, 'tcx>,
) where
    C: cr::ForFunctionCallingWithResult<'tcx>,
{
    instrument_call_general(inserter, params, false);
}

fn instrument_noop_intrinsic_call<'tcx, 'c, C>(
    inserter: &mut ProbeInserter<C>,
    params: CallParams<'_, 'tcx>,
) where
    C: cr::ForFunctionCallingWithResult<'tcx>,
{
    // Although ineffective in runtime, we still report it.
    instrument_call_general(inserter, params, true);
}

fn instrument_unsupported_call<'tcx, 'c, C>(
    inserter: &mut ProbeInserter<C>,
    params: CallParams<'_, 'tcx>,
) where
    C: cr::ForFunctionCallingWithResult<'tcx>,
{
    instrument_call_general(inserter, params, true);
}

fn instrument_call_general<'tcx, 'c, C>(
    inserter: &'c mut ProbeInserter<C>,
    CallParams {
        func,
        args,
        destination: _,
        is_diverging,
    }: CallParams<'_, 'tcx>,
    no_definition: bool,
) where
    C: cr::ForFunctionCallingWithResult<'tcx>,
{
    let mut inserter = inserter.before();

    inserter.before_call_func(func, args, no_definition);

    if is_diverging {
        return;
    }

    let mut inserter = inserter.after();
    inserter.after_call_func();
}
