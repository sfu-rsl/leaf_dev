/// This module contains the traits and implementations for adding calls to the
/// PRI in MIR bodies.
pub(super) mod context;

use rustc_middle::mir::{self, BasicBlock, Operand, Place, SwitchTargets};
use rustc_span::Spanned;

use serde::Serialize;

use common::pri::{AtomicBinaryOp, AtomicOrdering};

use super::pri::{self, sym::intrinsics::LeafIntrinsicSymbol};

use context::AssignmentInfoProvider;

/* Contexts and RuntimeCallAdder
 * Based on the location and the statement we are going to add runtime calls for,
 * there are some data that are required to be passed to the runtime or used in
 * MIR generation. We place these data in a `Context` and `RuntimeCallAdder`
 * capabilities is determined by this context. For example, if the information
 * for a destination place (left hand side of an assignment) is available in the
 * current context, then `RuntimeCallAdder` will be able to generate basic blocks
 * corresponding to calling the assignment functions in the runtime library.
 */

/*
 * The following traits are meant for definition of features that we expect
 * from `RuntimeCallAdder` for various call adding situations.
 */

pub(crate) trait AssignmentHandler<'tcx> {
    fn to_rvalue(&mut self, rvalue: &mir::Rvalue<'tcx>);

    fn its_discriminant_to(&mut self, variant_index: &rustc_abi::VariantIdx);
}

pub(crate) trait BranchingHandler<'tcx> {
    fn switch(&mut self, discr: &Operand<'tcx>, targets: &SwitchTargets);
}

pub(crate) trait AssertionHandler<'tcx> {
    fn check_assert(
        &mut self,
        cond: &Operand<'tcx>,
        expected: bool,
        msg: &rustc_middle::mir::AssertMessage<'tcx>,
    );
}

pub(crate) trait FunctionHandler<'tcx> {
    fn before_call_func(
        &mut self,
        func: &Operand<'tcx>,
        arguments: &[Spanned<Operand<'tcx>>],
        no_def: bool,
    );

    fn enter_func(&mut self);

    fn return_from_func(&mut self);

    fn after_call_func(&mut self)
    where
        Self: AssignmentInfoProvider<'tcx>;
}

pub(crate) trait DropHandler<'tcx> {
    fn before_call_drop(&mut self, place: &Place<'tcx>);

    fn before_call_drop_in_place(&mut self, func: &Operand<'tcx>, to_drop: &Spanned<Operand<'tcx>>);

    fn after_call_drop(&mut self);
}

pub(crate) trait StorageMarker<'tcx>: Sized {
    fn mark_live(&mut self, place: &Place<'tcx>);
    fn mark_dead(&mut self, place: &Place<'tcx>);
}

pub(crate) trait IntrinsicHandler<'tcx> {
    fn intrinsic_one_to_one_by<'a>(
        &mut self,
        intrinsic_func: rustc_hir::def_id::DefId,
        pri_func: LeafIntrinsicSymbol,
        args: impl Iterator<Item = &'a Spanned<Operand<'tcx>>>,
    ) where
        'tcx: 'a;
}

pub(crate) trait MemoryIntrinsicHandler<'tcx> {
    fn load(&mut self, is_ptr_aligned: bool)
    where
        Self: AssignmentInfoProvider<'tcx>;

    fn store(&mut self, val: &Spanned<Operand<'tcx>>, is_ptr_aligned: bool);

    fn copy(
        &mut self,
        dst: &Spanned<Operand<'tcx>>,
        count: &Spanned<Operand<'tcx>>,
        is_overlapping: bool,
    );

    fn set(&mut self, val: &Spanned<Operand<'tcx>>, count: &Spanned<Operand<'tcx>>);

    fn swap(&mut self, second: &Spanned<Operand<'tcx>>);

    fn raw_eq(&mut self, second: &Spanned<Operand<'tcx>>)
    where
        Self: AssignmentInfoProvider<'tcx>;

    fn compare_bytes(&mut self, second: &Spanned<Operand<'tcx>>, count: &Spanned<Operand<'tcx>>)
    where
        Self: AssignmentInfoProvider<'tcx>;
}

pub(crate) trait AtomicIntrinsicHandler<'tcx> {
    fn load(&mut self)
    where
        Self: AssignmentInfoProvider<'tcx>;

    fn store(&mut self, val: &Spanned<Operand<'tcx>>)
    where
        // This is a redundant requirement as it is a unit function with a ptr passed to it.
        // However, it is used for the assignment id.
        Self: AssignmentInfoProvider<'tcx>;

    fn exchange(&mut self, val: &Spanned<Operand<'tcx>>)
    where
        Self: AssignmentInfoProvider<'tcx>;

    fn compare_exchange(
        &mut self,
        failure_ordering: AtomicOrdering,
        weak: bool,
        old: &Spanned<Operand<'tcx>>,
        src: &Spanned<Operand<'tcx>>,
    ) where
        Self: AssignmentInfoProvider<'tcx>;

    fn binary_op(&mut self, operator: AtomicBinaryOp, src: &Spanned<Operand<'tcx>>)
    where
        Self: AssignmentInfoProvider<'tcx>;

    fn fence(&mut self, single_threaded: bool);
}

pub(crate) trait EntryFunctionHandler {
    fn init_runtime_lib(&mut self);

    fn shutdown_runtime_lib(&mut self);
}

pub(crate) trait DebugInfoHandler {
    fn debug_info<T: Serialize>(&mut self, info: &T);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum InsertionLocation {
    Before(BasicBlock),
    After(BasicBlock),
}

impl InsertionLocation {
    #[inline]
    fn index(&self) -> BasicBlock {
        match self {
            Self::Before(bb) | Self::After(bb) => *bb,
        }
    }
}

mod config {
    pub(super) use super::super::decision::rules::{
        AssignmentRules, CallFlowRules, ConstantTypeRules, DetailDecision, DropRules,
        EventDecision, OperandKindRules, PlaceInfoRules, PlaceStructureRules,
        StorageLifetimeMarkerRules, SwitchRules,
    };

    pub(crate) struct Config {
        pub place_info_filter: PlaceInfoRules<PlaceStructureRules<DetailDecision>, DetailDecision>,
        pub operand_info_filter:
            OperandKindRules<DetailDecision, Option<ConstantTypeRules<DetailDecision>>>,
        pub assignment_filter: AssignmentRules<EventDecision>,
        pub storage_lifetime_filter: StorageLifetimeMarkerRules<DetailDecision>,
        pub call_flow_filter: CallFlowRules<DetailDecision>,
        pub drop_filter: DropRules<DetailDecision>,
        pub switch_filter: SwitchRules<DetailDecision>,
    }
}
pub(crate) use config::Config;

mod implementation;

pub(super) use implementation::{RuntimeCallAdder, ctxt_reqs};
