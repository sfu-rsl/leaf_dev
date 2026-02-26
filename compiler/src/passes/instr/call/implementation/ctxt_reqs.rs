//! Context requirements work as aliases for context traits to guarantee that a
//! certain feature will be available in `RuntimeCallAdder` when its context
//! implement that set of traits.

use super::{context::*, *};

pub(crate) trait Basic<'tcx>: BaseContext<'tcx> + BodyProvider<'tcx> {}
impl<'tcx, C> Basic<'tcx> for C where C: BaseContext<'tcx> + BodyProvider<'tcx> {}

pub(crate) trait ForInsertion<'tcx>:
    Basic<'tcx> + InsertionLocationProvider + SourceInfoProvider
{
}
impl<'tcx, C> ForInsertion<'tcx> for C where
    C: Basic<'tcx> + InsertionLocationProvider + SourceInfoProvider
{
}

pub(crate) trait ForPlaceRef<'tcx>: ForInsertion<'tcx> {}
impl<'tcx, C> ForPlaceRef<'tcx> for C where C: ForInsertion<'tcx> {}

pub(crate) trait ForOperandRef<'tcx>: ForPlaceRef<'tcx> {}
impl<'tcx, C> ForOperandRef<'tcx> for C where C: ForPlaceRef<'tcx> {}

pub(crate) trait ForAssignment<'tcx>:
    ForInsertion<'tcx> + AssignmentInfoProvider<'tcx>
{
}
impl<'tcx, C> ForAssignment<'tcx> for C where C: ForInsertion<'tcx> + AssignmentInfoProvider<'tcx> {}

pub(crate) trait ForCasting<'tcx>: CastOperandProvider + ForAssignment<'tcx> {}
impl<'tcx, C> ForCasting<'tcx> for C where C: CastOperandProvider + ForAssignment<'tcx> {}

pub(crate) trait ForBranching<'tcx>:
    ForInsertion<'tcx> + BlockOriginalIndexProvider + JumpTargetModifier
{
}
impl<'tcx, C> ForBranching<'tcx> for C where
    C: ForInsertion<'tcx> + BlockOriginalIndexProvider + JumpTargetModifier
{
}

pub(crate) trait ForAssertion<'tcx>:
    ForOperandRef<'tcx> + BlockOriginalIndexProvider
{
}
impl<'tcx, C> ForAssertion<'tcx> for C where C: ForOperandRef<'tcx> + BlockOriginalIndexProvider {}

pub(crate) trait ForFunctionCalling<'tcx>:
    ForInsertion<'tcx> + JumpTargetModifier + BlockOriginalIndexProvider
{
}
impl<'tcx, C> ForFunctionCalling<'tcx> for C where
    C: ForInsertion<'tcx> + JumpTargetModifier + BlockOriginalIndexProvider
{
}

pub(crate) trait ForReturning<'tcx>: ForInsertion<'tcx> {}
impl<'tcx, C> ForReturning<'tcx> for C where C: ForInsertion<'tcx> {}

pub(crate) trait ForEntryFunction<'tcx>: ForInsertion<'tcx> + InEntryFunction {}
impl<'tcx, C> ForEntryFunction<'tcx> for C where C: ForInsertion<'tcx> + InEntryFunction {}

pub(crate) trait ForAtomicIntrinsic<'tcx>:
    ForInsertion<'tcx> + AtomicIntrinsicParamsProvider<'tcx> + PointerInfoProvider<'tcx>
{
}
impl<'tcx, C> ForAtomicIntrinsic<'tcx> for C where
    C: ForInsertion<'tcx> + AtomicIntrinsicParamsProvider<'tcx> + PointerInfoProvider<'tcx>
{
}

pub(crate) trait ForMemoryIntrinsic<'tcx>:
    ForAssignment<'tcx> + MemoryIntrinsicParamsProvider<'tcx> + PointerInfoProvider<'tcx>
{
}
impl<'tcx, C> ForMemoryIntrinsic<'tcx> for C where
    C: ForAssignment<'tcx> + MemoryIntrinsicParamsProvider<'tcx> + PointerInfoProvider<'tcx>
{
}

pub(crate) trait ForStorageMarking<'tcx>: ForInsertion<'tcx> {}
impl<'tcx, C> ForStorageMarking<'tcx> for C where C: ForInsertion<'tcx> {}
