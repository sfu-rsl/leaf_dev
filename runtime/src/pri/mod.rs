mod default;

pub(crate) use default::BranchingInfo as DefaultBranchingInfo;

#[cfg(not(any(runtime_impl = "noop")))]
pub(crate) use default::DefaultPri as PriImpl;

#[cfg(runtime_impl = "noop")]
mod noop;
#[cfg(runtime_impl = "noop")]
pub(crate) use noop::NoOpPri as PriImpl;
