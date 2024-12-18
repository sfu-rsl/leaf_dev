#![feature(iterator_try_collect)]
#![feature(exit_status_error)]

mod mutation;
mod stage;

pub use mutation::DivergingMutator;
pub use stage::NonBlockingMultiMutationalStage;
