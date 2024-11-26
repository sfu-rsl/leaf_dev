use crate::abs::{backend::TraceManager, Constraint};

mod immediate;
mod logger;
mod noop;

pub(crate) use immediate::ImmediateTraceManager;
pub(crate) use logger::{LoggerTraceManager, TraceManagerExt as LoggerTraceManagerExt};
pub(crate) use noop::NoopTraceManager;
