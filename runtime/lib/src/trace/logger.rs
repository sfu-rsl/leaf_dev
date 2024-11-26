use core::fmt::Display;

use common::log_info;
use itertools::Itertools;

use super::{Constraint, TraceManager};

pub(crate) struct LoggerTraceManager<S, V, M: TraceManager<S, V>> {
    inner: M,
    _phantom: core::marker::PhantomData<(S, V)>,
}

impl<S: Display, V: Display, M: TraceManager<S, V>> TraceManager<S, V>
    for LoggerTraceManager<S, V, M>
{
    fn notify_step(&mut self, step: S, new_constraints: Vec<Constraint<V>>) {
        log_info!(
            "Notified about constraints [{}] at step {}",
            new_constraints.iter().join(", "),
            step,
        );

        self.inner.notify_step(step, new_constraints);
    }
}

pub(crate) trait TraceManagerExt<S, V>: TraceManager<S, V> {
    fn into_logger(self) -> LoggerTraceManager<S, V, Self>
    where
        Self: Sized;
}
impl<S, V, M: TraceManager<S, V>> TraceManagerExt<S, V> for M {
    fn into_logger(self) -> LoggerTraceManager<S, V, Self> {
        LoggerTraceManager {
            inner: self,
            _phantom: core::marker::PhantomData,
        }
    }
}
