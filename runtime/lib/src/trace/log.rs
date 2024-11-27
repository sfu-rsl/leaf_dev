use core::fmt::Display;

use common::log_info;

use super::{Constraint, TraceManager};

pub(crate) struct LoggerTraceManager<S, V, M: TraceManager<S, V>> {
    inner: M,
    _phantom: core::marker::PhantomData<(S, V)>,
}

impl<S: Display, V: Display, M: TraceManager<S, V>> TraceManager<S, V>
    for LoggerTraceManager<S, V, M>
{
    fn notify_step(&mut self, step: S, constraint: Constraint<V>) {
        log_info!("Notified about constraints {} at step {}", constraint, step,);

        self.inner.notify_step(step, constraint);
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
