use common::log_debug;

use super::{Constraint, TraceManager};

struct StepFilterTraceManager<F, S, V, M: TraceManager<S, V>> {
    inner: M,
    filter: F,
    _phantom: core::marker::PhantomData<(S, V)>,
}

impl<F, S, V, M: TraceManager<S, V>> StepFilterTraceManager<F, S, V, M> {}

impl<F, S, V, M: TraceManager<S, V>> TraceManager<S, V> for StepFilterTraceManager<F, S, V, M>
where
    F: Fn(&S) -> bool,
{
    fn notify_step(&mut self, step: S, constraint: Constraint<V>) {
        if (self.filter)(&step) {
            self.inner.notify_step(step, constraint);
        } else {
            log_debug!("Filtered out step");
        }
    }
}
