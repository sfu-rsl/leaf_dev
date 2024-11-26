use super::{Constraint, TraceManager};

pub(crate) type NoopTraceManager = ();

impl<S, V> TraceManager<S, V> for () {
    fn notify_step(&mut self, _step: S, _new_constraints: Vec<Constraint<V>>) {
        // Do nothing
    }
}
