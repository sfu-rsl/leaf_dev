use super::{Constraint, TraceInspector, TraceManager};

pub(crate) struct AggregatorTraceManager<S, V, I: TraceInspector<S, V>> {
    steps: Vec<S>,
    constraints: Vec<Constraint<V>>,
    inspector: I,
}

impl<S, V, I: TraceInspector<S, V>> AggregatorTraceManager<S, V, I> {
    pub(crate) fn new(inspector: I) -> Self {
        Self {
            steps: Vec::new(),
            constraints: Vec::new(),
            inspector,
        }
    }
}

impl<S, V, I: TraceInspector<S, V>> TraceManager<S, V> for AggregatorTraceManager<S, V, I> {
    fn notify_step(&mut self, step: S, constraint: Constraint<V>) {
        self.steps.push(step);
        self.constraints.push(constraint);
        self.inspector.inspect(&self.steps, &self.constraints);
    }
}
