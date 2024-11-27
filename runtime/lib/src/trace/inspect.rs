use crate::abs::Constraint;

pub(crate) trait TraceInspector<S, V> {
    fn inspect(&mut self, steps: &[S], constraints: &[Constraint<V>]);
}

pub(crate) type NoopTraceInspector = ();

impl<S, V> TraceInspector<S, V> for NoopTraceInspector {
    fn inspect(&mut self, _steps: &[S], _constraints: &[Constraint<V>]) {}
}
