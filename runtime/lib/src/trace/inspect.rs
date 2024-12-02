use crate::abs::Constraint;

pub(crate) trait TraceInspector<S, V> {
    fn inspect(&mut self, steps: &[S], constraints: &[Constraint<V>]);
}

pub(crate) type NoopTraceInspector = ();

impl<S, V> TraceInspector<S, V> for NoopTraceInspector {
    fn inspect(&mut self, _steps: &[S], _constraints: &[Constraint<V>]) {}
}

pub(crate) struct StaticCompoundTraceInspector<
    S,
    V,
    I: TraceInspector<S, V>,
    J: TraceInspector<S, V>,
> {
    first: I,
    second: J,
    _phantom: core::marker::PhantomData<(S, V)>,
}

impl<S, V, I: TraceInspector<S, V>, J: TraceInspector<S, V>> From<(I, J)>
    for StaticCompoundTraceInspector<S, V, I, J>
{
    fn from((first, second): (I, J)) -> Self {
        Self {
            first,
            second,
            _phantom: Default::default(),
        }
    }
}

impl<S, V, I: TraceInspector<S, V>, J: TraceInspector<S, V>> TraceInspector<S, V>
    for StaticCompoundTraceInspector<S, V, I, J>
{
    fn inspect(&mut self, steps: &[S], constraints: &[Constraint<V>]) {
        self.first.inspect(steps, constraints);
        self.second.inspect(steps, constraints);
    }
}

pub(crate) struct CompoundTraceInspector<S, V> {
    inspectors: Vec<Box<dyn TraceInspector<S, V>>>,
}

impl<S, V> CompoundTraceInspector<S, V> {
    pub(crate) fn new(inspectors: Vec<Box<dyn TraceInspector<S, V>>>) -> Self {
        Self { inspectors }
    }
}

impl<S, V> TraceInspector<S, V> for CompoundTraceInspector<S, V> {
    fn inspect(&mut self, steps: &[S], constraints: &[Constraint<V>]) {
        for inspector in self.inspectors.iter_mut() {
            inspector.inspect(steps, constraints);
        }
    }
}
