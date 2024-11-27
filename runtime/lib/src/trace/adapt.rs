use super::{Constraint, TraceManager};

/// Adapts a trace manager to accept other types of steps and values (contravariance).
pub(crate) struct AdapterTraceManager<F, SFrom, VFrom, STo, VTo, M: TraceManager<STo, VTo>> {
    inner: M,
    adapter: F,
    _phantom: core::marker::PhantomData<(SFrom, VFrom, STo, VTo)>,
}

impl<F, SFrom, VFrom, STo, VTo, M: TraceManager<STo, VTo>>
    AdapterTraceManager<F, SFrom, VFrom, STo, VTo, M>
{
    pub fn new(inner: M, adapter: F) -> Self
    where
        F: FnMut(SFrom, Constraint<VFrom>) -> (STo, Constraint<VTo>),
    {
        Self {
            inner,
            adapter,
            _phantom: core::marker::PhantomData,
        }
    }
}

impl<F, SFrom, VFrom, STo, VTo, M: TraceManager<STo, VTo>> TraceManager<SFrom, VFrom>
    for AdapterTraceManager<F, SFrom, VFrom, STo, VTo, M>
where
    F: FnMut(SFrom, Constraint<VFrom>) -> (STo, Constraint<VTo>),
{
    fn notify_step(&mut self, step: SFrom, constraint: Constraint<VFrom>) {
        let (new_step, constraint) = (self.adapter)(step, constraint);
        self.inner.notify_step(new_step, constraint);
    }
}

pub(crate) trait TraceManagerExt<STo, VTo>: TraceManager<STo, VTo> {
    fn adapt<SFrom, VFrom, SF, CF>(
        self,
        step_adaptor: SF,
        value_adaptor: CF,
    ) -> impl TraceManager<SFrom, VFrom>
    where
        SF: FnMut(SFrom) -> STo,
        CF: FnMut(VFrom) -> VTo,
        Self: Sized;

    fn adapt_step<SFrom, SF>(self, step_adaptor: SF) -> impl TraceManager<SFrom, VTo>
    where
        SF: FnMut(SFrom) -> STo,
        Self: Sized,
    {
        self.adapt(step_adaptor, |v| v)
    }

    fn adapt_value<VFrom, CF>(self, value_adaptor: CF) -> impl TraceManager<STo, VFrom>
    where
        CF: FnMut(VFrom) -> VTo,
        Self: Sized,
    {
        self.adapt(|s| s, value_adaptor)
    }
}
impl<STo, VTo, M: TraceManager<STo, VTo>> TraceManagerExt<STo, VTo> for M {
    fn adapt<SFrom, VFrom, SF, CF>(
        self,
        mut step_adapter: SF,
        mut value_adapter: CF,
    ) -> impl TraceManager<SFrom, VFrom>
    where
        SF: FnMut(SFrom) -> STo,
        CF: FnMut(VFrom) -> VTo,
        Self: Sized,
    {
        AdapterTraceManager::new(self, move |step, constraint| {
            (step_adapter(step), constraint.map(&mut value_adapter))
        })
    }
}

impl<V> Constraint<V> {
    fn map<VTo>(self, f: &mut impl FnMut(V) -> VTo) -> Constraint<VTo> {
        match self {
            Constraint::Bool(v) => Constraint::Bool(f(v)),
            Constraint::Not(v) => Constraint::Not(f(v)),
        }
    }
}
