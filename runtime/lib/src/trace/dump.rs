use crate::utils::serdes::TypeSerializer;

use super::{Constraint, StepInspector};

pub(crate) struct StreamDumperStepInspector<S, V, C, Ser> {
    serializer: Ser,
    _phantom: core::marker::PhantomData<(S, V, C)>,
}

impl<S, V, C, Ser> StreamDumperStepInspector<S, V, C, Ser> {
    pub fn new(serializer: Ser) -> Self
    where
        Ser: for<'a> TypeSerializer<Record<'a, S, V, C>>,
    {
        Self {
            serializer,
            _phantom: Default::default(),
        }
    }
}

#[derive(serde::Serialize, bincode::Encode)]
pub(crate) struct Record<'a, S, V, C> {
    step: &'a S,
    constraint: Constraint<&'a V, &'a C>,
}

impl<S, V, C, Ser> StepInspector<S, V, C> for StreamDumperStepInspector<S, V, C, Ser>
where
    Ser: for<'a> TypeSerializer<Record<'a, S, V, C>>,
{
    fn inspect(&mut self, step: &S, constraint: Constraint<&V, &C>) {
        let record = Record {
            step: step,
            constraint: constraint,
        };
        self.serializer
            .serialize(&record)
            .unwrap_or_else(|e| panic!("Could not dump step: {e}"));
    }
}
