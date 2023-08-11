use crate::utils::meta::define_either_pair;

#[derive(Debug)]
pub(crate) struct Select<I, V> {
    pub index: I,
    pub target: SelectTarget<V, Self>,
}

define_either_pair! {
    #[derive(Debug)]
    pub(crate) SelectTarget<V, S> {
        Array(Vec<V>),
        Nested(Box<S>),
    }
}

impl<I, V> Select<I, V> {
    pub(crate) fn select(self, index: I) -> Self {
        Self {
            index,
            target: SelectTarget::Nested(Box::new(self)),
        }
    }
}

pub(crate) trait SymbolicReadResolver<I> {
    type SymValue<'a>;
    type PossibleValue<'a>;

    fn resolve<'a>(&mut self, sym_value: Self::SymValue<'a>) -> Select<I, Self::PossibleValue<'a>>;
}
