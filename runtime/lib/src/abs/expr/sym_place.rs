/// Represents a selection over a `target` using an `index`.
#[derive(Debug, Clone)]
pub(crate) struct Select<I, V> {
    pub index: I,
    pub target: SelectTarget<V, Self>,
}

/// Represents the possible targets of a selection, which can be an array of
/// values or recursively the result of another selection.
#[derive(Debug, Clone)]
pub(crate) enum SelectTarget<V, S> {
    Array(Vec<V>),
    Nested(Box<S>),
}

/// Resolves a symbolic read to a selection.
/// It may recursively resolve the possible values of the selection.
pub(crate) trait SymbolicReadResolver<I> {
    type SymValue<'a>;
    type PossibleValue<'a>;

    fn resolve<'a>(&self, sym_value: Self::SymValue<'a>) -> Select<I, Self::PossibleValue<'a>>;
}
