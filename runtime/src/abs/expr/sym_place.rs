use crate::utils::meta::define_either_pair;

/// Represents a selection over a `target` using an `index`.
#[derive(Debug)]
pub(crate) struct Select<I, V> {
    pub index: I,
    pub target: SelectTarget<V, Self>,
}

define_either_pair! {
    /// Represents the possible targets of a selection, which can be an array of
    /// values or recursively the result of another selection.
    #[derive(Debug)]
    pub(crate) SelectTarget<V, S> {
        Array(Vec<V>),
        Nested(Box<S>),
    }
}

impl<I, V> Select<I, V> {
    /// Creates a selection over this selection.
    /// For example: `a[x][y]`.
    pub(crate) fn select(self, index: I) -> Self {
        Self {
            index,
            target: SelectTarget::Nested(Box::new(self)),
        }
    }
}

/// Resolves a symbolic read to a selection.
/// It may recursively resolve the possible values of the selection.
pub(crate) trait SymbolicReadResolver<I> {
    type SymValue<'a>;
    type PossibleValue<'a>;

    fn resolve<'a>(&mut self, sym_value: Self::SymValue<'a>) -> Select<I, Self::PossibleValue<'a>>;
}
