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

#[derive(Debug, Clone)]
pub(crate) enum SymbolicReadTree<I, V> {
    /// A selection over a set of possible values using a symbolic index.
    /// # Remarks
    /// The root of a complete is expected to be from this variant.
    SymRead(Select<I, Self>),
    /// An array of possible values.
    /// # Remarks
    /// This variant is expected to appear as intermediate nodes as possible values for a selection.
    Array(Vec<Self>),
    /// A single value.
    /// # Remarks
    /// This variant is expected to appear as leaf nodes.
    Single(V),
}

impl<I, V> From<Select<I, Self>> for SymbolicReadTree<I, V> {
    fn from(select: Select<I, Self>) -> Self {
        SymbolicReadTree::SymRead(select)
    }
}

/// Resolves a symbolic read to a selection.
/// It may recursively resolve the possible values of the selection.
pub(crate) trait SymbolicReadResolver<I> {
    type SymValue<'a>;
    type PossibleValue<'a>;

    fn resolve<'a>(&self, sym_value: Self::SymValue<'a>) -> Select<I, Self::PossibleValue<'a>>;
}

pub(crate) enum SymbolicReadTreeLeafMutator<'m, I, V> {
    Mutator(&'m mut dyn FnMut(&mut V)),
    Replacer(&'m mut dyn FnMut(&mut V) -> SymbolicReadTree<I, V>),
}
use SymbolicReadTreeLeafMutator::*;

impl<I, V> Select<I, SymbolicReadTree<I, V>> {
    #[inline]
    pub(crate) fn mutate_leaves<'m>(
        &mut self,
        mut f: SymbolicReadTreeLeafMutator<'m, I, V>,
        expander: impl Fn(&V) -> SymbolicReadTree<I, V>,
    ) {
        self.internal_mutate_leaves(1, &mut f, &expander)
    }

    fn internal_mutate_leaves<'m>(
        &mut self,
        expected_dim: usize,
        f: &mut SymbolicReadTreeLeafMutator<'m, I, V>,
        expander: &dyn Fn(&V) -> SymbolicReadTree<I, V>,
    ) {
        match &mut self.target {
            SelectTarget::Array(ref mut values) => {
                values
                    .iter_mut()
                    .for_each(|v| v.internal_mutate_leaves(expected_dim - 1, f, expander));
            }
            SelectTarget::Nested(box nested) => {
                Self::internal_mutate_leaves(nested, expected_dim + 1, f, expander)
            }
        }
    }
}

impl<I, V> SymbolicReadTree<I, V> {
    pub(crate) fn mutate_leaves<'m>(
        &mut self,
        mut f: SymbolicReadTreeLeafMutator<'m, I, V>,
        expander: impl Fn(&V) -> Self,
    ) {
        match self {
            Self::SymRead(select) => select.mutate_leaves(f, expander),
            Self::Array(values) => values
                .iter_mut()
                .for_each(|v| v.internal_mutate_leaves(0, &mut f, &expander)),
            Self::Single(value) => match f {
                Mutator(f) => f(value),
                Replacer(f) => *self = f(value),
            },
        }
    }

    /// Applies the given function on the leaves (single value(s)) of this tree.
    /// - Expands the value if the expected dimension is more than one.
    /// - Recurses over subtrees.
    /// - Iterates over the arrays.
    ///
    /// # Arguments
    /// - `dim`: The expected dimension of the value(s).
    ///   Effectively this corresponds to one less than the number of `Select` values wrapping this.
    /// - `f`: The function to apply on the value(s).
    /// - `expander`: Used to expand the single values if they appear in the traversal
    ///   (before reaching the desired dimension).
    fn internal_mutate_leaves<'m>(
        &mut self,
        dim: usize,
        f: &mut SymbolicReadTreeLeafMutator<'m, I, V>,
        expander: &dyn Fn(&V) -> Self,
    ) {
        // Expand if expected.
        if dim > 0 {
            if let Self::Single(single) = self {
                *self = expander(single);
            }
        }

        match self {
            Self::SymRead(select) => select.internal_mutate_leaves(dim, f, expander),
            Self::Array(values) => values
                .iter_mut()
                .for_each(|v| v.internal_mutate_leaves(dim - 1, f, &expander)),
            Self::Single(value) => match f {
                Mutator(f) => f(value),
                Replacer(f) => *self = f(value),
            },
        }
    }
}
