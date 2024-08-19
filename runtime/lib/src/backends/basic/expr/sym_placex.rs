/* What is going on for a symbolic read?
 * In a nutshell a symbolic read corresponds to multiple possible values.
 * In safe Rust, the only point for a symbolic read is a symbolic index over a slice.
 * However, any further projection on the symbolic index is effectively another symbolic read.
 * The difference is that the symbolic index is resolved to multiple possible values
 * but other projections select a single value out of a possible value.
 * In this way, we can separate the symbolic index from other projections and
 * apply other projections on the possible values of the underlying symbolic index (select).
 * Example:
 * If `a` is an array of compound values, `a[x].1` is equivalent to `b[x]`,
 * where `for all i. b[i] == a[i].1`.
 *
 * Symbolic indices over symbolic reads only add another layer of select.
 * So if `a[x]` is represented as `select(a, x)` then `a[x][y]` is represented as
 * `select(select(a, x), y)`.
 * This effectively means that it doesn't matter where the other projections are
 * located. In other words, both `*(a[x].1)[y]` and `*(a[x][y].1)` will be
 * represented by two layers selections.
 *
 * Assuming that symbolic variables only come from primitive types, the start
 * point for a symbolic read is always an index projection over a concrete array.
 * Moreover, if a projection is applied on a symbolic value, it has to be another
 * symbolic read, which may be resolved on demand.
 */

use derive_more::From;

use common::log_debug;

use super::{
    super::alias::TypeManager, abs::expr::sym_place::SymbolicReadResolver, place::*, prelude::*,
    sym_place::RawPointerRetriever,
};

type SymIndex = SymValueRef;
pub(crate) type Select<V = SymbolicPlaceResult> = crate::abs::expr::sym_place::Select<SymIndex, V>;

/// # Remarks about Array (old)
/// Although both `Select` and `SingleResult` can hold array of values, there are semantical
/// and operational differences between this variant and them. This is the result of
/// an expansion where we expect to see multiple values. This variant appears in nested
/// `Select`s. Let's consider `a[x][y].1` where `a` is an array of arrays.
/// We expect to see all the first field of a[_][_] as a possible value. This can be done either
/// by generating a `Select` for each possible value of `a[x]`, or having a nested select with a
/// 2D array of possible values. We choose the second one, which means a series of nested
/// `Select`s will be created with a last one having an array. However, the possible values
/// shape an n-D array which means the leaf nodes of a `Select` should be arrays.
/// Also, let's say we want to apply the last projection (the first field access). For a
/// consistent behavior, intermediate single values that are expected to be array need to be
/// resolved (expanded) to be able to iterate over their values. Again, this variant is
/// necessary to hold the expanded view.
pub(crate) type SymbolicPlaceResult =
    super::abs::expr::sym_place::SymbolicReadTree<SymIndex, SinglePlaceResult>;

impl From<Vec<Self>> for SymbolicPlaceResult {
    fn from(value: Vec<Self>) -> Self {
        SymbolicPlaceResult::Array(value)
    }
}

impl From<SinglePlaceResult> for SymbolicPlaceResult {
    fn from(value: SinglePlaceResult) -> Self {
        SymbolicPlaceResult::Single(value)
    }
}

#[derive(Debug, Clone, From)]
pub(crate) struct SinglePlaceResult(pub DeterPlaceValueRef);

/// Resolves a symbolic place to the possible places it may refer to.
trait SymbolicPlaceResolver:
    for<'a> SymbolicReadResolver<SymIndex, PossibleValue<'a> = SymbolicPlaceResult>
{
    /// Expands the given value to another representation that is indexable.
    /// # Returns
    /// An expanded version of the given value.
    /// If the value gets resolved to a single possible value, that value must
    /// be indexable and it is expanded to an `Array`,
    /// otherwise a `Select` will be returned to be expanded later.
    /// The returned value is guaranteed not to be a `Single`.
    fn expand<'a>(&self, value: &'a SinglePlaceResult) -> SymbolicPlaceResult;
}

pub(crate) struct DefaultSymPlaceResolver<'a> {
    type_manager: &'a dyn TypeManager,
    retriever: &'a dyn RawPointerRetriever,
}

impl<'a> DefaultSymPlaceResolver<'a> {
    pub(crate) fn new(
        type_manager: &'a dyn TypeManager,
        retriever: &'a dyn RawPointerRetriever,
    ) -> Self {
        Self {
            type_manager,
            retriever,
        }
    }
}

mod implementation {
    use crate::{
        abs::expr::sym_place::{SelectTarget, SymbolicReadResolver},
        tyexp::TypeInfoExt,
    };

    use super::*;
    use common::tyexp::{ArrayShape, TypeInfo};

    impl SymbolicPlaceResolver for DefaultSymPlaceResolver<'_> {
        #[tracing::instrument(level = "debug", skip(self))]
        fn expand<'a>(&self, place: &'a SinglePlaceResult) -> SymbolicPlaceResult {
            SymbolicPlaceResult::Array(
                self.expand_deterministic(place.0.as_ref())
                    .into_iter()
                    .map(Into::into)
                    .collect(),
            )
        }
    }

    /// # Remarks
    /// If there is concrete host, we know for sure that the result is a select.
    /// However, transmutations turn into symbolic reads if there is a symbolic index.
    impl SymbolicReadResolver<SymIndex> for DefaultSymPlaceResolver<'_> {
        type SymValue<'a> = &'a SymbolicPlaceValue;
        type PossibleValue<'a> = SymbolicPlaceResult;

        fn resolve<'a>(&self, place_value: Self::SymValue<'a>) -> Select<Self::PossibleValue<'a>> {
            let mut base = match &place_value.base {
                SymbolicPlaceBase::Deref(host) => self.resolve_deref_of_sym(host),
                SymbolicPlaceBase::SymIndex(indexed) => self.resolve_sym_indexed(indexed),
            };
            if let Some(proj) = &place_value.proj {
                base.mutate_leaves(
                    Mutator(&mut |p| {
                        p.0 = DeterPlaceValueRef::new(proj.on_deter(p.0.as_ref()).to_value_ref())
                    }),
                    self,
                );
            }
            base
        }
    }

    impl DefaultSymPlaceResolver<'_> {
        fn resolve_deref_of_sym(&self, host: &DerefSymHostPlace) -> Select {
            let unexpected = || unreachable!("Unexpected symbolic host to dereference: {:?}", host);
            let SymValue::Expression(expr) = host.value.as_ref() else {
                unexpected()
            };
            match expr {
                Expr::Multi(_) => todo!(),
                Expr::Binary(_) => todo!(),
                Expr::Ite { .. } => todo!(),
                Expr::Extraction { .. } => todo!(),
                Expr::Unary { .. }
                | Expr::Extension { .. }
                | Expr::Ref(_)
                | Expr::Projection(_)
                | Expr::Len(_) => unexpected(),
            }
        }

        fn resolve_sym_indexed(&self, indexed: &SymIndexedPlace) -> Select {
            let target = match indexed.host.as_ref() {
                PlaceValue::Deterministic(..) => self.expand(&SinglePlaceResult(
                    DeterPlaceValueRef::new(indexed.host.clone()),
                )),
                PlaceValue::Symbolic(sym_host) => {
                    let mut resolved = self.resolve(sym_host);
                    resolved.mutate_leaves(Replacer(&mut |p| self.expand(p)), self);
                    resolved.into()
                }
            };
            let target = match target {
                SymbolicPlaceResult::SymRead(current) => SelectTarget::Nested(Box::new(current)),
                SymbolicPlaceResult::Array(values) => SelectTarget::Array(values),
                SymbolicPlaceResult::Single(..) => {
                    unreachable!("The single value is expected to be expanded at this point.")
                }
            };
            Select {
                index: indexed.index.clone(),
                target,
            }
        }

        #[tracing::instrument(level = "debug", skip(self))]
        pub(super) fn expand_deterministic(
            &self,
            place: &DeterministicPlaceValue,
        ) -> Vec<SinglePlaceResult> {
            let ty = self.type_manager.get_type(place.unwrap_type_id());
            if let Some(array) = ty.as_array() {
                self.expand_array(place.address(), array)
            } else {
                todo!()
            }
        }

        fn expand_array(
            &self,
            base_address: RawAddress,
            shape: &ArrayShape,
        ) -> Vec<SinglePlaceResult> {
            let item_ty = self.type_manager.get_type(shape.item_ty);
            let mut result = Vec::with_capacity(shape.len as usize);
            for i in 0..shape.len {
                let item_addr = base_address.wrapping_byte_offset((i * item_ty.size) as isize);
                let place = DeterministicPlaceValue::from_addr_type(item_addr, item_ty.id);
                result.push(DeterPlaceValueRef::new(place.to_value_ref()).into());
            }
            result
        }

        pub(super) fn expand_symbolic(&self, value: &SymValue) -> SymbolicPlaceResult {
            todo!()
        }
    }

    impl Select {
        #[inline]
        pub(super) fn mutate_leaves<'m>(
            &mut self,
            mut f: SymbolicPlaceResultValueMutator<'m>,
            resolver: &impl SymbolicPlaceResolver,
        ) {
            self.internal_mutate_leaves(1, &mut f, resolver)
        }

        fn internal_mutate_leaves<'m>(
            &mut self,
            expected_dim: usize,
            f: &mut SymbolicPlaceResultValueMutator<'m>,
            resolver: &impl SymbolicPlaceResolver,
        ) {
            match &mut self.target {
                SelectTarget::Array(ref mut values) => {
                    values
                        .iter_mut()
                        .for_each(|v| v.internal_mutate_values(expected_dim - 1, f, resolver));
                }
                SelectTarget::Nested(box nested) => {
                    Self::internal_mutate_leaves(nested, expected_dim + 1, f, resolver)
                }
            }
        }
    }

    pub(super) enum SymbolicPlaceResultValueMutator<'m> {
        Mutator(&'m mut dyn FnMut(&mut SinglePlaceResult)),
        Replacer(&'m mut dyn FnMut(&mut SinglePlaceResult) -> SymbolicPlaceResult),
    }
    use SymbolicPlaceResultValueMutator::*;

    impl SymbolicPlaceResult {
        pub(super) fn mutate_values<'m>(
            &mut self,
            mut f: SymbolicPlaceResultValueMutator<'m>,
            resolver: &impl SymbolicPlaceResolver,
        ) {
            match self {
                SymbolicPlaceResult::SymRead(select) => select.mutate_leaves(f, resolver),
                SymbolicPlaceResult::Array(values) => values
                    .iter_mut()
                    .for_each(|v| v.internal_mutate_values(0, &mut f, resolver)),
                SymbolicPlaceResult::Single(value) => match f {
                    Mutator(f) => f(value),
                    Replacer(f) => *self = f(value),
                },
            }
        }

        /// Applies the given function on the concrete value(s) of this `SymReadResult`.
        /// - Resolves the value if it is symbolic.
        /// - Expands the value if the expected dimension is more than one.
        /// - Recurses if self refers to a symbolic read.
        /// - Iterates over the values if self is an array.
        ///
        /// # Remarks
        /// This method is used to perform a mutation or extraction over concrete values.
        /// In our case, it translates to projections. Thus, it suppose that symbolic values
        /// are resolvable to symbolic reads and tries to resolve them if they are encountered.
        ///
        /// # Arguments
        /// - `dim`: The expected dimension of the value(s) of this `SymReadResult`.
        ///   Effectively this corresponds to one less than the number of `Select` values wrapping
        ///   this `SymReadResult`.
        /// - `f`: The function to apply on the value(s).
        /// - `resolver`: Used to resolve the inner symbolic values if they appear
        ///   in the traversal.
        fn internal_mutate_values<'m>(
            &mut self,
            dim: usize,
            f: &mut SymbolicPlaceResultValueMutator<'m>,
            resolver: &impl SymbolicPlaceResolver,
        ) {
            // Expand if expected.
            if dim > 0 {
                if let SymbolicPlaceResult::Single(single) = self {
                    *self = resolver.expand(single);
                }
            }

            match self {
                SymbolicPlaceResult::SymRead(select) => {
                    select.internal_mutate_leaves(dim, f, resolver)
                }
                SymbolicPlaceResult::Array(values) => values
                    .iter_mut()
                    .for_each(|v| v.internal_mutate_values(dim - 1, f, resolver)),
                SymbolicPlaceResult::Single(value) => match f {
                    Mutator(f) => f(value),
                    Replacer(f) => *self = f(value),
                },
            }
        }
    }

    pub(super) mod utils {}
}
