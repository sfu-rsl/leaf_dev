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

use crate::{
    abs::expr::{
        proj::{macros::impl_singular_projs_through_general, ProjectionOn, Projector},
        sym_place::{SelectTarget, SymbolicReadResolver},
    },
    backends::basic::{
        state::proj::{ConcreteProjector, ProjResultExt},
        FullPlace,
    },
};

use super::{builders::ConcreteBuilder, prelude::*, SliceIndex};

type SymIndex = SliceIndex<SymValueRef>;
pub(super) type Select<V = SymReadResult> = crate::abs::expr::sym_place::Select<SymIndex, V>;

/// Represents a possible value of a symbolic read.
/// It can be either a value or the result of another symbolic read (e.g.,
/// consider `a[x]` is stored in `b[1]`. Then a possible value for `b[y]` is
/// `a[x]` which is another symbolic read). This is particularly used when
/// applying further projections after the symbolic index.
#[derive(Debug, Clone)]
pub(crate) enum SymReadResult {
    Value(ValueRef),
    Array(Vec<SymReadResult>),
    SymRead(Select),
}

pub(super) trait ProjExprReadResolver:
    for<'a, 'b> SymbolicReadResolver<
        SymIndex,
        SymValue<'a> = &'a ProjExpr,
        PossibleValue<'a> = SymReadResult,
    >
{
}
impl<T> ProjExprReadResolver for T where
    T: for<'a, 'b> SymbolicReadResolver<
            SymIndex,
            SymValue<'a> = &'a ProjExpr,
            PossibleValue<'a> = SymReadResult,
        >
{
}

#[derive(Default)]
pub(super) struct DefaultProjExprReadResolver;

impl SymbolicReadResolver<SymIndex> for DefaultProjExprReadResolver {
    type SymValue<'a> = &'a ProjExpr;

    type PossibleValue<'a> = SymReadResult;

    fn resolve<'a>(&mut self, sym_value: Self::SymValue<'a>) -> Select<Self::PossibleValue<'a>> {
        let (base, projs) = sym_value.flatten();
        self.resolve_symbolic_projs(base, projs)
    }
}

pub(super) fn apply_address_of(
    mut host: Select,
    resolver: &mut impl ProjExprReadResolver,
) -> Select {
    SymReadResult::apply_on_leaf_nodes(
        &mut host,
        &mut |value| {
            use crate::abs::expr::UnaryExprBuilder;
            ConcreteBuilder::default().address_of(value)
        },
        resolver,
    );
    host
}

/// Applies the length operator on the given `Select` value, i.e. the result `Select` leaf values
/// correspond to the lengths of the host leaf values.
pub(super) fn apply_len(mut host: Select, resolver: &mut impl ProjExprReadResolver) -> Select {
    SymReadResult::apply_on_leaf_nodes(
        &mut host,
        &mut |value| {
            use crate::abs::expr::UnaryExprBuilder;
            ConcreteBuilder::default().len(value)
        },
        resolver,
    );
    host
}

mod implementation {
    use super::*;

    impl DefaultProjExprReadResolver {
        pub(super) fn resolve_symbolic_projs(
            &mut self,
            base: &ConcreteHostProj,
            projs: Vec<&ProjKind>,
        ) -> Select {
            let indices = projs.iter().enumerate().filter_map(|(i, p)| match p {
                ProjKind::Index(SliceIndex { index, from_end }) if index.is_symbolic() => Some((
                    i,
                    SliceIndex {
                        index: SymValueRef::new(index.clone()),
                        from_end: *from_end,
                    },
                )),
                _ => None,
            });

            let mut current = Self::resolve_concrete_host(base);

            let mut last_index = 0;
            for (i, index) in indices {
                current = self.project_one_to_ones(current, &projs[last_index..i]);
                last_index = i + 1;

                log::debug!("Found a symbolic index in the projection chain at {i}");
                current = current.select(index);
            }

            if last_index < projs.len() {
                current = self.project_one_to_ones(current, &projs[last_index..]);
            }

            current
        }

        /// # Remarks
        /// This is the base case for creation of a `Select`.
        fn resolve_concrete_host(proj: &ConcreteHostProj) -> Select {
            let possible_values = match proj.host.as_ref() {
                ConcreteValue::Array(ArrayValue { elements }) => elements
                    .iter()
                    .map(|v| SymReadResult::Value(v.clone()))
                    .collect(),
                _ => unreachable!("Symbolic index is only expected on an array."),
            };
            Select {
                index: proj.index.clone(),
                target: SelectTarget::Array(possible_values),
            }
        }

        /// Applies the given one-to-one projections to the given host.
        /// By one-to-one projection we mean a projection that doesn't get resolved
        /// to many possible values, i.e., all projections other than a symbolic index.
        /// These projections can be applied on the possible values of a `Select`.
        /// Given a `Select` value, this method returns a `Select` value which has
        /// the same index but with possible values with the projections applied on them.
        fn project_one_to_ones(&mut self, host: Select, projs: &[&ProjKind]) -> Select {
            debug_assert!(
                !projs.iter().any(
                    |p| matches!(p, ProjKind::Index (SliceIndex{ index, .. }) if index.is_symbolic())
                ),
                "Not meant for one-to-many projections (symbolic indices)."
            );

            log::debug!("Applying one-to-one projections: {projs:?} on {host}");

            if projs.is_empty() {
                return host;
            }

            let mut projector = PossibleValuesInPlaceProjector {
                projector: ConcreteProjectorAdapter::new(|_p: &FullPlace| -> ValueRef {
                    todo!("#234")
                }),
                resolver: self,
            };

            #[cfg(debug_assertions)]
            let original_index = host.index.index.clone();

            let mut current = host;
            for proj in projs {
                projector.project(proj.with_host(&mut current, ConcreteValueRef::new));
            }

            debug_assert!(
                ValueRef::ptr_eq(&current.index.index.0, &original_index.0),
                "The projector is expected to pass projection through selects."
            );
            current
        }
    }

    impl SymReadResult {
        pub(super) fn apply_on_leaf_nodes(
            select: &mut Select,
            f: &mut impl FnMut(ConcreteValueRef) -> ValueRef,
            resolver: &mut impl ProjExprReadResolver,
        ) {
            Self::internal_apply_on_leaf_nodes(select, 1, f, resolver)
        }

        fn internal_apply_on_leaf_nodes(
            select: &mut Select,
            expected_dim: usize,
            f: &mut impl FnMut(ConcreteValueRef) -> ValueRef,
            resolver: &mut impl ProjExprReadResolver,
        ) {
            match &mut select.target {
                SelectTarget::Array(ref mut values) => {
                    values
                        .iter_mut()
                        .for_each(|v| v.internal_apply_on_value(expected_dim - 1, f, resolver));
                }
                SelectTarget::Nested(box nested) => {
                    Self::internal_apply_on_leaf_nodes(nested, expected_dim + 1, f, resolver)
                }
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
        fn internal_apply_on_value(
            &mut self,
            dim: usize,
            f: &mut impl FnMut(ConcreteValueRef) -> ValueRef,
            resolver: &mut impl ProjExprReadResolver,
        ) {
            // Resolve or expand value for further mutation.
            if let SymReadResult::Value(value) = self {
                match value.as_ref() {
                    Value::Concrete(ConcreteValue::Array(array)) if dim > 0 => {
                        *self = SymReadResult::Array(
                            array
                                .elements
                                .iter()
                                .cloned()
                                .map(SymReadResult::Value)
                                .collect(),
                        )
                    }
                    Value::Symbolic(sym_value) => {
                        *self = SymReadResult::SymRead(resolver.resolve(sym_value.expect_proj()))
                    }
                    _ => (),
                };
            }

            match self {
                SymReadResult::Value(value) => match value.as_ref() {
                    Value::Concrete(_) => {
                        assert_eq!(dim, 0, "Concrete value is expected to be expandable.");
                        *value = f(ConcreteValueRef::new(value.clone()));
                    }
                    _ => unreachable!(),
                },
                SymReadResult::Array(values) => values
                    .iter_mut()
                    .for_each(|v| v.internal_apply_on_value(dim - 1, f, resolver)),
                SymReadResult::SymRead(select) => {
                    Self::internal_apply_on_leaf_nodes(select, dim, f, resolver)
                }
            }
        }
    }

    /// A projector for `Select` values that performs the projection in-place over
    /// their target.
    /// As the `Select` values are generated per each symbolic index, and they have
    /// a recursive structure, this projector should have less overhead than another
    /// that follows an immutable fashion.
    /// Note that it doesn't mutate actual `Value`s, but only the `Select` values.
    ///
    /// # Fields
    /// `projector`: The regular projector that is used for (concrete) leaf nodes.
    /// `resolver`: Used to resolve the inner symbolic values if they appear as a
    ///             leaf node that we need to apply some projections on.
    struct PossibleValuesInPlaceProjector<'r, P> {
        projector: P,
        resolver: &'r mut DefaultProjExprReadResolver,
    }

    impl<'r, P> Projector for PossibleValuesInPlaceProjector<'r, P>
    where
        for<'h> P: Projector<
                HostRef<'h> = ConcreteValueRef,
                HIRefPair<'h> = (ConcreteValueRef, ConcreteValueRef),
                Proj<'h> = Result<ValueRef, ConcreteValueRef>,
            >,
    {
        type HostRef<'a> = &'a mut Select;

        type HIRefPair<'a> = (Self::HostRef<'a>, ConcreteValueRef);

        type Proj<'a> = ();

        fn project<'a>(
            &mut self,
            proj_on: ProjectionOn<Self::HostRef<'a>, Self::HIRefPair<'a>>,
        ) -> Self::Proj<'a> {
            let (host, proj) = proj_on.destruct();
            SymReadResult::apply_on_leaf_nodes(
                host,
                &mut |value| {
                    self.projector
                        .project(proj.clone_with_host(value))
                        .unwrap_result(&proj)
                },
                self.resolver,
            );
        }

        impl_singular_projs_through_general!();
    }

    /// Makes the existing `ConcreteProjector` compatible for the usage of the resolver.
    struct ConcreteProjectorAdapter<F, I>(ConcreteProjector<F, I>);

    impl<F> ConcreteProjectorAdapter<F, ()> {
        fn new(
            get_place: F,
        ) -> ConcreteProjectorAdapter<F, impl Fn(ConcreteValueRef, SymValueRef, bool) -> ValueRef>
        where
            F: Fn(&FullPlace) -> ValueRef,
        {
            ConcreteProjectorAdapter(ConcreteProjector {
                get_place,
                handle_sym_index: |_, _, _| unreachable!("Structurally impossible."),
            })
        }
    }

    impl<F, I> Projector for ConcreteProjectorAdapter<F, I>
    where
        F: Fn(&FullPlace) -> ValueRef,
        I: Fn(ConcreteValueRef, SymValueRef, bool) -> ValueRef,
    {
        type HostRef<'a> = ConcreteValueRef;
        type HIRefPair<'a> = (ConcreteValueRef, ConcreteValueRef);
        type Proj<'a> = Result<ValueRef, ConcreteValueRef>;

        fn project<'a>(
            &mut self,
            pair: ProjectionOn<Self::HostRef<'a>, Self::HIRefPair<'a>>,
        ) -> Self::Proj<'a> {
            self.0.project(pair.index_into())
        }

        impl_singular_projs_through_general!();
    }

    pub(super) mod utils {
        use super::SliceIndex;
        use crate::{abs::expr::proj::ProjectionOn, backends::basic::expr::prelude::*};

        impl SymValue {
            pub(super) fn as_proj(&self) -> Option<&ProjExpr> {
                match self {
                    SymValue::Expression(Expr::Projection(host)) => Some(host),
                    _ => None,
                }
            }

            pub(super) fn expect_proj(&self) -> &ProjExpr {
                self.as_proj().unwrap_or_else(|| {
                    panic!(
                        concat!(
                            "Complex symbolic values are not supported by this resolver. ",
                            "The symbolic host is expected to be a projection expression.",
                            "Found: {:?}"
                        ),
                        self
                    )
                })
            }
        }

        impl ProjExpr {
            pub(in super::super) fn flatten(&self) -> (&ConcreteHostProj, Vec<&ProjKind>) {
                match self {
                    ProjExpr::SymIndex(sym_index) => (sym_index, vec![]),
                    ProjExpr::SymHost(sym_host) => sym_host.flatten(),
                }
            }
        }

        impl SymHostProj {
            fn flatten(&self) -> (&ConcreteHostProj, Vec<&ProjKind>) {
                let SymHostProj { host, kind } = self;
                let host = host.expect_proj();
                let (sym_index, mut projs) = host.flatten();
                projs.push(kind);
                (sym_index, projs)
            }
        }

        impl ProjKind {
            pub(super) fn with_host<H, I>(
                &self,
                host: H,
                map_index: impl FnOnce(ValueRef) -> I,
            ) -> ProjectionOn<H, (H, I)> {
                match self {
                    ProjKind::Deref => ProjectionOn::Deref(host),
                    ProjKind::Field(field) => ProjectionOn::Field(host, *field),
                    ProjKind::Index(SliceIndex { index, from_end }) => {
                        ProjectionOn::Index((host, map_index(index.clone())), *from_end)
                    }
                    ProjKind::Subslice { from, to, from_end } => {
                        ProjectionOn::Subslice(host, *from, *to, *from_end)
                    }
                    ProjKind::Downcast(to_variant) => ProjectionOn::Downcast(host, *to_variant),
                }
            }
        }
    }
}
