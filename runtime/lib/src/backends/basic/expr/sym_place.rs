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

use crate::abs::TypeId;
use common::log_debug;

use super::{prelude::*, SliceIndex};

type SymIndex = SliceIndex<SymValueRef>;
pub(super) type Select<V = SymbolicProjResult> = crate::abs::expr::sym_place::Select<SymIndex, V>;

/// Represents a possible value of a symbolic projection.
/// It can be either a symbolic read or a transmutation of a symbolic value.
#[derive(Clone, From)]
pub(crate) enum SymbolicProjResult {
    /// A symbolic read, i.e. projection with symbolic index.
    /// This one introduces multiple possible values.
    #[from]
    SymRead(Select),
    /// An array of values, as the result of an expansion.
    /// # Remarks
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
    Array(Vec<Self>),
    /// A single value that is result of projections with no symbolic index.
    /// For example, transmute(x).0 can generate such a value.
    #[from(forward)]
    Single(SingleProjResult),
}

#[derive(Debug, Clone, From)]
pub(crate) enum SingleProjResult {
    /// An intermediate symbolic value that is transmuted to another type.
    /// # Remarks
    /// This variation is not expected to appear in final solver expressions,
    /// but can accept operations on it.
    /// The underlying value then is turned into a symbolic value based on the
    /// operation. For example, `transmute(x)[1]` is different from `len(transmute(x))`.
    Transmuted(TransmutedValue),
    /* NOTE: Why not `SymValueRef`?
     * Theoretically, it is possible to obtain a definite value from a symbolic projection.
     * For example, `Len(transmute(x) as [u8; 4])` is a definite value but optimized by the
     * compiler. Or if symbolic writes are supported, a portion of a symbolic array can be
     * concrete. */
    Value(ValueRef),
}

#[derive(Debug, Clone)]
pub(crate) struct TransmutedValue {
    /* NOTE: Why `ValueRef`?
     * It is feasible to transmute possible values of a select, which are possibly
     * concrete. */
    pub(super) value: ValueRef,
    pub(super) dst_ty_id: TypeId,
}

pub(super) trait ProjExprResolver {
    fn resolve<'a>(&self, proj_value: &'a ProjExpr) -> SymbolicProjResult;

    /// Expands the given value to another representation that is indexable.
    /// # Returns
    /// An expanded version of the given value.
    /// If the value get resolved to a single possible value, that value must
    /// be indexable and it is expanded to an `Array`,
    /// otherwise a `Select` will be returned to be expanded later.
    /// The returned value is guaranteed to not be a `SingleResult`.
    fn expand<'a>(&self, value: &SingleProjResult) -> SymbolicProjResult;
}

#[derive(Default)]
pub(super) struct DefaultProjExprReadResolver;

pub(super) fn apply_address_of(
    mut host: SymbolicProjResult,
    resolver: &mut impl ProjExprResolver,
) -> SymbolicProjResult {
    host.mutate_values(
        &mut |value| {
            *value = match value {
                SingleProjResult::Transmuted(_) => todo!("Transmutation is not supported yet."),
                SingleProjResult::Value(value) => {
                    assert!(
                        !value.is_symbolic(),
                        "A symbolic value is not expected, got: {:?}",
                        value
                    );
                    use crate::abs::expr::UnaryExprBuilder;
                    use crate::backends::basic::expr::builders::ConcreteBuilder;
                    ConcreteBuilder::default()
                        .address_of(ConcreteValueRef::new(value.clone()))
                        .into()
                }
            }
        },
        resolver,
    );
    host
}

/// Applies the length operator on the given `Select` value, i.e. the result `Select` leaf values
/// correspond to the lengths of the host leaf values.
pub(super) fn apply_len(
    mut host: SymbolicProjResult,
    resolver: &mut impl ProjExprResolver,
) -> SymbolicProjResult {
    host.mutate_values(
        &mut |value| {
            *value = match value {
                SingleProjResult::Transmuted(_) => todo!("Transmutation is not supported yet."),
                SingleProjResult::Value(value) => {
                    assert!(
                        !value.is_symbolic(),
                        "A symbolic value is not expected, got: {:?}",
                        value
                    );
                    use crate::abs::expr::UnaryExprBuilder;
                    use crate::backends::basic::expr::builders::ConcreteBuilder;
                    ConcreteBuilder::default()
                        .len(ConcreteValueRef::new(value.clone()))
                        .into()
                }
            }
        },
        resolver,
    );
    host
}

mod implementation {
    use crate::{
        abs::{
            expr::{
                proj::{
                    macros::{
                        impl_singular_proj_through_general, impl_singular_projs_through_general,
                    },
                    ProjectionOn, Projector,
                },
                sym_place::{SelectTarget, SymbolicReadResolver},
            },
            TypeId,
        },
        backends::basic::{
            state::proj::{ConcreteProjector, ProjResultExt},
            FullPlace,
        },
    };

    use super::*;
    use utils::*;

    impl ProjExprResolver for DefaultProjExprReadResolver {
        fn resolve<'a>(&self, proj_value: &'a ProjExpr) -> SymbolicProjResult {
            // FIXME: No need to distinguish these cases anymore. We may remove ConcreteHostProj.
            let (base, projs) = proj_value.flatten();
            match base {
                ProjBase::Concrete(host) => SymbolicProjResult::SymRead(
                    SymbolicReadResolver::resolve(self, (host, &projs)).into(),
                ),
                ProjBase::Transmutation(host, dst_ty_id) => self.resolve_symbolic_projs(
                    TransmutedValue::new(host.clone().into(), dst_ty_id).into(),
                    &projs,
                ),
            }
        }

        fn expand<'a>(&self, value: &SingleProjResult) -> SymbolicProjResult {
            match value {
                SingleProjResult::Transmuted(trans) => SymbolicProjResult::Array(
                    self.expand_transmuted(trans)
                        .into_iter()
                        .map(Into::into)
                        .collect(),
                ),
                SingleProjResult::Value(value) => match value.as_ref() {
                    Value::Concrete(value) => SymbolicProjResult::Array(
                        self.expand_concrete(value)
                            .into_iter()
                            .map(Into::into)
                            .collect(),
                    ),
                    Value::Symbolic(value) => self.expand_symbolic(value),
                },
            }
        }
    }

    /// # Remarks
    /// If there is concrete host, we know for sure that the result is a select.
    /// However, transmutations turn into symbolic reads if there is a symbolic index.
    impl SymbolicReadResolver<SymIndex> for DefaultProjExprReadResolver {
        type SymValue<'a> = (&'a ConcreteHostProj, &'a [&'a ProjKind]);
        type PossibleValue<'a> = SymbolicProjResult;

        fn resolve<'a>(
            &self,
            (base, projs): Self::SymValue<'a>,
        ) -> Select<Self::PossibleValue<'a>> {
            let index = ProjKind::Index(SliceIndex {
                index: base.index.index.0.clone().into(),
                from_end: base.index.from_end,
            });
            let base = base.host.0.clone().into();
            let projs = [&[&index], projs].concat();
            let SymbolicProjResult::SymRead(select) = self.resolve_symbolic_projs(base, &projs)
            else {
                unreachable!("Symbolic index causes a symbolic read.")
            };
            select
        }
    }

    impl TransmutedValue {
        pub(super) fn new(value: ValueRef, dst_ty_id: TypeId) -> Self {
            let is_proj = match value.as_ref() {
                Value::Symbolic(SymValue::Expression(Expr::Projection(..))) => true,
                _ => false,
            };
            assert!(
                !is_proj,
                "The base value of a transmutation should be resolved at this point."
            );

            Self { value, dst_ty_id }
        }
    }

    impl DefaultProjExprReadResolver {
        pub(super) fn resolve_symbolic_projs(
            &self,
            base: SymbolicProjResult,
            projs: &[&ProjKind],
        ) -> SymbolicProjResult {
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

            let mut current = base;
            let mut last_index = 0;
            for (i, index) in indices {
                current = self.project_one_to_ones(current, &projs[last_index..i]);
                last_index = i + 1;

                log_debug!("Found a symbolic index in the projection chain at {i}");
                if let SymbolicProjResult::Single(value) = current {
                    current = self.expand(&value)
                }
                let target = match current {
                    SymbolicProjResult::SymRead(current) => SelectTarget::Nested(Box::new(current)),
                    SymbolicProjResult::Array(values) => SelectTarget::Array(values),
                    _ => unreachable!("The single value is expected to be expanded at this point."),
                };
                current = SymbolicProjResult::SymRead(Select { index, target }).into();
            }

            if last_index < projs.len() {
                current = self.project_one_to_ones(current, &projs[last_index..]);
            }

            current
        }

        pub(super) fn expand_transmuted(&self, value: &TransmutedValue) -> Vec<SingleProjResult> {
            unimplemented!("Transmutation is not supported yet, got: {:?}", value)
        }

        pub(super) fn expand_concrete(&self, value: &ConcreteValue) -> Vec<SingleProjResult> {
            match value {
                ConcreteValue::Array(array) => {
                    array.elements.iter().cloned().map(Into::into).collect()
                }
                _ => unreachable!("Only arrays are expected to be expanded, got: {:?}", value),
            }
        }

        pub(super) fn expand_symbolic(&self, value: &SymValue) -> SymbolicProjResult {
            let Some(proj) = value.as_proj() else {
                unreachable!(
                    "Only projections are expected to be expanded, got: {:?}",
                    value
                )
            };

            let resolved = ProjExprResolver::resolve(self, proj);
            match resolved {
                SymbolicProjResult::Single(single) => self.expand(&single),
                _ => resolved,
            }
        }

        /// Applies the given one-to-one projections to the given host.
        /// By one-to-one projection we mean a projection that doesn't get resolved
        /// to many possible values, i.e., all projections other than a symbolic index.
        fn project_one_to_ones(
            &self,
            mut host: SymbolicProjResult,
            projs: &[&ProjKind],
        ) -> SymbolicProjResult {
            if projs.is_empty() {
                return host;
            }

            debug_assert!(
                !projs.iter().any(
                    |p| matches!(p, ProjKind::Index (SliceIndex{ index, .. }) if index.is_symbolic())
                ),
                "Not meant for one-to-many projections (symbolic indices)."
            );

            let mut projector = ResultInPlaceProjector { resolver: self };

            for proj in projs {
                projector.project(proj.with_host(&mut host, ConcreteValueRef::new));
            }

            host
        }
    }

    impl Select {
        pub(super) fn mutate_leaf_nodes(
            &mut self,
            expected_dim: usize,
            f: &mut impl FnMut(&mut SingleProjResult),
            resolver: &impl ProjExprResolver,
        ) {
            match &mut self.target {
                SelectTarget::Array(ref mut values) => {
                    values
                        .iter_mut()
                        .for_each(|v| v.internal_mutate_values(expected_dim - 1, f, resolver));
                }
                SelectTarget::Nested(box nested) => {
                    Self::mutate_leaf_nodes(nested, expected_dim + 1, f, resolver)
                }
            }
        }
    }

    impl SymbolicProjResult {
        pub(super) fn mutate_values(
            &mut self,
            f: &mut impl FnMut(&mut SingleProjResult),
            resolver: &impl ProjExprResolver,
        ) {
            match self {
                SymbolicProjResult::SymRead(select) => select.mutate_leaf_nodes(1, f, resolver),
                SymbolicProjResult::Array(values) => values
                    .iter_mut()
                    .for_each(|v| v.internal_mutate_values(0, f, resolver)),
                SymbolicProjResult::Single(value) => f(value),
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
        fn internal_mutate_values(
            &mut self,
            dim: usize,
            f: &mut impl FnMut(&mut SingleProjResult),
            resolver: &impl ProjExprResolver,
        ) {
            if let SymbolicProjResult::Single(SingleProjResult::Value(value)) = self {
                // Resolve projections before the mutation.
                if let Value::Symbolic(SymValue::Expression(Expr::Projection(proj))) =
                    value.as_ref()
                {
                    *self = ProjExprResolver::resolve(resolver, proj);
                }
            }

            // Expand if expected.
            if dim > 0 {
                if let SymbolicProjResult::Single(single @ SingleProjResult::Value(..)) = self {
                    *self = resolver.expand(&single);
                }
            }

            match self {
                SymbolicProjResult::SymRead(select) => select.mutate_leaf_nodes(dim, f, resolver),
                SymbolicProjResult::Array(values) => values
                    .iter_mut()
                    .for_each(|v| v.internal_mutate_values(dim - 1, f, resolver)),
                SymbolicProjResult::Single(value) => f(value),
            }
        }
    }

    struct ResultInPlaceProjector<'r> {
        resolver: &'r DefaultProjExprReadResolver,
    }

    impl Projector for ResultInPlaceProjector<'_> {
        type HostRef<'a> = &'a mut SymbolicProjResult;
        type FieldAccessor = FieldAccessKind;
        type HIRefPair<'a> = (&'a mut SymbolicProjResult, ConcreteValueRef);
        type DowncastTarget = DowncastKind;
        type Proj<'a> = ();

        fn project<'a>(
            &mut self,
            proj_on: ProjectionOn<
                Self::HostRef<'a>,
                Self::FieldAccessor,
                Self::HIRefPair<'a>,
                Self::DowncastTarget,
            >,
        ) -> Self::Proj<'a> {
            let (host, proj) = proj_on.destruct();
            let resolver = self.resolver;
            let mut value_projector = SingleValueInPlaceProjector;
            match host {
                SymbolicProjResult::Single(single) => {
                    value_projector.project(proj.clone_with_host(single))
                }
                _ => host.mutate_values(
                    &mut |v| value_projector.project(proj.clone_with_host(v)),
                    resolver,
                ),
            }
        }

        impl_singular_projs_through_general!();
    }

    struct SingleValueInPlaceProjector;

    impl Projector for SingleValueInPlaceProjector {
        type HostRef<'a> = &'a mut SingleProjResult;
        type FieldAccessor = FieldAccessKind;
        type HIRefPair<'a> = (&'a mut SingleProjResult, ConcreteValueRef);
        type DowncastTarget = DowncastKind;
        type Proj<'a> = ();

        fn project<'a>(
            &mut self,
            proj_on: ProjectionOn<
                Self::HostRef<'a>,
                Self::FieldAccessor,
                Self::HIRefPair<'a>,
                Self::DowncastTarget,
            >,
        ) -> Self::Proj<'a> {
            let (host, proj) = proj_on.destruct();
            match host {
                SingleProjResult::Transmuted(_) => todo!("Transmutation is not supported yet."),
                SingleProjResult::Value(value) => {
                    debug_assert!(
                        !value.is_symbolic(),
                        "Symbolic value is not expected and has to be resolved before, got: {:?}",
                        value
                    );

                    let projector = &mut ConcreteProjector {
                        get_place: |_: &FullPlace| -> ValueRef { todo!("#234") },
                        handle_sym_index: |_, _, _| unreachable!("Structurally impossible."),
                    };

                    let proj_on = proj
                        .clone_with_host(ConcreteValueRef::new(value.clone()))
                        .map(
                            |h| h,
                            |fa| {
                                if let FieldAccessKind::Index(index) = fa {
                                    index
                                } else {
                                    todo!("PtrMetadata is not supported yet.")
                                }
                            },
                            |(h, i)| (h, i.into()),
                            |dc| {
                                if let DowncastKind::EnumVariant(variant_index) = dc {
                                    variant_index
                                } else {
                                    unreachable!()
                                }
                            },
                        );

                    *value = projector.project(proj_on).unwrap_result(&proj);
                }
            }
        }

        impl_singular_projs_through_general!(deref, field, index, subslice);

        fn downcast<'a>(
            &mut self,
            host: Self::HostRef<'a>,
            target: Self::DowncastTarget,
        ) -> Self::Proj<'a> {
            match target {
                DowncastKind::Transmutation(dst_ty_id) => {
                    let underlying_value = match host {
                        SingleProjResult::Transmuted(TransmutedValue { value, .. }) => {
                            // A transmutation overwrites another transmutation.
                            value
                        }
                        SingleProjResult::Value(value) => value,
                    }
                    .clone();
                    *host = TransmutedValue {
                        value: underlying_value,
                        dst_ty_id,
                    }
                    .into()
                }
                DowncastKind::EnumVariant(_) => self.project(ProjectionOn::Downcast(host, target)),
            }
        }
    }

    pub(super) mod utils {
        use super::{SliceIndex, TypeId};
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

        pub(super) enum ProjBase<'a> {
            Concrete(&'a ConcreteHostProj),
            Transmutation(&'a SymValueRef, TypeId),
        }

        impl ProjExpr {
            pub(super) fn flatten(&self) -> (ProjBase, Vec<&ProjKind>) {
                use ProjBase::*;
                match self {
                    ProjExpr::SymIndex(sym_index) => (Concrete(sym_index), vec![]),
                    ProjExpr::SymHost(sym_host) => sym_host.flatten(),
                }
            }
        }

        impl SymHostProj {
            fn flatten(&self) -> (ProjBase, Vec<&ProjKind>) {
                let SymHostProj { host, kind } = self;
                match kind {
                    ProjKind::Downcast(DowncastKind::Transmutation(dst_ty_id))
                        if host.as_proj().is_none() =>
                    {
                        (ProjBase::Transmutation(host, *dst_ty_id), vec![])
                    }
                    _ => {
                        let host = host.expect_proj();
                        let (sym_index, mut projs) = host.flatten();
                        projs.push(kind);
                        (sym_index, projs)
                    }
                }
            }
        }

        impl ProjKind {
            pub(super) fn with_host<H, I>(
                &self,
                host: H,
                map_index: impl FnOnce(ValueRef) -> I,
            ) -> ProjectionOn<H, FieldAccessKind, (H, I), DowncastKind> {
                match self {
                    ProjKind::Deref => ProjectionOn::Deref(host),
                    ProjKind::Field(field) => ProjectionOn::Field(host, field.clone()),
                    ProjKind::Index(SliceIndex { index, from_end }) => {
                        ProjectionOn::Index((host, map_index(index.clone())), *from_end)
                    }
                    ProjKind::Subslice { from, to, from_end } => {
                        ProjectionOn::Subslice(host, *from, *to, *from_end)
                    }
                    ProjKind::Downcast(target) => ProjectionOn::Downcast(host, *target),
                }
            }
        }
    }
}
