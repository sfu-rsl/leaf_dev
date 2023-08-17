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

use std::fmt::Display;

use crate::{
    abs::expr::{
        proj::{macros::impl_singular_projs_through_general, ProjectionOn, Projector},
        sym_place::{SelectTarget, SymbolicReadResolver},
    },
    backends::basic::{
        place::FullPlace,
        state::proj::{ConcreteProjector, ProjResultExt},
    },
    utils::meta::define_either_pair,
};

use super::{builders::ConcreteBuilder, prelude::*, ProjKind, SliceIndex, SymHostProj};

type SymIndex = SliceIndex<SymValueRef>;
pub(super) type Select<V = SymReadResult> = crate::abs::expr::sym_place::Select<SymIndex, V>;

define_either_pair! {
    /// Represents a possible value of a symbolic read.
    /// It can be either a value or the result of another symbolic read (e.g., an
    /// consider `a[x]` is stored in `b[1]`. Then a possible value for `b[x]` is
    /// is `a[x]` which is a symbolic read). This is particularly used when
    /// applying further projections after the symbolic index.
    #[derive(Debug)]
    pub(crate) SymReadResult {
        Value(ValueRef),
        SymRead(Select),
    }
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

impl SymReadResult {
    fn mut_concrete_value(
        &mut self,
        f: &mut impl FnMut(ConcreteValueRef) -> ValueRef,
        resolve: &mut impl ProjExprReadResolver,
    ) {
        match self {
            SymReadResult::Value(value) => match value.as_ref() {
                Value::Concrete(_) => {
                    *value = f(ConcreteValueRef::new(value.clone()));
                    return;
                }
                Value::Symbolic(sym_value) => {
                    let mut resolved = resolve.resolve(sym_value.expect_proj());
                    Self::mut_values(&mut resolved, &mut |v| v.mut_concrete_value(f, resolve));
                    *self = SymReadResult::SymRead(resolved);
                }
            },
            SymReadResult::SymRead(select) => {
                Self::mut_values(select, &mut |v| v.mut_concrete_value(f, resolve));
            }
        }
    }

    fn mut_values(select: &mut Select, f: &mut impl FnMut(&mut SymReadResult)) {
        match &mut select.target {
            SelectTarget::Array(ref mut values) => {
                values.iter_mut().for_each(f);
            }
            SelectTarget::Nested(box nested) => Self::mut_values(nested, f),
        }
    }
}

#[derive(Default)]
pub(super) struct DefaultProjExprReadResolver;

impl SymbolicReadResolver<SymIndex> for DefaultProjExprReadResolver {
    type SymValue<'a> = &'a ProjExpr;

    type PossibleValue<'a> = SymReadResult;

    fn resolve<'a>(&mut self, sym_value: Self::SymValue<'a>) -> Select<Self::PossibleValue<'a>> {
        let (base, projs) = sym_value.flatten();

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
            current = current.select(index);
        }

        if last_index < projs.len() {
            current = self.project_one_to_ones(current, &projs[last_index..]);
        }

        current
    }
}

impl DefaultProjExprReadResolver {
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

        match &mut host.target {
            SelectTarget::Array(values) => values
                .iter_mut()
                .for_each(|v| self.project_leaf_node(v, &proj)),
            // Let the projection pass through the selects.
            SelectTarget::Nested(box nested) => self.project(proj.map_host(|_| nested)),
        };
    }

    impl_singular_projs_through_general!();
}

impl<'r, P> PossibleValuesInPlaceProjector<'r, P>
where
    for<'h> P: Projector<
            HostRef<'h> = ConcreteValueRef,
            HIRefPair<'h> = (ConcreteValueRef, ConcreteValueRef),
            Proj<'h> = Result<ValueRef, ConcreteValueRef>,
        >,
{
    fn project_leaf_node(
        &mut self,
        host: &mut SymReadResult,
        proj: &ProjectionOn<(), ((), ConcreteValueRef)>,
    ) {
        host.mut_concrete_value(
            &mut |value| {
                self.projector
                    .project(proj.clone_with_host(value))
                    .unwrap_result(proj)
            },
            self.resolver,
        );
    }
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

impl SymValue {
    fn as_proj(&self) -> Option<&ProjExpr> {
        match self {
            SymValue::Expression(Expr::Projection(host)) => Some(host),
            _ => None,
        }
    }

    fn expect_proj(&self) -> &ProjExpr {
        self.as_proj().expect(concat!(
            "Complex symbolic values are not supported by this resolver. ",
            "The symbolic host is expected to be a projection expression."
        ))
    }
}

impl ProjExpr {
    fn flatten(&self) -> (&ConcreteHostProj, Vec<&ProjKind>) {
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
    fn with_host<H, I>(
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

pub(super) fn apply_len(mut host: Select, resolver: &mut impl ProjExprReadResolver) -> Select {
    SymReadResult::mut_values(&mut host, &mut |v| {
        v.mut_concrete_value(
            &mut |value| {
                use crate::abs::expr::UnaryExprBuilder;
                ConcreteBuilder::default().len(value)
            },
            resolver,
        )
    });
    host
}

impl Display for SymIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}ˢ", self.index, if self.from_end { "^" } else { "" })
    }
}
