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

use super::{prelude::*, ProjKind, SliceIndex, SymHostProj};

type SymIndex = SliceIndex<SymValueRef>;
pub(super) type Select<V> = crate::abs::expr::sym_place::Select<SymIndex, V>;
// type SelectTarget<V> = crate::abs::expr::sym_place::SelectTarget<V, Select<V>>;

define_either_pair! {
    #[derive(Debug)]
    pub(crate) SymReadResult {
        Value(ValueRef),
        SymRead(Select<SymReadResult>),
    }
}

define_either_pair! {
    #[derive(Debug)]
    pub(crate) ResolvedSymHost {
        Value(ConcreteValueRef),
        Nested(Select<ResolvedSymHost>),
    }
}

pub(super) struct ProjExprReadResolver;

impl SymbolicReadResolver<SymIndex> for ProjExprReadResolver {
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

impl ProjExprReadResolver {
    fn resolve_concrete_host(proj: &ConcreteHostProj) -> Select<SymReadResult> {
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

    fn project_one_to_ones(
        &mut self,
        host: Select<SymReadResult>,
        projs: &[&ProjKind],
    ) -> Select<SymReadResult> {
        debug_assert!(
            !projs.iter().any(
                |p| matches!(p, ProjKind::Index (SliceIndex{ index, .. }) if index.is_symbolic())
            ),
            "Not meant for one-to-many projections (symbolic indices)."
        );

        if projs.is_empty() {
            return host;
        }

        let mut projector = PossibleValuesInplaceProjector {
            projector: ConcreteProjectorAdapter::new(|p: &FullPlace| -> ValueRef { todo!() }),
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

    // fn resolve_for_projection(&mut self, mut value: SymReadResult) -> ResolvedSymHost {
    //     self.resolve_recursive(&mut value);
    //     value.try_into().expect("Complex symbolic values are not supported by this resolver. The symbolic host is expected to be a projection expression.")
    // }

    // fn resolve_recursive(&mut self, result: &mut SymReadResult) {
    //     match result {
    //         SymReadResult::Value(value) => {
    //             if let Some(proj) = value.as_proj() {
    //                 let mut select = self.resolve(proj);
    //                 self.resolve_recursive_select(&mut select);
    //                 *result = SymReadResult::SymRead(select);
    //             }
    //         }
    //         SymReadResult::SymRead(select) => {
    //             self.resolve_recursive_select(select);
    //         }
    //     }
    // }

    // fn resolve_recursive_select(&mut self, select: &mut Select<SymReadResult>) {
    //     match &mut select.target {
    //         SelectTarget::Values(values) => values.iter_mut().for_each(|v| {
    //             self.resolve_recursive(v);
    //         }),
    //         SelectTarget::Nested(box inner) => self.resolve_recursive_select(inner),
    //     };
    // }
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
    type Proj<'a> = ValueRef;

    fn project<'a>(
        &mut self,
        pair: ProjectionOn<Self::HostRef<'a>, Self::HIRefPair<'a>>,
    ) -> Self::Proj<'a> {
        self.0.project(pair.index_into()).unwrap_result(&())
    }

    impl_singular_projs_through_general!();
}

impl Value {
    fn as_proj(&self) -> Option<&ProjExpr> {
        match self {
            Value::Symbolic(sym) => sym.as_proj(),
            _ => None,
        }
    }
}

impl SymValue {
    fn as_proj(&self) -> Option<&ProjExpr> {
        match self {
            SymValue::Expression(Expr::Projection(host)) => Some(host),
            _ => None,
        }
    }

    fn expect_proj(&self) -> &ProjExpr {
        self.as_proj().expect("Complex symbolic values are not supported by this resolver. The symbolic host is expected to be a projection expression.")
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

struct PossibleValuesInplaceProjector<'r, P> {
    projector: P,
    resolver: &'r mut ProjExprReadResolver,
}

impl<'r, P> Projector for PossibleValuesInplaceProjector<'r, P>
where
    P: Projector,
    for<'h> P::HostRef<'h>: From<ConcreteValueRef>,
    for<'h> P::HIRefPair<'h>: From<(ConcreteValueRef, ConcreteValueRef)>,
    for<'h> P::Proj<'h>: Into<ValueRef>,
{
    type HostRef<'a> = &'a mut Select<SymReadResult>;

    type HIRefPair<'a> = (Self::HostRef<'a>, ConcreteValueRef);

    type Proj<'a> = ();

    fn project<'a>(
        &mut self,
        proj_on: ProjectionOn<Self::HostRef<'a>, Self::HIRefPair<'a>>,
    ) -> Self::Proj<'a> {
        let (host, proj) = proj_on.destruct();

        // Let the projection pass through the selects.
        match &mut host.target {
            SelectTarget::Array(values) => values
                .iter_mut()
                .for_each(|v| self.project_leaf_result(v, &proj)),
            SelectTarget::Nested(box nested) => self.project(proj.map_host(|_| nested)),
        };
    }

    impl_singular_projs_through_general!();
}

impl<'r, P> PossibleValuesInplaceProjector<'r, P>
where
    P: Projector,
    for<'h> P::HostRef<'h>: From<ConcreteValueRef>,
    for<'h> P::HIRefPair<'h>: From<(ConcreteValueRef, ConcreteValueRef)>,
    for<'h> P::Proj<'h>: Into<ValueRef>,
{
    fn project_leaf_result(
        &mut self,
        host: &mut SymReadResult,
        proj: &ProjectionOn<(), ((), ConcreteValueRef)>,
    ) {
        match host {
            SymReadResult::Value(ref mut value) => match value.as_ref() {
                Value::Concrete(_) => {
                    *value = self
                        .projector
                        .project(
                            proj.clone_with_host(ConcreteValueRef::new(value.clone()))
                                .into(),
                        )
                        .into();
                }
                Value::Symbolic(value) => {
                    let mut resolved = self.resolver.resolve(value.expect_proj());
                    self.project(proj.clone_with_host(&mut resolved));
                    *host = SymReadResult::SymRead(resolved);
                }
            },
            SymReadResult::SymRead(select) => self.project(proj.clone_with_host(select)),
        }
    }
}
