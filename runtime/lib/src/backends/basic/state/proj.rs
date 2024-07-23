use super::{
    super::{expr::prelude::*, FullPlace, ValueRef},
    PlaceError, ResolvedProjection, SymbolicProjector,
};
use crate::abs::{
    expr::proj::{macros::impl_general_proj_through_singulars, Projector},
    FieldIndex, VariantIndex,
};
use std::fmt::Debug;

pub(crate) trait MutRefResolver {
    fn get_full_place(&self, place: &FullPlace) -> Result<ValueRef, PlaceError>;
}

pub(crate) struct ConcreteProjector<F, I> {
    pub get_place: F,
    pub handle_sym_index: I,
}

impl<F, I> Projector for ConcreteProjector<F, I>
where
    F: Fn(&FullPlace) -> ValueRef,
    I: Fn(ConcreteValueRef, SymValueRef, bool) -> ValueRef,
{
    type HostRef<'a> = ConcreteValueRef;
    type FieldAccessor = FieldIndex;
    type HIRefPair<'a> = (ConcreteValueRef, ValueRef);
    type DowncastTarget = VariantIndex;
    type Proj<'a> = Result<ValueRef, ConcreteValueRef>;

    impl_general_proj_through_singulars!();

    fn deref<'a>(&mut self, host: Self::HostRef<'a>) -> Self::Proj<'a> {
        match host.as_ref() {
            ConcreteValue::Ref(RefValue::Immut(value)) => Ok(value.clone()),
            ConcreteValue::Ref(RefValue::Mut(place)) => Ok((self.get_place)(place)),
            _ => Err(host),
        }
    }

    fn field<'a>(&mut self, host: Self::HostRef<'a>, field: FieldIndex) -> Self::Proj<'a> {
        match host.as_ref() {
            ConcreteValue::Adt(AdtValue { fields, .. }) => Ok(fields[field as usize]
                .value
                .as_ref()
                .unwrap_or_else(|| panic!("Field should not be moved before. {field}"))
                .clone()),
            _ => Err(host),
        }
    }

    fn index<'a>(
        &mut self,
        (host, index): (ConcreteValueRef, ValueRef),
        from_end: bool,
    ) -> Self::Proj<'a> {
        match index.as_ref() {
            Value::Concrete(ConcreteValue::Const(ConstValue::Int { bit_rep, .. })) => {
                match host.as_ref() {
                    ConcreteValue::Array(ArrayValue { elements }) => {
                        let index = bit_rep.0 as usize;
                        Ok(if !from_end {
                            elements[index].clone()
                        } else {
                            elements[elements.len() - index].clone()
                        })
                    }
                    _ => Err(host),
                }
            }
            Value::Symbolic(_) => Ok((self.handle_sym_index)(
                host,
                SymValueRef::new(index),
                from_end,
            )),
            _ => panic!("Index should be an integer."),
        }
    }

    fn subslice<'a>(
        &mut self,
        host: Self::HostRef<'a>,
        from: u64,
        to: u64,
        from_end: bool,
    ) -> Self::Proj<'a> {
        todo!(
            "Add support for subslice {:?} {} {} {}.",
            host,
            from,
            to,
            from_end
        )
    }

    fn downcast<'a>(
        &mut self,
        host: Self::HostRef<'a>,
        to_variant: VariantIndex,
    ) -> Self::Proj<'a> {
        match host.as_ref() {
            ConcreteValue::Adt(AdtValue {
                kind: AdtKind::Enum { variant, .. },
                ..
            }) => {
                assert_eq!(
                    *variant, to_variant,
                    "Variant must be the same for concrete values."
                );
                Ok(host.0)
            }
            _ => Err(host),
        }
    }
}

pub(crate) trait ProjResultExt<R, P> {
    fn unwrap_result(self, proj: &P) -> R;
}
impl<R, H: Debug, P: Debug> ProjResultExt<R, P> for Result<R, H> {
    fn unwrap_result(self, proj: &P) -> R {
        self.unwrap_or_else(|host| {
            panic!(
                "Projection {:?} is not possible on this value {:?}.",
                &proj, &host
            )
        })
    }
}

pub(super) fn apply_projs_sym<'a, 'b>(
    sym_projector: &mut impl SymbolicProjector,
    host: &'a SymValueRef,
    projs: impl Iterator<Item = ResolvedProjection>,
) -> SymValueRef {
    projs.fold(host.clone(), |current, proj| {
        apply_proj_sym(sym_projector, current, &proj).to_value_ref()
    })
}

#[inline]
pub(super) fn apply_proj_sym<'b>(
    projector: &mut impl SymbolicProjector,
    host: SymValueRef,
    proj: &'b ResolvedProjection,
) -> ProjExpr {
    apply_proj(host, proj, projector)
}

pub(super) fn apply_proj<'b, 'h, 'p, Host, IndexPair, P, Result>(
    host: Host,
    proj: &'b ResolvedProjection,
    projector: &'p mut P,
) -> Result
where
    P: Projector,
    P::HostRef<'h>: From<Host>,
    P::FieldAccessor: From<FieldIndex>,
    IndexPair: From<(Host, ValueRef)>,
    P::HIRefPair<'h>: From<IndexPair>,
    P::DowncastTarget: From<VariantIndex>,
    P::Proj<'h>: Into<Result>,
{
    use crate::abs::place::Projection::*;
    match proj {
        Field(field) => projector.field(host.into(), (*field).into()),
        Deref => projector.deref(host.into()),
        Index(index) => {
            projector.index(Into::<IndexPair>::into((host, index.clone())).into(), false)
        }
        ConstantIndex {
            offset, from_end, ..
        } => {
            let index = ConstValue::from(*offset).to_value_ref();
            projector.index(Into::<IndexPair>::into((host, index)).into(), *from_end)
        }
        Subslice { from, to, from_end } => projector.subslice(host.into(), *from, *to, *from_end),
        Downcast(variant) => projector.downcast(host.into(), (*variant).into()),
        OpaqueCast => todo!(),
        Subtype => todo!("#285"),
    }
    .into()
}

pub(super) trait IndexResolver<L> {
    fn get(&self, local: &L) -> Option<ValueRef>;
}

pub(super) trait ProjectionResolutionExt<L> {
    fn resolved_index(&self, index_resolver: &impl IndexResolver<L>) -> ResolvedProjection;
}
impl<L> ProjectionResolutionExt<L> for crate::abs::Projection<L> {
    fn resolved_index(&self, index_resolver: &impl IndexResolver<L>) -> ResolvedProjection {
        use crate::abs::place::Projection::*;
        match self {
            Field(field) => Field(*field),
            Deref => Deref,
            Index(index) => Index(index_resolver.get(index).unwrap()),
            ConstantIndex {
                offset,
                min_length,
                from_end,
            } => ConstantIndex {
                offset: *offset,
                min_length: *min_length,
                from_end: *from_end,
            },
            Subslice { from, to, from_end } => Subslice {
                from: *from,
                to: *to,
                from_end: *from_end,
            },
            Downcast(variant) => Downcast(*variant),
            OpaqueCast => OpaqueCast,
            Subtype => Subtype,
        }
    }
}
