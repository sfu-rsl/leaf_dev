use super::{super::expr::prelude::*, ResolvedProjection, SymbolicProjector};
use crate::abs::{
    expr::proj::{macros::impl_general_proj_through_singulars, Projector},
    FieldIndex, VariantIndex,
};
use std::fmt::Debug;

pub(crate) struct ConcreteProjector;

impl Projector for ConcreteProjector {
    type HostRef<'a> = ConcreteValueRef;
    type Metadata<'a> = ProjMetadata;
    type FieldAccessor = FieldAccessKind;
    type HIRefPair<'a> = (ConcreteValueRef, ConcreteValueRef);
    type DowncastTarget = VariantIndex;
    type Proj<'a> = Result<ValueRef, ConcreteValueRef>;

    impl_general_proj_through_singulars!();

    fn deref<'a>(
        &mut self,
        host: Self::HostRef<'a>,
        _metadata: Self::Metadata<'a>,
    ) -> Self::Proj<'a> {
        match host.as_ref() {
            ConcreteValue::Const(ConstValue::Addr(addr)) => {
                Ok(RawConcreteValue(*addr, None, LazyTypeInfo::None).to_value_ref())
            }
            _ => Err(host),
        }
    }

    fn field<'a>(
        &mut self,
        host: Self::HostRef<'a>,
        field: Self::FieldAccessor,
        _metadata: Self::Metadata<'a>,
    ) -> Self::Proj<'a> {
        match (host.as_ref(), field) {
            (ConcreteValue::Adt(AdtValue { fields, .. }), FieldAccessKind::Index(field)) => {
                Ok(fields[field as usize]
                    .value
                    .as_ref()
                    .unwrap_or_else(|| panic!("Field should not be moved before. {field}"))
                    .clone())
            }
            (
                ConcreteValue::FatPointer(FatPtrValue { metadata, .. }),
                FieldAccessKind::PtrMetadata,
            ) => Ok(metadata.clone().into()),
            _ => Err(host),
        }
    }

    fn index<'a>(
        &mut self,
        (host, index): (ConcreteValueRef, ConcreteValueRef),
        from_end: bool,
        _metadata: Self::Metadata<'a>,
    ) -> Self::Proj<'a> {
        match index.as_ref() {
            ConcreteValue::Const(ConstValue::Int { bit_rep, .. }) => match host.as_ref() {
                ConcreteValue::Array(ArrayValue { elements }) => {
                    let index = bit_rep.0 as usize;
                    Ok(if !from_end {
                        elements[index].clone()
                    } else {
                        elements[elements.len() - index].clone()
                    })
                }
                _ => Err(host),
            },
            _ => panic!("Index should be an integer."),
        }
    }

    fn subslice<'a>(
        &mut self,
        host: Self::HostRef<'a>,
        from: u64,
        to: u64,
        from_end: bool,
        _metadata: Self::Metadata<'a>,
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
        _metadata: Self::Metadata<'a>,
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
    projs: impl Iterator<Item = (ResolvedProjection, ProjMetadata)>,
) -> SymValueRef {
    projs.fold(host.clone(), |current, (proj, metadata)| {
        apply_proj_sym(sym_projector, current, &proj, metadata).to_value_ref()
    })
}

#[inline]
pub(super) fn apply_proj_sym<'b>(
    projector: &mut impl SymbolicProjector,
    host: SymValueRef,
    proj: &'b ResolvedProjection,
    metadata: ProjMetadata,
) -> ProjExpr {
    apply_proj(host, proj, metadata, projector)
}

pub(super) fn apply_proj<'b, 'h, 'p, Host, Meta, IndexPair, P, Result>(
    host: Host,
    proj: &'b ResolvedProjection,
    metadata: Meta,
    projector: &'p mut P,
) -> Result
where
    P: Projector,
    P::HostRef<'h>: From<Host>,
    P::Metadata<'h>: From<Meta>,
    P::FieldAccessor: From<FieldIndex>,
    IndexPair: From<(Host, ValueRef)>,
    P::HIRefPair<'h>: From<IndexPair>,
    P::DowncastTarget: From<VariantIndex>,
    P::Proj<'h>: Into<Result>,
{
    use crate::abs::place::Projection::*;
    match proj {
        Field(field) => projector.field(host.into(), (*field).into(), metadata.into()),
        Deref => projector.deref(host.into(), metadata.into()),
        Index(index) => projector.index(
            Into::<IndexPair>::into((host, index.clone())).into(),
            false,
            metadata.into(),
        ),
        ConstantIndex {
            offset, from_end, ..
        } => {
            let index = ConstValue::from(*offset).to_value_ref();
            projector.index(
                Into::<IndexPair>::into((host, index)).into(),
                *from_end,
                metadata.into(),
            )
        }
        Subslice { from, to, from_end } => {
            projector.subslice(host.into(), *from, *to, *from_end, metadata.into())
        }
        Downcast(variant) => projector.downcast(host.into(), (*variant).into(), metadata.into()),
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
