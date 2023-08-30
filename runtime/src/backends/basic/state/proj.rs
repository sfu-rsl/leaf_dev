use super::*;
use crate::{
    abs::{
        expr::proj::{macros::impl_general_proj_through_singulars, Projector},
        FieldIndex, VariantIndex,
    },
    backends::basic::{expr::SymIndexPair, Projection},
};
use std::ops::DerefMut;

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
    type HIRefPair<'a> = (ConcreteValueRef, ValueRef);
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

struct MutProjector<I> {
    handle_sym_index: I,
}

impl<I> Projector for MutProjector<I>
where
    I: Fn(ConcreteValueMutRef, SymValueRef, bool) -> ProjExpr,
{
    type HostRef<'a> = ConcreteValueMutRef<'a>;
    type HIRefPair<'a> = (ConcreteValueMutRef<'a>, ValueRef);
    type Proj<'a> = Result<MutPlaceValue<'a>, &'a mut ConcreteValue>;

    impl_general_proj_through_singulars!();

    fn field<'a>(&mut self, host: Self::HostRef<'a>, field: FieldIndex) -> Self::Proj<'a> {
        match Self::make_mut(host) {
            ConcreteValue::Adt(AdtValue { fields, .. }) => Ok(MutPlaceValue::Normal(
                fields[field as usize]
                    .as_mut()
                    .unwrap_or_else(|| panic!("Field should not be moved before. {field}")),
            )),
            conc => Err(conc),
        }
    }

    fn deref<'a>(&mut self, _host: Self::HostRef<'a>) -> Self::Proj<'a> {
        unreachable!("Deref should be handled before.")
    }

    fn index<'a>(&mut self, (host, index): Self::HIRefPair<'a>, from_end: bool) -> Self::Proj<'a> {
        match index.as_ref() {
            Value::Concrete(ConcreteValue::Const(ConstValue::Int { bit_rep, .. })) => {
                match Self::make_mut(host) {
                    ConcreteValue::Array(ArrayValue { elements }) => {
                        let index = bit_rep.0 as usize;
                        Ok(MutPlaceValue::Normal(if !from_end {
                            &mut elements[index]
                        } else {
                            let len = elements.len();
                            &mut elements[len - index]
                        }))
                    }
                    conc => Err(conc),
                }
            }
            Value::Symbolic(_) => Ok(MutPlaceValue::SymProj((self.handle_sym_index)(
                host,
                SymValueRef::new(index),
                from_end,
            ))),
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
                kind: AdtKind::Enum { variant },
                ..
            }) => {
                assert_eq!(
                    *variant, to_variant,
                    "Variant must be the same for concrete values."
                );
                Ok(MutPlaceValue::Normal(host.0))
            }
            _ => Err(Self::make_mut(host)),
        }
    }
}

impl<I> MutProjector<I> {
    fn make_mut(value_ref: ConcreteValueMutRef) -> &mut ConcreteValue {
        let _original_addr = value_ref.as_ref() as *const ConcreteValue;
        let host_value = value_ref.make_mut();
        /* NOTE:
         * The following statement does not hold anymore. Counterexamples:
         * - Symbolic projections: If we have previously copied an element of an array,
         * using a symbolic index, then the array is expected to have some alive copies.
         *- In `clone`, it is observed that the array is copied using a copy operand.
         * -----------
         * Alive copies on projectable types does not seem to be possible.
         * For compound types, it is not observed in the MIR. Only immutable ref
         * can be copied, which is not possible to be passed to this function (it's meant for mutations).
         * Therefore, we expect that make_mut is not going to clone when there
         * are some projections.
         * debug_assert_eq!(original_addr, host_value as *const ConcreteValue);
         */
        host_value
    }
}

pub(super) fn apply_projs<'a, 'b, SP: SymbolicProjector>(
    place_resolver: &(impl PlaceResolver + FullPlaceResolver),
    sym_projector: RRef<SP>,
    host: &'a ValueRef,
    projs: impl Iterator<Item = &'b Projection>,
) -> ValueRef {
    projs.fold(host.clone(), |current, proj| {
        /* FIXME: Based on the current implementation, it is not possible to
         * get a concrete value after a symbolic projection or a projection
         * on a symbolic value. So you may want to optimize it.
         */
        if !current.is_symbolic() {
            apply_proj_con(
                place_resolver,
                sym_projector.clone(),
                ConcreteValueRef::new(current),
                proj,
            )
        } else {
            apply_proj_sym(
                place_resolver,
                sym_projector.as_ref().borrow_mut().deref_mut(),
                SymValueRef::new(current),
                proj,
            )
            .to_value_ref()
            .into()
        }
    })
}

fn apply_proj_con<SP: SymbolicProjector>(
    place_resolver: &(impl PlaceResolver + FullPlaceResolver),
    projector: RRef<SP>,
    host: ConcreteValueRef,
    proj: &Projection,
) -> ValueRef {
    let mut projector = ConcreteProjector {
        get_place: |p: &'_ FullPlace| -> ValueRef { place_resolver.get_full_place(p).unwrap() },
        handle_sym_index: |host: ConcreteValueRef, index: SymValueRef, c: bool| -> ValueRef {
            projector
                .as_ref()
                .borrow_mut()
                .index(
                    SymIndexPair::SymIndex {
                        index,
                        host: host.into(),
                    }
                    .into(),
                    c,
                )
                .into()
                .to_value_ref()
                .into()
        },
    };

    apply_proj::<_, _, _, Result<ValueRef, ConcreteValueRef>>(
        place_resolver,
        host,
        proj,
        &mut projector,
    )
    .unwrap_result(proj)
}

#[inline]
fn apply_proj_sym(
    place_resolver: &impl PlaceResolver,
    projector: &mut impl SymbolicProjector,
    host: SymValueRef,
    proj: &Projection,
) -> ProjExpr {
    apply_proj(place_resolver, host, proj, projector)
}

pub(super) fn apply_projs_mut<'a, 'b, 'h, SP: SymbolicProjector>(
    place_resolver: &impl PlaceResolver,
    sym_projector: RRef<SP>,
    host: MutPlaceValue<'h>,
    projs: impl Iterator<Item = &'b Projection>,
) -> MutPlaceValue<'h> {
    projs.fold(host, |current, proj| {
        /* FIXME: Based on the current implementation, it is not possible to
         * get a concrete value after a symbolic projection or a projection
         * on a symbolic value. So you may want to optimize it.
         */
        match current {
            MutPlaceValue::Normal(current) => {
                if !current.is_symbolic() {
                    apply_proj_con_mut(
                        place_resolver,
                        sym_projector.clone(),
                        ConcreteValueMutRef::new(current),
                        proj,
                    )
                } else {
                    MutPlaceValue::SymProj(apply_proj_sym(
                        place_resolver,
                        sym_projector.as_ref().borrow_mut().deref_mut(),
                        SymValueRef::new(current.clone()),
                        proj,
                    ))
                }
            }
            MutPlaceValue::SymProj(current) => MutPlaceValue::SymProj(apply_proj_sym(
                place_resolver,
                sym_projector.as_ref().borrow_mut().deref_mut(),
                current.to_value_ref(),
                proj,
            )),
        }
    })
}

fn apply_proj_con_mut<'h, SP: SymbolicProjector>(
    place_resolver: &impl PlaceResolver,
    sym_projector: RRef<SP>,
    host: ConcreteValueMutRef<'h>,
    proj: &Projection,
) -> MutPlaceValue<'h> {
    let mut projector = MutProjector {
        handle_sym_index: |host: ConcreteValueMutRef<'_>, index: SymValueRef, from_end: bool| {
            sym_projector
                .as_ref()
                .borrow_mut()
                .index(
                    SymIndexPair::SymIndex {
                        index,
                        host: host.0.clone(),
                    }
                    .into(),
                    from_end,
                )
                .into()
        },
    };
    apply_proj::<_, _, _, Result<MutPlaceValue, &mut ConcreteValue>>(
        place_resolver,
        host,
        proj,
        &mut projector,
    )
    .unwrap_result(proj)
}

fn apply_proj<'b, 'h, 'p, Host, IndexPair, P, Result>(
    index_resolver: &impl PlaceResolver,
    host: Host,
    proj: &'b Projection,
    projector: &'p mut P,
) -> Result
where
    P: Projector,
    P::HostRef<'h>: From<Host>,
    IndexPair: From<(Host, ValueRef)>,
    P::HIRefPair<'h>: From<IndexPair>,
    P::Proj<'h>: Into<Result>,
{
    match proj {
        Projection::Field(field) => projector.field(host.into(), *field),
        Projection::Deref => projector.deref(host.into()),
        Projection::Index(index) => {
            let index = index_resolver.get_place(&Place::from(*index)).unwrap();
            projector.index(Into::<IndexPair>::into((host, index)).into(), false)
        }
        Projection::ConstantIndex {
            offset, from_end, ..
        } => {
            let index = ConstValue::from(*offset).to_value_ref();
            projector.index(Into::<IndexPair>::into((host, index)).into(), *from_end)
        }
        Projection::Subslice { from, to, from_end } => {
            projector.subslice(host.into(), *from, *to, *from_end)
        }
        Projection::Downcast(variant) => projector.downcast(host.into(), *variant),
        Projection::OpaqueCast => todo!(),
    }
    .into()
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
