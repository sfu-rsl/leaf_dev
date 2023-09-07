use super::*;
use crate::{
    abs::{
        expr::proj::{macros::impl_general_proj_through_singulars, Projector},
        FieldIndex, VariantIndex,
    },
    backends::basic::expr::SymIndexPair,
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
    place_resolver: &impl FullPlaceResolver,
    sym_projector: RRef<SP>,
    host: &'a ValueRef,
    mut projs: impl Iterator<Item = ResolvedProjection>,
) -> ValueRef {
    {
        let mut current = host.clone();
        while let Some(proj) = projs.next() {
            current = apply_proj_con(
                place_resolver,
                sym_projector.clone(),
                ConcreteValueRef::new(current),
                &proj,
            );

            if current.is_symbolic() {
                break;
            }
        }

        current = apply_projs_sym(sym_projector.clone(), &SymValueRef::new(current), projs).0;

        current
    }
}

fn apply_proj_con<SP: SymbolicProjector>(
    place_resolver: &impl FullPlaceResolver,
    projector: RRef<SP>,
    host: ConcreteValueRef,
    proj: &ResolvedProjection,
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

    apply_proj::<_, _, _, Result<ValueRef, ConcreteValueRef>>(host, proj, &mut projector)
        .unwrap_result(proj)
}

pub(super) fn apply_projs_mut<'a, 'b, 'h, SP: SymbolicProjector>(
    sym_projector: RRef<SP>,
    host: MutPlaceValue<'h>,
    projs: impl Iterator<Item = ResolvedProjection>,
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
                        sym_projector.clone(),
                        ConcreteValueMutRef::new(current),
                        &proj,
                    )
                } else {
                    MutPlaceValue::SymProj(apply_proj_sym(
                        sym_projector.as_ref().borrow_mut().deref_mut(),
                        SymValueRef::new(current.clone()),
                        &proj,
                    ))
                }
            }
            MutPlaceValue::SymProj(current) => MutPlaceValue::SymProj(apply_proj_sym(
                sym_projector.as_ref().borrow_mut().deref_mut(),
                current.to_value_ref(),
                &proj,
            )),
        }
    })
}

fn apply_proj_con_mut<'h, SP: SymbolicProjector>(
    sym_projector: RRef<SP>,
    host: ConcreteValueMutRef<'h>,
    proj: &ResolvedProjection,
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
    apply_proj::<_, _, _, Result<MutPlaceValue, &mut ConcreteValue>>(host, proj, &mut projector)
        .unwrap_result(proj)
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

pub(super) fn apply_projs_sym<'a, 'b, SP: SymbolicProjector>(
    sym_projector: RRef<SP>,
    host: &'a SymValueRef,
    projs: impl Iterator<Item = ResolvedProjection>,
) -> SymValueRef {
    projs.fold(host.clone(), |current, proj| {
        apply_proj_sym(
            sym_projector.as_ref().borrow_mut().deref_mut(),
            current,
            &proj,
        )
        .to_value_ref()
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
    IndexPair: From<(Host, ValueRef)>,
    P::HIRefPair<'h>: From<IndexPair>,
    P::Proj<'h>: Into<Result>,
{
    use crate::abs::Projection::*;
    match proj {
        Field(field) => projector.field(host.into(), *field),
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
        Downcast(variant) => projector.downcast(host.into(), *variant),
        OpaqueCast => todo!(),
    }
    .into()
}

pub(super) trait LocalMap<L = Local> {
    fn get(&self, local: &L) -> Option<ValueRef>;
}

pub(super) trait ProjectionResolutionExt<L> {
    fn resolved_index(&self, local_resolver: &impl LocalMap<L>) -> ResolvedProjection;
}
impl<L> ProjectionResolutionExt<L> for crate::abs::Projection<L> {
    fn resolved_index(&self, local_resolver: &impl LocalMap<L>) -> ResolvedProjection {
        use crate::abs::Projection::*;
        match self {
            Field(field) => Field(*field),
            Deref => Deref,
            Index(index) => Index(local_resolver.get(index).unwrap()),
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
        }
    }
}
