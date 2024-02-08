/// This module holds the codes that are mostly related to mutable references
/// and projections on them.
use crate::abs::{
    expr::proj::{macros::impl_general_proj_through_singulars, Projector},
    VariantIndex,
};

use super::*;

pub(super) enum MutPlaceValue<'a> {
    Normal(&'a mut ValueRef),
    SymProj(ProjExpr),
}

pub(super) struct MutProjector<I> {
    handle_sym_index: I,
}

impl<I> MutProjector<I> {
    pub(crate) fn new(handle_sym_index: I) -> Self {
        Self { handle_sym_index }
    }
}

impl<I> Projector for MutProjector<I>
where
    I: Fn(ConcreteValueMutRef, SymValueRef, bool) -> ProjExpr,
{
    type HostRef<'a> = ConcreteValueMutRef<'a>;
    type HIRefPair<'a> = (ConcreteValueMutRef<'a>, ValueRef);
    type DowncastTarget = VariantIndex;
    type Proj<'a> = Result<MutPlaceValue<'a>, &'a mut ConcreteValue>;

    impl_general_proj_through_singulars!();

    fn field<'a>(&mut self, host: Self::HostRef<'a>, field: FieldIndex) -> Self::Proj<'a> {
        match Self::make_mut(host) {
            ConcreteValue::Adt(AdtValue { fields, .. }) => Ok(MutPlaceValue::Normal(
                fields[field as usize]
                    .value
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

/* We need to have this structure to avoid infinite recursion of closure instantiation. */
pub(super) enum MutateOnce<'a, SP: SymbolicProjector> {
    SetValue(ValueRef),
    Local(&'a LocalStorage<SP>, Vec<ResolvedProjection>, Box<Self>),
}

impl<'a, 'h, SP: SymbolicProjector> FnOnce<(MutPlaceValue<'h>,)> for MutateOnce<'a, SP> {
    type Output = ();

    extern "rust-call" fn call_once(self, args: (MutPlaceValue<'h>,)) -> Self::Output {
        match self {
            Self::SetValue(value) => set_value(args.0, value),
            Self::Local(storage, projs, mutator) => {
                storage.mut_host(args.0, projs.into_iter(), *mutator);
            }
        }
    }
}

pub(super) fn set_value(destination: MutPlaceValue, value: ValueRef) {
    match destination {
        MutPlaceValue::Normal(v) => *v = value,
        MutPlaceValue::SymProj(_) => {
            todo!("Setting values to symbolic destinations are not supported yet.")
        }
    }
}

/// Looks for the outmost deref in the projection chain, if there is any resolves the projections
/// up to that point, and returns the deref target as well as the remaining projections.
pub(super) fn resolve_to_last_deref<'b>(
    place_resolver: impl FnOnce(&Local, &[ResolvedProjection]) -> Result<ValueRef, PlaceError>,
    local: &Local,
    projs: &'b [ResolvedProjection],
) -> Result<Option<(ValueRef, &'b [ResolvedProjection])>, PlaceError> {
    /* NOTE: Shouldn't we use a while instead?
     * While makes sense where we have multiple mutable dereferences in the chain,
     * like &mut &mut ... &mut x.
     * Even in these cases, multiple deref projections will show up which we handle in the first
     * search.
     */
    if let Some(deref_index) = projs.iter().rposition(|p| matches!(p, Projection::Deref)) {
        /* Up to the last (mutable) deref, we only need the place.
         * Thus, we resolve the place immutably. */
        place_resolver(local, &projs[..deref_index]).map(|v| Some((v, &projs[deref_index + 1..])))
    } else {
        Ok(None)
    }
}

pub(super) fn ensure_mut_ref(value: &Value) -> &FullPlace {
    match value {
        Value::Concrete(ConcreteValue::Ref(RefValue::Mut(place))) => place,
        _ => panic!("Value should be a mutable reference. {value:?}"),
    }
}
