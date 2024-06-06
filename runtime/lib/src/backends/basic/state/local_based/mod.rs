mod mutation;
mod proj;

use std::{cell::RefCell, collections::HashMap, fmt::Debug, ops::Deref, rc::Rc};

use delegate::delegate;

use crate::{
    abs::{FieldIndex, Local},
    utils::SelfHierarchical,
};

use super::{
    super::{
        alias::SymValueRefProjector as SymbolicProjector,
        expr::prelude::*,
        place::{LocalWithMetadata, PlaceWithMetadata},
        FullPlace, Projection, ValueRef, VariablesState,
    },
    proj::*,
    PlaceError, RRef, ResolvedProjection,
};

use mutation::*;
use proj::*;

pub(super) type StateId = usize;

/// Provides a hierarchical stack based state (storage) for local variables.
/// It will perform all operations on the top most storage unless there are some
/// (mutable) references to the variables living in the ancestor states.
pub(crate) struct StackedLocalIndexVariablesState<SP: SymbolicProjector> {
    /// The top most (current) storage.
    top: LocalStorage<SP>,
    /// The recursive parent state.
    parent: Option<Box<Self>>,
    /// The projector that is used on demand to handle projections of symbolic values
    /// or symbolic projections (symbolic indices).
    sym_projector: RRef<SP>,
}

/// The storage of local variables for the current stack frame.
struct LocalStorage<SP: SymbolicProjector> {
    id: StateId,
    locals: HashMap<Local, ValueRef>,
    sym_projector: Rc<RefCell<SP>>,
}

impl<SP: SymbolicProjector> StackedLocalIndexVariablesState<SP> {
    pub(crate) fn new(id: StateId, sym_projector: RRef<SP>) -> Self {
        Self {
            top: LocalStorage {
                id,
                locals: HashMap::new(),
                sym_projector: sym_projector.clone(),
            },
            parent: None,
            sym_projector,
        }
    }
}

pub(in super::super) trait PlaceRef {
    type Local;

    fn local(&self) -> &Self::Local;

    fn projections(&self) -> &[Projection<Self::Local>];

    fn has_projection(&self) -> bool {
        !self.projections().is_empty()
    }
}

impl<Place, SP: SymbolicProjector> VariablesState<Place> for StackedLocalIndexVariablesState<SP>
where
    Place: PlaceRef,
    Place::Local: AsRef<Local> + Debug,
{
    fn id(&self) -> usize {
        self.top.id
    }

    fn ref_place(&self, place: &Place) -> ValueRef {
        self.copy_place(place)
    }

    fn copy_place(&self, place: &Place) -> ValueRef {
        self.get_place_iter(
            place.local().as_ref(),
            self.resolve_projs(place.projections()),
        )
        .unwrap()
    }

    fn try_take_place(&mut self, place: &Place) -> Option<ValueRef> {
        self.try_take_place_iter(
            place.local().as_ref(),
            &self.resolve_projs(place.projections()).collect::<Vec<_>>(),
        )
    }

    fn set_place(&mut self, place: &Place, value: ValueRef) {
        if !place.has_projection() {
            self.top.set(place.local().as_ref(), value);
        } else {
            self.mut_place_iter(
                place.local().as_ref(),
                &self.resolve_projs(place.projections()).collect::<Vec<_>>(),
                MutateOnce::SetValue(value),
            )
            .unwrap()
        }
    }
}

impl<SP: SymbolicProjector> StackedLocalIndexVariablesState<SP> {
    fn resolve_projs<'b, PLocal>(
        &'b self,
        projs: &'b [Projection<PLocal>],
    ) -> impl Iterator<Item = ResolvedProjection> + 'b
    where
        PLocal: AsRef<Local>,
        PLocal: Debug,
    {
        projs.iter().map(|p| p.resolved_index(&self.top))
    }
}

impl<SP: SymbolicProjector> StackedLocalIndexVariablesState<SP> {
    fn get_place_iter<'a>(
        &'a self,
        local: &Local,
        projs: impl Iterator<Item = ResolvedProjection>,
    ) -> Result<ValueRef, PlaceError> {
        let host = self.top.get_place_iter(local, std::iter::empty())?;
        Ok(apply_projs(self, self.sym_projector.clone(), &host, projs))
    }

    fn try_take_place_iter(
        &mut self,
        local: &Local,
        projs: &[ResolvedProjection],
    ) -> Option<ValueRef> {
        let mut value = self
            .take_local(local)
            .inspect_err(|e| {
                // Only local not found is acceptable.
                if !matches!(e, PlaceError::LocalNotFound(_)) {
                    panic!("Unexpected error: {:?}", e);
                }
            })
            .ok()?;

        if projs.is_empty() {
            return Some(value);
        }

        let (last, projs) = projs.split_last().unwrap();
        let last = match last {
            Projection::Field(field) => *field,
            _ => unreachable!(
                "Move is expected to happen only on locals or partially on struct fields."
            ),
        };

        let field_val = self.take_field(
            apply_projs_mut(
                self.sym_projector.clone(),
                MutPlaceValue::Normal(&mut value),
                projs.into_iter().cloned(),
            ),
            last,
        );
        self.top.set(local, value);
        Some(field_val)
    }

    fn mut_place_iter(
        &mut self,
        local: &Local,
        projs: &[ResolvedProjection],
        mutate: MutateOnce<SP>,
    ) -> Result<(), PlaceError> {
        if let Some((value, rest_projs)) = resolve_to_last_deref(
            |local, projs| self.get_place_iter(local, projs.iter().cloned()),
            local,
            projs,
        )? {
            let host_place = ensure_mut_ref(&value);
            let state_id = *host_place.state_id();

            if state_id == self.top.id {
                // Flattening the inner place.
                let projs = host_place.as_ref().projections().to_vec();
                let mut projs = self.resolve_projs(&projs).collect::<Vec<_>>();
                projs.extend_from_slice(rest_projs);
                self.mut_place_iter(host_place.as_ref().local().as_ref(), &projs, mutate)
            } else {
                self.parent
                    .as_mut()
                    .map(|p| {
                        p.mut_full_place(
                            host_place,
                            MutateOnce::Local(&self.top, rest_projs.to_vec(), Box::new(mutate)),
                        )
                    })
                    .ok_or(PlaceError::StateNotFound(state_id))
                    .flatten()
            }
        } else {
            self.top.mut_place_iter(local, projs, mutate)
        }
    }

    fn take_local(&mut self, local: &Local) -> Result<ValueRef, PlaceError> {
        self.top.take_local(local)
    }

    /// Takes an inner field from a struct value.
    /// If there are some projections left, it will recursively take the inner
    /// field while keeping the parent fields in place.
    ///
    /// # Returns
    /// The taken out inner field.
    ///
    /// # Panics
    /// Panics if the value is not a struct.
    fn take_field(&self, value: MutPlaceValue, field: FieldIndex) -> ValueRef {
        let take_symbolic = |host: SymValueRef| -> ValueRef {
            self.sym_projector
                .as_ref()
                .borrow_mut()
                .field(host.into(), field.into())
                .into()
                .to_value_ref()
                .into()
        };

        match value {
            MutPlaceValue::Normal(value) => {
                if !value.is_symbolic() {
                    match ValueRef::make_mut(value) {
                        Value::Concrete(ConcreteValue::Adt(AdtValue {
                            kind: AdtKind::Struct,
                            ref mut fields,
                        })) => fields[field as usize]
                            .value
                            .take()
                            .unwrap_or_else(|| panic!("Field should not be moved before. {field}")),
                        _ => unreachable!("Field projection should be done only on ADTs."),
                    }
                } else {
                    take_symbolic(SymValueRef::new(value.clone()))
                }
            }
            MutPlaceValue::SymProj(sym_value) => take_symbolic(sym_value.to_value_ref()),
        }
    }
}

/// Full Place
impl<SP: SymbolicProjector> StackedLocalIndexVariablesState<SP> {
    fn get_full_place(&self, place: &FullPlace) -> Result<ValueRef, PlaceError> {
        let state_id = *place.state_id();
        if state_id == self.top.id {
            self.get_place_iter(
                place.as_ref().local().as_ref(),
                self.resolve_projs(place.as_ref().projections()),
            )
        } else {
            self.parent
                .as_ref()
                .map(|p| p.get_full_place(place))
                .ok_or(PlaceError::StateNotFound(state_id))
                .flatten()
        }
    }

    fn mut_full_place(
        &mut self,
        place: &FullPlace,
        mutate: MutateOnce<SP>,
    ) -> Result<(), PlaceError> {
        let state_id = *place.state_id();
        if state_id == self.top.id {
            self.mut_place_iter(
                place.as_ref().local().as_ref(),
                &self
                    .resolve_projs(place.as_ref().projections())
                    .collect::<Vec<_>>(),
                mutate,
            )
        } else {
            self.parent
                .as_mut()
                .map(|p| p.mut_full_place(place, mutate))
                .ok_or(PlaceError::StateNotFound(state_id))
                .flatten()
        }
    }
}

impl<SP: SymbolicProjector> LocalStorage<SP> {
    fn get_place_iter<'a, 'b>(
        &'a self,
        local: &Local,
        projs: impl Iterator<Item = ResolvedProjection>,
    ) -> Result<ValueRef, PlaceError> {
        let host: &ValueRef = self
            .locals
            .get(local)
            .ok_or(PlaceError::LocalNotFound(*local))?;
        Ok(apply_projs(self, self.sym_projector.clone(), host, projs))
    }

    fn mut_place_iter<'b>(
        &mut self,
        local: &Local,
        projs: &'b [ResolvedProjection],
        mutate: MutateOnce<SP>,
    ) -> Result<(), PlaceError> {
        let mut projs = projs.to_vec();

        if let Some((value, rest_projs)) = resolve_to_last_deref(
            |local, projs| self.get_place_iter(local, projs.iter().cloned()),
            local,
            &projs,
        )? {
            let place = ensure_mut_ref(&value);
            self.assert_full_place::<FullPlace>(place);
            let place_projs = place
                .as_ref()
                .projections()
                .iter()
                .map(|p| p.resolved_index(self));
            let rest_projs = rest_projs.into_iter().cloned();
            projs.splice(.., place_projs.chain(rest_projs).collect::<Vec<_>>());
        }

        /* NOTE: As we are temporarily removing the local (to mitigate borrowing issues),
         * are we sure that the local is not used in the projections?
         * Yes. The recursive projection types are Deref and Index. The deref
         * cannot have reference to the same local, because it's against the
         * borrowing rules. The index also is on another local (not a place),
         * so the value will be in another local and not causing any issues.
         */
        let mut host = self.take_local(local)?;
        self.mut_host(MutPlaceValue::Normal(&mut host), projs.into_iter(), mutate);
        self.set(local, host);
        Ok(())
    }

    fn mut_host<'b, 'h>(
        &self,
        host: MutPlaceValue<'h>,
        projs: impl Iterator<Item = ResolvedProjection>,
        mutate: MutateOnce<'_, SP>,
    ) {
        let value = apply_projs_mut(self.sym_projector.clone(), host, projs);
        mutate(value);
    }

    fn take_local(&mut self, local: &Local) -> Result<ValueRef, PlaceError> {
        self.locals
            .remove(local)
            .ok_or(PlaceError::LocalNotFound(*local))
    }

    fn set(&mut self, local: &Local, value: ValueRef) {
        self.locals.insert(*local, value);
    }

    #[inline(always)]
    fn assert_full_place<Place: Debug>(&self, place: &FullPlace) {
        debug_assert_eq!(
            *place.state_id(),
            self.id,
            "An unresolved full place was passed. {place:?}"
        );
    }
}

impl<SP: SymbolicProjector> MutRefResolver for StackedLocalIndexVariablesState<SP> {
    fn get_full_place(&self, place: &FullPlace) -> Result<ValueRef, PlaceError> {
        self.get_full_place(place)
    }
}

impl<SP: SymbolicProjector> MutRefResolver for LocalStorage<SP> {
    fn get_full_place(&self, place: &FullPlace) -> Result<ValueRef, PlaceError> {
        self.assert_full_place::<FullPlace>(place);
        self.get_place_iter(
            place.as_ref().local().as_ref(),
            place
                .as_ref()
                .projections()
                .iter()
                .map(|p| p.resolved_index(self)),
        )
    }
}

impl<L, SP: SymbolicProjector> IndexResolver<L> for StackedLocalIndexVariablesState<SP>
where
    L: AsRef<Local>,
{
    delegate! {
        to self.top {
            fn get(&self, local: &L) -> Option<ValueRef>;
        }
    }
}

impl<L, SP: SymbolicProjector> IndexResolver<L> for LocalStorage<SP>
where
    L: AsRef<Local>,
{
    fn get(&self, local: &L) -> Option<ValueRef> {
        self.locals.get(local.as_ref()).cloned()
    }
}

impl<SP: SymbolicProjector> SelfHierarchical for StackedLocalIndexVariablesState<SP> {
    fn add_layer(self) -> Self {
        let mut result = Self::new(self.top.id + 1, self.sym_projector.clone());
        result.parent = Some(Box::new(self));
        result
    }

    fn drop_layer(self) -> Option<Self> {
        self.parent.map(|parent| *parent)
    }
}

impl PlaceRef for crate::abs::Place {
    type Local = crate::abs::Local;

    fn local(&self) -> &Self::Local {
        self.local()
    }

    fn projections(&self) -> &[Projection<Self::Local>] {
        self.projections()
    }
}

impl PlaceRef for PlaceWithMetadata {
    type Local = LocalWithMetadata;

    fn local(&self) -> &Self::Local {
        &self.deref().local()
    }

    fn projections(&self) -> &[Projection<Self::Local>] {
        &self.deref().projections()
    }
}

impl AsRef<Local> for Local {
    fn as_ref(&self) -> &Local {
        self
    }
}
