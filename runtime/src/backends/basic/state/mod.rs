pub(super) mod proj;

use std::{cell::RefCell, collections::HashMap, fmt::Debug, rc::Rc};

use crate::{
    abs::{FieldIndex, Local},
    utils::Hierarchical,
};

use super::{
    alias::SymValueRefProjector as SymbolicProjector, expr::prelude::*, FullPlace, Place,
    Projection, ValueRef, VariablesState,
};

use mutation::*;
use proj::*;

pub(super) type StateId = usize;

type RRef<T> = Rc<RefCell<T>>;

trait FullPlaceResolver {
    fn get_full_place(&self, place: &FullPlace) -> Result<ValueRef, PlaceError>;
}

trait PlaceResolver {
    fn get_place(&self, place: &Place) -> Result<ValueRef, PlaceError> {
        self.get_place_iter(place.local(), place.projections().iter())
    }

    fn get_place_iter<'b>(
        &self,
        local: &Local,
        projs: impl Iterator<Item = &'b Projection>,
    ) -> Result<ValueRef, PlaceError>;
}

enum PlaceError {
    LocalNotFound(Local),
    StateNotFound(StateId),
}

impl Debug for PlaceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PlaceError::LocalNotFound(local) => write!(
                f,
                "Local {} not found. It may be uninitialized, moved, or invalid.",
                local
            ),
            PlaceError::StateNotFound(state_id) => write!(f, "State {} not found.", state_id),
        }
    }
}

/// Provides a hierarchical stack based state (storage) for local variables.
/// It will perform all operations on the top most storage unless there are some
/// (mutable) references to the variables living in the ancestor states.
pub(super) struct HierarchicalVariablesState<SP: SymbolicProjector> {
    /// The top most (current) storage.
    top: LocalStorage<SP>,
    /// The recursive parent state.
    parent: Option<Box<Self>>,
    /// The projector that is used on demand to handle projections of symbolic values
    /// or symbolic projections (symbolic indices).
    sym_projector: RRef<SP>,
}

impl<SP: SymbolicProjector> HierarchicalVariablesState<SP> {
    pub(super) fn new(id: StateId, sym_projector: RRef<SP>) -> Self {
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

impl<SP: SymbolicProjector> Hierarchical<Self> for HierarchicalVariablesState<SP> {
    fn set_parent(&mut self, parent: Self) {
        self.parent = Some(Box::new(parent));
    }

    fn give_back_parent(&mut self) -> Option<Self> {
        self.parent.take().map(|b| *b)
    }
}

impl<SP: SymbolicProjector> VariablesState for HierarchicalVariablesState<SP> {
    fn id(&self) -> usize {
        self.top.id
    }

    fn copy_place(&self, place: &Place) -> ValueRef {
        self.get_place(place).unwrap()
    }

    fn take_place(&mut self, place: &Place) -> ValueRef {
        self.try_take_place(place).unwrap()
    }

    fn try_take_place(&mut self, place: &Place) -> Option<ValueRef> {
        let local = place.local();
        let mut value = self
            .take_local(local)
            .inspect_err(|e| {
                // Only local not found is acceptable.
                if !matches!(e, PlaceError::LocalNotFound(_)) {
                    panic!("Unexpected error: {:?}", e);
                }
            })
            .ok()?;

        if !place.has_projection() {
            return Some(value);
        }

        let (last, projs) = place.projections().split_last().unwrap();
        let last = match last {
            Projection::Field(field) => *field,
            _ => unreachable!(
                "Move is expected to happen only on locals or partially move on struct fields."
            ),
        };

        let field_val = self.take_field(
            apply_projs_mut(
                self,
                self.sym_projector.clone(),
                MutPlaceValue::Normal(&mut value),
                projs.iter(),
            ),
            last,
        );
        self.set_place(&Place::from(*local), value);
        Some(field_val)
    }

    fn set_place(&mut self, place: &Place, value: ValueRef) {
        if !place.has_projection() {
            self.top.set(place.local(), value);
        } else {
            self.mut_place_iter(
                place.local(),
                &place.projections(),
                MutateOnce::SetValue(value),
            )
            .unwrap()
        }
    }
}

impl<SP: SymbolicProjector> HierarchicalVariablesState<SP> {
    fn get_place_iter<'a, 'b>(
        &'a self,
        local: &Local,
        projs: impl Iterator<Item = &'b Projection>,
    ) -> Result<ValueRef, PlaceError> {
        let host = self.top.get_place_iter(local, std::iter::empty())?;
        Ok(apply_projs(self, self.sym_projector.clone(), &host, projs))
    }

    fn mut_place_iter(
        &mut self,
        local: &Local,
        projs: &[Projection],
        mutate: MutateOnce<SP>,
    ) -> Result<(), PlaceError> {
        if let Some((value, rest_projs)) = resolve_to_last_deref(self, local, projs)? {
            let host_place = ensure_mut_ref(&value);
            let state_id = *host_place.state_id();

            if state_id == self.top.id {
                // Flattening the inner place.
                let mut projs = host_place.as_ref().projections().to_vec();
                projs.extend_from_slice(rest_projs);
                self.mut_place_iter(host_place.as_ref().local(), &projs, mutate)
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
                .field(host.into(), field)
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

impl<SP: SymbolicProjector> HierarchicalVariablesState<SP> {
    fn get_full_place(&self, place: &FullPlace) -> Result<ValueRef, PlaceError> {
        let state_id = *place.state_id();
        if state_id == self.top.id {
            self.get_place_iter(place.as_ref().local(), place.as_ref().projections().iter())
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
                place.as_ref().local(),
                &place.as_ref().projections(),
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

impl<P: SymbolicProjector> FullPlaceResolver for HierarchicalVariablesState<P> {
    fn get_full_place(&self, place: &FullPlace) -> Result<ValueRef, PlaceError> {
        self.get_full_place(place)
    }
}

impl<P: SymbolicProjector> PlaceResolver for HierarchicalVariablesState<P> {
    fn get_place_iter<'b>(
        &self,
        local: &Local,
        projs: impl Iterator<Item = &'b Projection>,
    ) -> Result<ValueRef, PlaceError> {
        self.get_place_iter(local, projs)
    }
}

/// The storage of local variables for the current stack frame.
struct LocalStorage<SP: SymbolicProjector> {
    id: StateId,
    locals: HashMap<Local, ValueRef>,
    sym_projector: Rc<RefCell<SP>>,
}

impl<SP: SymbolicProjector> LocalStorage<SP> {
    fn get_place_iter<'a, 'b>(
        &'a self,
        local: &Local,
        projs: impl Iterator<Item = &'b Projection>,
    ) -> Result<ValueRef, PlaceError> {
        let host: &ValueRef = self
            .locals
            .get(local)
            .ok_or(PlaceError::LocalNotFound(*local))?;
        Ok(apply_projs(self, self.sym_projector.clone(), host, projs))
    }

    fn mut_place_iter(
        &mut self,
        local: &Local,
        projs: &[Projection],
        mutate: MutateOnce<SP>,
    ) -> Result<(), PlaceError> {
        let mut projs = projs.to_vec();

        if let Some((value, rest_projs)) = resolve_to_last_deref(self, local, &projs)? {
            let place = ensure_mut_ref(&value);
            self.assert_full_place(place);
            projs.splice(.., [place.as_ref().projections(), rest_projs].concat());
        }

        /* NOTE: As we are temporarily removing the local (to mitigate borrowing issues),
         * are we sure that the local is not used in the projections?
         * Yes. The recursive projection types are Deref and Index. The deref
         * cannot have reference to the same local, because it's against the
         * borrowing rules. The index also is on another local (not a place),
         * so the value will be in another local and not causing any issues.
         */
        let mut host = self.take_local(local)?;
        self.mut_host(MutPlaceValue::Normal(&mut host), projs.iter(), mutate);
        self.set(local, host);
        Ok(())
    }

    fn mut_host<'b, 'h>(
        &self,
        host: MutPlaceValue<'h>,
        projs: impl Iterator<Item = &'b Projection>,
        mutate: MutateOnce<'_, SP>,
    ) {
        let value = apply_projs_mut(self, self.sym_projector.clone(), host, projs);
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
    fn assert_full_place(&self, place: &FullPlace) {
        debug_assert_eq!(
            *place.state_id(),
            self.id,
            "An unresolved full place was passed. {place:?}"
        );
    }
}

impl<SP: SymbolicProjector> FullPlaceResolver for LocalStorage<SP> {
    fn get_full_place(&self, place: &FullPlace) -> Result<ValueRef, PlaceError> {
        self.assert_full_place(place);
        self.get_place_iter(place.as_ref().local(), place.as_ref().projections().iter())
    }
}

impl<SP: SymbolicProjector> PlaceResolver for LocalStorage<SP> {
    fn get_place_iter<'b>(
        &self,
        local: &Local,
        projs: impl Iterator<Item = &'b Projection>,
    ) -> Result<ValueRef, PlaceError> {
        self.get_place_iter(local, projs)
    }
}

mod mutation {
    use super::*;

    /* We need to have this structure to avoid infinite recursion of closure instantiation. */
    pub(super) enum MutateOnce<'a, SP: SymbolicProjector> {
        SetValue(ValueRef),
        Local(&'a LocalStorage<SP>, Vec<Projection>, Box<Self>),
    }

    impl<'a, 'h, SP: SymbolicProjector> FnOnce<(MutPlaceValue<'h>,)> for MutateOnce<'a, SP> {
        type Output = ();

        extern "rust-call" fn call_once(self, args: (MutPlaceValue<'h>,)) -> Self::Output {
            match self {
                Self::SetValue(value) => set_value(args.0, value),
                Self::Local(storage, projs, mutator) => {
                    storage.mut_host(args.0, projs.iter(), *mutator);
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
        place_resolver: &impl PlaceResolver,
        local: &Local,
        projs: &'b [Projection],
    ) -> Result<Option<(ValueRef, &'b [Projection])>, PlaceError> {
        /* NOTE: Shouldn't we use a while instead?
         * While makes sense where we have multiple mutable dereferences in the chain,
         * like &mut &mut ... &mut x.
         * Even in these cases, multiple deref projections will show up which we handle in the first
         * search.
         */
        if let Some(deref_index) = projs.iter().rposition(|p| matches!(p, Projection::Deref)) {
            /* Up to the last (mutable) deref, we only need the place.
             * Thus, we resolve the place immutably. */
            place_resolver
                .get_place_iter(local, projs[..deref_index].iter())
                .map(|v| Some((v, &projs[deref_index + 1..])))
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

    pub(super) enum MutPlaceValue<'a> {
        Normal(&'a mut ValueRef),
        SymProj(ProjExpr),
    }
}
