use std::{cell::RefCell, collections::HashMap, fmt::Debug, rc::Rc};

use crate::{
    abs::{FieldIndex, Local},
    utils::Hierarchical,
};

use super::{
    alias::SymValueRefProjector as SymbolicProjector,
    expr::prelude::*,
    place::{FullPlace, Place, Projection},
    ValueRef, VariablesState,
};

use proj::*;

pub(super) type StateId = usize;

type RRef<T> = Rc<RefCell<T>>;

pub(super) struct HierarchicalVariablesState<SP: SymbolicProjector> {
    current: LocalStorage<SP>,
    parent: Option<Box<Self>>,
    sym_projector: RRef<SP>,
}

impl<SP: SymbolicProjector> HierarchicalVariablesState<SP> {
    pub(super) fn new(id: StateId, sym_projector: RRef<SP>) -> Self {
        Self {
            current: LocalStorage::new(id, sym_projector.clone()),
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
        self.current.id
    }

    fn copy_place(&self, place: &Place) -> ValueRef {
        self.get_place(place).unwrap()
    }

    fn take_place(&mut self, place: &Place) -> ValueRef {
        self.try_take_place(place).unwrap()
    }

    fn try_take_place(&mut self, place: &Place) -> Option<ValueRef> {
        let local = place.local;
        let mut value = self.take_local(local).ok()?;

        if !place.has_projection() {
            return Some(value);
        }

        let (last, projs) = place.projections.split_last().unwrap();
        let last = match last {
            Projection::Field(field) => *field,
            _ => unreachable!(
                "Move is expected to happen only on locals or partially move on struct fields."
            ),
        };

        let field_val = take_field(
            self.sym_projector.clone(),
            apply_projs_mut(
                self,
                self.sym_projector.clone(),
                MutPlaceValue::Normal(&mut value),
                projs.iter(),
            ),
            last,
        );
        self.set_place(&Place::new(local), value);
        Some(field_val)
    }

    fn set_place(&mut self, place: &Place, value: ValueRef) {
        if !place.has_projection() {
            self.current.set(place.local, value);
        } else {
            self.mut_place_iter(
                place.local,
                &place.projections,
                |place_value| match place_value {
                    MutPlaceValue::Normal(v) => *v = value,
                    MutPlaceValue::SymProj(_) => {
                        todo!("Setting values to symbolic destinations are not supported yet.")
                    }
                },
            )
            .unwrap()
        }
    }
}

impl<SP: SymbolicProjector> HierarchicalVariablesState<SP> {
    fn get_full_place(&self, place: &FullPlace) -> Result<ValueRef, PlaceError> {
        let state_id = *place.state_id();
        if state_id == self.current.id {
            self.get_place_iter(place.as_ref().local, place.as_ref().projections.iter())
        } else {
            self.parent
                .as_ref()
                .map(|p| p.get_full_place(place))
                .ok_or(PlaceError::StateNotFound(state_id))
                .flatten()
        }
    }

    fn mut_full_place<'b>(
        &mut self,
        place: &FullPlace,
        top_state: &LocalStorage<SP>,
        rest_projs: impl Iterator<Item = &'b Projection>,
        mutate: impl FnOnce(MutPlaceValue),
    ) -> Result<(), PlaceError> {
        let state_id = *place.state_id();
        if state_id == self.current.id {
            self.current
                .mut_place_iter(place.as_ref().local, &place.as_ref().projections, |host| {
                    top_state.mut_host(host, rest_projs, mutate);
                })
        } else {
            self.parent
                .as_mut()
                .map(|p| p.mut_full_place(place, top_state, rest_projs, mutate))
                .ok_or(PlaceError::StateNotFound(state_id))
                .flatten()
        }
    }

    fn take_full_local(&mut self, state_id: StateId, local: Local) -> Result<ValueRef, PlaceError> {
        if state_id == self.current.id {
            self.take_local(local)
        } else {
            self.parent
                .as_mut()
                .map(|p| p.take_full_local(state_id, local))
                .ok_or(PlaceError::StateNotFound(state_id))
                .flatten()
        }
    }
}

impl<SP: SymbolicProjector> HierarchicalVariablesState<SP> {
    fn get_place_iter<'a, 'b, I: Iterator<Item = &'b Projection>>(
        &'a self,
        local: Local,
        projs: I,
    ) -> Result<ValueRef, PlaceError> {
        let host = self.current.get_place_iter(local, std::iter::empty())?;
        Ok(apply_projs(self, self.sym_projector.clone(), &host, projs))
    }

    fn mut_place_iter(
        &mut self,
        local: Local,
        projs: &[Projection],
        mutate: impl FnOnce(MutPlaceValue),
    ) -> Result<(), PlaceError> {
        /* Up to the last (mutable) deref, we only need the place. Thus, no mutable borrow is
         * necessary.
         *
         * NOTE: Shouldn't we use a while instead?
         * While makes sense where we have multiple mutable dereferences in the chain,
         * like &mut &mut ... &mut x.
         * Even in these cases, multiple deref projections will show up which we handle in the first
         * search.
         */
        if let Some(deref_index) = projs.iter().rposition(|p| matches!(p, Projection::Deref)) {
            let value = self
                .get_place_iter(local, projs.iter().take(deref_index))
                .unwrap();
            let host_place = match value.as_ref() {
                Value::Concrete(ConcreteValue::Ref(RefValue::Mut(place))) => place.clone(),
                _ => panic!("The last deref is expected to be on a mutable reference."),
            };

            let state_id = *host_place.state_id();
            let rest_projs = &projs[deref_index + 1..];

            if state_id == self.current.id {
                let mut projs = host_place.as_ref().projections.clone();
                projs.extend_from_slice(rest_projs);
                self.current
                    .mut_place_iter(host_place.as_ref().local, &projs, mutate)
            } else {
                self.parent
                    .as_mut()
                    .map(|p| {
                        p.mut_full_place(&host_place, &self.current, rest_projs.iter(), mutate)
                    })
                    .ok_or(PlaceError::StateNotFound(state_id))
                    .flatten()
            }
        } else {
            self.current.mut_place_iter(local, projs, mutate)
        }
    }

    fn take_local(&mut self, local: Local) -> Result<ValueRef, PlaceError> {
        self.current.take_local(local)
    }
}

struct LocalStorage<SP: SymbolicProjector> {
    id: StateId,
    locals: HashMap<Local, ValueRef>,
    /// The projector that is used to handle projections of symbolic values or
    /// symbolic projections (symbolic indices).
    sym_projector: Rc<RefCell<SP>>,
}

impl<SP: SymbolicProjector> LocalStorage<SP> {
    pub(super) fn new(id: StateId, sym_projector: RRef<SP>) -> Self {
        Self {
            id,
            locals: HashMap::new(),
            sym_projector,
        }
    }
}

impl<SP: SymbolicProjector> LocalStorage<SP> {
    fn get_place_iter<'a, 'b>(
        &'a self,
        local: Local,
        projs: impl Iterator<Item = &'b Projection>,
    ) -> Result<ValueRef, PlaceError> {
        let host: &ValueRef = self
            .locals
            .get(&local)
            .ok_or(PlaceError::LocalNotFound(local))?;
        Ok(apply_projs(self, self.sym_projector.clone(), host, projs))
    }

    fn mut_place_iter(
        &mut self,
        local: Local,
        projs: &[Projection],
        mutate: impl FnOnce(MutPlaceValue),
    ) -> Result<(), PlaceError> {
        // FIXME: Is there a way to avoid vector allocation?
        let mut projs = projs.to_vec();

        /* Up to the last (mutable) deref, we only need the place. Thus, no mutable borrow is
         * necessary.
         *
         * NOTE: Shouldn't we use a while instead?
         * While makes sense where we have multiple mutable dereferences in the chain,
         * like &mut &mut ... &mut x.
         * Even in these cases, multiple deref projections will show up which we handle in the first
         * search.
         */
        if let Some(deref_index) = projs.iter().rposition(|p| matches!(p, Projection::Deref)) {
            let value = self.get_place_iter(local, projs.iter().take(deref_index))?;
            match value.as_ref() {
                Value::Concrete(ConcreteValue::Ref(RefValue::Mut(place))) => {
                    /* NOTE: We are taking this approach because recursively
                     * calling the function was leading to an infinite recursion
                     * in the compiler's borrow checker.
                     */
                    debug_assert_eq!(*place.state_id(), self.id);
                    projs.splice(..=deref_index, place.as_ref().projections.iter().cloned());
                }
                _ => panic!("The last deref is expected to be on a mutable reference."),
            }
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
        mutate: impl FnOnce(MutPlaceValue<'h>),
    ) {
        let value = apply_projs_mut(self, self.sym_projector.clone(), host, projs);
        mutate(value);
    }

    fn take_local(&mut self, local: Local) -> Result<ValueRef, PlaceError> {
        self.locals
            .remove(&local)
            .ok_or(PlaceError::LocalNotFound(local))
    }

    fn set(&mut self, local: Local, value: ValueRef) {
        self.locals.insert(local, value);
    }
}

impl<SP: SymbolicProjector> FullPlaceResolver for LocalStorage<SP> {
    fn get_full_place(&self, place: &FullPlace) -> Result<ValueRef, PlaceError> {
        debug_assert_eq!(*place.state_id(), self.id);
        self.get_place_iter(place.as_ref().local(), place.as_ref().projections.iter())
    }
}

impl<SP: SymbolicProjector> PlaceResolver for LocalStorage<SP> {
    fn get_place(&self, place: &Place) -> Result<ValueRef, PlaceError> {
        self.get_place_iter(place.local, place.projections.iter())
    }
}

trait PlaceManager {
    fn get_place(&self, place: &Place) -> Result<ValueRef, PlaceError> {
        self.get_place_iter(place.local, place.projections.iter())
    }

    fn get_place_iter<'a, 'b, I: Iterator<Item = &'b Projection>>(
        &'a self,
        local: Local,
        projs: I,
    ) -> Result<ValueRef, PlaceError>;

    fn mut_place(
        &mut self,
        place: &Place,
        mutate: impl FnOnce(MutPlaceValue),
    ) -> Result<(), PlaceError> {
        self.mut_place_iter(place.local, &place.projections, mutate)
    }

    fn mut_place_iter(
        &mut self,
        local: Local,
        projs: &[Projection],
        mutate: impl FnOnce(MutPlaceValue),
    ) -> Result<(), PlaceError>;

    fn take_local(&mut self, local: Local) -> Result<ValueRef, PlaceError>;
}

trait FullPlaceResolver {
    fn get_full_place(&self, place: &FullPlace) -> Result<ValueRef, PlaceError>;
}

trait PlaceResolver {
    fn get_place(&self, place: &Place) -> Result<ValueRef, PlaceError>;
}

impl<P: SymbolicProjector> FullPlaceResolver for HierarchicalVariablesState<P> {
    fn get_full_place(&self, place: &FullPlace) -> Result<ValueRef, PlaceError> {
        self.get_full_place(place)
    }
}

impl<P: SymbolicProjector> PlaceResolver for HierarchicalVariablesState<P> {
    fn get_place(&self, place: &Place) -> Result<ValueRef, PlaceError> {
        self.get_place_iter(place.local, place.projections.iter())
    }
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

/// Takes an inner field from a struct value.
/// If there are some projections left, it will recursively take the inner
/// field while keeping the parent fields in place.
///
/// # Returns
/// The taken out inner field.
///
/// # Panics
/// Panics if the value is not a struct.
fn take_field<SP: SymbolicProjector>(
    sym_projector: RRef<SP>,
    value: MutPlaceValue,
    field: FieldIndex,
) -> ValueRef {
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
                    _ => panic!("Only (concrete) structs can be partially moved."),
                }
            } else {
                sym_projector
                    .as_ref()
                    .borrow_mut()
                    .field(SymValueRef::new(value.clone()).into(), field)
                    .into()
                    .to_value_ref()
                    .into()
            }
        }
        MutPlaceValue::SymProj(sym_value) => sym_projector
            .as_ref()
            .borrow_mut()
            .field(sym_value.to_value_ref().into(), field)
            .into()
            .to_value_ref()
            .into(),
    }
}

enum MutPlaceValue<'a> {
    Normal(&'a mut ValueRef),
    SymProj(ProjExpr),
}

mod proj {
    use std::ops::DerefMut;

    use super::{FullPlaceResolver, MutPlaceValue, PlaceResolver, RRef, SymbolicProjector};
    use crate::{
        abs::{expr::proj::Projector, FieldIndex},
        backends::basic::{
            expr::{prelude::*, SymIndexPair},
            place::{FullPlace, Projection},
        },
    };

    struct ConcreteProjector<F, I> {
        get_place: F,
        handle_sym_index: I,
    }

    impl<F, I> Projector for ConcreteProjector<F, I>
    where
        F: Fn(&FullPlace) -> ValueRef,
        I: Fn(ConcreteValueRef, SymValueRef, bool) -> ValueRef,
    {
        type HostRef<'a> = ConcreteValueRef;
        type HIRefPair<'a> = (ConcreteValueRef, ValueRef);
        type Proj<'a> = Result<ValueRef, ConcreteValueRef>;

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

        fn index<'a>(
            &mut self,
            (host, index): Self::HIRefPair<'a>,
            from_end: bool,
        ) -> Self::Proj<'a> {
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
                    sym_projector.as_ref().borrow_mut(),
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
        .unwrap_or_else(|host| {
            panic!(
                "Projection {:?} is not possible on this value {:?}.",
                &proj, &host
            )
        })
    }

    fn apply_proj_sym<SP: SymbolicProjector>(
        place_resolver: &impl PlaceResolver,
        mut projector: impl DerefMut<Target = SP>,
        host: SymValueRef,
        proj: &Projection,
    ) -> ProjExpr {
        apply_proj(place_resolver, host, proj, projector.deref_mut())
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
                            sym_projector.as_ref().borrow_mut(),
                            SymValueRef::new(current.clone()),
                            proj,
                        ))
                    }
                }
                MutPlaceValue::SymProj(current) => MutPlaceValue::SymProj(apply_proj_sym(
                    place_resolver,
                    sym_projector.as_ref().borrow_mut(),
                    current.to_value_ref(),
                    proj,
                )),
            }
        })
    }

    fn apply_proj_con_mut<'a, 'h, SP: SymbolicProjector>(
        place_resolver: &impl PlaceResolver,
        sym_projector: RRef<SP>,
        host: ConcreteValueMutRef<'h>,
        proj: &Projection,
    ) -> MutPlaceValue<'h> {
        let mut projector = MutProjector {
            handle_sym_index: |host: ConcreteValueMutRef<'_>,
                               index: SymValueRef,
                               from_end: bool| {
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
        .unwrap_or_else(|host_conc| {
            panic!(
                "Projection {:?} is not possible on this value {:?}.",
                &proj, &host_conc
            )
        })
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
                assert!(
                    !index.has_projection(),
                    "Index should be a simple local with no projection."
                );
                let index = index_resolver.get_place(index).unwrap();
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
            Projection::Downcast(_) => todo!("#156"),
            Projection::OpaqueCast => todo!(),
        }
        .into()
    }
}
