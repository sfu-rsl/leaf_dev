use std::{cell::RefCell, collections::HashMap, ops::DerefMut, rc::Rc};

use crate::{
    abs::{expr::proj::Projector, FieldIndex, Local},
    utils::Hierarchical,
};

use super::{
    alias::SymValueRefProjector as SymbolicProjector,
    expr::{
        AdtKind, AdtValue, ArrayValue, ConcreteValue, ConcreteValueMutRef, ConcreteValueRef,
        ConstValue, ProjExpr, RefValue, SymIndexPair, SymValueRef, Value,
    },
    place::{FullPlace, Place, Projection},
    ValueRef, VariablesState,
};

pub(super) type StateId = usize;

pub(super) struct MutableVariablesState<P: SymbolicProjector> {
    id: StateId,
    locals: HashMap<Local, ValueRef>,
    /// The projector that is used to handle projections of symbolic values or
    /// symbolic projections (symbolic indices).
    sym_projector: Rc<RefCell<P>>,
    parent: Option<Box<Self>>,
}

impl<P: SymbolicProjector> MutableVariablesState<P> {
    pub(super) fn new(sym_projector: Rc<RefCell<P>>, id: StateId) -> Self {
        Self {
            id,
            locals: HashMap::new(),
            sym_projector,
            parent: None,
        }
    }
}

impl<P: SymbolicProjector> Hierarchical<Self> for MutableVariablesState<P> {
    fn set_parent(&mut self, parent: Self) {
        self.parent = Some(Box::new(parent));
    }

    fn give_back_parent(&mut self) -> Option<Self> {
        self.parent.take().map(|b| *b)
    }
}

impl<P: SymbolicProjector> VariablesState for MutableVariablesState<P> {
    #[inline]
    fn id(&self) -> usize {
        self.id
    }

    fn copy_place(&self, place: &Place) -> ValueRef {
        self.get_place(place)
    }

    fn take_place(&mut self, place: &Place) -> ValueRef {
        self.try_take_place(place)
            .unwrap_or_else(|| panic!("{}", self.get_err_message(&place.local())))
    }

    fn try_take_place(&mut self, place: &Place) -> Option<ValueRef> {
        let local = place.local();
        let value = self.locals.remove(&local)?;

        let mut value = value;

        if !place.has_projection() {
            return Some(value);
        }

        let (last, projs) = place.projections.split_last().unwrap();
        let last = match last {
            Projection::Field(field) => *field,
            _ => panic!("Move can only happen on locals or partially move on struct fields."),
        };

        let field_val = self.take_field(
            self.apply_projs_mut(MutPlaceValue::Normal(&mut value), projs.iter()),
            last,
        );
        self.locals.insert(local, value);
        Some(field_val)
    }

    fn set_place(&mut self, place: &Place, value: ValueRef) {
        let local = place.local();
        if !place.has_projection() {
            self.locals.insert(local, value);
        } else {
            self.mut_place(place, |place_value| match place_value {
                MutPlaceValue::Normal(v) => *v = value,
                MutPlaceValue::SymProj(_) => {
                    todo!("Setting values to symbolic destinations are not supported yet.")
                }
            });
        }
    }
}

impl<P: SymbolicProjector> MutableVariablesState<P> {
    /// Takes an inner field from a struct value.
    /// If there are some projections left, it will recursively take the inner
    /// field while keeping the parent fields in place.
    ///
    /// # Returns
    /// The taken out inner field.
    ///
    /// # Panics
    ///
    /// Panics if the value is not a struct.
    fn take_field(&self, value: MutPlaceValue, field: FieldIndex) -> ValueRef {
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
                    self.sym_projector()
                        .field(SymValueRef::new(value.clone()).into(), field)
                        .into()
                        .to_value_ref()
                        .into()
                }
            }
            MutPlaceValue::SymProj(sym_value) => self
                .sym_projector()
                .field(sym_value.to_value_ref().into(), field)
                .into()
                .to_value_ref()
                .into(),
        }
    }

    fn get_place(&self, place: &Place) -> ValueRef {
        self.get_place_iter(place.local(), place.projections.iter())
    }

    fn get_place_iter<'a, 'b>(
        &'a self,
        local: Local,
        projs: impl Iterator<Item = &'b Projection>,
    ) -> ValueRef {
        let host: &ValueRef = self
            .locals
            .get(&local)
            .unwrap_or_else(|| panic!("{}", self.get_err_message(&local)));
        self.apply_projs(host, projs)
    }

    fn apply_projs<'a, 'b>(
        &'a self,
        host: &'a ValueRef,
        projs: impl Iterator<Item = &'b Projection>,
    ) -> ValueRef {
        projs.fold(host.clone(), |current, proj| {
            /* FIXME: Based on the current implementation, it is not possible to
             * get a concrete value after a symbolic projection or a projection
             * on a symbolic value. So you may want to optimize it.
             */
            if !current.is_symbolic() {
                self.apply_proj_con(ConcreteValueRef::new(current), proj)
            } else {
                self.apply_proj_sym(SymValueRef::new(current), proj)
                    .to_value_ref()
                    .into()
            }
        })
    }

    fn apply_proj_con(&self, host: ConcreteValueRef, proj: &Projection) -> ValueRef {
        let mut projector = ConcreteProjector {
            get_place: |p: &'_ FullPlace| -> ValueRef {
                self.get_mut_ref_state(*p.state_id()).get_place(p.as_ref())
            },
            sym_index_handler: |host: ConcreteValueRef, index: SymValueRef, c: bool| -> ValueRef {
                self.sym_projector()
                    .deref_mut()
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
            |p: &Place| -> ValueRef { self.get_place(p) },
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

    fn apply_proj_sym(&self, host: SymValueRef, proj: &Projection) -> ProjExpr {
        let mut projector = self.sym_projector();
        apply_proj(|p| self.get_place(p), host, proj, projector.deref_mut())
    }

    fn mut_place(&mut self, place: &Place, mutate: impl FnOnce(MutPlaceValue)) {
        self.mut_place_iter(place.local(), &place.projections, mutate);
    }

    fn mut_place_iter(
        &mut self,
        local: Local,
        projs: &[Projection],
        mutate: impl FnOnce(MutPlaceValue),
    ) {
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
            let value = self.get_place_iter(local, projs.iter().take(deref_index));
            let host_place = match value.as_ref() {
                Value::Concrete(ConcreteValue::Ref(RefValue::Mut(place))) => place.clone(),
                _ => panic!("The last deref is expected to be on a mutable reference."),
            };
            self.mut_host(
                *host_place.state_id(),
                host_place.as_ref().local(),
                host_place.as_ref().projections.iter(),
                projs[deref_index + 1..].iter(),
                mutate,
            )
        } else {
            self.mut_host(self.id(), local, projs.iter(), std::iter::empty(), mutate)
        }
    }

    /// Mutates a projection of a host value in some state.
    /// It resolves the place ([`host_local`], [`host_projs`]) in the host owner state, and then
    /// applies the [`rest_projs`] in the current state, and finally mutates the result by
    /// [`mutate`].
    /// This separation of parameters particularly has meaning when there is a dereferencing on
    /// a mutable reference, where we should resolve the [`Place`] inside the reference and then
    /// continue. In cases where there is no dereferencing, the owner will be self and either
    /// [`host_projs`] or [`rest_projs`] will be empty.
    ///
    /// # Parameters
    /// - `host_owner_id`: The id of the state in which the host value is located.
    /// - `host_local`: The local of the host value.
    /// - `host_projs`: The projections of the host value.
    /// - `rest_projs`: The projections of the value to mutate.
    /// - `mutate`: The function to mutate the value.
    fn mut_host<'b>(
        &mut self,
        host_owner_id: usize,
        host_local: Local,
        host_projs: impl Iterator<Item = &'b Projection>,
        rest_projs: impl Iterator<Item = &'b Projection>,
        mutate: impl FnOnce(MutPlaceValue<'_>),
    ) {
        /* NOTE: Can we make this cleaner?
         * Yes. Mostly, it requires separating the parent field from the rest of the structure
         * to avoid false negatives in the borrow checker. Then we may not need to manipulate
         * the locals of the owner.
         */
        let owner = self.get_mut_ref_state_mut(host_owner_id);
        /* NOTE: As we are temporarily removing the local (to mitigate borrowing issues),
         * are we sure that the local is not used in the projections?
         * Yes. The recursive projection types are Deref and Index. Deref
         * cannot have reference to the same local, because it's against the
         * borrowing rules. Index also is based on another local (no projection for that place),
         * so the value will be in another local and not causing any issues.
         */
        let mut host_local_value = owner
            .locals
            .remove(&host_local)
            .unwrap_or_else(|| panic!("{}", owner.get_err_message(&host_local)));
        let host = owner.apply_projs_mut(MutPlaceValue::Normal(&mut host_local_value), host_projs);
        let value = self.apply_projs_mut(host, rest_projs);
        mutate(value);
        self.get_mut_ref_state_mut(host_owner_id)
            .locals
            .insert(host_local, host_local_value);
    }

    fn apply_projs_mut<'a, 'b, 'h>(
        &'a self,
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
                        self.apply_proj_con_mut(ConcreteValueMutRef::new(current), proj)
                    } else {
                        MutPlaceValue::SymProj(
                            self.apply_proj_sym(SymValueRef::new(current.clone()), proj),
                        )
                    }
                }
                MutPlaceValue::SymProj(current) => {
                    MutPlaceValue::SymProj(self.apply_proj_sym(current.to_value_ref(), proj))
                }
            }
        })
    }

    fn apply_proj_con_mut<'a, 'h>(
        &'a self,
        host: ConcreteValueMutRef<'h>,
        proj: &Projection,
    ) -> MutPlaceValue<'h> {
        let mut projector = MutProjector {
            sym_index_handler: |host: ConcreteValueMutRef<'_>,
                                index: SymValueRef,
                                from_end: bool| {
                self.sym_projector()
                    .deref_mut()
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
            |p: &Place| self.get_place(p),
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

    #[inline]
    fn get_err_message(&self, local: &Local) -> String {
        format!(
            "Uninitialized, moved, or invalid local. {local} was not found among {} variables in {:#?}",
            self.locals.len(),
            self.locals
        )
    }
}

impl<P: SymbolicProjector> MutableVariablesState<P> {
    fn sym_projector(&self) -> impl DerefMut<Target = P> + '_ {
        self.sym_projector.as_ref().borrow_mut()
    }

    fn find_state(&self, id: StateId) -> Option<&Self> {
        if id == self.id {
            Some(self)
        } else {
            self.parent.as_ref().map(|p| p.find_state(id)).flatten()
        }
    }

    fn find_state_mut(&mut self, id: StateId) -> Option<&mut Self> {
        if id == self.id {
            Some(self)
        } else {
            self.parent.as_mut().map(|p| p.find_state_mut(id)).flatten()
        }
    }

    fn get_mut_ref_state(&self, id: StateId) -> &Self {
        self.find_state(id)
            .unwrap_or_else(|| panic!("Could not find mutable reference target state. {}", id))
    }

    fn get_mut_ref_state_mut(&mut self, id: StateId) -> &mut Self {
        self.find_state_mut(id)
            .unwrap_or_else(|| panic!("Could not find mutable reference target state. {}", id))
    }
}

struct ConcreteProjector<F, I> {
    get_place: F,
    sym_index_handler: I,
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
            Value::Symbolic(_) => Ok((self.sym_index_handler)(
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
    sym_index_handler: I,
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
            Value::Symbolic(_) => Ok(MutPlaceValue::SymProj((self.sym_index_handler)(
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

enum MutPlaceValue<'a> {
    Normal(&'a mut ValueRef),
    SymProj(ProjExpr),
}

fn apply_proj<'b, 'h, 'p, Host, IndexPair, P, Result>(
    get_place: impl Fn(&'b Place) -> ValueRef,
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
            let index = get_place(index);
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
