use std::{cell::RefCell, collections::HashMap, ops::DerefMut, rc::Rc};

use crate::abs::{expr::proj::Projector, FieldIndex, Local};

use super::{
    alias::SymValueRefProjector as SymbolicProjector,
    expr::{
        AdtKind, AdtValue, ArrayValue, ConcreteValue, ConcreteValueMutRef, ConcreteValueRef,
        ConstValue, ProjExpr, RefValue, SymIndexPair, SymValueRef, Value,
    },
    place::{Place, Projection},
    ValueRef, VariablesState,
};

pub(super) struct MutableVariablesState<P: SymbolicProjector> {
    locals: HashMap<Local, ValueRef>,
    /// The projector that is used to handle projections of symbolic values or
    /// symbolic projections (symbolic indices).
    sym_projector: Rc<RefCell<P>>,
}

impl<P: SymbolicProjector> MutableVariablesState<P> {
    pub(super) fn new(sym_projector: Rc<RefCell<P>>) -> Self {
        Self {
            locals: HashMap::new(),
            sym_projector,
        }
    }
}

impl<P: SymbolicProjector> VariablesState for MutableVariablesState<P> {
    fn copy_place(&self, place: &Place) -> ValueRef {
        self.get_place(place)
    }

    fn take_place(&mut self, place: &Place) -> ValueRef {
        self.try_take_place(place)
            .unwrap_or_else(|| panic!("{}", Self::get_err_message(&place.local())))
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

        let field_val = self.take_field(self.apply_projs_mut(&mut value, projs.iter()), last);
        self.locals.insert(local, value);
        Some(field_val)
    }

    fn set_place(&mut self, place: &Place, value: ValueRef) {
        let local = place.local();
        if !place.has_projection() {
            self.locals.insert(local, value);
        } else {
            self.mut_place(place, |_, place_value| match place_value {
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
    ) -> ValueRef
    where
        'a: 'b,
    {
        let host: &ValueRef = self
            .locals
            .get(&local)
            .unwrap_or_else(|| panic!("{}", Self::get_err_message(&local)));
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
            get_place: |p: &'_ Place| -> ValueRef { self.get_place(p) },
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

    fn mut_place(&mut self, place: &Place, mutate: impl FnOnce(&Self, MutPlaceValue)) {
        self.mut_place_iter(place.local(), &place.projections, mutate);
    }

    fn mut_place_iter(
        &mut self,
        local: Local,
        projs: &[Projection],
        mutate: impl FnOnce(&Self, MutPlaceValue),
    ) {
        // FIXME: Is there a way to avoid vector allocation?
        let mut projs = projs.to_vec();

        // Up to the last deref, we only need the place. So no mutable borrow is needed.
        while let Some(deref_index) = projs.iter().rposition(|p| matches!(p, Projection::Deref)) {
            let value = self.get_place_iter(local, projs.iter().take(deref_index));
            match value.as_ref() {
                Value::Concrete(ConcreteValue::Ref(RefValue::Mut(place))) => {
                    /* NOTE: We are taking this approach because recursively
                     * calling the function was leading to an infinite recursion
                     * in the compiler's borrow checker.
                     */
                    projs.splice(..=deref_index, place.projections.iter().cloned());
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
        let mut host = self
            .locals
            .remove(&local)
            .unwrap_or_else(|| panic!("{}", Self::get_err_message(&local)));
        let value = self.apply_projs_mut(&mut host, projs.iter());
        mutate(self, value);
        self.locals.insert(local, host);
    }

    fn apply_projs_mut<'a, 'b>(
        &'a self,
        host: &'a mut ValueRef,
        projs: impl Iterator<Item = &'b Projection>,
    ) -> MutPlaceValue<'_> {
        projs.fold(MutPlaceValue::Normal(host), |current, proj| {
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

    fn apply_proj_con_mut<'a>(
        &'a self,
        host: ConcreteValueMutRef<'a>,
        proj: &Projection,
    ) -> MutPlaceValue {
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
    fn get_err_message(local: &Local) -> String {
        format!("Uninitialized, moved, or invalid local. {local}")
    }
}

impl<P: SymbolicProjector> MutableVariablesState<P> {
    fn sym_projector(&self) -> impl DerefMut<Target = P> + '_ {
        self.sym_projector.as_ref().borrow_mut()
    }
}

struct ConcreteProjector<F, I> {
    get_place: F,
    sym_index_handler: I,
}

impl<F, I> Projector for ConcreteProjector<F, I>
where
    F: Fn(&Place) -> ValueRef,
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
                        let index = *bit_rep as usize;
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
                        let index = *bit_rep as usize;
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
        let original_addr = value_ref.as_ref() as *const ConcreteValue;
        let host_value = value_ref.make_mut();
        /* NOTE: Alive copies on projectable types does not seem to be possible.
         * For compound types, it is not observed in the MIR. Only immutable ref
         * can be copied, which is not possible to be given to this function.
         * Therefore, we expect that make_mut is not going to clone when there
         * are some projections.
         */
        debug_assert_eq!(original_addr, host_value);
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
        Projection::Downcast(_) => todo!(),
        Projection::OpaqueCast => todo!(),
    }
    .into()
}
