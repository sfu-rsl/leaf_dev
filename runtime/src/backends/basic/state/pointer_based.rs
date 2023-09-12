use std::{
    cell::RefCell,
    collections::{
        btree_map::{Cursor, CursorMut, Entry},
        BTreeMap, HashMap,
    },
    ops::{Bound, RangeBounds},
    rc::Rc,
};

use delegate::delegate;

use crate::{
    abs::{self, PointerOffset, RawPointer, TypeSize, ValueType, USIZE_TYPE},
    backends::basic::{
        expr::{RawConcreteValue, SymOwnerValue},
        place::LocalWithAddress,
        VariablesState,
    },
    utils::SelfHierarchical,
};

use super::{
    super::{
        alias::SymValueRefProjector as SymbolicProjector, expr::prelude::*,
        place::PlaceWithAddress, ValueRef,
    },
    proj::{apply_projs_sym, IndexResolver, ProjectionResolutionExt},
};

type Local = LocalWithAddress;
type Place = PlaceWithAddress;
type Projection = crate::abs::Projection<Local>;

type RRef<T> = Rc<RefCell<T>>;

type Memory = BTreeMap<RawPointer, SymValueRef>;

/// Provides a mapping for raw pointers to symbolic values.
/// All places that have a valid address are handled by this state, otherwise
/// they will be sent to the `fallback` state to be handled.
pub(in super::super) struct RawPointerVariableState<VS, SP: SymbolicProjector> {
    memory: Memory,
    fallback: VS,
    sym_projector: RRef<SP>,
    return_value_addr: Option<RawPointer>,
}

impl<VS, SP: SymbolicProjector> RawPointerVariableState<VS, SP> {
    pub fn new(fallback: VS, sym_projector: RRef<SP>) -> Self
    where
        VS: VariablesState<Place>,
    {
        Self {
            memory: Default::default(),
            fallback,
            sym_projector,
            return_value_addr: None,
        }
    }

    #[inline]
    fn get<'a, 'b>(&'a self, addr: &'b RawPointer) -> Option<&'a SymValueRef> {
        self.memory.get(addr)
    }
}

impl<VS: VariablesState<Place>, SP: SymbolicProjector> VariablesState<Place>
    for RawPointerVariableState<VS, SP>
where
    Self: IndexResolver<Local>,
{
    delegate! {
        to self.fallback {
            fn id(&self) -> usize;
        }
    }

    fn copy_place(&self, place: &Place) -> ValueRef {
        let Some(addr) = place.address() else {
            return self.fallback.copy_place(place);
        };

        if let Some((sym_val, sym_projs)) = self.first_symbolic_value(place) {
            return self.handle_sym_value(sym_val, sym_projs).into();
        }

        if let Some(size) = place.size() {
            if let Some(sym_owner) = Self::try_create_sym_owner(
                addr,
                size,
                |start| self.memory.lower_bound(start),
                |c| c.key_value(),
                |c| c.move_next(),
            ) {
                return Into::<UnevalValue>::into(sym_owner).to_value_ref();
            }
        }

        Self::create_lazy(addr, place.ty()).to_value_ref()
    }

    fn try_take_place(&mut self, place: &Place) -> Option<ValueRef> {
        let addr = place.address().or(
            if matches!(place.local().as_ref(), abs::Local::ReturnValue) {
                self.return_value_addr.take()
            } else {
                None
            },
        );
        let Some(addr) = addr else {
            return self.fallback.try_take_place(place);
        };

        if let Some((sym_val, sym_projs)) =
            self.first_symbolic_value_iter(addr, place.projections(), place.proj_addresses())
        {
            return Some(if sym_projs.is_empty() {
                let value = sym_val.clone_to();
                self.memory.remove(&addr);
                value
            } else {
                self.handle_sym_value(sym_val, sym_projs).into()
            });
        }

        if let Some(size) = place.size() {
            if let Some(sym_owner) = Self::try_create_sym_owner(
                addr,
                size,
                |start| self.memory.lower_bound_mut(start),
                |c| c.key_value(),
                |c| {
                    c.remove_current();
                },
            ) {
                return Some(sym_owner.to_value_ref());
            }
        }

        Some(Self::create_lazy(addr, place.ty()).to_value_ref())
    }

    fn set_place(&mut self, place: &Place, value: ValueRef) {
        let Some(addr) = place.address() else {
            return self.fallback.set_place(place, value);
        };

        if matches!(place.local().as_ref(), abs::Local::ReturnValue) {
            self.return_value_addr = Some(addr);
        }

        if let Some((_sym_val, sym_projs)) = self.first_symbolic_value(place) {
            if !sym_projs.is_empty() {
                todo!("#238");
            }
        }

        self.set_addr(addr, value);

        log::debug!("Current memory state: {:?}", self.memory);
    }
}

impl<VS: VariablesState<Place>, SP: SymbolicProjector> RawPointerVariableState<VS, SP> {
    /// Finds the first symbolic value in the chain of projections leading to the place.
    /// # Returns
    /// The first symbolic value and the remaining projections to be applied on it.
    fn first_symbolic_value<'a, 'b>(
        &'a self,
        place: &'b Place,
    ) -> Option<(&'a SymValueRef, &'b [Projection])> {
        self.first_symbolic_value_iter(
            place.local().address()?,
            place.projections(),
            place.proj_addresses(),
        )
    }

    fn first_symbolic_value_iter<'a, 'b>(
        &'a self,
        local_address: RawPointer,
        projs: &'b [Projection],
        proj_addresses: impl Iterator<Item = Option<RawPointer>>,
    ) -> Option<(&'a SymValueRef, &'b [Projection])> {
        if let Some(sym_val) = self.get(&local_address) {
            Some((sym_val, projs))
        } else {
            // Checking for the value after each projection.
            proj_addresses
                .enumerate()
                .find_map(|(i, addr)| {
                    addr.and_then(|addr| self.get(&addr))
                        .map(|sym_val| (i, sym_val))
                })
                .map(|(i, sym_val)| (sym_val, &projs[i..projs.len()]))
        }
    }

    fn handle_sym_value<'a, 'b>(
        &self,
        host: &'a SymValueRef,
        projs: &'b [Projection],
    ) -> SymValueRef
    where
        Self: IndexResolver<Local>,
    {
        log::debug!("Handling symbolic value: {:?}", host);
        apply_projs_sym(
            self.sym_projector.clone(),
            host,
            projs.iter().map(|p| p.resolved_index(self)),
        )
    }

    fn try_create_sym_owner<'a, C: 'a>(
        addr: RawPointer,
        size: TypeSize,
        lower_bound: impl FnOnce(Bound<&RawPointer>) -> C,
        key_value: impl Fn(&C) -> Option<(&RawPointer, &SymValueRef)>,
        move_next: impl Fn(&mut C),
    ) -> Option<SymOwnerValue> {
        let range = addr..addr + size;
        let mut cursor = lower_bound(range.start_bound());
        let mut sym_values = Vec::new();
        while let Some((sym_addr, sym_value)) = key_value(&cursor) {
            if !range.contains(sym_addr) {
                break;
            }

            let offset: PointerOffset = sym_addr - addr;
            sym_values.push((offset, sym_value.clone()));
            move_next(&mut cursor);
        }

        if !sym_values.is_empty() {
            Some(SymOwnerValue {
                base: addr,
                sym_values,
            })
        } else {
            None
        }
    }

    #[inline]
    fn create_lazy(addr: RawPointer, ty: Option<&ValueType>) -> RawConcreteValue {
        RawConcreteValue(addr, ty.cloned())
    }

    fn set_addr(&mut self, addr: RawPointer, value: ValueRef) {
        fn insert(entry: Entry<RawPointer, SymValueRef>, value: SymValueRef) {
            match entry {
                Entry::Occupied(mut entry) => {
                    entry.insert(value);
                }
                Entry::Vacant(entry) => {
                    entry.insert(value);
                }
            }
        }

        let entry = self.memory.entry(addr);

        match value.as_ref() {
            Value::Symbolic(_) => {
                insert(entry, SymValueRef::new(value));
            }
            Value::Concrete(ConcreteValue::Adt(adt)) => {
                for field in adt.fields.iter() {
                    if let Some(field) = field {
                        self.set_addr(todo!(), field.clone());
                    }
                }
            }
            Value::Concrete(ConcreteValue::Array(array)) => {
                for element in array.elements.iter() {
                    self.set_addr(todo!(), element.clone());
                }
            }
            Value::Concrete(ConcreteValue::Unevaluated(UnevalValue::SymbolicOwner(sym_owner))) => {
                for (offset, sym_value) in sym_owner.sym_values.iter() {
                    self.memory.insert(addr + offset, sym_value.clone());
                }
            }
            Value::Concrete(_) => {
                if let Entry::Occupied(entry) = entry {
                    entry.remove();
                }
            }
        }
    }
}

impl<VS, SP: SymbolicProjector> IndexResolver<Local> for RawPointerVariableState<VS, SP>
where
    VS: IndexResolver<Local>,
{
    fn get(&self, local: &Local) -> Option<ValueRef> {
        let Some(addr) = local.address() else {
            return self.fallback.get(local);
        };

        Some(if let Some(sym_val) = self.get(&addr) {
            sym_val.clone_to()
        } else {
            UnevalValue::Lazy(RawConcreteValue(addr, Some(USIZE_TYPE.into()))).to_value_ref()
        })
    }
}

impl<VS, SP: SymbolicProjector> SelfHierarchical for RawPointerVariableState<VS, SP>
where
    VS: SelfHierarchical,
{
    fn add_layer(self) -> Self {
        Self {
            fallback: self.fallback.add_layer(),
            ..self
        }
    }

    fn drop_layer(self) -> Option<Self> {
        self.fallback.drop_layer().map(|f| Self {
            fallback: f,
            ..self
        })
    }
}
