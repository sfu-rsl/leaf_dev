use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    rc::Rc,
};

use delegate::delegate;

use crate::{
    abs::{self, RawPointer, USIZE_TYPE},
    backends::basic::{expr::RawConcreteValue, place::LocalWithAddress, VariablesState},
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

/// Provides a mapping for raw pointers to symbolic values.
/// All places that have a valid address are handled by this state, otherwise
/// they will be sent to the `fallback` state to be handled.
pub(in super::super) struct RawPointerVariableState<VS, SP: SymbolicProjector> {
    memory: HashMap<RawPointer, SymValueRef>,
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
            memory: HashMap::new(),
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
            apply_projs_sym(
                self.sym_projector.clone(),
                sym_val,
                sym_projs.iter().map(|p| p.resolved_index(self)),
            )
            .into()
        } else {
            UnevalValue::Lazy(RawConcreteValue(addr, place.ty().cloned())).to_value_ref()
        }
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

        let result = if let Some((sym_val, sym_projs)) =
            self.first_symbolic_value_iter(addr, place.projections(), place.proj_addresses())
        {
            if sym_projs.is_empty() {
                let value = sym_val.clone_to();
                self.memory.remove(&addr);
                value
            } else {
                apply_projs_sym(
                    self.sym_projector.clone(),
                    sym_val,
                    sym_projs.iter().map(|p| p.resolved_index(self)),
                )
                .into()
            }
        } else {
            UnevalValue::Lazy(RawConcreteValue(addr, place.ty().cloned())).to_value_ref()
        };
        Some(result)
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

        let entry = self.memory.entry(addr);
        if !value.is_symbolic() {
            if let Entry::Occupied(entry) = entry {
                entry.remove();
            }

            return;
        }

        entry.insert_entry(SymValueRef::new(value));
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
