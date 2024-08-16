pub(crate) mod strategies;

use core::{cell::RefMut, iter, ops::Bound};

use common::log_debug;

use crate::{
    abs::place::HasMetadata,
    backends::basic::{
        alias::TypeManager,
        expr::{
            place::{
                DerefSymHostPlace, DeterministicPlaceValue, DeterministicProjection,
                SymIndexedPlace, SymbolicPlaceValue,
            },
            prelude::*,
        },
        place::PlaceMetadata,
        state::proj::IndexResolver,
    },
};

use super::*;

impl<SP: SymbolicProjector> RawPointerVariableState<SP> {
    pub(super) fn get_place<'a, 'b>(
        &'a self,
        place: &'b Place,
        mut sym_place_handler: RefMut<'a, SymPlaceHandlerObject>,
    ) -> PlaceValueRef {
        let place_value = self.get_place_iter_raw(
            place.local().metadata(),
            place.projections(),
            place.projs_metadata(),
        );
        if place_value.is_symbolic() {
            sym_place_handler.handle(SymPlaceValueRef::new(place_value), place.metadata())
        } else {
            place_value
        }
    }

    pub(super) fn get_place_iter_raw<'b, I: Iterator<Item = &'b PlaceMetadata>>(
        &self,
        local_metadata: &'b PlaceMetadata,
        mut projs: &'b [Projection],
        mut projs_metadata: I,
    ) -> PlaceValueRef {
        /* NOTE: How does this work?
         * The main responsibility of this is function is to check if any non-determinism
         * is present for the given place. The following cases are only expected:
         * 1. Dereferencing a symbolic value.
         * 2. Indexing by a symbolic value.
         * Note that other projections on symbolic values do not cause non-determinism
         * for the place and they correspond to a single deterministic place (but with
         * a symbolic value).
         */

        let addr = local_metadata.address();
        // 1. Dereferencing a symbolic value.
        let value = if let Some(Projection::Deref) = projs.first() {
            if let Some(sym_value) = self.get(addr, local_metadata.unwrap_type_id()) {
                let base = handle_deref(sym_value.clone(), local_metadata);
                debug_assert!(
                    !has_deref(projs.iter().skip(1)),
                    "Based on the documentation, Deref can only appear as the first projection after MIR optimizations."
                );
                base.into()
            } else {
                projs = &projs[(Bound::Excluded(0), Bound::Unbounded)];
                DeterministicPlaceValue::new(projs_metadata.next().unwrap().clone()).to_value_ref()
            }
        } else {
            // PlaceValue::from_base(DeterministicPlaceBase { addr }.into()).to_value_ref()
            DeterministicPlaceValue::new(local_metadata.clone()).to_value_ref()
        };

        projs
            .iter()
            .zip(
                iter::once(local_metadata)
                    .chain(projs_metadata.by_ref())
                    .map_windows(|[a, b]| (*a, *b)),
            )
            .fold(value, |mut value, (proj, (host_meta, meta))| {
                // 2. Indexing by a symbolic value.
                if let Projection::Index(index) = proj {
                    if let Some(index_val) = IndexResolver::get(self, index) {
                        if index_val.is_symbolic() {
                            log_debug!("Symbolic index observed: {}", index_val.as_ref());
                            return SymbolicPlaceValue::from_base(SymIndexedPlace {
                                base: value,
                                base_metadata: host_meta.clone(),
                                index: SymValueRef::new(index_val),
                                index_metadata: index.metadata().clone(),
                            })
                            .to_value_ref();
                        }
                    }
                }

                match PlaceValueRef::make_mut(&mut value) {
                    PlaceValue::Deterministic(deter) => {
                        *deter = DeterministicPlaceValue::new(meta.clone());
                    }
                    PlaceValue::Symbolic(sym) => {
                        sym.proj = Some(to_deterministic_proj(
                            sym.proj.as_ref(),
                            proj,
                            host_meta,
                            meta,
                            self.type_manager.as_ref(),
                        ));
                    }
                }

                value
            })
    }
}

fn handle_deref(value: SymValueRef, meta: &PlaceMetadata) -> SymPlaceValueRef {
    let unexpected = || unreachable!("Unexpected symbolic value to dereference: {:?}", value);

    let SymValue::Expression(expr) = value.as_ref() else {
        unexpected()
    };

    match expr {
        Expr::Ref(place) => place.clone(),
        _ => SymPlaceValueRef::new(
            SymbolicPlaceValue::from_base(DerefSymHostPlace {
                host: value,
                host_metadata: meta.clone(),
            })
            .to_value_ref(),
        ),
    }
}

#[inline]
fn has_deref<'p>(mut projs: impl Iterator<Item = &'p Projection>) -> bool {
    projs.any(|p| matches!(p, Projection::Deref))
}

fn to_deterministic_proj<'a>(
    current: Option<&DeterministicProjection>,
    _proj: &'a Projection,
    host_meta: &PlaceMetadata,
    meta: &PlaceMetadata,
    type_manager: &dyn TypeManager,
) -> DeterministicProjection {
    let offset = unsafe { meta.address().byte_offset_from(host_meta.address()) };
    let offset =
        current.map_or(0, |p| p.offset) + TryInto::<PointerOffset>::try_into(offset).unwrap();

    let size = meta
        .size()
        .unwrap_or(type_manager.get_type(meta.unwrap_type_id()).size);
    DeterministicProjection { offset, size }
}
