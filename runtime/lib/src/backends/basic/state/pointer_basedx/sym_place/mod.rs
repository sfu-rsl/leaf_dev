pub(crate) mod strategies;

use core::{cell::RefMut, iter, ops::Bound};

use common::log_debug;

use crate::{
    abs::{
        expr::sym_place::{SelectTarget, SymbolicReadResolver},
        place::HasMetadata,
    },
    backends::basic::{
        expr::{
            place::*,
            sym_placex::{DefaultSymPlaceResolver, SinglePlaceResult, SymbolicPlaceResult},
            MultiValueArray, SliceIndex,
        },
        place::PlaceMetadata,
        state::proj::IndexResolver,
    },
};

use super::*;

use crate::backends::basic::expr::sym_placex::Select as PlaceSelect;
use crate::backends::basic::expr::MultiValue as ValueSelect;

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
        mut host_metadata: &'b PlaceMetadata,
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

        let addr = host_metadata.address();

        // 1. Dereferencing a symbolic value.
        let opt_sym_deref = projs.first().is_some_and(|p| matches!(p, Projection::Deref)).then(|| {
            let opt_sym_value = self
                .get(addr, host_metadata.unwrap_type_id())
                .map(|sym_value| handle_deref(sym_value.clone(), host_metadata));

            // Symbolic or not, pass the deref projection.
            host_metadata = projs_metadata.next().unwrap();
            projs = &projs[(Bound::Excluded(0), Bound::Unbounded)];
            debug_assert!(
                !has_deref(projs.iter().skip(1)),
                "Based on the documentation, Deref can only appear as the first projection after MIR optimizations."
            );

            opt_sym_value
        }).flatten();

        let value = opt_sym_deref
            .map(Into::into)
            .unwrap_or_else(|| DeterministicPlaceValue::new(host_metadata.clone()).to_value_ref());

        projs
            .iter()
            .zip(
                iter::once(host_metadata)
                    .chain(projs_metadata.by_ref())
                    .map_windows(|[a, b]| (*a, *b)),
            )
            .fold(value, |mut value, (proj, (host_meta, meta))| {
                // 2. Indexing by a symbolic value.
                let opt_sym_index = proj
                    .as_index()
                    .map(|index| (index, IndexResolver::get(self, index)))
                    .take_if(|(_, index_val)| index_val.is_symbolic())
                    .map(|(index, index_val)| {
                        handle_index(value.clone(), host_meta, index_val, index.metadata())
                    });

                let project_further = || {
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
                            ));
                        }
                    }
                    value
                };

                opt_sym_index
                    .map(Into::into)
                    .unwrap_or_else(project_further)
            })
    }
}

impl<SP: SymbolicProjector> RawPointerVariableState<SP> {
    pub(super) fn resolve_and_retrieve_symbolic_place(
        &self,
        place_val: &SymbolicPlaceValue,
        type_id: TypeId,
    ) -> SymValueRef {
        let resolved = self.resolve_symbolic_place(place_val);
        let mut copied = self.get_select_sym_place_result(&resolved);
        self.retrieve_select_proj_result(&mut copied, type_id);
        Expr::Multi(copied).to_value_ref()
    }
}

// Getting Symbolic Place
impl<SP: SymbolicProjector> RawPointerVariableState<SP> {
    fn resolve_symbolic_place(&self, place_val: &SymbolicPlaceValue) -> PlaceSelect {
        let resolver = DefaultSymPlaceResolver::new(self.type_manager.as_ref(), self);
        resolver.resolve(place_val)
    }

    fn get_sym_place_result(&self, resolved: &SymbolicPlaceResult) -> MultiValueTree {
        match resolved {
            SymbolicPlaceResult::SymRead(select) => {
                MultiValueTree::SymRead(self.get_select_sym_place_result(select))
            }
            SymbolicPlaceResult::Array(array) => {
                MultiValueTree::Array(self.get_array_sym_place_result(array))
            }
            SymbolicPlaceResult::Single(single) => {
                MultiValueTree::Single(self.get_single_sym_place_result(single))
            }
        }
    }

    fn get_select_sym_place_result(&self, select: &PlaceSelect) -> ValueSelect {
        use crate::abs::expr::sym_place::SelectTarget::*;
        ValueSelect {
            index: SliceIndex {
                index: select.index.clone(),
                // FIXME
                from_end: false,
            },
            target: match &select.target {
                Array(array) => Array(self.get_array_sym_place_result(array)),
                Nested(nested) => Nested(Box::new(self.get_select_sym_place_result(nested))),
            },
        }
    }

    fn get_array_sym_place_result(&self, array: &Vec<SymbolicPlaceResult>) -> MultiValueArray {
        array
            .into_iter()
            .map(|item| self.get_sym_place_result(item).into())
            .collect()
    }

    fn get_single_sym_place_result(&self, single: &SinglePlaceResult) -> ValueRef {
        self.copy_deterministic_place(single.0.as_ref())
    }
}

// Retrieving (Raw) Values
impl<SP: SymbolicProjector> RawPointerVariableState<SP> {
    /// Retrieves the memory content for the given symbolic value.
    /// It makes sure that the result value can live independently with no
    /// lazily-evaluated parts.
    /// In fact checks if the symbolic value is a multi expression then
    /// retrieves all of its possible values recursively.
    pub(super) fn retrieve_sym_value(
        &self,
        mut value: SymValueRef,
        type_id: TypeId,
    ) -> SymValueRef {
        match value.as_ref() {
            SymValue::Expression(expr) => match expr {
                Expr::Multi(..) => {
                    let SymValue::Expression(Expr::Multi(select)) =
                        SymValueRef::make_mut(&mut value)
                    else {
                        unreachable!()
                    };
                    self.retrieve_select_proj_result(select, type_id);
                    value
                }
                Expr::Partial(porter) => self.retrieve_porter_value(porter).to_value_ref(),
                Expr::Len(place) => self.retrieve_len_value(place),
                Expr::Projection(proj) => {
                    let ProjExpr::SymHost(SymHostProj {
                        host,
                        kind: ProjKind::Field(FieldAccessKind::PtrMetadata),
                        metadata: _,
                    }) = proj
                    else {
                        unreachable!("Unexpected projection expression for retrieval: {:?}", proj)
                    };
                    self.retrieve_ptr_metadata(host.clone())
                }
                _ => value,
            },
            _ => value,
        }
    }

    fn retrieve_multi_value_tree(&self, result: &mut MultiValueTree, type_id: TypeId) {
        log_debug!(
            "Retrieving symbolic projection result: {} with type {type_id}",
            result
        );
        match result {
            MultiValueTree::SymRead(select) => self.retrieve_select_proj_result(select, type_id),
            MultiValueTree::Array(items) => items
                .iter_mut()
                .for_each(|item| self.retrieve_multi_value_tree(item, type_id)),
            MultiValueTree::Single(single) => self.retrieve_multi_value_leaf(single, type_id),
        }
    }

    fn retrieve_select_proj_result(&self, select: &mut MultiValue, type_id: TypeId) {
        log_debug!(
            "Retrieving select projection result: {} with type {type_id}",
            select
        );
        match &mut select.target {
            SelectTarget::Array(possible_values) => possible_values
                .iter_mut()
                .for_each(|value| self.retrieve_multi_value_tree(value, type_id)),
            SelectTarget::Nested(box inner) => self.retrieve_select_proj_result(inner, type_id),
        };
    }

    fn retrieve_multi_value_leaf(&self, single: &mut MultiValueLeaf, type_id: TypeId) {
        log_debug!(
            "Retrieving single projection result: {} with type {type_id}",
            single
        );
        *single = self.retrieve_value(single.clone(), type_id);
    }

    fn retrieve_value(&self, value: ValueRef, type_id: TypeId) -> ValueRef {
        match value.as_ref() {
            Value::Concrete(_) => self
                .retrieve_conc_value(ConcreteValueRef::new(value), type_id)
                .into(),
            Value::Symbolic(_) => self
                .retrieve_sym_value(SymValueRef::new(value), type_id)
                .into(),
        }
    }

    fn retrieve_conc_value(&self, value: ConcreteValueRef, type_id: TypeId) -> ConcreteValueRef {
        ConcreteValueRef::new(match value.as_ref() {
            ConcreteValue::Array(array) => {
                let item_type_id = self.get_type(type_id).expect_array().item_ty;
                ArrayValue {
                    elements: array
                        .elements
                        .iter()
                        .map(|element| self.retrieve_value(element.clone(), item_type_id))
                        .collect(),
                }
                .to_value_ref()
            }
            ConcreteValue::Adt(adt) => {
                let variant_index = match adt.kind {
                    AdtKind::Enum { variant } => Some(variant),
                    _ => None,
                };
                AdtValue {
                    kind: adt.kind.clone(),
                    fields: adt
                        .fields
                        .iter()
                        .zip(self.get_type(type_id).child_type_ids(variant_index))
                        .map(|(field, type_id)| AdtField {
                            value: field
                                .value
                                .as_ref()
                                .map(|value| self.retrieve_value(value.clone(), type_id)),
                        })
                        .collect(),
                }
                .to_value_ref()
            }
            ConcreteValue::FatPointer(fat_ptr) => {
                debug_assert_eq!(fat_ptr.ty, type_id, "Type ids are not consistent.");
                let field_type_ids = self.get_type(type_id).child_type_ids(None);
                debug_assert_eq!(
                    field_type_ids.len(),
                    2,
                    "A fat pointer is expected to have two fields."
                );
                // FIXME: Implicit assumption about the order of fields.
                FatPtrValue {
                    address: self.retrieve_conc_value(fat_ptr.address.clone(), field_type_ids[0]),
                    metadata: self.retrieve_conc_value(fat_ptr.metadata.clone(), field_type_ids[1]),
                    ty: fat_ptr.ty,
                }
                .to_value_ref()
            }
            ConcreteValue::Unevaluated(UnevalValue::Lazy(raw)) => {
                let raw = if let Some(id) = raw.2.id() {
                    debug_assert_eq!(id, type_id, "The type id is not consistent.");
                    raw.clone()
                } else {
                    RawConcreteValue(raw.0, raw.1.clone(), LazyTypeInfo::Id(type_id))
                };
                let retrieved = unsafe { raw.retrieve(self.type_manager.as_ref(), self) }.unwrap();
                // Possible to introduce retrievable values (e.g., arrays) again.
                self.retrieve_conc_value(retrieved, type_id).into()
            }
            ConcreteValue::Unevaluated(UnevalValue::Porter(porter)) => {
                self.retrieve_porter_value(porter).to_value_ref()
            }
            _ => value.into(),
        })
    }

    pub(super) fn retrieve_porter_value(&self, porter: &PorterValue) -> PorterValue {
        PorterValue {
            sym_values: porter
                .sym_values
                .iter()
                .map(|(index, type_id, sym_value)| {
                    (
                        *index,
                        *type_id,
                        self.retrieve_sym_value(sym_value.clone(), *type_id),
                    )
                })
                .collect(),
        }
    }

    fn retrieve_len_value(&self, place: &SymbolicPlaceValue) -> SymValueRef {
        let SymbolicPlaceValue {
            base: SymbolicPlaceBase::Deref(host),
            proj: None,
        } = place
        else {
            unreachable!(
                "Len place is expected to be applied on a dereference of slice pointer. Got: {:?}",
                place
            )
        };

        // Equivalent to accessing the pointer's metadata.
        self.retrieve_ptr_metadata(host.value.clone())
    }

    fn retrieve_ptr_metadata(&self, host: SymValueRef) -> SymValueRef {
        match host.as_ref() {
            SymValue::Expression(Expr::Multi(multi)) => {
                Expr::from(multi.map_leaves(|value| match value.as_ref() {
                    Value::Concrete(ConcreteValue::FatPointer(fat_ptr)) => {
                        fat_ptr.metadata.0.clone()
                    }
                    _ => unreachable!(
                        "Only (retrieved) fat pointers are expected to appear. Got: {:?}",
                        value
                    ),
                }))
                .to_value_ref()
            }
            _ => {
                unreachable!(
                    "Only retrieved multi values are expected to retrieve pointer metadata from. Got: {:?}",
                    host
                )
            }
        }
    }
}

impl Projection {
    fn as_index(&self) -> Option<&LocalWithMetadata> {
        match self {
            Projection::Index(index) => Some(index),
            _ => None,
        }
    }
}

fn handle_deref(host: SymValueRef, host_metadata: &PlaceMetadata) -> SymPlaceValueRef {
    let unexpected = || unreachable!("Unexpected symbolic value to dereference: {:?}", host);

    let SymValue::Expression(expr) = host.as_ref() else {
        unexpected()
    };

    match expr {
        Expr::Ref(place) => place.clone(),
        _ => {
            log_debug!("Deref of symbolic value observed: {}", host);
            SymPlaceValueRef::new(
                SymbolicPlaceValue::from_base(DerefSymHostPlace {
                    value: host,
                    metadata: host_metadata.clone(),
                })
                .to_value_ref(),
            )
        }
    }
}

fn handle_index(
    host: Rc<PlaceValue>,
    host_metadata: &PlaceMetadata,
    index_val: Rc<Value>,
    index_metadata: &PlaceMetadata,
) -> SymPlaceValueRef {
    log_debug!("Symbolic index observed: {}", index_val.as_ref());
    SymPlaceValueRef::new(
        SymbolicPlaceValue::from_base(SymIndexedPlace {
            host,
            host_metadata: host_metadata.clone(),
            index: SymValueRef::new(index_val),
            index_metadata: index_metadata.clone(),
        })
        .to_value_ref(),
    )
}

#[inline]
fn has_deref<'p>(mut projs: impl Iterator<Item = &'p Projection>) -> bool {
    projs.any(|p| matches!(p, Projection::Deref))
}

fn to_deterministic_proj<'a>(
    current: Option<&DeterministicProjection>,
    proj: &'a Projection,
    host_meta: &PlaceMetadata,
    meta: &PlaceMetadata,
) -> DeterministicProjection {
    debug_assert!(!matches!(proj, Projection::Deref));

    let offset = unsafe { meta.address().byte_offset_from(host_meta.address()) };
    let offset =
        current.map_or(0, |p| p.offset) + TryInto::<PointerOffset>::try_into(offset).unwrap();

    DeterministicProjection {
        offset,
        ty_id: meta.unwrap_type_id(),
    }
}
