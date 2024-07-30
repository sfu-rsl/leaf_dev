use std::{
    cell::{RefCell, RefMut},
    collections::{btree_map::Entry, VecDeque},
    iter,
    ops::Bound,
    rc::Rc,
};

use delegate::delegate;

use crate::{
    abs::{
        expr::{
            proj::{
                macros::{impl_singular_proj_through_general, impl_singular_projs_through_general},
                ProjectionOn, Projector,
            },
            sym_place::SelectTarget,
        },
        place::HasMetadata,
        PointerOffset, RawPointer, TypeId, TypeSize, USIZE_TYPE,
    },
    backends::basic::{
        alias::TypeManager,
        config::{SymbolicPlaceConfig, SymbolicPlaceStrategy},
        expr::LazyTypeInfo,
        VariablesState,
    },
    tyexp::TypeInfoExt,
    utils::SelfHierarchical,
};
use common::tyexp::{ArrayShape, FieldsShapeInfo, StructShape, TypeInfo, UnionShape};

use super::{
    super::{
        alias::{RRef, SymValueRefProjector as SymbolicProjector},
        expr::{prelude::*, sym_place::*, PorterValue, SymIndexPair},
        place::{LocalWithMetadata, PlaceMetadata, PlaceWithMetadata},
        ValueRef,
    },
    proj::{apply_projs_sym, IndexResolver, ProjectionResolutionExt},
};

mod memory;
pub(super) mod sym_place;
mod utils;

use common::log_debug;
use utils::*;

type Local = LocalWithMetadata;
type Place = PlaceWithMetadata;
type Projection = crate::abs::Projection<Local>;

type SymPlaceHandlerObject = Box<dyn super::SymPlaceHandler<PlaceMetadata>>;

/* NOTE: Memory structure
 * How does this state tries to store (symbolic) objects?
 * We divide symbolic objects into two categories:
 * - Primitives: We assume that symbolic _variables_ can be only from primitive types,
 *   and all expressions built based on them are also from primitive types.
 *   These contribute to the majority of values we keep in the memory.
 * - Non-primitives: These are the rest of symbolic values that may correspond to
 *   non-primitive types such as arrays or ADTs. Since symbolic variables
 *   cannot be of these types, they can only appear when a symbolic projection
 *   occurs.
 *
 * Effectively, non-primitive symbolic values always correspond to multiple
 * objects, and the memory regions they are associated with is non-deterministic.
 * This means any read or write to these parts of memory will be also
 * non-deterministic, and we cannot store a value inside their target memory
 * regions directly. Instead, we derive another symbolic value that corresponds
 * to multiple objects (in case of read), or update the non-determinism
 * information of the current symbolic value (in case of write).
 * To understand it better, let's look at an example:
 * ```
 * let x = 10.mark_symbolic();
 * let y = a[x];
 * let z = y.1 + 20;
 * y.0 = z;
 * ```
 * Here, the value of `y` is non-deterministic (it is the result of a symbolic
 * index on `a`). Later, the field projection on `y` is also symbolic but
 * corresponds to different objects in the memory (the second field of `a[x]`).
 * We derive another value based on `y` for it. In the last line, we write to
 * `y.0`, a non-deterministic location, so we need to update the value we have
 * stored in place of `y` to reflect the change that the first field is set to
 * `z`.
 *
 * Therefore, any write to any part of a non-deterministic memory region will
 * update the information for the whole region. This brings the important
 * guarantee that there will be no overlapping objects in the memory.
 *
 * Getting back to the implementation, to keep track of these regions, we need
 * to know the memory layout of each object during the runtime. This can be
 * achieved by getting type of each place (and having the information about
 * each type). In the base case, the size of the type is enough to determine
 * the region. This also assists us when different places have the same address.
 * (e.g., `y` and `y.0` in the example above).
 *
 * Current state: (*)
 * We don't have the exact type information and just have an id. So we can partly
 * distinguish between primitive and non-primitive types and non-primitive types
 * themselves.
 * Also, we don't have the layout information so we skip exact reasoning about
 * regions.
 */

type MemoryObject = (SymValueRef, TypeId);

/// Provides a mapping for raw pointers to symbolic values.
/// All places that have a valid address are handled by this state, otherwise
/// they will be sent to the `fallback` state to be handled.
pub(in super::super) struct RawPointerVariableState<VS, SP: SymbolicProjector> {
    memory: memory::Memory,
    fallback: VS,
    sym_projector: RRef<SP>,
    type_manager: Rc<dyn TypeManager>,
    sym_read_handler: RefCell<SymPlaceHandlerObject>,
    sym_write_handler: RefCell<SymPlaceHandlerObject>,
}

impl<VS, SP: SymbolicProjector> RawPointerVariableState<VS, SP> {
    pub fn new(
        fallback: VS,
        sym_projector: RRef<SP>,
        type_manager: Rc<dyn TypeManager>,
        sym_place_handler_factory: impl Fn(SymbolicPlaceStrategy) -> SymPlaceHandlerObject,
        sym_place_config: &SymbolicPlaceConfig,
    ) -> Self
    where
        VS: VariablesState<Place>,
    {
        Self {
            memory: Default::default(),
            fallback,
            sym_projector,
            type_manager,
            sym_read_handler: RefCell::new(sym_place_handler_factory(sym_place_config.read)),
            sym_write_handler: RefCell::new(sym_place_handler_factory(sym_place_config.write)),
        }
    }

    fn get<'a, 'b>(&'a self, addr: &'b RawPointer, type_id: TypeId) -> Option<&'a SymValueRef> {
        log_debug!(
            "Querying memory for address: {} with type: {:?}",
            addr,
            type_id
        );

        let (obj_address, (obj_value, obj_type_id)) = self.get_object(*addr)?;

        log_debug!(
            "Found value {} for address: {} with type: {:?}",
            obj_value,
            addr,
            type_id
        );

        // FIXME: (*)
        debug_assert_eq!(
            obj_address, addr,
            "Non-deterministic memory regions are not supported yet."
        );

        /* We assume that a parent host will be queried before its children.
         * So, if the type id is not the same, it means that the object is
         * nested inside the queried object. */
        if obj_type_id.eq(&type_id) {
            Some(obj_value)
        } else {
            log_debug!(
                "Faced an (nested) object with different type: {:?}",
                obj_type_id
            );
            None
        }
    }

    /// Returns the object that contains the given address.
    fn get_object<'a, 'b>(
        &'a self,
        addr: RawPointer,
    ) -> Option<(&'a RawPointer, &'a MemoryObject)> {
        let cursor = self.memory.before_or_at(&addr);
        if let entry @ Some((start, ..)) = cursor.peek_prev() {
            let size = 1;
            let region = *start..(start + size);
            if region.contains(&addr) {
                return entry;
            }
        }

        None
    }

    fn entry_object<'a, 'b>(&'a mut self, addr: RawPointer) -> Entry<'a, RawPointer, MemoryObject> {
        let key = self
            .get_object(addr)
            .map(|(start, _)| *start)
            .unwrap_or(addr);
        self.memory.entry_at(key)
    }

    #[inline]
    fn get_type(&self, type_id: TypeId) -> &'static TypeInfo {
        self.type_manager.get_type(type_id)
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

    fn ref_place(&self, place: &Place) -> Rc<Value> {
        let Some(addr) = place.address() else {
            return self.fallback.ref_place(place);
        };

        // If the place is pointing to a symbolic value.
        if let Some((sym_val, host_metadata, sym_projs, projs_metadata)) =
            self.try_find_sym_value(place, self.sym_read_handler.borrow_mut())
        {
            return self
                .apply_projs_on_sym_value(
                    &sym_val,
                    sym_projs,
                    iter::once(host_metadata).chain(projs_metadata),
                )
                .into();
        }

        create_lazy(addr, place.metadata().ty().cloned())
    }

    fn copy_place(&self, place: &Place) -> ValueRef {
        let Some(addr) = place.address() else {
            return self.fallback.copy_place(place);
        };

        let place_val = self.ref_place(place);
        if place_val.is_symbolic() {
            return self
                .retrieve_sym_value(
                    SymValueRef::new(place_val),
                    place.metadata().unwrap_type_id(),
                )
                .into();
        }

        // Or it is pointing to an object embracing symbolic values.
        if let Some(size) = place.metadata().size() {
            // FIXME: Double querying memory.
            if let Some(porter) = self.try_create_porter_for_copy(addr, size) {
                return porter.to_value_ref();
            }
        }

        create_lazy(addr, place.metadata().ty().cloned())
    }

    fn try_take_place(&mut self, place: &Place) -> Option<ValueRef> {
        let Some(addr) = place.address() else {
            return self.fallback.try_take_place(place);
        };

        // If the place is pointing to a symbolic value.
        let place_val = self.ref_place(place);
        if place_val.is_symbolic() {
            let retrieved_val = self.retrieve_sym_value(
                SymValueRef::new(place_val),
                place.metadata().unwrap_type_id(),
            );
            // FIXME: (*) Also remove non-deterministic values.
            if retrieved_val.as_proj().is_none() {
                // Disabling removal because of possible use after move.
                // https://github.com/rust-lang/unsafe-code-guidelines/issues/188
                // self.memory.remove_at(&addr);
            }
            return Some(retrieved_val.into());
        }

        // Or it is pointing to an object embracing symbolic values.
        if let Some(size) = place.metadata().size() {
            // FIXME: Double querying memory.
            if let Some(porter) = Self::try_create_porter(
                addr,
                size,
                |start| self.memory.after_or_at_mut(start),
                |c| c.as_cursor().peek_next(),
                |c| {
                    // FIXME: (*)
                    // Disabling removal because of possible use after move.
                    // https://github.com/rust-lang/unsafe-code-guidelines/issues/188
                    c.remove_next();
                },
            ) {
                return Some(porter.to_value_ref());
            }
        }

        Some(create_lazy(addr, place.metadata().ty().cloned()))
    }

    fn set_place(&mut self, place: &Place, value: ValueRef) {
        let Some(addr) = place.address() else {
            return self.fallback.set_place(place, value);
        };

        if let Some((_sym_val, _, sym_projs, _)) =
            self.try_find_sym_value(place, self.sym_write_handler.borrow_mut())
        {
            if !sym_projs.is_empty() {
                todo!("#238");
            }
        }

        self.set_addr(addr, value, place.metadata().unwrap_type_id());
    }
}

impl<VS: VariablesState<Place>, SP: SymbolicProjector> RawPointerVariableState<VS, SP> {
    /// Finds the first symbolic value in the chain of projections (hosts) leading to the place.
    /// # Returns
    /// The first symbolic value and the remaining projections to be applied on it.
    fn try_find_sym_value<'a, 'b>(
        &'a self,
        place: &'b Place,
        sym_place_handler: RefMut<'a, SymPlaceHandlerObject>,
    ) -> Option<(
        SymValueRef,
        &'b PlaceMetadata,
        &'b [Projection],
        impl Iterator<Item = &'b PlaceMetadata>,
    )>
    where
        Self: IndexResolver<Local>,
    {
        self.try_find_sym_value_iter(
            place.local().metadata(),
            place.projections(),
            place.projs_metadata(),
            sym_place_handler,
        )
    }

    fn try_find_sym_value_iter<'a, 'b, I: Iterator<Item = &'b PlaceMetadata>>(
        &'a self,
        local_metadata: &'b PlaceMetadata,
        projs: &'b [Projection],
        mut projs_metadata: I,
        mut sym_place_handler: RefMut<'a, SymPlaceHandlerObject>,
    ) -> Option<(SymValueRef, &'b PlaceMetadata, &'b [Projection], I)>
    where
        Self: IndexResolver<Local>,
    {
        /* NOTE: We probably can reverse the iteration order for faster hits. */
        if let Some(sym_val) = self.get(
            local_metadata.address().as_ref()?,
            local_metadata.unwrap_type_id(),
        ) {
            if let Some(Projection::Deref) = projs.first() {
                let ref_val = sym_place_handler.handle(sym_val.clone(), local_metadata);

                // If concretized by the strategy, we continue with the dereferenced place.
                if !ref_val.is_symbolic() {
                    debug_assert!(
                        !projs.iter().skip(1).any(|p| matches!(p, Projection::Deref)),
                        "Based on the documentation, Deref can only appear as the first projection after MIR optimizations."
                    );
                    let after_deref = projs_metadata.next().unwrap();
                    return self.try_find_sym_value_iter(
                        &after_deref,
                        &projs[1..],
                        projs_metadata,
                        sym_place_handler,
                    );
                }
            }
            Some((sym_val.clone(), local_metadata, projs, projs_metadata))
        } else {
            // Checking for the value after each projection.
            projs
                .iter()
                .zip(
                    iter::once(local_metadata)
                        .chain(projs_metadata.by_ref())
                        .map_windows(|[a, b]| (*a, *b)),
                )
                .enumerate()
                // The first symbolic value in the projection chain.
                .find_map(|(i, (proj, (host_meta, meta)))| {
                    // Index may be symbolic.
                    if let Projection::Index(index) = proj {
                        if let Some(index_val) = IndexResolver::get(self, index) {
                            if index_val.is_symbolic() {
                                log_debug!("Symbolic index observed: {}", index_val.as_ref());
                                return self
                                    .handle_first_sym_index(
                                        SymValueRef::new(index_val),
                                        index.metadata(),
                                        host_meta,
                                        &mut sym_place_handler,
                                    )
                                    .map(|sym_val| (i, sym_val, meta));
                            }
                        }
                    }

                    // Default behavior for any projection.
                    meta.address()
                        .and_then(|addr| self.get(&addr, meta.unwrap_type_id()))
                        .map(|sym_val| (i, sym_val.clone(), meta))
                })
                // Returning the remaining projections.
                .map(|(i, sym_val, meta)| {
                    (
                        sym_val,
                        meta,
                        &projs[(Bound::Excluded(i), Bound::Unbounded)],
                    )
                })
                .map(|(sym_val, meta, projs)| (sym_val, meta, projs, projs_metadata))
        }
    }

    fn handle_first_sym_index<'a, 'b>(
        &'a self,
        index_val: SymValueRef,
        index_local_metadata: &'b PlaceMetadata,
        host_metadata: &'b PlaceMetadata,
        sym_place_handler: &mut RefMut<'a, SymPlaceHandlerObject>,
    ) -> Option<SymValueRef> {
        // First apply the strategy.
        let index_val = sym_place_handler.handle(index_val, index_local_metadata);

        // If the index is still symbolic, we need to project it.
        index_val.is_symbolic().then(|| {
            debug_assert!(
                host_metadata
                    .address()
                    .and_then(|addr| self.get(&addr, host_metadata.unwrap_type_id()))
                    .is_none(),
                "The host is expected to be concrete for the first symbolic index."
            );
            self.sym_projector
                .borrow_mut()
                .index(
                    SymIndexPair::SymIndex {
                        host: lazy_from_meta(&host_metadata).unwrap(),
                        index: SymValueRef::new(index_val),
                    }
                    .into(),
                    false,
                    ProjMetadata::new(host_metadata.unwrap_type_id()).into(),
                )
                .into()
                .to_value_ref()
        })
    }

    /// Retrieves the memory content for the given symbolic value.
    /// It makes sure that the result value can live independently with no
    /// lazily-evaluated parts.
    /// In fact checks if the symbolic value is a projection expression then
    /// resolves it to its corresponding possible values.
    fn retrieve_sym_value(&self, value: SymValueRef, type_id: TypeId) -> SymValueRef {
        match value.as_ref() {
            SymValue::Expression(expr) => match expr {
                Expr::Projection(proj) => {
                    log_debug!("Resolving and retrieving symbolic projection expression: {proj}");
                    let resolved = self.resolve_sym_proj(proj);
                    debug_assert!(
                        if let SymbolicProjResult::Array(_) = &resolved {
                            false
                        } else {
                            true
                        },
                        "The root resolved value cannot be an array."
                    );
                    log_debug!("Resolved value: {}", resolved);
                    /* NOTE: How is this guaranteed to be symbolic?
                     * Basically: A symbolic value cannot be resolved to a concrete value.
                     * Also, those structurally possible concrete values (like transmuted values)
                     * cannot appear as the root of the resolved value. */
                    SymValueRef::new(self.retrieve_sym_proj_result(&resolved, type_id))
                }
                Expr::Multi(select) => {
                    Into::<Expr>::into(self.retrieve_select_proj_result(select, type_id))
                        .to_value_ref()
                }
                Expr::Ref(..) | Expr::Len(..) => {
                    unimplemented!("Not supported yet.")
                }
                _ => value,
            },
            _ => value,
        }
    }

    /// Takes a symbolic value as the host and applies the given projections on it.
    fn apply_projs_on_sym_value<'a, 'b>(
        &self,
        host: &'a SymValueRef,
        projs: &'b [Projection],
        place_metadata: impl Iterator<Item = &'b PlaceMetadata>,
    ) -> SymValueRef
    where
        Self: IndexResolver<Local>,
    {
        if projs.is_empty() {
            return host.clone();
        }

        let mut sym_place_handler = SymIndexHandler {
            sym_projector: self.sym_projector.clone(),
            sym_read_handler: self.sym_read_handler.borrow_mut(),
            projs: projs.iter().collect(),
        };
        apply_projs_sym(
            &mut sym_place_handler,
            host,
            projs.iter().zip(place_metadata).map(|(proj, meta)| {
                (
                    proj.resolved_index(self),
                    ProjMetadata::new(meta.unwrap_type_id()),
                )
            }),
        )
    }

    fn try_create_porter_for_copy(&self, addr: RawPointer, size: TypeSize) -> Option<PorterValue> {
        Self::try_create_porter(
            addr,
            size,
            /* At this point, we are looking for inner values, effectively
             * located at the same address or after it. */
            |start| self.memory.after_or_at(start),
            |c| c.peek_next(),
            |c| {
                c.next();
            },
        )
    }

    /// Looks in the region indicated by `addr` and `size` and picks all
    /// symbolic values that are residing in that region. If there is no
    /// symbolic value in that region, returns `None`.
    fn try_create_porter<'a, C: 'a>(
        addr: RawPointer,
        size: TypeSize,
        after_or_at: impl FnOnce(&RawPointer) -> C,
        entry: impl Fn(&C) -> Option<(&RawPointer, &MemoryObject)>,
        move_next: impl Fn(&mut C),
    ) -> Option<PorterValue> {
        let range = addr..addr + size;
        log_debug!("Checking to create a porter for range: {:?}", range);

        // TODO: What if the address is at the middle of a symbolic value?
        let mut cursor = after_or_at(&range.start);
        let mut sym_values = Vec::new();
        while let Some((sym_addr, (sym_value, sym_type_id))) = entry(&cursor) {
            if !range.contains(sym_addr) {
                break;
            }

            let offset: PointerOffset = sym_addr - addr;
            sym_values.push((offset, *sym_type_id, sym_value.clone()));
            move_next(&mut cursor);
        }

        if !sym_values.is_empty() {
            Some(PorterValue { sym_values })
        } else {
            None
        }
    }
}

impl<VS: VariablesState<Place>, SP: SymbolicProjector> RawPointerVariableState<VS, SP> {
    fn resolve_sym_proj(&self, proj: &ProjExpr) -> SymbolicProjResult {
        DefaultProjExprReadResolver::new(self.type_manager.as_ref(), self).resolve(proj)
    }

    fn retrieve_sym_proj_result(
        &self,
        proj_result: &SymbolicProjResult,
        type_id: TypeId,
    ) -> ValueRef {
        log_debug!("Retrieving symbolic projection result: {}", proj_result);
        match proj_result {
            SymbolicProjResult::Single(single) => self.retrieve_single_proj_result(single, type_id),
            SymbolicProjResult::Array(items) => Into::<ConcreteValue>::into(ArrayValue {
                elements: items
                    .iter()
                    .map(|item| self.retrieve_sym_proj_result(item, type_id))
                    .collect(),
            })
            .to_value_ref(),
            SymbolicProjResult::SymRead(select) => {
                Into::<Expr>::into(self.retrieve_select_proj_result(select, type_id))
                    .to_value_ref()
                    .into()
            }
        }
    }

    fn retrieve_single_proj_result(&self, single: &SingleProjResult, type_id: TypeId) -> ValueRef {
        log_debug!("Retrieving single projection result: {}", single);
        let result = match single {
            SingleProjResult::Transmuted(value) => {
                // This is guaranteed to not be a projection expression.
                value.value().clone()
            }
            SingleProjResult::Value(value) => value.clone(),
        };
        self.retrieve_value(result, type_id)
    }

    fn retrieve_select_proj_result(&self, select: &Select, type_id: TypeId) -> Select {
        log_debug!("Retrieving select projection result: {}", select);
        let target = match &select.target {
            SelectTarget::Array(possible_values) => SelectTarget::Array(
                possible_values
                    .iter()
                    .map(|value| self.retrieve_sym_proj_result(value, type_id))
                    .map(SingleProjResult::Value)
                    .map(SymbolicProjResult::Single)
                    .collect(),
            ),
            SelectTarget::Nested(box inner) => {
                let inner = self.retrieve_select_proj_result(inner, type_id);
                SelectTarget::Nested(Box::new(inner))
            }
        };

        Select {
            index: select.index.clone(),
            target,
        }
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
            ConcreteValue::Array(array) => ArrayValue {
                elements: array
                    .elements
                    .iter()
                    .map(|element| self.retrieve_value(element.clone(), type_id))
                    .collect(),
            }
            .to_value_ref(),
            ConcreteValue::Adt(adt) => AdtValue {
                kind: adt.kind.clone(),
                fields: adt
                    .fields
                    .iter()
                    .map(|field| AdtField {
                        value: field
                            .value
                            .as_ref()
                            .map(|value| self.retrieve_value(value.clone(), type_id)),
                    })
                    .collect(),
            }
            .to_value_ref(),
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
            _ => value.into(),
        })
    }
}

impl<VS: VariablesState<Place>, SP: SymbolicProjector> RawPointerVariableState<VS, SP> {
    fn set_addr(&mut self, addr: RawPointer, value: ValueRef, type_id: TypeId) {
        fn insert(entry: Entry<RawPointer, MemoryObject>, value: MemoryObject) {
            log_debug!("Storing value: {:?} at address: {}", value, entry.key());
            match entry {
                Entry::Occupied(mut entry) => {
                    entry.insert(value);
                }
                Entry::Vacant(entry) => {
                    entry.insert(value);
                }
            }
        }

        let entry = self.entry_object(addr);

        // FIXME: (*)
        debug_assert_eq!(
            *entry.key(),
            addr,
            "Non-deterministic memory regions are not supported yet."
        );

        match value.as_ref() {
            Value::Symbolic(_) => {
                insert(entry, (SymValueRef::new(value), type_id));
            }
            #[cfg(place_addr)]
            Value::Concrete(ConcreteValue::Adt(adt)) => {
                self.set_addr_adt(type_id, adt, addr);
            }
            Value::Concrete(ConcreteValue::Array(array)) => {
                self.set_addr_array(addr, array, type_id)
            }
            Value::Concrete(ConcreteValue::Unevaluated(UnevalValue::Porter(porter))) => {
                for (offset, type_id, sym_value) in porter.sym_values.iter() {
                    self.set_addr(addr + offset, sym_value.clone_to(), *type_id);
                }
            }
            Value::Concrete(_) => {
                if let Entry::Occupied(entry) = entry {
                    // FIXME: (*)
                    entry.remove();
                }
            }
        }
    }

    fn set_addr_adt(&mut self, type_id: TypeId, adt: &AdtValue, addr: u64) {
        let ty = self.get_type(type_id);
        let variant = match adt.kind {
            AdtKind::Enum { variant } => &ty.variants[variant as usize],
            _ => ty.expect_single_variant(),
        };

        match &variant.fields {
            FieldsShapeInfo::Struct(StructShape { fields }) => {
                for (field, info) in adt.fields.iter().zip(fields) {
                    if let Some(value) = &field.value {
                        if !value.is_symbolic() {
                            continue;
                        }

                        self.set_addr(addr + info.offset, value.clone(), info.ty);
                    }
                }
            }
            FieldsShapeInfo::Union(UnionShape { fields }) => {
                let field_find = &mut adt
                    .fields
                    .iter()
                    .enumerate()
                    .filter_map(|(i, f)| f.value.as_ref().map(|v| (i, v.clone())));
                let (i, value) = field_find
                    .next()
                    .expect("Could not find the single field of a union.");
                debug_assert!(
                    field_find.next().is_none(),
                    "Multiple fields found in a union."
                );
                /* NOTE: The documents about union is a bit unreliable.
                 * - https://doc.rust-lang.org/reference/types/union.html
                 * - https://doc.rust-lang.org/nightly/nightly-rustc/rustc_target/abi/enum.FieldsShape.html#variant.Union
                 */
                let offset = 0;
                self.set_addr(addr + offset, value.clone(), fields[i].ty);
            }
            _ => panic!(
                "Unexpected shape for fields of an ADT: {:?}",
                variant.fields
            ),
        };
    }

    fn set_addr_array(&mut self, addr: RawPointer, array: &ArrayValue, type_id: TypeId) {
        let item_ty = {
            let array_ty = self.get_type(type_id);
            let fields = &array_ty.expect_single_variant().fields;
            let FieldsShapeInfo::Array(ArrayShape {
                item_ty: item_ty_id,
                ..
            }) = fields
            else {
                panic!("Expected the variant to be an array, found: {:?}", fields)
            };
            self.get_type(*item_ty_id)
        };

        for (i, element) in array.elements.iter().enumerate() {
            if !element.is_symbolic() {
                continue;
            }

            let item_addr = addr + item_ty.size * i as TypeSize;
            self.set_addr(item_addr, element.clone(), item_ty.id);
        }
    }
}

impl<VS: VariablesState<Place>, SP: SymbolicProjector> RawPointerRetriever
    for RawPointerVariableState<VS, SP>
{
    fn retrieve(&self, addr: RawPointer, type_id: TypeId) -> ValueRef {
        log_debug!(
            "Retrieving value at address: {:x} with type: {:?}",
            addr,
            type_id
        );

        if let Some(sym_val) = self.get(&addr, type_id) {
            return self.retrieve_sym_value(sym_val.clone(), type_id).into();
        }

        // Or it is pointing to an object embracing symbolic values.
        // FIXME: Double querying memory.
        if let Some(porter) = self.try_create_porter_for_copy(addr, self.get_type(type_id).size) {
            return porter.to_value_ref();
        }

        RawConcreteValue::try_from(&{
            let mut metadata = PlaceMetadata::default();
            metadata.set_address(addr);
            metadata.set_type_id(type_id);
            metadata
        })
        .unwrap()
        .to_value_ref()
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

        Some(
            if let Some(sym_val) = self.get(
                &addr,
                // FIXME: As runtime library is compiled independently,
                // this id is not guaranteed to be the same as the id used in the program.
                common::utils::type_id_of::<usize>(),
            ) {
                sym_val.clone_to()
            } else {
                create_lazy(addr, Some(USIZE_TYPE.into()))
            },
        )
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

struct SymIndexHandler<'a, 'b, SP: SymbolicProjector> {
    sym_projector: RRef<SP>,
    sym_read_handler: RefMut<'a, SymPlaceHandlerObject>,
    /* NOTE: This is a workaround to access the metadata while implementing the trait.
     * We are relying on the fact that the projections will be applied in the
     * same order and only once. */
    projs: VecDeque<&'b Projection>,
}

impl<SP: SymbolicProjector> Projector for SymIndexHandler<'_, '_, SP> {
    type HostRef<'a> = SymValueRef;
    type Metadata<'a> = ProjMetadata;
    type FieldAccessor = FieldAccessKind;
    type HIRefPair<'a> = SymIndexPair;
    type DowncastTarget = DowncastKind;
    type Proj<'a> = ProjExpr;

    fn project<'a>(
        &mut self,
        proj_on: ProjectionOn<
            Self::HostRef<'a>,
            Self::FieldAccessor,
            Self::HIRefPair<'a>,
            Self::DowncastTarget,
        >,
        metadata: Self::Metadata<'a>,
    ) -> Self::Proj<'a> {
        let proj = self
            .projs
            .pop_front()
            .expect("Inconsistent projection data.");

        match proj_on {
            ProjectionOn::Index(index_pair, from_end) => {
                let (sym_host,mut index) = match index_pair {
                    SymIndexPair::SymHost { host, index } => {
                        (host, index)
                    }
                    SymIndexPair::SymIndex { host, index } => {
                        debug_assert!(
                            host.is_symbolic(),
                            "The initial symbolic index is not expected to be handled by this projector."
                        );
                        (SymValueRef::new(host), index.into())
                    }
                };

                if index.is_symbolic() {
                    let Projection::Index(index_local) = proj else {
                        unreachable!()
                    };
                    index = self.sym_read_handler.handle(SymValueRef::new(index), index_local.metadata());
                }

                self.sym_projector.borrow_mut().index(
                    Into::<SymIndexPair>::into((sym_host, index)).into(),
                    from_end,
                    metadata.into(),
                )
            }
            _ => self.sym_projector.borrow_mut().project(proj_on.map_into(), metadata.into()),
        }
        .into()
    }

    impl_singular_projs_through_general!();
}
impl<SP: SymbolicProjector> SymbolicProjector for SymIndexHandler<'_, '_, SP> {}
