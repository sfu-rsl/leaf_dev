use std::{cell::RefCell, collections::btree_map::Entry, rc::Rc};

use crate::{
    abs::{PointerOffset, TypeId, TypeSize, USIZE_TYPE},
    backends::basic::{
        alias::TypeManager,
        config::{SymbolicPlaceConfig, SymbolicPlaceStrategy},
        expr::place::DeterministicPlaceValue,
        VariablesState,
    },
    tyexp::TypeInfoExt,
    utils::SelfHierarchical,
};
use common::tyexp::{FieldsShapeInfo, StructShape, TypeInfo, UnionShape};

use super::{
    super::{
        alias::{RRef, SymValueRefProjector as SymbolicProjector},
        expr::prelude::*,
        place::{LocalWithMetadata, PlaceMetadata, PlaceWithMetadata},
        ValueRef,
    },
    proj::IndexResolver,
};

mod memory;
pub(super) mod sym_place;
mod utils;

use common::log_debug;
use memory::*;
use utils::*;

type Local = LocalWithMetadata;
type Place = PlaceWithMetadata;
type Projection = crate::abs::Projection<Local>;

type SymPlaceHandlerObject = Box<
    dyn super::SymPlaceHandler<
            PlaceMetadata,
            SymPlaceValue = SymPlaceValueRef,
            PlaceValue = PlaceValueRef,
        >,
>;

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
pub(in super::super) struct RawPointerVariableState<SP: SymbolicProjector> {
    memory: memory::Memory,
    sym_projector: RRef<SP>,
    type_manager: Rc<dyn TypeManager>,
    sym_read_handler: RefCell<SymPlaceHandlerObject>,
    sym_write_handler: RefCell<SymPlaceHandlerObject>,
}

impl<SP: SymbolicProjector> RawPointerVariableState<SP> {
    pub fn new(
        sym_projector: RRef<SP>,
        type_manager: Rc<dyn TypeManager>,
        sym_place_handler_factory: impl Fn(SymbolicPlaceStrategy) -> SymPlaceHandlerObject,
        sym_place_config: &SymbolicPlaceConfig,
    ) -> Self {
        Self {
            memory: Default::default(),
            sym_projector,
            type_manager,
            sym_read_handler: RefCell::new(sym_place_handler_factory(sym_place_config.read)),
            sym_write_handler: RefCell::new(sym_place_handler_factory(sym_place_config.write)),
        }
    }

    fn get<'a, 'b>(&'a self, addr: Address, type_id: TypeId) -> Option<&'a SymValueRef> {
        log_debug!(
            "Querying memory for address: {:p} with type: {:?}",
            addr,
            type_id
        );

        let (obj_address, (obj_value, obj_type_id)) = self.get_object(addr)?;

        log_debug!(
            "Found value {} for address: {:p} with type: {:?}",
            obj_value,
            addr,
            type_id
        );

        // FIXME: (*)
        debug_assert_eq!(
            *obj_address, addr,
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
    fn get_object<'a, 'b>(&'a self, addr: Address) -> Option<(&'a Address, &'a MemoryObject)> {
        let cursor = self.memory.before_or_at(&addr);
        if let entry @ Some((start, ..)) = cursor.peek_prev() {
            let size = 1;
            let region = *start..(start.wrapping_byte_add(size));
            if region.contains(&addr) {
                return entry;
            }
        }

        None
    }

    fn entry_object<'a, 'b>(&'a mut self, addr: Address) -> Entry<'a, Address, MemoryObject> {
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

    fn get_type_size(&self, metadata: &PlaceMetadata) -> Option<TypeSize> {
        metadata.size().or_else(|| {
            metadata.type_id().and_then(|type_id| {
                let ty = self.type_manager.get_type(type_id);
                ty.is_sized().then_some(ty.size)
            })
        })
    }
}

impl<SP: SymbolicProjector> VariablesState<Place, ValueRef, PlaceValueRef>
    for RawPointerVariableState<SP>
where
    Self: IndexResolver<Local>,
{
    fn id(&self) -> usize {
        // FIXME
        0
    }

    #[tracing::instrument(level = "debug", skip(self))]
    fn ref_place(&self, place: &Place) -> PlaceValueRef {
        self.get_place(place, self.sym_read_handler.borrow_mut())
    }

    #[tracing::instrument(level = "debug", skip(self))]
    fn copy_place(&self, place: &Place) -> ValueRef {
        let place_val = self.get_place(place, self.sym_read_handler.borrow_mut());
        match place_val.as_ref() {
            PlaceValue::Deterministic(ref place) => self.copy_deterministic_place(place),
            PlaceValue::Symbolic(..) => todo!(),
        }
    }

    #[tracing::instrument(level = "debug", skip(self))]
    fn try_take_place(&mut self, place: &Place) -> Option<ValueRef> {
        let place_val = self.get_place(place, self.sym_write_handler.borrow_mut());
        match place_val.as_ref() {
            PlaceValue::Deterministic(ref place) => Some(self.take_deterministic_place(place)),
            PlaceValue::Symbolic(..) => todo!(),
        }
    }

    fn set_place(&mut self, place: &Place, value: ValueRef) {
        let place_val = self.get_place(place, self.sym_write_handler.borrow_mut());
        match place_val.as_ref() {
            PlaceValue::Deterministic(ref place) => {
                self.set_deterministic_place(place, value);
            }
            PlaceValue::Symbolic(..) => todo!("#238"),
        }
    }
}

// Deterministic Place
impl<SP: SymbolicProjector> RawPointerVariableState<SP> {
    fn copy_deterministic_place(&self, place_val: &DeterministicPlaceValue) -> ValueRef {
        // let addr = get_address(place_val, proj);
        let addr = place_val.address();

        // If the place is pointing to a symbolic value.
        if let Some(sym_val) = self.get(addr, place_val.unwrap_type_id()) {
            return sym_val.clone().into();
        }

        // Or it is pointing to an object embracing symbolic values.
        if let Some(size) = self.get_type_size(place_val.as_ref()) {
            // FIXME: Double querying memory.
            if let Some(porter) = self.try_create_porter_for_copy(addr, size) {
                return porter.to_value_ref();
            }
        }

        create_lazy(addr, place_val.ty().cloned())
    }

    fn take_deterministic_place(&mut self, place_val: &DeterministicPlaceValue) -> ValueRef {
        // let addr = get_address(place_val, proj);
        let addr = place_val.address();

        // If the place is pointing to a symbolic value.
        if let Some(sym_val) = self.get(addr, place_val.unwrap_type_id()) {
            // Disabling removal because of possible use after move.
            // https://github.com/rust-lang/unsafe-code-guidelines/issues/188
            // self.memory.remove_at(&addr);

            return sym_val.clone().into();
        }

        // Or it is pointing to an object embracing symbolic values.
        if let Some(size) = self.get_type_size(place_val.as_ref()) {
            // FIXME: Double querying memory.
            if let Some(porter) = self.try_create_porter_for_move(addr, size) {
                return porter.to_value_ref();
            }
        }

        create_lazy(addr, place_val.ty().cloned())
    }

    fn set_deterministic_place(&mut self, place_val: &DeterministicPlaceValue, value: ValueRef) {
        self.set_addr(place_val.address(), 0, value, place_val.unwrap_type_id())
    }
}

// Porters
impl<SP: SymbolicProjector> RawPointerVariableState<SP> {
    fn try_create_porter_for_copy(&self, addr: Address, size: TypeSize) -> Option<PorterValue> {
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

    fn try_create_porter_for_move(&mut self, addr: Address, size: TypeSize) -> Option<PorterValue> {
        Self::try_create_porter(
            addr,
            size,
            |start| self.memory.after_or_at_mut(start),
            |c| c.as_cursor().peek_next(),
            |c| {
                // FIXME: (*)
                // Disabling removal because of possible use after move.
                // https://github.com/rust-lang/unsafe-code-guidelines/issues/188
                // c.remove_next();
                c.next();
            },
        )
    }

    /// Looks in the region indicated by `addr` and `size` and picks all
    /// symbolic values that are residing in that region. If there is no
    /// symbolic value in that region, returns `None`.
    #[tracing::instrument(level = "debug", skip(after_or_at, entry, move_next))]
    fn try_create_porter<'a, C: 'a + core::fmt::Debug>(
        addr: Address,
        size: TypeSize,
        after_or_at: impl FnOnce(&Address) -> C,
        entry: impl Fn(&C) -> Option<(&Address, &MemoryObject)>,
        move_next: impl Fn(&mut C),
    ) -> Option<PorterValue> {
        let range = addr..(addr.wrapping_byte_add(size as usize));

        // TODO: What if the address is at the middle of a symbolic value?
        let mut cursor = after_or_at(&range.start);
        let mut sym_values = Vec::new();
        while let Some((sym_addr, (sym_value, sym_type_id))) = entry(&cursor) {
            if !range.contains(sym_addr) {
                break;
            }

            let offset: PointerOffset = unsafe { sym_addr.byte_offset_from(addr) }
                .try_into()
                .unwrap();
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

// Setting (storing)
impl<SP: SymbolicProjector> RawPointerVariableState<SP> {
    fn set_addr(&mut self, addr: Address, offset: PointerOffset, value: ValueRef, type_id: TypeId) {
        fn insert(entry: Entry<Address, MemoryObject>, value: MemoryObject) {
            log_debug!(
                "Storing value: {} with type {} at address: {:p}",
                value.0,
                value.1,
                *entry.key()
            );
            match entry {
                Entry::Occupied(mut entry) => {
                    entry.insert(value);
                }
                Entry::Vacant(entry) => {
                    entry.insert(value);
                }
            }
        }

        let addr = addr.wrapping_byte_add(offset as usize);
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
            Value::Concrete(ConcreteValue::Adt(adt)) => {
                self.set_addr_adt(addr, adt, type_id);
            }
            Value::Concrete(ConcreteValue::Array(array)) => {
                self.set_addr_array(addr, array, type_id)
            }
            Value::Concrete(ConcreteValue::Unevaluated(UnevalValue::Porter(porter))) => {
                for (offset, type_id, sym_value) in porter.sym_values.iter() {
                    self.set_addr(addr, *offset, sym_value.clone_to(), *type_id);
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

    fn set_addr_adt(&mut self, addr: Address, adt: &AdtValue, type_id: TypeId) {
        log_debug!(
            "Setting ADT at address: {:p} with type: {:?}",
            addr,
            type_id
        );
        let ty = self.get_type(type_id);
        let variant = match adt.kind {
            AdtKind::Enum { variant } => &ty.variants[variant as usize],
            _ => ty.expect_single_variant(),
        };

        match &variant.fields {
            FieldsShapeInfo::Struct(StructShape { fields }) => {
                for (field, info) in adt.fields.iter().zip(fields) {
                    if let Some(value) = &field.value {
                        self.set_addr(addr, info.offset, value.clone(), info.ty);
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
                self.set_addr(addr, offset, value.clone(), fields[i].ty);
            }
            _ => panic!(
                "Unexpected shape for fields of an ADT: {:?}",
                variant.fields
            ),
        };
    }

    fn set_addr_array(&mut self, addr: Address, array: &ArrayValue, type_id: TypeId) {
        let item_ty = {
            let item_ty_id = self.get_type(type_id).expect_array().item_ty;
            self.get_type(item_ty_id)
        };

        for (i, element) in array.elements.iter().enumerate() {
            self.set_addr(
                addr,
                item_ty.size * i as TypeSize,
                element.clone(),
                item_ty.id,
            );
        }
    }
}

impl<SP: SymbolicProjector> IndexResolver<Local> for RawPointerVariableState<SP> {
    fn get(&self, local: &Local) -> Option<ValueRef> {
        let addr = local.address();

        Some(
            if let Some(sym_val) = self.get(
                addr,
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

impl<SP: SymbolicProjector> SelfHierarchical for RawPointerVariableState<SP> {
    fn add_layer(self) -> Self {
        self
    }

    fn drop_layer(self) -> Option<Self> {
        Some(self)
    }
}
