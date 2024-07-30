use core::num::Wrapping;

use common::{
    log_debug,
    pri::RawPointer,
    tyexp::{FieldInfo, TypeInfo},
};

use crate::{
    abs::{IntType, ValueType, USIZE_TYPE},
    backends::basic::{alias::TypeManager, expr::PtrValue, UnevalValue},
    tyexp::TypeInfoExt,
};

use super::{sym_place::RawPointerRetriever, *};

impl RawConcreteValue {
    /// Tries to retrieve the value from a primitive type.
    /// Primitive types have no fields and no risk of nested symbolic value.
    /// Thus, they can be directly retrieved from memory.
    /// Returns the value if the type information is available.
    /// # Safety
    /// The value must be retrieved within the period that the address is still
    /// pointing to the desired memory location.
    /// This should be ensured by the caller.
    pub(crate) unsafe fn try_retrieve_as_primitive(
        &self,
        type_manager: Option<&dyn TypeManager>,
    ) -> Result<ConstValue, LazyTypeInfo> {
        let value_ty = if let Some(ty) = &self.1 {
            Ok(ty.clone())
        } else {
            let ty = if let (LazyTypeInfo::Id(ty_id), Some(type_manager)) = (self.2, type_manager) {
                Some(type_manager.get_type(ty_id))
            } else if let LazyTypeInfo::Fetched(ty) = self.2 {
                Some(ty)
            } else {
                None
            };
            ty.ok_or(self.2)
                .and_then(|ty| ty.try_into().map_err(|_| LazyTypeInfo::Fetched(ty)))
        };

        Ok(retrieve_primitive(self.0, &value_ty?))
    }

    /// Retrieves the value from the raw pointer from a possibly structured type.
    /// Types that have fields or possible nested symbolic values need to be
    /// queried from the engines memory.
    /// # Safety
    /// The value and its dependents (like items of an array) must be retrieved
    /// within the period that the address is still pointing to the desired
    /// memory location.
    /// This should be ensured by the caller.
    pub(crate) unsafe fn retrieve(
        &self,
        type_manager: &dyn TypeManager,
        field_retriever: &dyn RawPointerRetriever,
    ) -> Result<ConcreteValueRef, LazyTypeInfo> {
        if let Some(ty) = &self.1 {
            return Ok(ConcreteValueRef::new(
                retrieve_primitive(self.0, ty).to_value_ref(),
            ));
        }

        let ty = if let LazyTypeInfo::Id(ty_id) = self.2 {
            type_manager.get_type(ty_id)
        } else if let LazyTypeInfo::Fetched(ty) = self.2 {
            ty
        } else {
            return Err(self.2);
        };

        self.retrieve_with_type(ty, type_manager, field_retriever)
    }

    pub(crate) unsafe fn retrieve_with_type(
        &self,
        ty: &TypeInfo,
        type_manager: &dyn TypeManager,
        field_retriever: &dyn RawPointerRetriever,
    ) -> Result<ConcreteValueRef, LazyTypeInfo> {
        let result = retrieve_by_type(self.0, ty, type_manager, field_retriever);

        debug_assert!(
            if let ConcreteValue::Unevaluated(UnevalValue::Lazy(_)) = &result {
                false
            } else {
                true
            },
            "RawConcreteValue should not be unevaluated after evaluation."
        );
        Ok(ConcreteValueRef::new(result.to_value_ref()))
    }
}

unsafe fn retrieve_primitive(addr: RawPointer, ty: &ValueType) -> ConstValue {
    use std::ptr::with_exposed_provenance as to_ptr;
    let addr = addr as usize;
    match ty {
        ValueType::Bool => (*(to_ptr::<bool>(addr))).into(),
        ValueType::Char => (*(to_ptr::<char>(addr))).into(),
        ValueType::Int(ty @ IntType { bit_size, .. }) => ConstValue::Int {
            bit_rep: Wrapping(retrieve_int(addr, *bit_size as usize)),
            ty: *ty,
        },
        ValueType::Float(_) => unimplemented!(),
    }
}

unsafe fn retrieve_int(addr: usize, bit_size: usize) -> u128 {
    let bytes =
        std::slice::from_raw_parts(std::ptr::with_exposed_provenance::<u8>(addr), bit_size / 8);

    #[cfg(target_endian = "big")]
    let bytes = bytes.iter();
    #[cfg(target_endian = "little")]
    let bytes = bytes.iter().rev();

    let mut result: u128 = 0;
    for byte in bytes {
        result = result << 8 | *byte as u128;
    }
    result
}

unsafe fn retrieve_by_type(
    addr: RawPointer,
    ty: &TypeInfo,
    type_manager: &dyn TypeManager,
    field_retriever: &dyn RawPointerRetriever,
) -> ConcreteValue {
    log_debug!("Retrieving value at {} of type: {:?}", addr, ty.id);
    let unsupported = || {
        unimplemented!(
            "Evaluation of raw concrete value with this type is not supported yet. {:?}",
            ty
        )
    };
    debug_assert!(
        ty.is_sized(),
        "Unsized types are not expected to be retrieved.",
    );
    if let Some(variant) = ty.as_single_variant() {
        use common::tyexp::FieldsShapeInfo::*;
        match &variant.fields {
            Array(shape) => retrieve_array(
                addr,
                shape.len as usize,
                type_manager.get_type(shape.item_ty),
                |addr| field_retriever.retrieve(addr, shape.item_ty),
            )
            .into(),
            NoFields => {
                if let Ok(ty) = ValueType::try_from(ty) {
                    retrieve_primitive(addr, &ty).into()
                } else if let Some(pointee_ty) = ty.pointee_ty {
                    debug_assert!(
                        type_manager.get_type(pointee_ty).is_sized(),
                        "Pointer with no fields is expected to be thin.",
                    );
                    retrieve_primitive(addr, &USIZE_TYPE.into()).into()
                } else {
                    unsupported()
                }
            }
            Union(..) => unsupported(),
            Struct(shape) => match ty.pointee_ty {
                None => retrieve_struct(addr, &shape.fields, field_retriever).into(),
                Some(pointee_ty) => {
                    debug_assert!(
                        !type_manager.get_type(pointee_ty).is_sized(),
                        "Pointer with struct shape is expected to be fat."
                    );
                    retrieve_fat_ptr(addr, ty.id, &shape.fields, field_retriever)
                }
            },
        }
    } else {
        unsupported()
    }
}

fn retrieve_array(
    addr: RawPointer,
    len: usize,
    item_ty: &TypeInfo,
    retrieve_addr: impl Fn(RawPointer) -> ValueRef,
) -> ArrayValue {
    /* FIXME: We can do better by querying the memory in a coarser way.
     * The current memory supports checking ranges and porter values.
     * Thus we can check for the whole array region and place those symbolic
     * values at the right indices. */
    let mut elements = Vec::with_capacity(len);
    for i in 0..(len as u64) {
        let item_addr = addr + i * item_ty.size;
        elements.push(retrieve_addr(item_addr));
    }
    ArrayValue { elements }
}

fn retrieve_struct(
    addr: RawPointer,
    fields: &[FieldInfo],
    field_retriever: &dyn RawPointerRetriever,
) -> AdtValue {
    /* FIXME: We can do better by querying the memory in a coarser way.
     * The current memory supports checking ranges and porter values.
     * Thus we can check for the whole struct region and place those symbolic
     * values at the right indices. */
    AdtValue {
        kind: super::AdtKind::Struct,
        fields: fields
            .iter()
            .map(|f| field_retriever.retrieve(addr + f.offset, f.ty))
            .map(Some)
            .map(Into::into)
            .collect(),
    }
}

fn retrieve_fat_ptr(
    addr: u64,
    type_id: TypeId,
    fields: &[FieldInfo],
    field_retriever: &dyn RawPointerRetriever,
) -> ConcreteValue {
    let mut adt = retrieve_struct(addr, fields, field_retriever);
    // FIXME: There is no guarantee for this structure.
    let metadata = adt.fields.pop().unwrap().value.unwrap(); // Field 1
    let address = adt.fields.pop().unwrap().value.unwrap(); // Field 0
    PtrValue {
        address: ConcreteValueRef::new(address),
        metadata: Some(ConcreteValueRef::new(metadata)),
        ty: type_id,
    }
    .into()
}