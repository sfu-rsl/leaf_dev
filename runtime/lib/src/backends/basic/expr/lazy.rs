use core::num::Wrapping;

use common::{pri::RawPointer, tyexp::TypeInfo};

use crate::{
    abs::{IntType, ValueType},
    backends::basic::{alias::TypeManager, UnevalValue},
    tyexp::TypeInfoExt,
};

use super::{
    sym_place::RawPointerRetriever, ArrayValue, ConcreteValue, ConcreteValueRef, ConstValue,
    LazyTypeInfo, RawConcreteValue, ValueRef,
};

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
    ty: &'static TypeInfo,
    type_manager: &dyn TypeManager,
    field_retriever: &dyn RawPointerRetriever,
) -> ConcreteValue {
    let unsupported = || {
        unimplemented!(
            "Evaluation of raw concrete value with this type is not supported yet. {:?}",
            ty
        )
    };
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
                } else {
                    unsupported()
                }
            }
            Union | Struct(_) => unsupported(),
        }
    } else {
        unsupported()
    }
}

fn retrieve_array(
    addr: RawPointer,
    len: usize,
    item_ty: &'static TypeInfo,
    retrieve_addr: impl Fn(RawPointer) -> ValueRef,
) -> ArrayValue {
    let mut elements = Vec::with_capacity(len);
    for i in 0..(len as u64) {
        let item_addr = addr + i * item_ty.size;
        elements.push(retrieve_addr(item_addr));
    }
    ArrayValue { elements }
}
