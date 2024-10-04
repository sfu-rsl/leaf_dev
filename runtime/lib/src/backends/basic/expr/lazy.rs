use common::tyexp::{FieldInfo, TypeInfo};
use common::{log_debug, log_warn};

use crate::{
    abs::{IntType, ValueType},
    backends::basic::alias::TypeManager,
    tyexp::TypeInfoExt,
};

use super::*;

mod retrieval {
    use common::{
        log_warn,
        tyexp::{FieldsShapeInfo, VariantInfo},
    };

    use super::*;

    use core::num::Wrapping;

    pub(crate) trait RawPointerRetriever {
        fn retrieve(&self, addr: RawAddress, type_id: TypeId) -> ValueRef;
    }

    impl RawConcreteValue {
        pub(crate) fn type_as_scalar(
            &self,
            type_manager: Option<&dyn TypeManager>,
        ) -> Result<ScalarType, &LazyTypeInfo> {
            if let LazyTypeInfo::IdPrimitive(_, value_ty) = &self.1 {
                Ok(value_ty.clone().into())
            } else {
                let ty = if let (LazyTypeInfo::Id(ty_id), Some(type_manager)) =
                    (&self.1, type_manager)
                {
                    Some(type_manager.get_type(*ty_id))
                } else if let LazyTypeInfo::Fetched(ty) = &self.1 {
                    Some(*ty)
                } else if let LazyTypeInfo::Forced(ty) = &self.1 {
                    Some(ty.as_ref())
                } else {
                    None
                };
                ty.ok_or(&self.1)
                    .and_then(|ty| ty.try_into().map_err(|_| &self.1))
            }
        }

        /// Tries to retrieve the value from a scalar type.
        /// Scalar types have no fields and no risk of nested symbolic value.
        /// Thus, they can be directly retrieved from memory.
        /// Returns the value if the type information is available.
        /// # Safety
        /// The value must be retrieved within the period that the address is still
        /// pointing to the desired memory location.
        /// This should be ensured by the caller.
        pub(crate) unsafe fn try_retrieve_as_scalar(
            &self,
            type_manager: Option<&dyn TypeManager>,
        ) -> Result<ConstValue, &LazyTypeInfo> {
            self.type_as_scalar(type_manager)
                .map(|ty| retrieve_scalar(self.0, &ty))
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
        ) -> Result<ConcreteValueRef, &LazyTypeInfo> {
            let ty = match &self.1 {
                LazyTypeInfo::IdPrimitive(_, ty) => {
                    return Ok(ConcreteValueRef::new(
                        retrieve_scalar(self.0, &ty.clone().into()).to_value_ref(),
                    ));
                }
                LazyTypeInfo::Id(..) | LazyTypeInfo::Fetched(..) | LazyTypeInfo::Forced(..) => {
                    self.1.get_type(type_manager).unwrap()
                }
                LazyTypeInfo::None => return Err(&self.1),
            };

            Ok(self.retrieve_with_type(ty, type_manager, field_retriever))
        }

        unsafe fn retrieve_with_type(
            &self,
            ty: &TypeInfo,
            type_manager: &dyn TypeManager,
            field_retriever: &dyn RawPointerRetriever,
        ) -> ConcreteValueRef {
            let result = retrieve_by_type(self.0, ty, type_manager, field_retriever);

            debug_assert!(
                if let ConcreteValue::Unevaluated(UnevalValue::Lazy(_)) = &result {
                    false
                } else {
                    true
                },
                "RawConcreteValue should not be unevaluated after evaluation."
            );
            ConcreteValueRef::new(result.to_value_ref())
        }
    }

    impl LazyTypeInfo {
        #[inline]
        pub(crate) fn get_type(&self, type_manager: &dyn TypeManager) -> Option<&TypeInfo> {
            match self {
                LazyTypeInfo::Id(ty_id) | LazyTypeInfo::IdPrimitive(ty_id, _) => {
                    Some(type_manager.get_type(*ty_id))
                }
                LazyTypeInfo::Fetched(ty) => Some(*ty),
                LazyTypeInfo::Forced(ty) => Some(ty.as_ref()),
                LazyTypeInfo::None => None,
            }
        }
    }

    #[derive(Debug, Clone, Copy, derive_more::From)]
    pub(crate) enum ScalarType {
        Bool,
        Char,
        Int(IntType),
        Float(FloatType),
        Address,
    }

    impl<'a> TryFrom<&'a TypeInfo> for ScalarType {
        type Error = ();

        fn try_from(ty: &'a TypeInfo) -> Result<Self, Self::Error> {
            // TODO: To be replaced with a well-cached implementation
            let name = ty.name.as_str();
            match name {
                "bool" => Ok(ScalarType::Bool),
                "char" => Ok(ScalarType::Char),
                _ if name.starts_with('i') || name.starts_with('u') => name[1..]
                    .parse()
                    .map(|bit_size| {
                        IntType {
                            bit_size,
                            is_signed: name.starts_with('i'),
                        }
                        .into()
                    })
                    .or_else(|_| {
                        if name[1..] == *"size" {
                            Ok(IntType {
                                is_signed: name.starts_with('i'),
                                ..IntType::USIZE
                            }
                            .into())
                        } else {
                            Err(())
                        }
                    }),
                "f32" | "f64" => unimplemented!(),
                _ if ty.pointee_ty.is_some() => match ty.as_single_variant() {
                    Some(VariantInfo {
                        fields: FieldsShapeInfo::NoFields,
                        ..
                    }) => Ok(ScalarType::Address),
                    _ => Err(()),
                },
                _ => Err(()),
            }
        }
    }

    impl From<ValueType> for ScalarType {
        fn from(ty: ValueType) -> Self {
            match ty {
                ValueType::Bool => ScalarType::Bool,
                ValueType::Char => ScalarType::Char,
                ValueType::Int(ty) => ScalarType::Int(ty),
                ValueType::Float(ty) => ScalarType::Float(ty),
            }
        }
    }

    impl From<ScalarType> for ValueType {
        fn from(value: ScalarType) -> Self {
            match value {
                ScalarType::Bool => ValueType::Bool,
                ScalarType::Char => ValueType::Char,
                ScalarType::Int(ty) => ValueType::Int(ty),
                ScalarType::Float(ty) => ValueType::Float(ty),
                ScalarType::Address => {
                    log_warn!("Converting address type to int type.");
                    ValueType::Int(IntType {
                        bit_size: std::mem::size_of::<usize>() as u64 * 8,
                        is_signed: false,
                    })
                }
            }
        }
    }

    impl ScalarType {
        fn bit_size(&self) -> u64 {
            match self {
                ScalarType::Bool => core::mem::size_of::<bool>() as u64 * 8,
                ScalarType::Char => core::mem::size_of::<char>() as u64 * 8,
                ScalarType::Int(ty) => ty.bit_size,
                ScalarType::Float(ty) => ty.e_bits + ty.s_bits,
                ScalarType::Address => core::mem::size_of::<*const ()>() as u64 * 8,
            }
        }
    }

    unsafe fn retrieve_scalar(addr: RawAddress, ty: &ScalarType) -> ConstValue {
        use std::ptr::with_exposed_provenance as to_ptr;
        let addr = addr as usize;
        match ty {
            ScalarType::Bool => (*(to_ptr::<bool>(addr))).into(),
            ScalarType::Char => (*(to_ptr::<char>(addr))).into(),
            ScalarType::Int(ty @ IntType { bit_size, .. }) => ConstValue::Int {
                bit_rep: Wrapping(retrieve_int(addr, *bit_size as usize)),
                ty: *ty,
            },
            ScalarType::Float(_) => unimplemented!(),
            ScalarType::Address => ConstValue::Addr(*(to_ptr::<*const ()>(addr))),
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
        addr: RawAddress,
        ty: &TypeInfo,
        type_manager: &dyn TypeManager,
        field_retriever: &dyn RawPointerRetriever,
    ) -> ConcreteValue {
        log_debug!("Retrieving value at {:p} of type: {:?}", addr, ty.id);
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
                    if let Ok(ty) = ScalarType::try_from(ty) {
                        retrieve_scalar(addr, &ty).into()
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
        addr: RawAddress,
        len: usize,
        item_ty: &TypeInfo,
        retrieve_addr: impl Fn(RawAddress) -> ValueRef,
    ) -> ArrayValue {
        /* FIXME: We can do better by querying the memory in a coarser way.
         * The current memory supports checking ranges and porter values.
         * Thus we can check for the whole array region and place those symbolic
         * values at the right indices. */
        let mut elements = Vec::with_capacity(len);
        for i in 0..(len as u64) {
            let item_addr = addr.wrapping_byte_offset((i * item_ty.size) as isize);
            elements.push(retrieve_addr(item_addr));
        }
        ArrayValue { elements }
    }

    fn retrieve_struct(
        addr: RawAddress,
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
                .map(|f| {
                    field_retriever.retrieve(addr.wrapping_byte_offset(f.offset as isize), f.ty)
                })
                .map(Some)
                .map(Into::into)
                .collect(),
        }
    }

    fn retrieve_fat_ptr(
        addr: RawAddress,
        type_id: TypeId,
        fields: &[FieldInfo],
        field_retriever: &dyn RawPointerRetriever,
    ) -> ConcreteValue {
        let mut adt = retrieve_struct(addr, fields, field_retriever);
        // FIXME: There is no guarantee for this structure.
        let metadata = adt.fields.pop().unwrap().value.unwrap(); // Field 1
        let address = adt.fields.pop().unwrap().value.unwrap(); // Field 0
        FatPtrValue {
            address: ConcreteValueRef::new(address),
            metadata: ConcreteValueRef::new(metadata),
            ty: type_id,
        }
        .into()
    }

    impl PorterValue {
        pub(crate) fn try_to_masked_value(
            &self,
            type_manager: &dyn TypeManager,
        ) -> Result<SymValueRef, &LazyTypeInfo> {
            let whole_ty = self.as_concrete.type_as_scalar(Some(type_manager))?;

            log_debug!(
                "Making a masked value from {} symbolic values",
                self.sym_values.len()
            );

            let whole_value = unsafe { retrieve_scalar(self.as_concrete.0, &whole_ty) };
            let whole_value = Self::bit_rep_const(whole_value, &whole_ty);
            let whole_bit_size = whole_ty.bit_size() as u32;
            let whole_value_ty = ValueType::from(whole_ty);

            let result = self.sym_values.iter().fold(
                whole_value.to_value_ref(),
                |value, (at, ty_id, sym_value)| {
                    let ty = type_manager.get_type(*ty_id);
                    let sym_value = if let Some(value_ty) = ValueType::try_from(sym_value.as_ref())
                        .ok()
                        .map(Into::into)
                        .or_else(|| ScalarType::try_from(ty).ok())
                    {
                        Self::bit_rep_sym(sym_value.clone(), &value_ty)
                    } else {
                        sym_value.clone()
                    };

                    let value_bit_size = ty.size as u32 * u8::BITS;
                    let at_bit = *at as u32 * u8::BITS;

                    let sym_value = self.as_mask(
                        sym_value,
                        at_bit,
                        value_bit_size,
                        whole_bit_size,
                        whole_value_ty.clone(),
                    );

                    if at_bit == 0 && value_bit_size >= whole_bit_size {
                        return sym_value.into();
                    }
                    BinaryExpr {
                        operator: BinaryOp::BitAnd,
                        operands: SymBinaryOperands::Rev {
                            second: sym_value,
                            first: value,
                        },
                    }
                    .to_value_ref()
                    .into()
                },
            );

            let result = SymValueRef::new(result);

            let result = if let ScalarType::Bool = whole_ty {
                Self::bit_rep_to_bool(result)
            } else {
                result
            };

            Ok(result)
        }

        fn bit_rep_const(value: ConstValue, ty: &ScalarType) -> ConstValue {
            match (&value, ty) {
                // FIXME: Duplication with expression builder for cast.
                // FIXME: This might not be necessarily the bit representation.
                // FIXME: The whole value is a single byte.
                (ConstValue::Bool(value), ScalarType::Bool) => (*value as u8).into(),
                (ConstValue::Char(value), ScalarType::Char) => (*value as u32).into(),
                (ConstValue::Int { .. }, ScalarType::Int(..) | ScalarType::Address) => value,
                (ConstValue::Float { .. }, ScalarType::Float(..)) => value,
                _ => unreachable!("Unexpected value and type pair: {:?} {:?}", value, ty),
            }
        }

        fn bit_rep_sym(value: SymValueRef, ty: &ScalarType) -> SymValueRef {
            match ty {
                // FIXME: Duplication with expression builder for cast.
                ScalarType::Bool => Expr::Ite {
                    condition: value,
                    if_target: ConstValue::from(1_u8).to_value_ref(),
                    else_target: ConstValue::from(0_u8).to_value_ref(),
                }
                .to_value_ref(),
                ScalarType::Char
                | ScalarType::Int(..)
                | ScalarType::Float(..)
                | ScalarType::Address => value,
            }
        }

        /// Creates a mask from a symbolic value.
        /// By mask, we mean a value that if bitwise ANDed with the whole value,
        /// it will only keep the bits that are filled by the symbolic value at the specified locations.
        /// # Example
        /// If a symbolic u8 value at byte offset 1 in little-endian order is requested to be masked
        /// for a u32 value, the mask will be 0x0000xx00, where xx is the symbolic value.
        fn as_mask(
            &self,
            value: SymValueRef,
            at_bit: u32,
            value_bit_size: u32,
            whole_bit_size: u32,
            dst_ty: ValueType,
        ) -> SymValueRef {
            let mut result = value;

            // Extract the first n bits of the value.
            // (the equal case handles possible casting)
            if at_bit + value_bit_size >= whole_bit_size {
                let n = whole_bit_size - at_bit;
                debug_assert!(n % u8::BITS as u32 == 0, "Not aligned by byte {:?}", self);
                // We know want the first n bits of the value,
                // but the order of the bytes is as same as the whole object.
                #[cfg(target_endian = "big")]
                let (high, low) = (value_bit_size, value_bit_size - n);
                #[cfg(target_endian = "little")]
                let (high, low) = (n, 0u32);
                if low > 0 {
                    result = BinaryExpr {
                        operator: BinaryOp::Shr,
                        operands: SymBinaryOperands::Orig {
                            first: result,
                            second: ConstValue::from(low).to_value_ref(),
                        },
                    }
                    .to_value_ref();
                }
                result = Expr::Truncation {
                    source: result,
                    high: high - 1,
                    ty: IntType {
                        bit_size: n as u64,
                        is_signed: false,
                    }
                    .into(),
                }
                .to_value_ref();
            }

            if whole_bit_size > value_bit_size {
                result = Expr::Extension {
                    source: result,
                    is_zero_ext: true,
                    bits_to_add: (whole_bit_size - value_bit_size) as u32,
                    ty: dst_ty,
                }
                .to_value_ref();

                // We know that the value fills offset..offset+value_size,
                // but the order of the bytes is as same as the whole object.
                #[cfg(target_endian = "big")]
                let shift_amount: u32 = whole_bit_size - at_bit as u32 - value_bit_size;
                #[cfg(target_endian = "little")]
                let shift_amount: u32 = at_bit as u32;
                if shift_amount > 0 {
                    result = BinaryExpr {
                        operator: BinaryOp::Shl,
                        operands: SymBinaryOperands::Orig {
                            first: result,
                            second: ConstValue::from(shift_amount).to_value_ref(),
                        },
                    }
                    .to_value_ref();
                }
            }

            result
        }

        fn bit_rep_to_bool(result: SymValueRef) -> SymValueRef {
            BinaryExpr {
                operator: BinaryOp::Ne,
                operands: SymBinaryOperands::Orig {
                    first: result,
                    second: ConstValue::from(0_u8).to_value_ref(),
                },
            }
            .to_value_ref()
        }
    }
}

pub(crate) use retrieval::RawPointerRetriever;

mod proj {
    use super::*;

    fn pseudo_array_ty_from_slice(
        slice_ty: &TypeInfo,
        len: u64,
        type_manager: &dyn TypeManager,
    ) -> TypeInfo {
        debug_assert!(slice_ty.is_slice());
        let item_ty = type_manager.get_type(slice_ty.expect_array().item_ty);
        let array_ty =
            TypeInfo::new_pseudo_array_from_slice(&slice_ty, len, item_ty.align, item_ty.size);
        log_debug!("Pseudo array type: {:?}", array_ty);
        array_ty
    }

    impl FatPtrValue {
        pub(crate) fn deref(
            &self,
            type_manager: &dyn TypeManager,
            retriever: &dyn RawPointerRetriever,
        ) -> RawConcreteValue {
            let addr = self.address.expect_addr(type_manager, retriever);
            let ty = type_manager.get_type(self.ty);
            let pointee_ty = type_manager.get_type(ty.pointee_ty.unwrap());
            let value = if pointee_ty.is_slice() {
                // Simulate an array.
                let len = self
                    .metadata
                    .expect_int(type_manager, retriever)
                    .try_into()
                    .unwrap();
                let array_ty = pseudo_array_ty_from_slice(pointee_ty, len, type_manager);
                RawConcreteValue(addr, LazyTypeInfo::Forced(Rc::new(array_ty)))
            } else {
                // Do we need to worry about the loss of metadata? Is it going to be processed further?
                RawConcreteValue(addr, LazyTypeInfo::Id(pointee_ty.id))
            };
            value
        }
    }

    impl ConcreteValue {
        pub(crate) fn expect_int(
            &self,
            type_manager: &dyn TypeManager,
            retriever: &dyn RawPointerRetriever,
        ) -> u128 {
            match self {
                ConcreteValue::Const(ConstValue::Int { bit_rep, .. }) => bit_rep.0,
                ConcreteValue::Unevaluated(UnevalValue::Lazy(raw)) => {
                    unsafe { raw.retrieve(type_manager, retriever) }
                        .unwrap()
                        .expect_int(type_manager, retriever)
                }
                _ => panic!("Not an integer: {:?}", self),
            }
        }

        pub(crate) fn expect_addr(
            &self,
            type_manager: &dyn TypeManager,
            retriever: &dyn RawPointerRetriever,
        ) -> RawAddress {
            match self {
                ConcreteValue::Const(ConstValue::Addr(addr)) => *addr,
                ConcreteValue::Unevaluated(UnevalValue::Lazy(raw)) => {
                    unsafe { raw.retrieve(type_manager, retriever) }
                        .unwrap()
                        .expect_addr(type_manager, retriever)
                }
                _ => panic!("Not an address: {:?}", self),
            }
        }
    }
}
