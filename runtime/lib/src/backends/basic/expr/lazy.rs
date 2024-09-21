use common::log_debug;
use common::tyexp::{FieldInfo, TypeInfo, TypeSize};

use crate::{
    abs::{IntType, ValueType, USIZE_TYPE},
    backends::basic::alias::TypeManager,
    tyexp::{FieldsShapeInfoExt, TypeInfoExt},
};

use super::{sym_place::RawPointerRetriever, *};

mod retrieval {
    use common::tyexp::{FieldsShapeInfo, VariantInfo};

    use super::*;

    use core::num::Wrapping;

    impl RawConcreteValue {
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
            let value_ty = if let Some(ty) = &self.1 {
                Ok(ty.clone().into())
            } else {
                let ty = if let (LazyTypeInfo::Id(ty_id), Some(type_manager)) =
                    (&self.2, type_manager)
                {
                    Some(type_manager.get_type(*ty_id))
                } else if let LazyTypeInfo::Fetched(ty) = &self.2 {
                    Some(*ty)
                } else if let LazyTypeInfo::Forced(ty) = &self.2 {
                    Some(ty.as_ref())
                } else {
                    None
                };
                ty.ok_or(&self.2)
                    .and_then(|ty| ty.try_into().map_err(|_| &self.2))
            };

            Ok(retrieve_scalar(self.0, &value_ty?))
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
            if let Some(ty) = &self.1 {
                return Ok(ConcreteValueRef::new(
                    retrieve_scalar(self.0, &ty.clone().into()).to_value_ref(),
                ));
            }

            let ty = match &self.2 {
                LazyTypeInfo::Id(..) | LazyTypeInfo::Fetched(..) | LazyTypeInfo::Forced(..) => {
                    self.2.get_type(type_manager).unwrap()
                }
                LazyTypeInfo::None => return Err(&self.2),
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
                LazyTypeInfo::Id(ty_id) => Some(type_manager.get_type(*ty_id)),
                LazyTypeInfo::Fetched(ty) => Some(*ty),
                LazyTypeInfo::Forced(ty) => Some(ty.as_ref()),
                LazyTypeInfo::None => None,
            }
        }
    }

    #[derive(Debug, Clone, Copy, derive_more::From)]
    enum ScalarType {
        Bool,
        Char,
        Int(IntType),
        Float(FloatType),
        Address,
    }

    impl<'a> TryFrom<&'a TypeInfo> for ScalarType {
        type Error = ();

        fn try_from(ty: &'a TypeInfo) -> Result<Self, Self::Error> {
            use abs::USIZE_TYPE;
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
                                ..USIZE_TYPE
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
}

mod proj {
    use abs::expr::proj::{
        macros::{
            impl_general_proj_through_singulars, impl_singular_proj_through_general,
            impl_singular_projs_through_general,
        },
        ProjectionOn, Projector,
    };

    use super::*;

    pub(crate) struct RawConcreteValueProjector<'a> {
        type_manager: &'a dyn TypeManager,
        retriever: &'a dyn RawPointerRetriever,
    }

    impl<'a> RawConcreteValueProjector<'a> {
        pub fn new(
            type_manager: &'a dyn TypeManager,
            retriever: &'a dyn RawPointerRetriever,
        ) -> Self {
            Self {
                type_manager,
                retriever,
            }
        }
    }

    impl<'t> Projector for RawConcreteValueProjector<'t> {
        type HostRef<'a> = &'a RawConcreteValue;
        type Metadata<'a> = ProjMetadata;
        type FieldAccessor = FieldAccessKind;
        type HIRefPair<'a> = (Self::HostRef<'a>, usize);
        type DowncastTarget = DowncastKind;
        type Proj<'a> = ConcreteValue;

        impl_general_proj_through_singulars!();

        fn deref<'a>(
            &mut self,
            host: Self::HostRef<'a>,
            metadata: Self::Metadata<'a>,
        ) -> Self::Proj<'a> {
            /* NOTE: This shouldn't get called.
             * As this projector is currently only used during resolution of symbolic projections,
             * the ref/pointer value should be copied before this, thus will be retrieved as
             * a FatPtrValue. */
            let ty = self.get_type(host, metadata.host_type_id());
            let pointee_ty = self.type_manager.get_type(ty.pointee_ty.unwrap());
            if pointee_ty.is_sized() {
                let host = RawConcreteValue(host.0, Some(USIZE_TYPE.into()), LazyTypeInfo::None);
                let addr = Into::<ConcreteValue>::into(host.clone())
                    .expect_addr(self.type_manager, self.retriever);
                RawConcreteValue(addr, None, LazyTypeInfo::Fetched(pointee_ty)).into()
            } else if pointee_ty.is_slice() {
                let len = unsafe {
                    /* NOTE: Hacky solution to avoid complications in the structures.
                     * This is a slice pointer that holds the length next to it.
                     * u8 should be replaceable with any type. It is just used to make a slice type. */
                    let raw = *(host.0 as *const [u8; core::mem::size_of::<*const [u8]>()]);
                    let slice_ptr: *const [u8] = core::mem::transmute(raw);
                    slice_ptr.len() as u64
                };
                // Simulate an array.
                let array_ty = pseudo_array_ty_from_slice(pointee_ty, len, self.type_manager);
                RawConcreteValue(host.0, None, LazyTypeInfo::Forced(Rc::new(array_ty))).into()
            } else {
                unimplemented!("Unexpected deref over non-slice DST pointer.")
            }
        }

        fn field<'a>(
            &mut self,
            host: Self::HostRef<'a>,
            field: Self::FieldAccessor,
            metadata: Self::Metadata<'a>,
        ) -> Self::Proj<'a> {
            let ty = self.get_type(host, metadata.host_type_id());
            match field {
                FieldAccessKind::Index(index) => {
                    // TODO: Handle downcast
                    let field = &ty
                        .as_single_variant()
                        .expect("Enums are not supported yet.")
                        .fields
                        .expect_struct()
                        .fields[index as usize];
                    RawConcreteValue(
                        host.0.wrapping_byte_offset(field.offset as isize),
                        None,
                        LazyTypeInfo::Id(field.ty),
                    )
                    .into()
                }
                FieldAccessKind::PtrMetadata => {
                    let pointee_ty = self.type_manager.get_type(ty.pointee_ty.unwrap());
                    if pointee_ty.is_sized() {
                        UnevalValue::Some.into()
                    } else {
                        // FIXME: There is no guarantee for this structure.
                        self.field(host, FieldAccessKind::Index(1), metadata)
                    }
                }
            }
        }

        fn index<'a>(
            &mut self,
            (host, index): Self::HIRefPair<'a>,
            from_end: bool,
            metadata: Self::Metadata<'a>,
        ) -> Self::Proj<'a> {
            let ty = self.get_type(host, metadata.host_type_id());
            let ty_shape = ty.expect_array();
            let item_ty = self.type_manager.get_type(ty_shape.item_ty);
            let addr = host.0;
            let index = if from_end {
                assert!(
                    ty.is_sized(),
                    "Slice pointer is dereferenced without changing its type."
                );
                ty_shape.len as usize - index
            } else {
                index
            };
            RawConcreteValue(
                addr.wrapping_byte_offset((item_ty.size * (index as TypeSize)) as isize),
                None,
                LazyTypeInfo::Fetched(item_ty),
            )
            .into()
        }

        fn subslice<'a>(
            &mut self,
            host: Self::HostRef<'a>,
            from: u64,
            to: u64,
            from_end: bool,
            metadata: Self::Metadata<'a>,
        ) -> Self::Proj<'a> {
            todo!("#128")
        }

        fn downcast<'a>(
            &mut self,
            host: Self::HostRef<'a>,
            target: Self::DowncastTarget,
            _metadata: Self::Metadata<'a>,
        ) -> Self::Proj<'a> {
            match target {
                DowncastKind::EnumVariant(_) => todo!(),
                DowncastKind::Transmutation(dst_type_id) => {
                    RawConcreteValue(host.0, None, LazyTypeInfo::Id(dst_type_id)).into()
                }
            }
        }
    }

    impl<'a> RawConcreteValueProjector<'a> {
        fn get_type<'b>(&self, raw: &'b RawConcreteValue, type_id: TypeId) -> &'b TypeInfo {
            match &raw.2 {
                LazyTypeInfo::None => self.type_manager.get_type(type_id),
                LazyTypeInfo::Id(embedded_type_id) => {
                    debug_assert_eq!(
                        type_id, *embedded_type_id,
                        "Inconsistent type ids. {type_id} != {embedded_type_id}"
                    );
                    self.type_manager.get_type(type_id)
                }
                LazyTypeInfo::Fetched(ty) => ty,
                LazyTypeInfo::Forced(ty) => ty.as_ref(),
            }
        }
    }

    impl<'a> RawConcreteValueProjector<'a> {}

    pub(crate) struct FatPtrValueProjector<'a> {
        type_manager: &'a dyn TypeManager,
        retriever: &'a dyn RawPointerRetriever,
    }

    impl<'a> FatPtrValueProjector<'a> {
        pub fn new(
            type_manager: &'a dyn TypeManager,
            retriever: &'a dyn RawPointerRetriever,
        ) -> Self {
            Self {
                type_manager,
                retriever,
            }
        }
    }

    impl<'t> Projector for FatPtrValueProjector<'t> {
        type HostRef<'a> = &'a FatPtrValue;
        type Metadata<'a> = ProjMetadata;
        type FieldAccessor = FieldAccessKind;
        type HIRefPair<'a> = (Self::HostRef<'a>, ConcreteValueRef);
        type DowncastTarget = DowncastKind;
        type Proj<'a> = ConcreteValueRef;

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
            match proj_on {
                ProjectionOn::Deref(host) => self.deref(host, metadata),
                ProjectionOn::Field(host, FieldAccessKind::PtrMetadata) => host.metadata.clone(),
                ProjectionOn::Downcast(_, DowncastKind::Transmutation(..)) => {
                    unimplemented!(
                        "Transmutation of FatPtrValue is not supported but also not expected. {:?}",
                        proj_on,
                    )
                }
                ProjectionOn::Field(_, FieldAccessKind::Index(..))
                | ProjectionOn::Index(..)
                | ProjectionOn::Subslice(..)
                | ProjectionOn::Downcast(..) => {
                    unreachable!("Unexpected projection on a fat pointer. {:?}", proj_on)
                }
            }
        }

        fn deref<'a>(
            &mut self,
            host: Self::HostRef<'a>,
            metadata: Self::Metadata<'a>,
        ) -> Self::Proj<'a> {
            debug_assert_eq!(
                host.ty,
                metadata.host_type_id(),
                "Inconsistent type ids. Are you dereferencing a fat pointer multiple times?"
            );

            ConcreteValueRef::new(host.deref(self.type_manager, self.retriever).to_value_ref())
        }

        impl_singular_projs_through_general!(field, index, subslice, downcast);
    }

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
                RawConcreteValue(addr, None, LazyTypeInfo::Forced(Rc::new(array_ty)))
            } else {
                // Do we need to worry about the loss of metadata? Is it going to be processed further?
                RawConcreteValue(addr, None, LazyTypeInfo::Id(pointee_ty.id))
            };
            value
        }
    }
}

pub(super) use proj::{FatPtrValueProjector, RawConcreteValueProjector};
