use std::collections::HashMap;

use common::tyexp::{pass_core_type_names_to, CoreTypes, TypeExport, TypesData};

use crate::{
    abs::{backend::TypeManager, IntType},
    tyexp,
};

use super::{CoreTypeProvider, LazyTypeInfo, TypeId, TypeInfo, ValueType};

pub(crate) struct BasicTypeManager<'t> {
    all_types: &'t HashMap<TypeId, TypeInfo>,
    core_types: CoreTypes<&'t TypeInfo>,
}

impl<'t> BasicTypeManager<'t> {
    fn new(types: &'t TypesData) -> Self {
        Self {
            all_types: &types.all_types,
            core_types: types.core_types.map(|id| &types.all_types[&id]),
        }
    }
}

impl Default for BasicTypeManager<'static> {
    fn default() -> Self {
        Self::new(tyexp::instance::PROGRAM_TYPES.get_or_init(|| TypeExport::read().unwrap()))
    }
}

impl<'t> TypeManager for BasicTypeManager<'t> {
    type Key = TypeId;
    type Value = &'t TypeInfo;

    fn get_type(&self, key: Self::Key) -> Self::Value {
        self.all_types
            .get(&key)
            .unwrap_or_else(|| panic!("Type information was not found. TypeId: {}", key))
    }
}

macro_rules! delegate_to_core_types {
    ($($method: ident),*$(,)?) => {
        delegate::delegate! {
            to self.core_types {
                $(
                    fn $method(&self) -> &'t TypeInfo;
                )*
            }
        }
    };
}

impl<'t> CoreTypeProvider<&'t TypeInfo> for BasicTypeManager<'t> {
    pass_core_type_names_to!(delegate_to_core_types);
}

macro_rules! impl_int_type {
    ($($name: ident),*$(,)?) => {
        $(
            fn $name(&self) -> LazyTypeInfo {
                #[allow(unused_comparisons)]
                const IS_SIGNED: bool = $name::MIN < 0;
                LazyTypeInfo::IdPrimitive(
                    self.core_types.$name().id,
                    IntType {
                        bit_size: $name::BITS as u64,
                        is_signed: IS_SIGNED,
                    }.into(),
                )
            }
        )*
    };
}
macro_rules! delegate_to_type_info {
    ($($method: ident),*$(,)?) => {
        delegate::delegate! {
            to self {
                $(
                    #[into]
                    fn $method(&self) -> LazyTypeInfo;
                )*
            }
        }
    };
}

impl CoreTypeProvider<LazyTypeInfo> for BasicTypeManager<'_>
where
    Self: CoreTypeProvider<&'static TypeInfo>,
{
    fn bool(&self) -> LazyTypeInfo {
        LazyTypeInfo::IdPrimitive(self.core_types.bool().id, ValueType::Bool)
    }

    fn char(&self) -> LazyTypeInfo {
        LazyTypeInfo::IdPrimitive(self.core_types.char().id, ValueType::Char)
    }

    impl_int_type!(
        i8, i16, i32, i64, i128, isize, u8, u16, u32, u64, u128, usize
    );

    delegate_to_type_info!(f16, f32, f64, f128, raw_addr, raw_mut_addr);
}
