use crate::{
    abs::ValueType,
    backends::basic::{
        expr::{LazyTypeInfo, RawConcreteValue},
        place::PlaceMetadata,
    },
};

use super::{Address, ValueRef};

#[inline]
pub(super) fn create_lazy(addr: Address, ty: Option<ValueType>) -> ValueRef {
    RawConcreteValue(addr, ty, LazyTypeInfo::None).to_value_ref()
}

#[inline]
pub(super) fn lazy_from_meta(metadata: &PlaceMetadata) -> ValueRef {
    RawConcreteValue::from(metadata).to_value_ref()
}

impl<'a> From<&'a PlaceMetadata> for RawConcreteValue {
    #[inline]
    fn from(metadata: &'a PlaceMetadata) -> Self {
        RawConcreteValue(
            metadata.address(),
            metadata.ty().cloned(),
            metadata.type_id().into(),
        )
    }
}
