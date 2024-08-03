use common::types::RawAddress;

use crate::{
    abs::ValueType,
    backends::basic::{
        expr::{LazyTypeInfo, RawConcreteValue},
        place::PlaceMetadata,
    },
};

use super::ValueRef;

#[inline]
pub(super) fn create_lazy(addr: u64, ty: Option<ValueType>) -> ValueRef {
    let addr = addr as RawAddress; // FIXME
    RawConcreteValue(addr, ty, LazyTypeInfo::None).to_value_ref()
}

#[inline]
pub(super) fn lazy_from_meta(metadata: &PlaceMetadata) -> Result<ValueRef, &'static str> {
    RawConcreteValue::try_from(metadata).map(RawConcreteValue::to_value_ref)
}

impl<'a> TryFrom<&'a PlaceMetadata> for RawConcreteValue {
    type Error = &'static str;

    #[inline]
    fn try_from(metadata: &'a PlaceMetadata) -> Result<Self, Self::Error> {
        let addr = metadata.address().ok_or("Address is not available")?;
        let addr = addr as RawAddress; // FIXME
        Ok(RawConcreteValue(
            addr,
            metadata.ty().cloned(),
            metadata.type_id().into(),
        ))
    }
}
