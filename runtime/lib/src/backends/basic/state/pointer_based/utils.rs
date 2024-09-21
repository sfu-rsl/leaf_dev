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
