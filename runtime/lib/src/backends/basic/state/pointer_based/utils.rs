use crate::{abs::ValueType, backends::basic::expr::RawConcreteValue};

use super::{RawPointer, ValueRef};

#[inline]
pub(super) fn create_lazy(addr: RawPointer, ty: Option<&ValueType>) -> ValueRef {
    RawConcreteValue(addr, ty.cloned()).to_value_ref()
}
