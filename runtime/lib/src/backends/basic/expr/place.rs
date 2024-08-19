use std::rc::Rc;

use derive_more as dm;

use common::{pri::RawPointer, types::PointerOffset};

use crate::backends::basic::place::PlaceMetadata;

use super::prelude::*;

pub(crate) type PlaceValueRef = Rc<PlaceValue>;
pub(crate) type DeterPlaceValueRef = guards::DeterPlaceValueGuard<PlaceValueRef>;
pub(crate) type SymPlaceValueRef = guards::SymPlaceValueGuard<PlaceValueRef>;

#[derive(Clone, Debug, dm::From)]
pub(crate) enum PlaceValue {
    #[from]
    Deterministic(DeterministicPlaceValue),
    #[from(forward)]
    Symbolic(SymbolicPlaceValue),
}

impl PlaceValue {
    #[inline]
    pub fn is_symbolic(&self) -> bool {
        matches!(self, PlaceValue::Symbolic(..))
    }
}

#[derive(Clone, Debug, dm::Deref, dm::AsRef)]
pub(crate) struct DeterministicPlaceValue(PlaceMetadata);

impl DeterministicPlaceValue {
    pub(crate) fn new(metadata: PlaceMetadata) -> Self {
        Self(metadata)
    }

    pub(crate) fn from_addr_type(addr: RawAddress, ty: TypeId) -> Self {
        let mut metadata = PlaceMetadata::default();
        metadata.set_address(addr as RawPointer);
        metadata.set_type_id(ty);
        Self(metadata)
    }
}

#[derive(Clone, Debug)]
pub(crate) struct SymbolicPlaceValue {
    pub base: SymbolicPlaceBase,
    pub proj: Option<DeterministicProjection>,
}

impl SymbolicPlaceValue {
    pub(crate) fn from_base<B: Into<SymbolicPlaceBase>>(base: B) -> Self {
        Self {
            base: base.into(),
            proj: None,
        }
    }
}

#[derive(Clone, Debug, dm::From)]
pub(crate) enum SymbolicPlaceBase {
    Deref(DerefSymHostPlace),
    SymIndex(SymIndexedPlace),
}

#[derive(Clone, Debug)]
pub(crate) struct DeterministicProjection {
    pub offset: PointerOffset,
    pub ty_id: TypeId,
}

impl DeterministicProjection {
    pub(crate) fn on_deter(&self, base: &DeterministicPlaceValue) -> DeterministicPlaceValue {
        DeterministicPlaceValue::from_addr_type(
            base.address().wrapping_byte_add(self.offset as usize),
            self.ty_id,
        )
    }
}

#[derive(Clone, Debug, dm::From)]
pub(crate) struct DerefSymHostPlace {
    pub value: SymValueRef,
    pub metadata: PlaceMetadata,
}

#[derive(Clone, Debug)]
pub(crate) struct SymIndexedPlace {
    pub host: PlaceValueRef,
    pub host_metadata: PlaceMetadata,
    pub index: SymValueRef,
    pub index_metadata: PlaceMetadata,
}

mod guards {
    use super::super::define_guard;
    use super::*;

    macro_rules! define_place_value_guard {
        ($guarded_type:ty, $name: ident, $pattern:pat, $value_name:ident) => {
            define_guard!(
                PlaceValue,
                PlaceValueRef,
                $guarded_type,
                $name,
                $pattern,
                $value_name
            );

            impl<V> core::ops::Deref for $name<V>
            where
                V: AsRef<PlaceValue>,
            {
                type Target = $guarded_type;

                fn deref(&self) -> &Self::Target {
                    self.$value_name()
                }
            }

            impl<V> AsRef<$guarded_type> for $name<V>
            where
                Self: core::ops::Deref<Target = $guarded_type>,
            {
                fn as_ref(&self) -> &$guarded_type {
                    self
                }
            }
        };
    }

    define_place_value_guard!(
        DeterministicPlaceValue,
        DeterPlaceValueGuard,
        PlaceValue::Deterministic(place),
        place
    );

    define_place_value_guard!(
        SymbolicPlaceValue,
        SymPlaceValueGuard,
        PlaceValue::Symbolic(place),
        place
    );
}

mod convert {
    use super::*;

    impl PlaceValue {
        #[inline]
        pub fn to_value_ref(self) -> PlaceValueRef {
            Rc::new(self)
        }
    }

    macro_rules! impl_to_value_ref {
        ($($ty: ty),*) => {
            $(
                impl $ty {
                    #[inline]
                    pub(crate) fn to_value_ref(self) -> PlaceValueRef {
                        Into::<PlaceValue>::into(self).to_value_ref()
                    }
                }
            )*
        };
    }

    impl_to_value_ref!(DeterministicPlaceValue, SymbolicPlaceValue);
}
