use super::{place::PlaceMetadata, ValueRef};

mod pointer_based;

pub(super) use pointer_based::sym_place::strategies::make_sym_place_handler;
pub(super) use pointer_based::RawPointerVariableState;

pub(crate) trait SymPlaceHandler<M = PlaceMetadata> {
    type SymPlaceValue;
    type PlaceValue;

    fn handle(&mut self, place_value: Self::SymPlaceValue, place_meta: &M) -> Self::PlaceValue;
}

impl<M, SPV, PV> SymPlaceHandler<M>
    for Box<dyn SymPlaceHandler<M, SymPlaceValue = SPV, PlaceValue = PV>>
{
    type SymPlaceValue = SPV;
    type PlaceValue = PV;

    fn handle(&mut self, place_value: Self::SymPlaceValue, place_meta: &M) -> Self::PlaceValue {
        self.as_mut().handle(place_value, place_meta)
    }
}
