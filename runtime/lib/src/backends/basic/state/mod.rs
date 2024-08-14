use super::{
    alias::SymValueRefProjector as SymbolicProjector, place::PlaceMetadata, Projection,
    SymValueRef, ValueRef,
};

mod pointer_based;
pub(super) mod proj;

/// A projection that its possible index is resolved.
type ResolvedProjection = Projection<ValueRef>;

pub(super) use pointer_based::sym_place::make_sym_place_handler;
pub(super) use pointer_based::RawPointerVariableState;

pub(crate) trait SymPlaceHandler<M = PlaceMetadata> {
    fn handle(&mut self, place_value: SymValueRef, place_meta: &M) -> ValueRef;
}

impl<M> SymPlaceHandler<M> for Box<dyn SymPlaceHandler<M>> {
    fn handle(&mut self, place_value: SymValueRef, place_meta: &M) -> ValueRef {
        self.as_mut().handle(place_value, place_meta)
    }
}
