use crate::{
    abs::ValueType,
    backends::basic::{
        concrete::Concretizer, config::SymbolicPlaceStrategy, expr::RawConcreteValue,
        place::PlaceMetadata, state::SymPlaceHandler, ValueRef,
    },
};

use super::{ConcreteValueRef, SymValueRef};

pub(crate) fn make_sym_place_handler(
    config: SymbolicPlaceStrategy,
    concretizer_factory: impl FnOnce() -> Box<dyn Concretizer>,
) -> Box<dyn SymPlaceHandler<PlaceMetadata>> {
    use SymbolicPlaceStrategy::*;
    match config {
        ProjExpr => Box::new(ProjExprSymPlaceHandler),
        Concretizer => Box::new(ConcretizerSymPlaceHandler),
        Stamper => Box::new(StamperSymPlaceHandler {
            concretizer: concretizer_factory(),
        }),
    }
}

struct ProjExprSymPlaceHandler;
impl<M> SymPlaceHandler<M> for ProjExprSymPlaceHandler {
    fn handle(&mut self, place_value: SymValueRef, _metadata: &M) -> ValueRef {
        // NOTE: Later we might want to handle more parts here.
        place_value.into()
    }
}

struct ConcretizerSymPlaceHandler;
impl SymPlaceHandler<PlaceMetadata> for ConcretizerSymPlaceHandler {
    fn handle(&mut self, place_value: SymValueRef, place_meta: &PlaceMetadata) -> ValueRef {
        get_concrete_value(&place_meta, place_value.as_ref().try_into().ok())
    }
}

struct StamperSymPlaceHandler {
    concretizer: Box<dyn Concretizer>,
}
impl SymPlaceHandler<PlaceMetadata> for StamperSymPlaceHandler {
    fn handle(&mut self, place_value: SymValueRef, place_meta: &PlaceMetadata) -> ValueRef {
        let conc_val = get_concrete_value(&place_meta, place_value.as_ref().try_into().ok());
        self.concretizer
            .stamp(place_value, ConcreteValueRef::new(conc_val.clone()));
        conc_val
    }
}

fn get_concrete_value(place_meta: &PlaceMetadata, ty: Option<ValueType>) -> ValueRef {
    let addr = place_meta
        .address()
        .expect("Cannot concretize a place without the raw address.");
    let ty = ty.or_else(|| place_meta.ty().cloned());
    RawConcreteValue(addr, ty).to_value_ref()
}
