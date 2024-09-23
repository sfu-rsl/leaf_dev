use crate::backends::basic::{
    concrete::Concretizer,
    config::SymbolicPlaceStrategy,
    expr::place::{DerefSymHostPlace, DeterministicPlaceValue, SymIndexedPlace, SymbolicPlaceBase},
    place::PlaceMetadata,
    state::SymPlaceHandler,
    LazyTypeInfo,
};

use super::{ConcreteValueRef, PlaceValueRef, RawConcreteValue, SymPlaceValueRef};
use common::log_debug;

pub(crate) fn make_sym_place_handler(
    config: SymbolicPlaceStrategy,
    concretizer_factory: impl FnOnce() -> Box<dyn Concretizer>,
) -> Box<
    dyn SymPlaceHandler<PlaceMetadata, SymPlaceValue = SymPlaceValueRef, PlaceValue = PlaceValueRef>,
> {
    log_debug!(
        "Creating a symbolic place handler for strategy {:?}",
        config
    );
    use SymbolicPlaceStrategy::*;
    match config {
        ProjExpression => Box::new(ProjExprSymPlaceHandler),
        Concretization => Box::new(ConcretizerSymPlaceHandler),
        Stamping => Box::new(StamperSymPlaceHandler {
            concretizer: concretizer_factory(),
        }),
    }
}

struct ProjExprSymPlaceHandler;
impl<M> SymPlaceHandler<M> for ProjExprSymPlaceHandler {
    type SymPlaceValue = SymPlaceValueRef;
    type PlaceValue = PlaceValueRef;

    fn handle(&mut self, place_value: Self::SymPlaceValue, _metadata: &M) -> Self::PlaceValue {
        place_value.into()
    }
}

struct ConcretizerSymPlaceHandler;
impl SymPlaceHandler<PlaceMetadata> for ConcretizerSymPlaceHandler {
    type SymPlaceValue = SymPlaceValueRef;
    type PlaceValue = PlaceValueRef;

    fn handle(
        &mut self,
        _place_value: Self::SymPlaceValue,
        place_meta: &PlaceMetadata,
    ) -> Self::PlaceValue {
        DeterministicPlaceValue::new(place_meta).to_value_ref()
    }
}

struct StamperSymPlaceHandler {
    concretizer: Box<dyn Concretizer>,
}
impl SymPlaceHandler<PlaceMetadata> for StamperSymPlaceHandler {
    type SymPlaceValue = SymPlaceValueRef;
    type PlaceValue = PlaceValueRef;

    fn handle(
        &mut self,
        place_value: Self::SymPlaceValue,
        place_meta: &PlaceMetadata,
    ) -> Self::PlaceValue {
        let (sym_value, meta) = match &place_value.base {
            SymbolicPlaceBase::Deref(DerefSymHostPlace {
                value: host,
                metadata: host_metadata,
            }) => (host, host_metadata),
            SymbolicPlaceBase::SymIndex(SymIndexedPlace {
                index,
                index_metadata,
                host: base,
                host_metadata: base_metadata,
            }) => {
                if base.is_symbolic() {
                    self.handle(SymPlaceValueRef::new(base.clone()), base_metadata);
                }
                (index, index_metadata)
            }
        };
        let conc_val = RawConcreteValue(
            meta.address(),
            LazyTypeInfo::from((meta.type_id(), meta.ty().copied())),
        )
        .to_value_ref();

        log_debug!(
            "Stamping symbolic value {} with concrete value {}",
            place_value,
            conc_val
        );
        self.concretizer
            .stamp(sym_value.clone(), ConcreteValueRef::new(conc_val));

        ConcretizerSymPlaceHandler.handle(place_value, place_meta)
    }
}
