use std::path::PathBuf;

use crate::options::LibfuzzerOptions;

use libafl::{
    corpus::Corpus,
    events::EventProcessor,
    inputs::{HasMutatorBytes, Input, UsesInput},
    stages::{HasNestedStageStatus, OptionalStage, Stage},
    state::{HasCorpus, HasRand, State, UsesState},
    Evaluator, HasNamedMetadata,
};
use libafl_bolts::tuples::tuple_list;
use libafl_leaf::{DivergingMutator, NonBlockingMultiMutationalStage};

pub(super) fn make_concolic_stage<S, I, E, EM, Z>(
    options: &LibfuzzerOptions,
) -> impl Stage<E, EM, Z, State = S>
where
    I: Input + HasMutatorBytes + Send + Clone + 'static,
    S: State<Input = I> + HasCorpus + HasRand + HasNamedMetadata + HasNestedStageStatus,
    <S as HasCorpus>::Corpus: Corpus<Input = I>,
    E: UsesState<State = S>,
    EM: EventProcessor<E, Z> + UsesState<State = S>,
    Z: Evaluator<E, EM> + UsesState<State = S>,
{
    let path = options.concolic_exe();
    let concolic = path
        .map(PathBuf::from)
        .map(|p| {
            DivergingMutator::new(
                PathBuf::from("leafo_onetime"),
                p,
                std::env::current_dir().unwrap(),
            )
        })
        .map(|m| NonBlockingMultiMutationalStage::new(std::borrow::Cow::Borrowed("Concolic"), m))
        .map(|s| tuple_list!(s));
    OptionalStage::new(concolic)
}
