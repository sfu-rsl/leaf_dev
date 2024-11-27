use derive_more as dm;

use core::fmt::Display;
use std::collections::HashMap;

use common::{log_debug, pri::BasicBlockIndex};

use crate::{
    abs::{
        backend::{Model, TraceManager},
        HasTags, Tag,
    },
    solvers::{z3::Z3Solver, MapSolverExt},
    trace::{
        divergence::{DivergenceFilter, ImmediateDivergingAnswerFinder},
        AdapterTraceManagerExt, AggregatorTraceManager, LoggerTraceManagerExt,
    },
    utils::alias::RRef,
};

use super::{
    expr::translators::z3::Z3ValueTranslator, outgen::BasicOutputGenerator, ConcreteValueRef,
    SymValueRef, SymVarId, ValueRef,
};

pub(super) type Step = BasicBlockIndex;

#[derive(Clone, Debug, dm::Deref)]
struct Tagged<T> {
    #[deref]
    value: T,
    tags: Vec<Tag>,
}

impl HasTags for Tagged<Step> {
    fn tags(&self) -> &[Tag] {
        &self.tags
    }
}

impl<T: Display> Display for Tagged<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} #[{}]", self.value, self.tags.join(", "))
    }
}

pub(super) fn new_trace_manager(
    tags: RRef<Vec<Tag>>,
    all_sym_values: RRef<HashMap<u32, (SymValueRef, ConcreteValueRef)>>,
    mut output_generator: BasicOutputGenerator,
) -> impl TraceManager<Step, ValueRef> {
    let solver = Z3Solver::<SymVarId>::new_in_global_context();
    let solver_context = solver.context;

    let model_consumer = Box::new(move |mut answers: Model<SymVarId, ValueRef>| {
        // FIXME: Performance can be improved.
        let all_sym_values = all_sym_values.borrow();
        let missing_answers = all_sym_values
            .iter()
            .filter(|(id, _)| !answers.contains_key(id))
            .map(|(id, (_, conc))| (*id, conc.clone().0))
            .collect::<Vec<_>>();
        answers.extend(missing_answers);
        output_generator.generate(&answers)
    });

    let divergence_filter = DivergenceTagFilter {
        exclude_with_any_of: vec![common::pri::tags::NO_DIVERGE],
    };

    let inspector = ImmediateDivergingAnswerFinder::new(
        solver.map_answers(ValueRef::from),
        divergence_filter,
        true,
        model_consumer,
    );

    AggregatorTraceManager::new(inspector)
        .adapt_value(|v| Z3ValueTranslator::new(solver_context).translate_from(&v))
        .into_logger()
        .adapt_step(move |s| Tagged {
            value: s,
            tags: tags.borrow().clone(),
        })
}

struct DivergenceTagFilter {
    exclude_with_any_of: Vec<Tag>,
}

impl<S: HasTags> DivergenceFilter<S> for DivergenceTagFilter {
    fn should_find(&mut self, trace: &[S]) -> bool {
        let latest = trace.last().unwrap();
        let exclude = self
            .exclude_with_any_of
            .iter()
            .any(|tag| latest.has_tag(tag));

        if exclude {
            log_debug!(
                "Filtering out step with tags {:?} from divergence",
                latest.tags()
            );
        }
        !exclude
    }
}
