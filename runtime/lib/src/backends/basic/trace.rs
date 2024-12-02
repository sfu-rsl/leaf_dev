use derive_more as dm;

use core::fmt::Display;

use common::{log_debug, pri::BasicBlockLocation};

use crate::{
    abs::{
        backend::{Model, TraceManager},
        Constraint, HasTags, Tag,
    },
    solvers::{z3::Z3Solver, MapSolverExt},
    trace::{
        divergence::{DivergenceFilter, ImmediateDivergingAnswerFinder},
        inspect::CompoundTraceInspector,
        sanity_check::ConstraintSanityChecker,
        AdapterTraceManagerExt, AggregatorTraceManager, LoggerTraceManagerExt, TraceInspector,
    },
    utils::alias::RRef,
};

use super::{
    expr::translators::z3::Z3ValueTranslator, outgen::BasicOutputGenerator,
    sym_vars::SymVariablesManager, Solver, SymVarId, ValueRef,
};

pub(super) type Step = BasicBlockLocation;

type CurrentSolver<'ctx> = Z3Solver<'ctx, SymVarId>;
type CurrentSolverValue<'ctx> = <Z3Solver<'ctx, SymVarId> as Solver>::Value;
type CurrentSolverTranslator<'ctx> = Z3ValueTranslator<'ctx>;

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
        if self.tags.is_empty() {
            return self.value.fmt(f);
        } else {
            write!(f, "{} #[{}]", self.value, self.tags.join(", "))
        }
    }
}

pub(super) fn new_trace_manager(
    tags: RRef<Vec<Tag>>,
    sym_var_manager: RRef<impl SymVariablesManager + 'static>,
    output_generator: BasicOutputGenerator,
) -> impl TraceManager<Step, ValueRef> {
    let solver: CurrentSolver = Z3Solver::<SymVarId>::new_in_global_context();
    let translator = Z3ValueTranslator::new(solver.context);
    let sym_var_manager_ref = sym_var_manager;

    let model_consumer = create_model_consumer(sym_var_manager_ref.clone(), output_generator);
    let divergence_filter = DivergenceTagFilter {
        exclude_with_any_of: vec![common::pri::tags::NO_DIVERGE],
    };

    let mut inspectors: Vec<Box<dyn TraceInspector<_, _>>> = Vec::new();

    inspectors.push(Box::new(ConstraintSanityChecker::new(
        solver.clone(),
        ConcretizationConstraintsCache::new(sym_var_manager_ref.clone(), translator.clone()),
    )));

    inspectors.push(Box::new(ImmediateDivergingAnswerFinder::new(
        solver.clone().map_answers(ValueRef::from),
        divergence_filter,
        false,
        Box::new(model_consumer),
    )));

    let inspector = CompoundTraceInspector::new(inspectors);
    AggregatorTraceManager::new(inspector)
        .adapt_value(move |v| translator.clone().translate(&v))
        .into_logger()
        .adapt_step(move |s| Tagged {
            value: s,
            tags: tags.borrow().clone(),
        })
}

fn create_model_consumer(
    sym_var_manager: RRef<impl SymVariablesManager>,
    mut output_generator: BasicOutputGenerator,
) -> impl FnMut(Model<SymVarId, ValueRef>) {
    move |mut model| {
        // Add missing answers.
        // FIXME: Performance can be improved.
        let all_sym_values = sym_var_manager.borrow();
        let missing_answers = all_sym_values
            .iter_variables()
            .filter(|(id, _, _)| !model.contains_key(id))
            .map(|(id, _, conc)| (*id, conc.clone().0))
            .collect::<Vec<_>>();
        model.extend(missing_answers);

        output_generator.generate(&model)
    }
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

struct ConcretizationConstraintsCache<M: SymVariablesManager, T, V> {
    manager: RRef<M>,
    translator: T,
    constraints: Vec<Constraint<V>>,
}

impl<M: SymVariablesManager, T, V> ConcretizationConstraintsCache<M, T, V> {
    fn new(manager: RRef<M>, translator: T) -> Self {
        Self {
            manager,
            translator,
            constraints: Vec::new(),
        }
    }
}

impl<'a, 'ctx: 'a, M: SymVariablesManager> IntoIterator
    for &'a mut ConcretizationConstraintsCache<
        M,
        CurrentSolverTranslator<'ctx>,
        CurrentSolverValue<'ctx>,
    >
// FIXME: Could not make generics work here.
{
    type Item = &'a Constraint<CurrentSolverValue<'ctx>>;

    type IntoIter = core::slice::Iter<'a, Constraint<CurrentSolverValue<'ctx>>>;

    fn into_iter(self) -> Self::IntoIter {
        let manager: std::cell::Ref<'a, M> = self.manager.borrow();
        let all_constraints = manager.iter_concretization_constraints();
        if all_constraints.len() > self.constraints.len() {
            self.constraints.extend(
                all_constraints
                    .skip(self.constraints.len())
                    .map(|(_, v)| Constraint::Bool(self.translator.call_mut((v,)))),
            );
        }

        self.constraints.iter()
    }
}
