pub(crate) mod assignment_id;
mod body;
mod call;
mod config;
mod decision;
pub(crate) mod pri_utils;
mod subpasses;
mod visit;

use const_format::concatcp;

use rustc_middle::{
    mir::{self, BasicBlockData, Body, HasLocalDecls, Location, MirSource},
    ty::TyCtxt,
};
use rustc_span::def_id::DefId;

use std::{num::NonZeroUsize, sync::atomic};

use common::{log_info, log_warn};

use crate::{
    mir_transform::{self, BodyInstrumentationUnit},
    passes::{Leak, StorageExt},
};

use super::{CompilationPass, OverrideFlags, Storage};

use self::call::{
    Config, EntryFunctionHandler, FunctionHandler,
    InsertionLocation::Before,
    PlaceReferencer, RuntimeCallAdder, StorageMarker,
    context::{BodyProvider, PriItems},
    ctxt_reqs as cr,
};

pub(crate) use config::InstrumentationRules;
pub(crate) use subpasses::counter::InstrumentationCounter;
pub(crate) use subpasses::rec_check::InstrumentationRecursionChecker;

const TAG_INSTRUMENTATION: &str = "instrumentation";
use TAG_INSTRUMENTATION as TAG_INSTR;
const TAG_INSTR_COUNTER: &str = concatcp!(TAG_INSTRUMENTATION, "::counter");

const KEY_PRI_ITEMS: &str = "pri_items";
const KEY_TOTAL_COUNT: &str = "total_body_count";

#[derive(Default)]
pub(crate) struct Instrumentor {
    total_body_count: Option<NonZeroUsize>,
    rules: Option<InstrumentationRules>,
}

impl Instrumentor {
    pub(crate) fn new(
        total_body_count: Option<NonZeroUsize>,
        filters: InstrumentationRules,
    ) -> Self {
        Self {
            total_body_count,
            rules: Some(filters),
        }
    }
}

impl CompilationPass for Instrumentor {
    fn override_flags() -> OverrideFlags {
        OverrideFlags::OPTIMIZED_MIR
            | OverrideFlags::EXTERN_OPTIMIZED_MIR
            | OverrideFlags::MIR_SHIMS
    }

    fn visit_ast_before(
        &mut self,
        _krate: &rustc_ast::Crate,
        storage: &mut dyn Storage,
    ) -> rustc_driver::Compilation {
        storage.get_or_insert_with(KEY_TOTAL_COUNT.to_owned(), || self.total_body_count);
        storage.get_or_insert_with(decision::rules::KEY_RULES.to_owned(), || {
            self.rules.take().unwrap()
        });
        decision::rules::bake_rules(storage, decision::get_exceptional_exclusions);
        rustc_driver::Compilation::Continue
    }

    fn visit_mir_body_before<'tcx>(
        _tcx: TyCtxt<'tcx>,
        body: &Body<'tcx>,
        storage: &mut dyn Storage,
    ) {
        body::record_original_indices(body, storage);
    }

    fn transform_mir_body<'tcx>(
        tcx: TyCtxt<'tcx>,
        body: &mut Body<'tcx>,
        storage: &mut dyn Storage,
    ) {
        transform(tcx, body, storage);
    }
}

fn transform<'tcx>(tcx: TyCtxt<'tcx>, body: &mut Body<'tcx>, storage: &mut dyn Storage) {
    on_start(tcx, storage);

    let def_id = body.source.def_id();

    if !decision::should_instrument(tcx, body, storage) {
        log_info!(
            target: decision::TAG_INSTR_DECISION,
            "Skipping instrumentation for {:#?}",
            body.source.to_log_str(),
        );
        return;
    }

    log_info!(
        target: TAG_INSTR,
        "Running instrumentation pass on body of {} at {:?}",
        body.source.to_log_str(),
        body.span,
    );

    let pri_items = get_pri_items(tcx, storage);

    let config = make_config(storage, tcx, def_id);

    if body::clear_existing_instrumentation(body, &pri_items.all_items) {
        log_warn!("Instrumentations exist at the transformation {:?}", def_id);
    }
    mir_transform::split_blocks_with(body, body::requires_immediate_instr_after);

    let orig_index_map = body::make_orig_index_map(body, storage);

    let mut modification = BodyInstrumentationUnit::new(body.local_decls());
    let mut call_adder = RuntimeCallAdder::new(tcx, &mut modification, &pri_items, storage, config);
    let mut call_adder = call_adder.in_body(body, orig_index_map);

    let is_entry = tcx.entry_fn(()).is_some_and(|(id, _)| id == def_id);

    if is_entry {
        handle_entry_function_pre(&mut call_adder, body);
    }

    handle_body_pre_blocks(
        &mut call_adder
            .at(Before(body.basic_blocks.indices().next().unwrap()))
            .with_source_info(*body.source_info(Location::START)),
    );

    visit::instrument_body(&mut call_adder, body);

    if is_entry {
        handle_entry_function_post(&mut call_adder, body);
    }

    modification.commit(
        body,
        Some(|bb: &BasicBlockData<'tcx>| {
            body::sanity_check_inserted_block(bb, &pri_items.all_items)
        }),
    );

    pri_items.return_to(storage);
}

fn on_start(_tcx: TyCtxt, storage: &mut dyn Storage) {
    {
        static COUNTER: atomic::AtomicUsize = atomic::AtomicUsize::new(0);
        let counter = COUNTER.fetch_add(1, atomic::Ordering::SeqCst);
        let total = *storage
            .get_mut::<Option<NonZeroUsize>>(&KEY_TOTAL_COUNT.to_owned())
            .unwrap();
        let total_num: usize = total.unwrap_or(NonZeroUsize::MAX).into();
        let update_interval = total.map_or(100, |t| usize::from(t) / 100);
        if total_num - update_interval < counter || counter % update_interval == 0 {
            log_info!(
                target: TAG_INSTR_COUNTER,
                "Transforming {} / {}",
                counter,
                total.as_ref().map(NonZeroUsize::to_string).unwrap_or("?".to_owned()),
            );
        }
    }
}

fn get_pri_items<'tcx>(
    tcx: TyCtxt<'tcx>,
    storage: &mut dyn Storage,
) -> <dyn Storage as StorageExt>::Leaked<PriItems> {
    storage
        .get_or_insert_with(KEY_PRI_ITEMS.to_owned(), || make_pri_items(tcx))
        .leak()
}

fn make_pri_items(tcx: TyCtxt) -> PriItems {
    use pri_utils::*;
    let all_items = all_pri_items(tcx);
    let main_funcs = filter_main_funcs(tcx, &all_items);
    let helper_items = filter_helper_items(tcx, &all_items);
    PriItems {
        funcs: main_funcs,
        types: collect_helper_types(&helper_items),
        helper_funcs: collect_helper_funcs(helper_items),
        all_items: all_items.into_iter().collect(),
    }
}

fn make_config<'tcx>(storage: &mut dyn Storage, tcx: TyCtxt<'tcx>, def_id: DefId) -> Config {
    use decision::rules::*;
    let item = &(tcx, def_id);
    let policy = decision::rules::get_baked_policy(storage);
    let operand_info = policy.operand_info_decisions(item);
    let operand_info_filter = OperandKindRules {
        copy: operand_info.copy,
        mov: operand_info.mov,
        constant: if operand_info.constant.is_enabled() {
            Some(policy.constant_type_decisions(item))
        } else {
            None
        },
    };

    Config {
        place_info_filter: policy.place_info_decisions(item),
        operand_info_filter,
        assignment_filter: policy.assignment_decisions(item),
        storage_lifetime_filter: policy.storage_lifetime_decisions(item),
        call_flow_filter: policy.call_flow_decisions(item),
        drop_filter: policy.drop_decisions(item),
        switch_filter: policy.switch_decisions(item),
    }
}

fn handle_body_pre_blocks<'tcx, C>(call_adder: &mut RuntimeCallAdder<C>)
where
    C: cr::ForFunctionCalling<'tcx> + cr::ForStorageMarking<'tcx>,
{
    call_adder.enter_func();

    rustc_mir_dataflow::impls::always_storage_live_locals(call_adder.body())
        .iter()
        .for_each(|l| match call_adder.body().local_kind(l) {
            mir::LocalKind::Temp => {
                call_adder.mark_live(|call_adder| call_adder.reference_place(&l.into()));
            }
            mir::LocalKind::Arg => {}
            mir::LocalKind::ReturnPointer => {}
        });
}

fn handle_entry_function_pre<'tcx, C>(call_adder: &mut RuntimeCallAdder<C>, body: &Body<'tcx>)
where
    C: cr::Basic<'tcx>,
{
    let mut call_adder = call_adder.in_entry_fn();
    let first_block = body.basic_blocks.indices().next().unwrap();
    let mut call_adder = call_adder.with_source_info(*body.source_info(Location::START));
    let mut call_adder = call_adder.at(Before(first_block));
    call_adder.init_runtime_lib();
}

fn handle_entry_function_post<'tcx, C>(call_adder: &mut RuntimeCallAdder<C>, body: &Body<'tcx>)
where
    C: cr::Basic<'tcx>,
{
    let mut call_adder = call_adder.in_entry_fn();
    body.basic_blocks
        .iter_enumerated()
        .filter(|(_, bb)| bb.terminator().kind == mir::TerminatorKind::Return)
        .for_each(|(index, bb)| {
            call_adder
                .at(Before(index))
                .with_source_info(bb.terminator().source_info)
                .shutdown_runtime_lib();
        });
}

trait MirSourceExt {
    fn to_log_str(&self) -> String;
}
impl MirSourceExt for MirSource<'_> {
    fn to_log_str(&self) -> String {
        format!(
            "{:?}{}",
            self.instance,
            self.promoted
                .map(|p| format!("::promoted[{:?}]", p))
                .unwrap_or_default()
        )
    }
}

pub(crate) fn clear_existing_instrumentation<'tcx>(
    tcx: TyCtxt<'tcx>,
    body: &mut Body<'tcx>,
    storage: &mut dyn Storage,
) -> bool {
    let pri_items = get_pri_items(tcx, storage);
    body::clear_existing_instrumentation(body, &pri_items.all_items)
}
