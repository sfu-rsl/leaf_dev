pub(crate) mod assignment_id;
mod body;
mod call;
mod config;
mod decision;
pub(crate) mod pri;
mod subpasses;
mod visit;

use rustc_middle::{
    mir::{BasicBlock, BasicBlockData, Body, HasLocalDecls, MirSource},
    ty::TyCtxt,
};
use rustc_span::def_id::DefId;

use std::collections::{HashMap, HashSet};

use common::{log_info, log_warn};

use crate::{
    mir_transform::{self, BodyInstrumentationUnit},
    passes::StorageExt,
};

use super::{CompilationPass, OverrideFlags, Storage};

use self::call::{Config, RuntimeCallAdder};

pub(crate) use config::InstrumentationRules;
pub(crate) use subpasses::counter::InstrumentationCounter;
pub(crate) use subpasses::rec_check::InstrumentationRecursionChecker;

const TAG_INSTRUMENTATION: &str = "instrumentation";
use TAG_INSTRUMENTATION as TAG_INSTR;

#[derive(Default)]
pub(crate) struct Instrumentor {
    rules: Option<InstrumentationRules>,
}

impl Instrumentor {
    pub(crate) fn new(filters: InstrumentationRules) -> Self {
        Self {
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

    let config = make_config(storage, tcx, def_id);
    let pri_items = pri::get_pri_items(tcx, storage);

    clear_body(body, def_id, &pri_items.all_items);

    let orig_index_map = split_blocks(body, storage);

    let mut unit = BodyInstrumentationUnit::new(body.local_decls());

    // Instrumentation
    {
        let mut call_adder = RuntimeCallAdder::new(tcx, &mut unit, &pri_items, storage, config);
        let mut call_adder = call_adder.in_body(body, orig_index_map);

        visit::instrument_body(&mut call_adder, body);
    }

    unit.commit(
        body,
        Some(|bb: &BasicBlockData<'tcx>| {
            body::sanity_check_inserted_block(bb, &pri_items.all_items)
        }),
    );

    storage.take_back(pri_items);
}

fn clear_body(body: &mut Body<'_>, def_id: DefId, all_pri_items: &HashSet<DefId>) {
    if body::clear_existing_instrumentation(body, all_pri_items) {
        /* Why is this a warning?
         * In the default configuration, we only perform instrumentation when the primary (final)
         * crate is being compiled. So we don't expect to see any existing instrumentation
         * for any body. */
        /* Still it is possible to see this happen.
         * As inlining happens over the optimized MIR (the same query that we override),
         * it is possible that a body with instrumentation is inlined into another body,
         * and then see existing instrumentation. That is why we actually clear the existing instrumentation,
         * although it is not expected to happen.
         * But doesn't this approach cause inefficiency?
         * Isn't it destructive enough to propose changes to the compiler?
         * Maybe. But we should note that LLVM optimizations with inlining are
         * still performed after our instrumentation. Therefore, unless there is definite
         * evidence that this is a performance bottleneck, we should not worry about it. */
        log_warn!("Instrumentations exist at the transformation {:?}", def_id);
    }
}

fn split_blocks<'tcx>(
    body: &mut Body<'tcx>,
    storage: &mut dyn Storage,
) -> HashMap<BasicBlock, BasicBlock> {
    mir_transform::split_blocks_with(body, body::requires_immediate_instr_after);
    let orig_index_map = body::make_orig_index_map(body, storage);
    orig_index_map
}

pub(super) fn make_config<'tcx>(
    storage: &mut dyn Storage,
    tcx: TyCtxt<'tcx>,
    def_id: DefId,
) -> Config {
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
    let pri_items = pri::get_pri_items(tcx, storage);
    body::clear_existing_instrumentation(body, &pri_items.all_items)
}
