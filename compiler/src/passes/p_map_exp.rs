use rustc_middle::{
    mir::{BasicBlock, Body, HasLocalDecls},
    ty::{InstanceKind, TyCtxt},
};

use common::{
    directed::{
        BasicBlockIndex, CallDebugInfo, CallGraphEdgeDestination, CfgConstraint,
        CfgEdgeDestination, ControlFlowGraph, ProgramMap,
    },
    log_info,
};

use super::{CompilationPass, OverrideFlags, Storage, StorageExt};
use crate::utils::{
    file::TyCtxtFileExt,
    mir::{InstanceKindExt, TyCtxtExt},
};

type Calls = Vec<CallGraphEdgeDestination>;
type ReturnPoints = Vec<BasicBlockIndex>;

#[derive(Default)]
pub(crate) struct ProgramMapExporter;

const KEY_MAP: &str = "program_map";

const FILE_OUTPUT: &str = "program_map.json";

impl CompilationPass for ProgramMapExporter {
    fn override_flags() -> super::OverrideFlags {
        OverrideFlags::OPTIMIZED_MIR
            | OverrideFlags::EXTERN_OPTIMIZED_MIR
            | OverrideFlags::MIR_SHIMS
            | OverrideFlags::MAKE_CODEGEN_BACKEND
    }

    fn visit_mir_body_before<'tcx>(
        tcx: TyCtxt<'tcx>,
        body: &Body<'tcx>,
        storage: &mut dyn Storage,
    ) {
        let mut p_map = storage.get_or_default::<ProgramMap>(KEY_MAP.to_owned());
        visit_and_add(&mut p_map, tcx, body);
    }

    fn visit_tcx_at_codegen_after(&mut self, tcx: TyCtxt, storage: &mut dyn Storage) {
        log_info!("Exporting program map");

        let mut p_map = storage.get_or_default::<ProgramMap>(KEY_MAP.to_owned());

        tcx.collect_and_partition_mono_items(())
            .codegen_units
            .iter()
            .flat_map(|unit| unit.items())
            .flat_map(|(item, _)| match item {
                rustc_middle::mir::mono::MonoItem::Fn(instance) => Some(instance.def),
                _ => None,
            })
            .for_each(|instance| {
                visit_and_add(&mut p_map, tcx, tcx.instance_mir(instance));
            });

        p_map.entry_points.extend(
            tcx.entry_fn(())
                .iter()
                .map(|(def_id, _)| InstanceKind::Item(*def_id).to_plain_id()),
        );

        p_map
            .write(tcx.output_dir().join(FILE_OUTPUT))
            .expect("Failed to write program map");
    }
}

fn visit_and_add<'tcx>(p_map: &mut ProgramMap, tcx: TyCtxt<'tcx>, body: &Body<'tcx>) {
    let key = body.source.instance.to_plain_id();
    if p_map.cfgs.contains_key(&key) {
        return;
    }

    let data = visit_body(tcx, &body);
    p_map.cfgs.insert(key, data.0);
    p_map.ret_points.insert(key, data.1);
    p_map.call_graph.insert(key, data.2);
    p_map
        .debug_info
        .func_names
        .insert(key, tcx.def_path_str(body.source.def_id()));
}

fn visit_body<'tcx>(
    tcx: TyCtxt<'tcx>,
    body: &Body<'tcx>,
) -> (ControlFlowGraph, ReturnPoints, Calls) {
    let mut cfg = ControlFlowGraph::new();
    let mut ret_points = Vec::new();
    let mut calls = Calls::new();

    let predecessors_map = body.basic_blocks.predecessors();
    // FIXME: Can be replaced with a preprocessed map.
    let merged_chain_head = |bb: BasicBlock| {
        let mut current = bb;
        // Walk back goto predecessors.
        while let Some(prev) = {
            let predecessors = &predecessors_map[current];
            if predecessors.len() != 1 {
                None
            } else {
                let prev = predecessors[0];
                body.basic_blocks[prev]
                    .terminator()
                    .kind
                    .as_goto()
                    .is_some()
                    .then_some(prev)
            }
        } {
            current = prev;
        }
        current
    };

    for (index, block) in body.basic_blocks.iter_enumerated() {
        use rustc_middle::mir::TerminatorKind::*;

        let mut insert_to_cfg = |targets: Vec<CfgEdgeDestination>| {
            cfg.insert(
                merged_chain_head(index).as_u32(),
                targets
                    .into_iter()
                    .map(|(bb, c)| (merged_chain_head(bb.into()).as_u32(), c))
                    .collect(),
            );
        };

        let mut insert_to_calls = |def_id, generic_args, dbg| {
            if let Ok(Some(instance_kind)) = tcx.resolve_instance_raw(
                tcx.typing_env_in_body(body.source.def_id())
                    .as_query_input((def_id, generic_args)),
            ) {
                calls.push((index.as_u32(), instance_kind.def.to_plain_id(), dbg));
            }
        };

        // FIXME: Utilize the `edges` instead.
        match &block.terminator().kind {
            FalseEdge {
                real_target: target,
                imaginary_target: _,
            }
            | FalseUnwind {
                real_target: target,
                unwind: _,
            }
            | Drop {
                target, unwind: _, ..
            }
            | Goto { target } => {
                insert_to_cfg(vec![(target.as_u32(), None)]);
            }
            SwitchInt { discr: _, targets } => {
                let mut successors: Vec<CfgEdgeDestination> = Vec::new();
                for (value, block) in targets.iter() {
                    successors.push((block.as_u32(), Some(CfgConstraint::Case(value))));
                }
                if !body.basic_blocks[targets.otherwise()].is_empty_unreachable() {
                    successors.push((targets.otherwise().as_u32(), Some(CfgConstraint::Otherwise)));
                }
                insert_to_cfg(successors);
            }
            Assert {
                target,
                expected,
                unwind: _,
                ..
            } => {
                // TODO: Handle unwind
                insert_to_cfg(vec![(
                    target.as_u32(),
                    Some(CfgConstraint::Case(*expected as u128)),
                )]);
            }
            Return => ret_points.push(index.as_u32()),
            UnwindResume | UnwindTerminate(_) | Unreachable | CoroutineDrop => {}
            kind @ (Call { func, fn_span, .. } | TailCall { func, fn_span, .. }) => {
                use rustc_type_ir::TyKind::*;
                match func.ty(body.local_decls(), tcx).kind() {
                    FnDef(def_id, generic_args)
                    | Closure(def_id, generic_args)
                    | Coroutine(def_id, generic_args)
                    | CoroutineClosure(def_id, generic_args) => {
                        let dbg = CallDebugInfo {
                            location: tcx.sess.source_map().span_to_string(
                                *fn_span,
                                rustc_span::FileNameDisplayPreference::Short,
                            ),
                        };
                        insert_to_calls(*def_id, generic_args, dbg);
                    }
                    FnPtr(..) => {
                        // TODO
                    }
                    _ => {}
                }

                if let Call {
                    target: Some(target),
                    ..
                } = kind
                {
                    insert_to_cfg(vec![(target.as_u32(), None)]);
                }
            }
            Yield { resume, .. } => {
                insert_to_cfg(vec![(resume.as_u32(), None)]);
            }

            InlineAsm { targets, .. } => {
                // TODO: Handle unwind
                for target in targets.iter() {
                    insert_to_cfg(vec![(target.as_u32(), None)]);
                }
            }
        }
    }
    (cfg, ret_points, calls)
}
