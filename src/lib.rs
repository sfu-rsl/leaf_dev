#![feature(rustc_private)]
#![deny(rustc::internal)]

extern crate rustc_driver;
extern crate rustc_index;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_mir_transform;

use log::debug;
use rustc_index::vec::IndexVec;
use rustc_interface::interface;
use rustc_middle::{
    mir::{
        self,
        terminator::{Terminator, TerminatorKind::*},
        BasicBlock, BasicBlockData,
    },
    ty::{self, query},
};
use std::{collections::HashMap, path::PathBuf};

pub struct RunCompiler;

impl RunCompiler {
    pub fn run(args: &mut Vec<String>, input_path: Option<PathBuf>) -> i32 {
        // Taken frim MIRI <https://github.com/rust-lang/miri/blob/master/src/bin/miri.rs#L205>
        let home = option_env!("RUSTUP_HOME").or(option_env!("MULTIRUST_HOME"));
        let toolchain = option_env!("RUSTUP_TOOLCHAIN").or(option_env!("MULTIRUST_TOOLCHAIN"));
        let sysroot = match (home, toolchain) {
            (Some(home), Some(toolchain)) => format!("{}/toolchains/{}", home, toolchain),
            _ => option_env!("RUST_SYSROOT")
                .expect("To build without rustup, set the `RUST_SYSROOT` env var at build time")
                .to_owned(),
        };

        args.push(String::from("--sysroot"));
        args.push(sysroot);

        if let Some(ip) = input_path {
            args.push(ip.to_str().unwrap().to_string());
        }

        let mut cb = Callbacks;
        rustc_driver::install_ice_hook();
        rustc_driver::catch_with_exit_code(|| rustc_driver::RunCompiler::new(args, &mut cb).run())
    }
}

struct Callbacks;

impl rustc_driver::Callbacks for Callbacks {
    fn config(&mut self, config: &mut interface::Config) {
        config.override_queries = Some(|_session, lprov, _eprov| {
            lprov.optimized_mir = Callbacks::local_optimized_mir;
            //eprov.optimized_mir = Callbacks::extern_optimized_mir;
        });
    }

    /*
    fn after_analysis<'tcx>(
        &mut self,
        _compiler: &interface::Compiler,
        queries: &'tcx rustc_interface::Queries<'tcx>,
    ) -> rustc_driver::Compilation {
        queries.global_ctxt().unwrap().peek_mut().enter(|_tcx| {
            debug!("queries");
        });

        rustc_driver::Compilation::Continue
    }
    */
}

impl Callbacks {
    fn local_optimized_mir<'tcx>(
        tcx: ty::TyCtxt<'tcx>,
        opt_mir: query::query_keys::optimized_mir<'tcx>,
    ) -> query::query_values::optimized_mir<'tcx> {
        let mut providers = query::Providers::default();
        rustc_mir_transform::provide(&mut providers);
        let body = (providers.optimized_mir)(tcx, opt_mir);
        // Cloning here is probably not ideal but couldn't find a different way
        let mut body = tcx.alloc_steal_mir((*body).clone()).steal();
        let (mut basic_blocks, _local_decls) = body.basic_blocks_and_local_decls_mut();

        Self::transform_basic_blocks(&mut basic_blocks);
        //for local_decl in local_decls.iter() {
        //    debug!("{local_decl:?}");
        //}
        tcx.arena.alloc(body)
    }

    /*
    fn extern_optimized_mir<'tcx>(
        tcx: ty::TyCtxt<'tcx>,
        opt_mir: query::query_keys::optimized_mir<'tcx>,
    ) -> query::query_values::optimized_mir<'tcx> {
        debug!("external optimized_mir for {opt_mir:?}");
        let mut providers = query::ExternProviders::default();
        rustc_metadata::provide_extern(&mut providers);
        let body = (providers.optimized_mir)(tcx, opt_mir);
        //debug!("{body:?}");
        body
    }
    */

    fn transform_basic_blocks(
        basic_blocks: &mut rustc_index::vec::IndexVec<BasicBlock, BasicBlockData>,
    ) {
        // First clear up the basic_block list
        let mut reverse = Self::clear_and_get_reverse(basic_blocks);

        // Create a new block for each statement
        let index_counts = Self::repopulate_basic_blocks(basic_blocks, &mut reverse);

        // Calculate index mappings
        let index_map = Self::create_index_mappings(&index_counts);

        Self::process_terminators(basic_blocks, &index_map);
    }

    fn clear_and_get_reverse<'tcx>(
        basic_blocks: &mut IndexVec<BasicBlock, BasicBlockData<'tcx>>,
    ) -> Vec<BasicBlockData<'tcx>> {
        let mut reverse = Vec::new();
        while let Some(basic_block) = basic_blocks.pop() {
            reverse.push(basic_block);
        }

        reverse
    }

    fn repopulate_basic_blocks<'tcx>(
        basic_blocks: &mut IndexVec<BasicBlock, BasicBlockData<'tcx>>,
        reverse: &mut Vec<BasicBlockData<'tcx>>,
    ) -> Vec<usize> {
        let mut index_counts = Vec::new();
        while let Some(basic_block) = reverse.pop() {
            Self::process_each_block(basic_block, basic_blocks, &mut index_counts);
        }

        index_counts
    }

    fn process_each_block<'tcx>(
        mut basic_block: BasicBlockData<'tcx>,
        basic_blocks: &mut IndexVec<BasicBlock, BasicBlockData<'tcx>>,
        index_counts: &mut Vec<usize>,
    ) {
        if basic_block.statements.len() == 0 {
            basic_blocks.push(basic_block);
            index_counts.push(1);
            return;
        }

        let mut new_basic_blocks = Vec::new();

        index_counts.push(basic_block.statements.len());
        while let Some(statement) = basic_block.statements.pop() {
            // Process from the last statement in the block
            let mut new_basic_block = mir::BasicBlockData::new(None);
            new_basic_block.statements = Vec::from([statement]);
            new_basic_block.is_cleanup = basic_block.is_cleanup;
            new_basic_blocks.push(new_basic_block);
        }

        if let Some(first_block) = new_basic_blocks.first_mut() {
            // first_block contains the last statement, so give the original
            // block's terminator
            first_block.terminator = basic_block.terminator;
        }

        while let Some(new_basic_block) = new_basic_blocks.pop() {
            basic_blocks.push(new_basic_block);
        }
    }

    fn create_index_mappings(index_counts: &Vec<usize>) -> HashMap<BasicBlock, BasicBlock> {
        let mut index_map = HashMap::<BasicBlock, BasicBlock>::new();
        let mut origin_index: usize = 0;
        let mut new_index = 0;

        for cnt in index_counts {
            // At this point, index_counts contains a list of how many blocks replace each
            // original block
            index_map.insert(
                BasicBlock::from_usize(origin_index),
                BasicBlock::from_usize(new_index),
            );
            origin_index += 1;
            new_index += cnt;
        }

        index_map
    }

    fn remap_jump_targets(
        terminator: &mut Terminator,
        index_map: &HashMap<BasicBlock, BasicBlock>,
    ) {
        // Adjust jump targets
        match &mut terminator.kind {
            Goto { target } => {
                // target has to exist in index_map
                *target = index_map.get(&target).unwrap().to_owned();
            }
            SwitchInt {
                discr: _,     //: Operand<'tcx>,
                switch_ty: _, //: Ty<'tcx>,
                targets,      //: SwitchTargets,
            } => {
                for target in targets.all_targets_mut() {
                    *target = index_map.get(&target).unwrap().to_owned();
                }
            }
            ////Resume,
            ////Abort,
            ////Return,
            ////Unreachable,
            Drop {
                place: _, //: Place<'tcx>,
                target,   //: BasicBlock,
                unwind,   //: Option<BasicBlock>,
            } => {
                *target = index_map.get(&target).unwrap().to_owned();
                *unwind = unwind.map(|idx| index_map.get(&idx).unwrap().to_owned());
            }
            DropAndReplace {
                place: _, //: Place<'tcx>,
                value: _, //: Operand<'tcx>,
                target,   //: BasicBlock,
                unwind,   //: Option<BasicBlock>,
            } => {
                *target = index_map.get(&target).unwrap().to_owned();
                *unwind = unwind.map(|idx| index_map.get(&idx).unwrap().to_owned());
            }
            Call {
                func: _,          //: Operand<'tcx>,
                args: _,          //: Vec<Operand<'tcx>>,
                destination,      //: Option<(Place<'tcx>, BasicBlock)>,
                cleanup,          //: Option<BasicBlock>,
                from_hir_call: _, //: bool,
                fn_span: _,       //: Span,
            } => {
                *destination = destination
                    .map(|(place, idx)| (place, index_map.get(&idx).unwrap().to_owned()));
                *cleanup = cleanup.map(|idx| index_map.get(&idx).unwrap().to_owned());
            }
            Assert {
                cond: _,     //: Operand<'tcx>,
                expected: _, //: bool,
                msg: _,      //: AssertMessage<'tcx>,
                target,      //: BasicBlock,
                cleanup,     //: Option<BasicBlock>,
            } => {
                *target = index_map.get(&target).unwrap().to_owned();
                *cleanup = cleanup.map(|idx| index_map.get(&idx).unwrap().to_owned());
            }
            Yield {
                value: _,      //: Operand<'tcx>,
                resume,        //: BasicBlock,
                resume_arg: _, //: Place<'tcx>,
                drop,          //: Option<BasicBlock>,
            } => {
                *resume = index_map.get(&resume).unwrap().to_owned();
                *drop = drop.map(|idx| index_map.get(&idx).unwrap().to_owned());
            }
            //GeneratorDrop,
            FalseEdge {
                real_target,      //: BasicBlock,
                imaginary_target, //: BasicBlock,
            } => {
                *real_target = index_map.get(&real_target).unwrap().to_owned();
                *imaginary_target = index_map.get(&imaginary_target).unwrap().to_owned();
            }
            FalseUnwind {
                real_target, //: BasicBlock,
                unwind,      //: Option<BasicBlock>,
            } => {
                *real_target = index_map.get(&real_target).unwrap().to_owned();
                *unwind = unwind.map(|idx| index_map.get(&idx).unwrap().to_owned());
            }
            InlineAsm {
                template: _,   //: &'tcx [InlineAsmTemplatePiece],
                operands: _,   //: Vec<InlineAsmOperand<'tcx>>,
                options: _,    //: InlineAsmOptions,
                line_spans: _, //: &'tcx [Span],
                destination,   //: Option<BasicBlock>,
                cleanup,       //: Option<BasicBlock>,
            } => {
                *destination = destination.map(|idx| index_map.get(&idx).unwrap().to_owned());
                *cleanup = cleanup.map(|idx| index_map.get(&idx).unwrap().to_owned());
            }
            _ => (),
        }
    }

    fn process_terminators(
        basic_blocks: &mut IndexVec<BasicBlock, BasicBlockData>,
        index_map: &HashMap<BasicBlock, BasicBlock>,
    ) {
        for (basic_block_idx, basic_block) in basic_blocks.iter_enumerated_mut() {
            match &mut basic_block.terminator {
                Some(terminator) => {
                    // Adjust jump targets
                    Self::remap_jump_targets(terminator, &index_map);
                }
                None => {
                    // At this point, every block should contain one statement exactly except the last block
                    // Reuse the same source_info from the original statement
                    let new_terminator = Terminator {
                        source_info: basic_block.statements.first().unwrap().source_info,
                        kind: Goto {
                            target: basic_block_idx + 1,
                        },
                    };

                    basic_block.terminator = Some(new_terminator);
                }
            };

            debug!("{basic_block_idx:?} {basic_block:?}");
        }
    }
}
