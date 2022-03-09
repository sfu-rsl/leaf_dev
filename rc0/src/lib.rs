#![feature(rustc_private)]
#![deny(rustc::internal)]

extern crate rustc_driver;
extern crate rustc_index;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_mir_transform;
extern crate rustc_session;
extern crate rustc_span;

use log::debug;
use rustc_index::vec::IndexVec;
use rustc_interface::interface;
use rustc_middle::{
    middle::exported_symbols::ExportedSymbol,
    mir::{
        self,
        terminator::{Terminator, TerminatorKind::*},
        BasicBlock, BasicBlockData, LocalDecl, LocalDecls, Operand, Place, SourceInfo,
    },
    ty::{self, query},
};
use rustc_span::{def_id::DefId, source_map};
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

        let mut cb = Callbacks {};
        rustc_driver::install_ice_hook();
        rustc_driver::catch_with_exit_code(|| rustc_driver::RunCompiler::new(args, &mut cb).run())
    }
}

struct Callbacks;

impl rustc_driver::Callbacks for Callbacks {
    fn config(&mut self, config: &mut interface::Config) {
        // We're injecting "extern crate rc0lib;" into the source file itself. Since we're adding
        // calls to the functions in rc0lib, there has to be a way to bring rc0lib into the crate's
        // scope. The easiest way seems to be adding extern crate in the source file. There are
        // probably other ways such as adding it in HIR. But I couldn't find a way to do
        // that. "extern crate std;" seems to be automatically added (this is what's called
        // "extern preludes") when generating HIR. So if that's true, then we just need to do the
        // same thing for rc0lib.
        //
        // When adding "extern crate rc0lib;" to the source file, you could generate a new file and
        // add it there. But I chose not to do it since there has to be a way to clean up the
        // temporary file created in the process and I thought it'd be ugly.
        //
        // What I chose to do is to read the whole file as a string, add "extern crate rc0lib;" in
        // the string, and use the string for the compilation. This seems equally ugly but for now,
        // that's what we have.
        let input_path = &config.input_path.as_ref().expect("No file given");
        let mut contents =
            std::fs::read_to_string(input_path).expect("Fail to read the input file");
        contents = format!("extern crate rc0lib;\n{}", contents);
        config.input = rustc_session::config::Input::Str {
            name: source_map::FileName::Custom(input_path.to_str().unwrap().to_string()),
            input: contents,
        };
        config.override_queries = Some(|_session, lprov, _eprov| {
            lprov.optimized_mir = Callbacks::local_optimized_mir;
            //eprov.optimized_mir = Callbacks::extern_optimized_mir;
        });
    }
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

        let (mut basic_blocks, mut local_decls) = body.basic_blocks_and_local_decls_mut();
        Self::transform_basic_blocks(&tcx, &mut basic_blocks, &mut local_decls);

        tcx.arena.alloc(body)

        // Not removing this to keep it as an example on how to get DefId for a function.
        //
        //let def_id = body.source.def_id();
        //match tcx.def_path_str(def_id).as_str() {
        //    "add_this" => {
        //        debug!("skipping add_this");
        //    }
        //    _ => {
        //        let (mut basic_blocks, mut local_decls) = body.basic_blocks_and_local_decls_mut();
        //        Self::transform_basic_blocks(&tcx, &mut basic_blocks, &mut local_decls);
        //    }
        //}
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

    fn transform_basic_blocks<'tcx>(
        tcx: &ty::TyCtxt<'tcx>,
        basic_blocks: &mut IndexVec<BasicBlock, BasicBlockData<'tcx>>,
        local_decls: &mut LocalDecls<'tcx>,
    ) {
        // First clear up the basic_block list
        let mut reverse = Self::clear_and_get_reverse(basic_blocks);

        // Create a new block for each statement
        let index_counts = Self::repopulate_basic_blocks(basic_blocks, &mut reverse);

        // Calculate index mappings
        let index_map = Self::create_index_mappings(&index_counts);

        // Find the rc0lib crate and the functions in it. Perhaps we shouldn't use expect here (and
        // not panic)?
        let cnum = tcx
            .crates(())
            .iter()
            .find(|cnum| tcx.crate_name(**cnum).as_str() == "rc0lib")
            .expect("rc0lib crate not found");
        let def_ids = tcx
            .exported_symbols(*cnum)
            .iter()
            .filter_map(|(exported_symbol, _)| match exported_symbol {
                ExportedSymbol::NonGeneric(def_id) => Some(def_id),
                _ => None,
            })
            .collect();

        for (basic_block_idx, basic_block) in basic_blocks.iter_enumerated_mut() {
            Self::process_terminator(
                tcx,
                basic_block_idx,
                basic_block,
                local_decls,
                &index_map,
                &def_ids,
            );
        }
    }

    fn clear_and_get_reverse<'tcx>(
        basic_blocks: &mut IndexVec<BasicBlock, BasicBlockData<'tcx>>,
    ) -> Vec<BasicBlockData<'tcx>> {
        let mut reverse = vec![];
        while let Some(basic_block) = basic_blocks.pop() {
            reverse.push(basic_block);
        }

        reverse
    }

    fn repopulate_basic_blocks<'tcx>(
        basic_blocks: &mut IndexVec<BasicBlock, BasicBlockData<'tcx>>,
        reverse: &mut Vec<BasicBlockData<'tcx>>,
    ) -> Vec<usize> {
        let mut index_counts = vec![];
        while let Some(basic_block) = reverse.pop() {
            Self::create_new_blocks(basic_block, basic_blocks, &mut index_counts);
        }

        index_counts
    }

    fn create_new_blocks<'tcx>(
        mut basic_block: BasicBlockData<'tcx>,
        basic_blocks: &mut IndexVec<BasicBlock, BasicBlockData<'tcx>>,
        index_counts: &mut Vec<usize>,
    ) {
        let mut new_basic_blocks = vec![];

        // # of statements + block for teminator call + terminator
        // We do this before we pop basic_block.statements.
        index_counts.push(basic_block.statements.len() + 2);

        // Create a new block with the original terminator and no statements.
        let mut new_basic_block = mir::BasicBlockData::new(basic_block.terminator);
        new_basic_block.is_cleanup = basic_block.is_cleanup;
        new_basic_blocks.push(new_basic_block);

        // Create a new block that will include a rc0lib call for the terminator
        new_basic_blocks.push(mir::BasicBlockData::new(None));

        while let Some(statement) = basic_block.statements.pop() {
            // Process from the last statement in the block
            let mut new_basic_block = mir::BasicBlockData::new(None);
            new_basic_block.statements = Vec::from([statement]);
            new_basic_blocks.push(new_basic_block);
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
            // At this point, index_counts contains a list of how many basic blocks replace each of
            // the original basic blocks
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

    fn add_new_terminator<'tcx>(
        tcx: &ty::TyCtxt<'tcx>,
        basic_block_idx: BasicBlock,
        basic_block: &mut BasicBlockData<'tcx>,
        local_decls: &mut LocalDecls<'tcx>,
        def_ids: &Vec<&DefId>,
    ) {
        // TODO: Should build a (function_name, def_id) map. Assume only one function
        // for now.
        let def_id = def_ids.first().unwrap();

        let span = tcx.def_span(*def_id);

        // Add a new local_decl of the () type, which is used as an lvalue for a function call with
        // no return values.
        let local_decl = LocalDecl::new(tcx.intern_tup(&[]), span.to_owned());
        let local_decl_idx = local_decls.push(local_decl);

        // Create arguments to pass

        let new_terminator = Terminator {
            source_info: SourceInfo::outermost(span), // TODO: Not sure how good
            kind: Call {
                func: Operand::function_handle(
                    *tcx,
                    **def_id,
                    tcx.intern_substs(&[]),
                    span.to_owned(),
                ),
                args: vec![],
                destination: Some((Place::from(local_decl_idx), basic_block_idx + 1)),
                cleanup: None,
                from_hir_call: true,
                fn_span: span.to_owned(),
            },
        };

        basic_block.terminator = Some(new_terminator);
    }

    fn process_terminator<'tcx>(
        tcx: &ty::TyCtxt<'tcx>,
        basic_block_idx: BasicBlock,
        basic_block: &mut BasicBlockData<'tcx>,
        local_decls: &mut LocalDecls<'tcx>,
        index_map: &HashMap<BasicBlock, BasicBlock>,
        def_ids: &Vec<&DefId>,
    ) {
        match &mut basic_block.terminator {
            Some(terminator) => {
                // The basic block already has a terminator (it's the last basic block).
                // Adjust jump targets.
                Self::remap_jump_targets(terminator, &index_map);
            }
            None => {
                // A newly-added basic block
                Self::add_new_terminator(tcx, basic_block_idx, basic_block, local_decls, def_ids);
            }
        };
    }
}
