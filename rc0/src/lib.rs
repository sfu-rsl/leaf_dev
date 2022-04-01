#![feature(rustc_private)]
#![deny(rustc::internal)]

extern crate rustc_ast;
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
        interpret::{Allocation, ConstValue},
        terminator::{Terminator, TerminatorKind::*},
        BasicBlock, BasicBlockData, LocalDecl, LocalDecls, Operand, Place, SourceInfo,
    },
    ty::{self, query},
};
use rustc_span::def_id::DefId;
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
        if let Some(output_dir) = &config.output_dir {
            if output_dir
                .to_str()
                .expect("Output directory invalid string")
                .contains("/build/")
            {
                // Skip build scripts.
                return;
            }
        }

        config.override_queries = Some(|_session, lprov, _eprov| {
            lprov.optimized_mir = Instrumenter::local_optimized_mir;
            //eprov.optimized_mir = Callbacks::extern_optimized_mir;
        });
    }

    fn after_parsing<'tcx>(
        &mut self,
        compiler: &interface::Compiler,
        queries: &'tcx rustc_interface::Queries<'tcx>,
    ) -> rustc_driver::Compilation {
        compiler.session().abort_if_errors();
        let items = &mut queries.parse().unwrap().peek_mut().items;
        let item = rustc_ast::ast::Item {
            attrs: Vec::new(),
            id: rustc_ast::DUMMY_NODE_ID,
            span: rustc_span::DUMMY_SP,
            vis: rustc_ast::ast::Visibility {
                kind: rustc_ast::ast::VisibilityKind::Inherited,
                span: rustc_span::DUMMY_SP,
                tokens: None,
            },
            ident: rustc_span::symbol::Ident::with_dummy_span(rustc_span::symbol::Symbol::intern(
                "rc0lib",
            )),
            kind: rustc_ast::ast::ItemKind::ExternCrate(None),
            tokens: None,
        };
        items.insert(0, rustc_ast::ptr::P(item));
        debug!("{items:?}");

        rustc_driver::Compilation::Continue
    }
}

struct Instrumenter;

impl Instrumenter {
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

        let mut iter = basic_blocks.iter_enumerated_mut().peekable();
        for _ in 0..iter.len() {
            if let Some((basic_block_idx, basic_block)) = iter.next() {
                let next_terminator = iter
                    .peek()
                    .map(|(_, next_block)| next_block.terminator.as_ref())
                    .flatten();
                Self::process_terminator(
                    tcx,
                    basic_block_idx,
                    basic_block,
                    local_decls,
                    &index_map,
                    &def_ids,
                    next_terminator,
                );
            }
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
        next_terminator: Option<&Terminator>,
    ) {
        // TODO: Should build a (function_name, def_id) map. Assume only one function
        // for now.
        let def_id = def_ids.first().unwrap();

        let span = tcx.def_span(*def_id);

        // Add a new local_decl of the () type, which is used as an lvalue for a function call with
        // no return values.
        let ret_local_decl = LocalDecl::new(tcx.intern_tup(&[]), span.to_owned());
        let ret_local_decl_idx = local_decls.push(ret_local_decl);

        // Create arguments to pass.
        // First create a &str constant.
        let s = if basic_block.statements.len() == 0 {
            let s = format!("{:?}", next_terminator.unwrap().kind);
            s
        } else {
            let s = format!("{:?}", basic_block.statements.first().unwrap());
            s
        };
        let allocation = Allocation::from_bytes_byte_aligned_immutable(s.as_bytes());
        let allocation = tcx.intern_const_alloc(allocation);
        let constant = mir::Constant {
            span: span.to_owned(),
            user_ty: None, // TODO: not sure about this but this is not coming from a user, so...
            literal: mir::ConstantKind::Val(
                ConstValue::Slice {
                    data: allocation,
                    start: 0,
                    end: s.len(),
                },
                tcx.mk_static_str(),
            ),
        };
        let str_operand = Operand::Constant(Box::new(constant));

        let new_terminator = Terminator {
            source_info: SourceInfo::outermost(span), // TODO: Not sure how good
            kind: Call {
                func: Operand::function_handle(
                    *tcx,
                    **def_id,
                    tcx.intern_substs(&[]),
                    span.to_owned(),
                ),
                args: vec![str_operand],
                destination: Some((Place::from(ret_local_decl_idx), basic_block_idx + 1)),
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
        next_terminator: Option<&Terminator>,
    ) {
        match &mut basic_block.terminator {
            Some(terminator) => {
                // The basic block already has a terminator (it's the last basic block).
                // Adjust jump targets.
                Self::remap_jump_targets(terminator, &index_map);
            }
            None => {
                // A newly-added basic block
                Self::add_new_terminator(
                    tcx,
                    basic_block_idx,
                    basic_block,
                    local_decls,
                    &def_ids,
                    next_terminator,
                );
            }
        };
    }
}
