#![feature(rustc_private)]
#![deny(rustc::internal)]

extern crate rustc_ast;
extern crate rustc_driver;
extern crate rustc_index;
extern crate rustc_interface;
extern crate rustc_metadata;
extern crate rustc_middle;
extern crate rustc_mir_transform;
extern crate rustc_session;
extern crate rustc_span;

use log::debug;
use rustc_driver::Compilation;
use rustc_index::vec::IndexVec;
use rustc_interface::{
    interface::{self, Compiler},
    Queries,
};
use rustc_middle::{
    middle::exported_symbols::ExportedSymbol,
    mir::{
        self,
        interpret::{Allocation, ConstValue},
        terminator::{Terminator, TerminatorKind::*},
        BasicBlock, BasicBlockData, Local, LocalDecl, LocalDecls, Operand, Place, Rvalue,
        SourceInfo, StatementKind,
    },
    ty::{query, TyCtxt},
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
            // Skip build scripts.
            if output_dir
                .to_str()
                .expect("Output directory invalid string")
                .contains("/build/")
            {
                return;
            }
        }

        config.override_queries = Some(|_session, lprov, _eprov| {
            lprov.optimized_mir = local_optimized_mir;
            //eprov.optimized_mir = extern_optimized_mir; // TODO: Bring this back (see below).
        });
    }

    fn after_parsing<'tcx>(
        &mut self,
        compiler: &Compiler,
        queries: &'tcx Queries<'tcx>,
    ) -> Compilation {
        compiler.session().abort_if_errors();

        // The following adds a new statement "extern crate rc0lib" to the parsed AST as a new item.
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

        Compilation::Continue
    }
}

fn local_optimized_mir<'tcx>(
    tcx: TyCtxt<'tcx>,
    opt_mir: query::query_keys::optimized_mir<'tcx>,
) -> query::query_values::optimized_mir<'tcx> {
    let mut providers = query::Providers::default();
    rustc_mir_transform::provide(&mut providers);
    let body = (providers.optimized_mir)(tcx, opt_mir);

    // Cloning here is probably not ideal but couldn't find a different way
    let mut body = tcx.alloc_steal_mir((*body).clone()).steal();

    let (mut basic_blocks, mut local_decls) = body.basic_blocks_and_local_decls_mut();
    transform_basic_blocks(&tcx, &mut basic_blocks, &mut local_decls);

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
 * TODO: Bring this back.
fn extern_optimized_mir<'tcx>(
    tcx: TyCtxt<'tcx>,
    opt_mir: query::query_keys::optimized_mir<'tcx>,
) -> query::query_values::optimized_mir<'tcx> {
    let mut providers = query::ExternProviders::default();
    rustc_metadata::provide_extern(&mut providers);
    let body = (providers.optimized_mir)(tcx, opt_mir);

    //// Cloning here is probably not ideal but couldn't find a different way
    let mut body = tcx.alloc_steal_mir((*body).clone()).steal();

    let (mut basic_blocks, mut local_decls) = body.basic_blocks_and_local_decls_mut();
    transform_basic_blocks(&tcx, &mut basic_blocks, &mut local_decls);

    tcx.arena.alloc(body)
}
*/

fn transform_basic_blocks<'tcx>(
    tcx: &TyCtxt<'tcx>,
    basic_blocks: &mut IndexVec<BasicBlock, BasicBlockData<'tcx>>,
    local_decls: &mut LocalDecls<'tcx>,
) {
    // First clear up the basic_block list
    let mut reverse = clear_and_get_reverse(basic_blocks);

    // Create a new block for each statement
    let index_counts = repopulate_basic_blocks(basic_blocks, &mut reverse);

    // Calculate index mappings
    let index_map = create_index_mappings(&index_counts);

    // Find the rc0lib crate and the functions in it.
    // TODO: Perhaps we shouldn't use expect here (and not panic)?
    let cnum = tcx
        .crates(())
        .iter()
        .find(|cnum| tcx.crate_name(**cnum).as_str() == "rc0lib")
        .expect("rc0lib crate not found");
    let def_ids: Vec<&DefId> = tcx
        .exported_symbols(*cnum)
        .iter()
        .filter_map(|(exported_symbol, _)| match exported_symbol {
            ExportedSymbol::NonGeneric(def_id) => Some(def_id),
            _ => None,
        })
        .collect();

    let mut func_map = HashMap::<String, DefId>::new();
    for def_id in def_ids.iter() {
        func_map.insert(tcx.def_path_str(**def_id), **def_id);
    }

    let mut iter = basic_blocks.iter_enumerated_mut().peekable();
    for _ in 0..iter.len() {
        if let Some((basic_block_idx, basic_block)) = iter.next() {
            debug!("{basic_block_idx:?}, {basic_block:?}");
            let next_terminator = iter
                .peek()
                .map(|(_, next_block)| next_block.terminator.as_ref())
                .flatten();
            process_terminator(
                tcx,
                basic_block_idx,
                basic_block,
                local_decls,
                &index_map,
                next_terminator,
                &func_map,
            );
            debug!("{basic_block_idx:?}, {basic_block:?}");
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
        create_new_blocks(basic_block, basic_blocks, &mut index_counts);
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

fn process_terminator<'tcx>(
    tcx: &TyCtxt<'tcx>,
    basic_block_idx: BasicBlock,
    basic_block: &mut BasicBlockData<'tcx>,
    local_decls: &mut LocalDecls<'tcx>,
    index_map: &HashMap<BasicBlock, BasicBlock>,
    next_terminator: Option<&Terminator>,
    func_map: &HashMap<String, DefId>,
) {
    match &mut basic_block.terminator {
        Some(terminator) => {
            // The basic block already has a terminator (it's the last basic block).
            // Adjust jump targets.
            remap_jump_targets(terminator, &index_map);
        }
        None => {
            // A newly-added basic block
            add_new_terminator(
                tcx,
                basic_block_idx,
                basic_block,
                local_decls,
                next_terminator,
                func_map,
            );
        }
    };
}

fn remap_jump_targets(terminator: &mut Terminator, index_map: &HashMap<BasicBlock, BasicBlock>) {
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
            *destination =
                destination.map(|(place, idx)| (place, index_map.get(&idx).unwrap().to_owned()));
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
    tcx: &TyCtxt<'tcx>,
    basic_block_idx: BasicBlock,
    basic_block: &mut BasicBlockData<'tcx>,
    local_decls: &mut LocalDecls<'tcx>,
    next_terminator: Option<&Terminator>,
    func_map: &HashMap<String, DefId>,
) {
    basic_block.terminator = Some(if basic_block.statements.len() == 0 {
        match next_terminator.unwrap().kind {
            SwitchInt {
                discr: _,
                switch_ty: _,
                targets: _,
            } => get_switch_int(tcx, basic_block_idx, local_decls, func_map),
            Return => get_ret(tcx, basic_block_idx, local_decls, func_map),
            Call {
                func: _,
                args: _,
                destination: _,
                cleanup: _,
                from_hir_call: _,
                fn_span: _,
            } => get_call(tcx, basic_block_idx, local_decls, func_map),
            // TODO: Check if we need to handle anything else.
            _ => get_goto(basic_block_idx),
        }
    } else {
        match &basic_block.statements.first().unwrap().kind {
            StatementKind::Assign(asgn) => {
                get_assign(tcx, basic_block_idx, local_decls, func_map, asgn)
            }
            // TODO: Check if we need to handle anything else.
            _ => get_goto(basic_block_idx),
        }
    });
}

// TODO: Filler
fn get_switch_int<'tcx>(
    tcx: &TyCtxt<'tcx>,
    basic_block_idx: BasicBlock,
    local_decls: &mut LocalDecls<'tcx>,
    func_map: &HashMap<String, DefId>,
) -> Terminator<'tcx> {
    let def_id = func_map.get("rc0lib::switch_int").unwrap();

    // Add a new local_decl of the () type, which is used as an lvalue for a function call with
    // no return values.
    let ret_local_decl = LocalDecl::new(tcx.intern_tup(&[]), rustc_span::DUMMY_SP);
    let ret_local_decl_idx = local_decls.push(ret_local_decl);

    Terminator {
        source_info: SourceInfo::outermost(rustc_span::DUMMY_SP), // TODO: Not sure how good
        kind: Call {
            func: Operand::function_handle(
                *tcx,
                *def_id,
                tcx.intern_substs(&[]),
                rustc_span::DUMMY_SP,
            ),
            args: vec![],
            destination: Some((Place::from(ret_local_decl_idx), basic_block_idx + 1)),
            cleanup: None,
            from_hir_call: true,
            fn_span: rustc_span::DUMMY_SP,
        },
    }
}

// TODO: Filler
fn get_ret<'tcx>(
    tcx: &TyCtxt<'tcx>,
    basic_block_idx: BasicBlock,
    local_decls: &mut LocalDecls<'tcx>,
    func_map: &HashMap<String, DefId>,
) -> Terminator<'tcx> {
    let def_id = func_map.get("rc0lib::ret").unwrap();
    let ret_local_decl = LocalDecl::new(tcx.intern_tup(&[]), rustc_span::DUMMY_SP);
    let ret_local_decl_idx = local_decls.push(ret_local_decl);

    Terminator {
        source_info: SourceInfo::outermost(rustc_span::DUMMY_SP), // TODO: Not sure how good
        kind: Call {
            func: Operand::function_handle(
                *tcx,
                *def_id,
                tcx.intern_substs(&[]),
                rustc_span::DUMMY_SP,
            ),
            args: vec![],
            destination: Some((Place::from(ret_local_decl_idx), basic_block_idx + 1)),
            cleanup: None,
            from_hir_call: true,
            fn_span: rustc_span::DUMMY_SP,
        },
    }
}

// TODO: Filler
fn get_call<'tcx>(
    tcx: &TyCtxt<'tcx>,
    basic_block_idx: BasicBlock,
    local_decls: &mut LocalDecls<'tcx>,
    func_map: &HashMap<String, DefId>,
) -> Terminator<'tcx> {
    let def_id = func_map.get("rc0lib::call").unwrap();
    let ret_local_decl = LocalDecl::new(tcx.intern_tup(&[]), rustc_span::DUMMY_SP);
    let ret_local_decl_idx = local_decls.push(ret_local_decl);

    Terminator {
        source_info: SourceInfo::outermost(rustc_span::DUMMY_SP), // TODO: Not sure how good
        kind: Call {
            func: Operand::function_handle(
                *tcx,
                *def_id,
                tcx.intern_substs(&[]),
                rustc_span::DUMMY_SP,
            ),
            args: vec![],
            destination: Some((Place::from(ret_local_decl_idx), basic_block_idx + 1)),
            cleanup: None,
            from_hir_call: true,
            fn_span: rustc_span::DUMMY_SP,
        },
    }
}

// TODO: Filler
fn get_assign<'tcx>(
    tcx: &TyCtxt<'tcx>,
    basic_block_idx: BasicBlock,
    local_decls: &mut LocalDecls<'tcx>,
    func_map: &HashMap<String, DefId>,
    asgn: &Box<(Place<'tcx>, Rvalue<'tcx>)>,
) -> Terminator<'tcx> {
    let def_id = func_map.get("rc0lib::assign").unwrap();
    let ret_local_decl = LocalDecl::new(tcx.intern_tup(&[]), rustc_span::DUMMY_SP);
    let ret_local_decl_idx = local_decls.push(ret_local_decl);

    let (place, rvalue) = asgn.as_ref();
    let place = place.local.as_u32(); // TODO: not sure if we need .projection in addition to .local
    match rvalue {
        /*
            pub enum Rvalue<'tcx> {
                Use(Operand<'tcx>),
                Repeat(Operand<'tcx>, Const<'tcx>),
                Ref(Region<'tcx>, BorrowKind, Place<'tcx>),
                ThreadLocalRef(DefId),
                AddressOf(Mutability, Place<'tcx>),
                Len(Place<'tcx>),
                Cast(CastKind, Operand<'tcx>, Ty<'tcx>),
                BinaryOp(BinOp, Box<(Operand<'tcx>, Operand<'tcx>)>),
                CheckedBinaryOp(BinOp, Box<(Operand<'tcx>, Operand<'tcx>)>),
                NullaryOp(NullOp, Ty<'tcx>),
                UnaryOp(UnOp, Operand<'tcx>),
                Discriminant(Place<'tcx>),
                Aggregate(Box<AggregateKind<'tcx>>, Vec<Operand<'tcx>>),
                ShallowInitBox(Operand<'tcx>, Ty<'tcx>),
            }
        */
        Rvalue::Use(operand) => {
            get_assign_use(tcx, def_id, ret_local_decl_idx, basic_block_idx, operand)
        }
        _ => Terminator {
            source_info: SourceInfo::outermost(rustc_span::DUMMY_SP), // TODO: Not sure how good
            kind: Call {
                func: Operand::function_handle(
                    *tcx,
                    *def_id,
                    tcx.intern_substs(&[]),
                    rustc_span::DUMMY_SP,
                ),
                args: vec![build_str(tcx, String::new()), build_str(tcx, String::new())],
                destination: Some((Place::from(ret_local_decl_idx), basic_block_idx + 1)),
                cleanup: None,
                from_hir_call: true,
                fn_span: rustc_span::DUMMY_SP,
            },
        },
    }
}

fn get_assign_use<'tcx>(
    tcx: &TyCtxt<'tcx>,
    def_id: &DefId,
    ret_local_decl_idx: Local,
    basic_block_idx: BasicBlock,
    operand: &Operand,
) -> Terminator<'tcx> {
    debug!("get_assign_use");
    let (arg0, arg1) = match operand {
        Operand::Copy(place) => (
            build_str(tcx, "copy".to_string()),
            build_str(tcx, format!("{place:?}")),
        ),
        Operand::Move(place) => (
            build_str(tcx, "move".to_string()),
            build_str(tcx, format!("{place:?}")),
        ),
        Operand::Constant(constant) => (
            build_str(tcx, "constant".to_string()),
            build_str(tcx, format!("{constant}")),
        ),
    };

    Terminator {
        source_info: SourceInfo::outermost(rustc_span::DUMMY_SP), // TODO: Not sure how good
        kind: Call {
            func: Operand::function_handle(
                *tcx,
                *def_id,
                tcx.intern_substs(&[]),
                rustc_span::DUMMY_SP,
            ),
            args: vec![arg0, arg1],
            destination: Some((Place::from(ret_local_decl_idx), basic_block_idx + 1)),
            cleanup: None,
            from_hir_call: true,
            fn_span: rustc_span::DUMMY_SP,
        },
    }
}

fn get_goto<'tcx>(basic_block_idx: BasicBlock) -> Terminator<'tcx> {
    Terminator {
        source_info: SourceInfo::outermost(rustc_span::DUMMY_SP), // TODO: Not sure how good
        kind: Goto {
            target: basic_block_idx + 1,
        },
    }
}

fn build_str<'tcx>(tcx: &TyCtxt<'tcx>, s: String) -> Operand<'tcx> {
    let allocation = Allocation::from_bytes_byte_aligned_immutable(s.as_bytes());
    let allocation = tcx.intern_const_alloc(allocation);
    let span = rustc_span::DUMMY_SP;
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
    Operand::Constant(Box::new(constant))
}
