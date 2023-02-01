#![feature(rustc_private)]
#![feature(custom_mir)]
#![feature(let_chains)]
#![feature(is_some_and)]
#![feature(extend_one)]
#![feature(box_patterns)]
#![feature(drain_filter)]
#![deny(rustc::internal)]

extern crate rustc_abi;
extern crate rustc_apfloat;
extern crate rustc_ast;
extern crate rustc_const_eval;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_index;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_mir_build;
extern crate rustc_mir_transform;
extern crate rustc_span;
extern crate rustc_target;
extern crate rustc_type_ir;
extern crate thin_vec;

mod mir_transform;
mod pass;
mod visit;

use rustc_ast::{
    ast::{Item, ItemKind, Visibility, VisibilityKind},
    ptr::P,
    DUMMY_NODE_ID,
};
use rustc_driver::Compilation;
// use rustc_hir::def_id::LocalDefId;
use rustc_interface::{
    interface::{Compiler, Config},
    Queries,
};

use rustc_middle::{
    mir,
    ty::{
        query::{query_keys, query_values, Providers},
        InstanceDef, TyCtxt, WithOptConstParam,
    },
};
use rustc_span::{
    symbol::{Ident, Symbol},
    DUMMY_SP,
};
use std::{borrow::BorrowMut, path::PathBuf};

use crate::pass::LeafPass;

pub struct RunCompiler;

impl RunCompiler {
    pub fn run(
        args: &mut Vec<String>,
        input_path: Option<PathBuf>,
        build_statement_list: bool,
    ) -> i32 {
        // Taken from MIRI <https://github.com/rust-lang/miri/blob/master/src/bin/miri.rs#L205>
        let home = option_env!("RUSTUP_HOME").or(option_env!("MULTIRUST_HOME"));
        let toolchain = option_env!("RUSTUP_TOOLCHAIN").or(option_env!("MULTIRUST_TOOLCHAIN"));
        let sysroot = match (home, toolchain) {
            (Some(home), Some(toolchain)) => format!("{}/toolchains/{}", home, toolchain),
            _ => option_env!("RUST_SYSROOT")
                .expect("To build without rustup, set the `RUST_SYSROOT` env var at build time")
                .to_owned(),
        };

        // rustc --crate-name i32_assign --edition=2021 leafc/examples/i32_assign/main.rs --error-format=json --json=diagnostic-rendered-ansi,artifacts,future-incompat --crate-type bin --emit=dep-info,link -C embed-bitcode=no -C debuginfo=2 -C metadata=129e4a25c5bbe997 -C extra-filename=-129e4a25c5bbe997 --out-dir /home/user/leaf/target/debug/examples -C incremental=/home/user/leaf/target/debug/incremental -L dependency=/home/user/leaf/target/debug/deps --extern env_logger=/home/user/leaf/target/debug/deps/libenv_logger-0b42aca91155c51c.rlib --extern leafc=/home/user/leaf/target/debug/deps/libleafc-a1df32eaa2961956.rlib --extern leafcommon=/home/user/leaf/target/debug/deps/libleafcommon-0c15f656f6f3dc87.rlib --extern leafrt=/home/user/leaf/target/debug/deps/libleafrt-0150a0981a5e4328.rlib --extern log=/home/user/leaf/target/debug/deps/liblog-2aea261907451eb5.rlib --extern test_log=/home/user/leaf/target/debug/deps/libtest_log-69f1ffe1e8db2c68.so -L native=/home/user/leaf/target/debug/build/z3-sys-7f7fe1645917b4e9/out/lib
        // FIXME: Doesn't run properly without this?. Gives an error of "Can't find extern crate leafrt"
        //  Get the target/build directory. This is obviously not portable.
        let mut path: PathBuf = option_env!("CARGO_MANIFEST_DIR").map_or_else(
            || std::env::current_dir().expect("valid directory"),
            |dir| {
                let mut path = PathBuf::from(dir);
                path.pop();
                path
            },
        );
        path.push("target/debug/deps");
        args.push(String::from("-L"));
        args.push(format!("dependency={}", path.to_string_lossy()));

        // Add pri library as extern crate
        std::fs::read_dir(&path)
            .expect("unable to read directory")
            .filter_map(|r| r.ok())
            .filter(|file_name| {
                let name = file_name.file_name().to_string_lossy().to_string();
                name.ends_with(".rlib") && name.starts_with("libpri-")
            })
            .take(1)
            .for_each(|file_name| {
                args.push(String::from("--extern"));
                args.push(format!(
                    "pri={}",
                    path.join(file_name.file_name())
                        .to_string_lossy()
                        .to_string()
                ));
            });

        args.push(String::from("--sysroot"));
        args.push(sysroot);

        if let Some(ip) = input_path {
            args.push(ip.to_str().unwrap().to_string());
        }

        let mut cb = Callbacks {
            config: if build_statement_list {
                LeafConfig::UnitTest
            } else {
                LeafConfig::Normal
            },
        };
        rustc_driver::install_ice_hook();
        let exit_code = rustc_driver::catch_with_exit_code(|| {
            rustc_driver::RunCompiler::new(args, &mut cb).run()
        });

        exit_code
    }
}

pub enum LeafConfig {
    Normal,
    UnitTest,
}

struct Callbacks {
    config: LeafConfig,
}

impl rustc_driver::Callbacks for Callbacks {
    fn config(&mut self, config: &mut Config) {
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

        if let LeafConfig::UnitTest = self.config {
            // Skip optimizing MIR for unit tests.
            return;
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

        if let LeafConfig::UnitTest = self.config {
            queries
                .global_ctxt()
                .unwrap()
                .get_mut()
                .enter(|tcx| self.visit_mir_body(tcx));
            return Compilation::Stop;
        }

        // The following adds a new statement "extern crate pri" to the parsed AST as a new item.
        let mut steal = queries.parse().unwrap();
        let items = &mut steal.get_mut().items;
        let item = Item {
            attrs: thin_vec::ThinVec::new(),
            id: DUMMY_NODE_ID,
            span: DUMMY_SP,
            vis: Visibility {
                kind: VisibilityKind::Inherited,
                span: DUMMY_SP,
                tokens: None,
            },
            ident: Ident::with_dummy_span(Symbol::intern("pri")),
            kind: ItemKind::ExternCrate(None),
            tokens: None,
        };
        items.insert(0, P(item));

        Compilation::Continue
    }

    fn after_analysis<'tcx>(
        &mut self,
        compiler: &rustc_interface::interface::Compiler,
        queries: &'tcx Queries<'tcx>,
    ) -> Compilation {
        compiler.session().abort_if_errors();

        queries
            .global_ctxt()
            .unwrap()
            .get_mut()
            .enter(|tcx| self.pass_through_leaf(compiler, tcx));

        Compilation::Continue
    }
}

impl Callbacks {
    fn pass_through_leaf<'tcx>(
        &self,
        compiler: &rustc_interface::interface::Compiler,
        tcx: TyCtxt<'tcx>,
    ) {
        use rustc_middle::mir::MirPass;
        let pass: Box<dyn MirPass> = Box::new(pass::LeafPass {});

        for local_def_id in tcx.hir().body_owners() {
            let def_id = local_def_id.to_def_id();
            // let id: WithOptConstParam<LocalDefId> = rustc_middle::ty::WithOptConstParam::unknown(def_id);
            // let def = rustc_middle::ty::InstanceDef::Item(id);
            // let body = tcx.mir_borrowck_opt_const_arg(id).borrow_mut();
            // pass.run_pass(tcx, body);
        }
    }
}

impl Callbacks {
    // https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/mir/struct.Body.html
    fn visit_mir_body(&mut self, tcx: TyCtxt) {
        for body_id in tcx.hir().body_owners() {
            let id = WithOptConstParam::unknown(body_id.to_def_id());
            let def = InstanceDef::Item(id);
            let body = tcx.instance_mir(def);
            let phase = body.phase;
            log::debug!("Phase: {phase:?}");
            self.visit_basic_blocks(body);
        }
    }

    fn visit_basic_blocks(&mut self, body: &mir::Body) {
        // LocalDecls has all the local variable definitions including parameters.
        // https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/mir/type.LocalDecls.html
        self.visit_local_decls(&body.local_decls);

        // Basic blocks have statements.
        // for basic_block in body.basic_blocks_mut().iter() {
        //     self.visit_basic_block(basic_block);
        // }
    }

    fn visit_local_decls(&mut self, local_decls: &mir::LocalDecls) {
        for local_decl in local_decls {
            log::debug!("{local_decl:?}");
        }
    }

    // https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/mir/struct.BasicBlockData.html
    fn visit_basic_block(&mut self, basic_block: &mir::BasicBlockData) {
        for statement in &basic_block.statements {
            self.visit_statement(&statement);
        }
    }

    fn visit_statement(&mut self, statement: &mir::Statement) {
        // Note: According to https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/mir/enum.StatementKind.html
        // not all StatementKinds are allowed at every MirPhase
        if let LeafConfig::UnitTest = &mut self.config {
            // statement_list.push((&statement.kind).into());
        }
    }
}

fn local_optimized_mir<'tcx>(
    tcx: TyCtxt<'tcx>,
    opt_mir: query_keys::optimized_mir<'tcx>,
) -> query_values::optimized_mir<'tcx> {
    let mut providers = Providers::default();
    rustc_mir_transform::provide(&mut providers);
    // Cloning here is probably not ideal but couldn't find a different way
    let mut body = (providers.optimized_mir)(tcx, opt_mir).clone();

    let mut body = tcx.alloc_steal_mir(body).steal();
    use rustc_middle::mir::MirPass;
    let pass = pass::LeafPass {};
    (&pass as &dyn MirPass).run_pass(tcx, &mut body);

    tcx.arena.alloc(body)
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
*/
