#![feature(rustc_private)]
#![feature(custom_mir)]
#![feature(let_chains)]
#![feature(extend_one)]
#![feature(box_patterns)]
#![feature(drain_filter)]
#![deny(rustc::internal)]
#![feature(iter_order_by)]
#![feature(macro_metavar_expr)]

mod mir_transform;
mod pass;
mod visit;

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

use rustc_driver::Compilation;
use rustc_interface::{
    interface::{Compiler, Config},
    Queries,
};
use rustc_middle::{
    mir::Body,
    query::{AsLocalKey, Providers},
    ty::TyCtxt,
};
use rustc_span::{
    def_id::DefId,
    symbol::{Ident, Symbol},
    DUMMY_SP,
};

use std::path::PathBuf;

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
            (Some(home), Some(toolchain)) => format!("{home}/toolchains/{toolchain}"),
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

        // Add runtime library as extern crate
        std::fs::read_dir(&path)
            .expect("unable to read directory")
            .filter_map(|r| r.ok())
            .filter(|file_name| {
                let name = file_name.file_name().to_string_lossy().to_string();
                name.ends_with(".rlib") && name.starts_with("libruntime-")
            })
            .take(1)
            .for_each(|file_name| {
                args.push(String::from("--extern"));
                args.push(format!(
                    "runtime={}",
                    path.join(file_name.file_name()).to_string_lossy()
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
        rustc_driver::install_ice_hook("https://github.com/sfu-rsl/leaf/issues/new", |_| ());

        rustc_driver::catch_with_exit_code(|| rustc_driver::RunCompiler::new(args, &mut cb).run())
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

        config.override_queries = Some(|_session, lprov, _eprov| {
            // TODO: Compare this approach with after_analysis.
            lprov.optimized_mir = local_optimized_mir;
        });
    }

    fn after_parsing<'tcx>(
        &mut self,
        compiler: &Compiler,
        queries: &'tcx Queries<'tcx>,
    ) -> Compilation {
        compiler.session().abort_if_errors();

        // The following adds "extern crate runtime" to the parsed AST.
        use rustc_ast::{
            ast::{Item, ItemKind, Visibility, VisibilityKind, DUMMY_NODE_ID},
            ptr::P,
        };

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
            ident: Ident::with_dummy_span(Symbol::intern("runtime")),
            kind: ItemKind::ExternCrate(None),
            tokens: None,
        };
        items.insert(0, P(item));

        Compilation::Continue
    }
}

fn local_optimized_mir(tcx: TyCtxt, opt_mir: <DefId as AsLocalKey>::LocalKey) -> &Body {
    let mut providers = Providers::default();
    rustc_mir_transform::provide(&mut providers);
    // Cloning here is probably not ideal but couldn't find a different way
    let body = (providers.optimized_mir)(tcx, opt_mir).clone();

    let mut body = tcx.alloc_steal_mir(body).steal();
    use rustc_middle::mir::MirPass;
    let pass = pass::LeafPass {};
    (&pass as &dyn MirPass).run_pass(tcx, &mut body);

    tcx.arena.alloc(body)
}

/*
 * //TODO: Bring this back.
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
