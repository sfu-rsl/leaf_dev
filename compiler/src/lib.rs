#![feature(rustc_private)]
#![feature(custom_mir)]
#![feature(let_chains)]
#![feature(extend_one)]
#![feature(box_patterns)]
#![feature(extract_if)]
#![deny(rustc::internal)]
#![feature(iter_order_by)]
#![feature(macro_metavar_expr)]

mod mir_transform;
mod passes;
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
extern crate rustc_session;
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

use std::env;
use std::path::PathBuf;

use constants::*;

pub struct RunCompiler;

macro_rules! read_var {
    ($name:expr) => {{ std::env::var($name).ok() }};
}

impl RunCompiler {
    pub fn run(args: &mut Vec<String>, input_path: Option<PathBuf>) -> i32 {
        args.push(OPT_SYSROOT.to_owned());
        args.push(find_sysroot());

        args.push(OPT_UNSTABLE.to_owned());

        // Add runtime library as a direct dependency.
        args.push(OPT_EXTERN.to_owned());
        args.push(format!(
            "force:{}={}",
            CRATE_RUNTIME,
            find_runtime_lib_path()
        ));

        // Add all project's dependencies into the search path.
        args.push(OPT_SEARCH_PATH.to_owned());
        args.push(find_deps_path());

        if let Some(ip) = input_path {
            args.push(ip.to_str().unwrap().to_string());
        }

        rustc_driver::install_ice_hook(URL_BUG_REPORT, |_| ());

        let mut cb = Callbacks {};

        log::info!("Running compiler with args: {:?}", args);
        rustc_driver::catch_with_exit_code(|| rustc_driver::RunCompiler::new(args, &mut cb).run())
    }
}

fn find_sysroot() -> String {
    let try_rustc = || {
        std::process::Command::new(CMD_RUSTC)
            .arg(OPT_PRINT_SYSROOT)
            .current_dir(".")
            .output()
            .ok()
            .map(|out| std::str::from_utf8(&out.stdout).unwrap().trim().to_owned())
    };

    let try_toolchain_env = || {
        read_var!(ENV_RUSTUP_HOME)
            .zip(read_var!(ENV_TOOLCHAIN))
            .map(|(home, toolchain)| format!("{home}/toolchains/{toolchain}"))
    };

    let try_sysroot_env = || read_var!(ENV_SYSROOT).map(|s| s.to_owned());

    // NOTE: Check the correct priority of these variables.
    try_rustc()
        .or_else(try_toolchain_env)
        .or_else(try_sysroot_env)
        .expect("Unable to find sysroot.")
}

fn find_runtime_lib_path() -> String {
    // FIXME: Do not depend on the project's structure and adjacency of runtime.

    let try_exe_path = || {
        std::env::current_exe()
            .ok()
            .map(|path| path.parent().map(|p| p.join(FILE_RUNTIME_LIB)))
            .flatten()
            .filter(|path| path.exists())
            .map(|path| path.to_string_lossy().to_string())
    };

    try_exe_path().expect("Unable to find runtime lib file.")
}

fn find_deps_path() -> String {
    // FIXME: Don't depend on the project structure and adjacency of runtime.

    let try_exe_path = || {
        std::env::current_exe()
            .ok()
            .map(|path| path.parent().map(|p| p.join(DIR_DEPS)))
            .flatten()
            .filter(|path| path.exists())
            .map(|path| path.to_string_lossy().to_string())
    };

    try_exe_path().expect("Unable to find deps path.")
}

#[allow(dead_code)]
struct Callbacks {}

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
    let pass = mir_transform::instrumentation::passes::LeafPass {};
    (&pass as &dyn MirPass).run_pass(tcx, &mut body);

    tcx.arena.alloc(body)
}


mod constants {
    pub(super) const CMD_RUSTC: &str = "rustc";

    pub(super) const CRATE_RUNTIME: &str = "runtime";

    pub(super) const DIR_DEPS: &str = "deps";

    pub(super) const ENV_RUSTUP_HOME: &str = "RUSTUP_HOME";
    pub(super) const ENV_SYSROOT: &str = "RUST_SYSROOT";
    pub(super) const ENV_TOOLCHAIN: &str = "RUSTUP_TOOLCHAIN";

    pub(super) const FILE_RUNTIME_LIB: &str = "libruntime.rlib";

    pub(super) const OPT_EXTERN: &str = "--extern";
    pub(super) const OPT_PRINT_SYSROOT: &str = "--print=sysroot";
    pub(super) const OPT_SYSROOT: &str = "--sysroot";
    pub(super) const OPT_SEARCH_PATH: &str = "-L";
    pub(super) const OPT_UNSTABLE: &str = "-Zunstable-options";

    pub(super) const URL_BUG_REPORT: &str = "https://github.com/sfu-rsl/leaf/issues/new";
}
