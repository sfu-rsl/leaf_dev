#![feature(rustc_private)]
#![deny(rustc::internal)]

extern crate rustc_ast;
extern crate rustc_driver;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_mir_transform;
extern crate rustc_span;

mod transformer;

use rustc_ast::{ast, ptr};
use rustc_interface::interface;
use rustc_middle::ty::{
    self,
    query::{query_keys, query_values, Providers},
};
use rustc_span::symbol;
use std::path;

pub struct RunCompiler;

impl RunCompiler {
    pub fn run(args: &mut Vec<String>, input_path: Option<path::PathBuf>) -> i32 {
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
        compiler: &interface::Compiler,
        queries: &'tcx rustc_interface::Queries<'tcx>,
    ) -> rustc_driver::Compilation {
        compiler.session().abort_if_errors();

        // The following adds a new statement "extern crate rc0lib" to the parsed AST as a new item.
        let items = &mut queries.parse().unwrap().peek_mut().items;
        let item = ast::Item {
            attrs: Vec::new(),
            id: rustc_ast::DUMMY_NODE_ID,
            span: rustc_span::DUMMY_SP,
            vis: ast::Visibility {
                kind: ast::VisibilityKind::Inherited,
                span: rustc_span::DUMMY_SP,
                tokens: None,
            },
            ident: symbol::Ident::with_dummy_span(symbol::Symbol::intern("rc0lib")),
            kind: ast::ItemKind::ExternCrate(None),
            tokens: None,
        };
        items.insert(0, ptr::P(item));

        rustc_driver::Compilation::Continue
    }
}

fn local_optimized_mir<'tcx>(
    tcx: ty::TyCtxt<'tcx>,
    opt_mir: query_keys::optimized_mir<'tcx>,
) -> query_values::optimized_mir<'tcx> {
    let mut providers = Providers::default();
    rustc_mir_transform::provide(&mut providers);
    let body = (providers.optimized_mir)(tcx, opt_mir);

    // Cloning here is probably not ideal but couldn't find a different way
    let mut body = tcx.alloc_steal_mir((*body).clone()).steal();
    transformer::Transformer::new(tcx).transform(&mut body);

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
