#![feature(rustc_private, box_patterns)]
#![deny(rustc::internal)]

extern crate rustc_ast;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_mir_build;
extern crate rustc_mir_transform;
extern crate rustc_span;

mod const_separator;
mod preprocessor;
mod transformer;

use rustc_ast::{
    ast::{Item, ItemKind, Visibility, VisibilityKind},
    ptr::P,
    DUMMY_NODE_ID,
};
use rustc_driver::Compilation;
use rustc_interface::{
    interface::{Compiler, Config},
    Queries,
};
use rustc_middle::ty::{
    query::{query_keys, query_values, Providers},
    TyCtxt,
};
use rustc_span::{
    symbol::{Ident, Symbol},
    DUMMY_SP,
};
use std::path::PathBuf;

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

        // The following adds a new statement "extern crate leafrt" to the parsed AST as a new item.
        let items = &mut queries.parse().unwrap().peek_mut().items;
        let item = Item {
            attrs: Vec::new(),
            id: DUMMY_NODE_ID,
            span: DUMMY_SP,
            vis: Visibility {
                kind: VisibilityKind::Inherited,
                span: DUMMY_SP,
                tokens: None,
            },
            ident: Ident::with_dummy_span(Symbol::intern("leafrt")),
            kind: ItemKind::ExternCrate(None),
            tokens: None,
        };
        items.insert(0, P(item));

        Compilation::Continue
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

    //let mut body = tcx.alloc_steal_mir(body).steal();
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
