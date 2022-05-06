#![feature(rustc_private)]
#![deny(rustc::internal)]

extern crate rustc_driver;
extern crate rustc_interface;
extern crate rustc_middle;

use log;
use rustc_driver::Compilation;
use rustc_interface::{
    interface::{Compiler, Config},
    Queries,
};
use rustc_middle::{
    mir::{BasicBlockData, Body, LocalDecls, Statement, StatementKind},
    ty::{InstanceDef, TyCtxt, WithOptConstParam},
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
    }

    fn after_parsing<'tcx>(
        &mut self,
        compiler: &Compiler,
        queries: &'tcx Queries<'tcx>,
    ) -> Compilation {
        compiler.session().abort_if_errors();

        queries
            .global_ctxt()
            .unwrap()
            .peek_mut()
            .enter(|tcx| self.visit_mir_body(tcx));

        Compilation::Stop
    }
}

impl Callbacks {
    // https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/mir/struct.Body.html
    fn visit_mir_body(&self, tcx: TyCtxt) {
        for body_id in tcx.hir().body_owners() {
            let id = WithOptConstParam::unknown(body_id.to_def_id());
            let def = InstanceDef::Item(id);
            let body = tcx.instance_mir(def);
            self.visit_basic_blocks(body);
        }
    }

    fn visit_basic_blocks(&self, body: &Body) {
        // LocalDecls has all the local variable definitions including parameters.
        // https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/mir/type.LocalDecls.html
        self.visit_local_decls(&body.local_decls);

        // Basic blocks have statements.
        for basic_block in body.basic_blocks().iter() {
            self.visit_basic_block(basic_block);
        }
    }

    fn visit_local_decls(&self, local_decls: &LocalDecls) {
        for local_decl in local_decls {
            log::debug!("{local_decl:?}");
        }
    }

    // https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/mir/struct.BasicBlockData.html
    fn visit_basic_block(&self, basic_block: &BasicBlockData) {
        for statement in &basic_block.statements {
            self.visit_statement(&statement);
        }
    }

    fn visit_statement(&self, statement: &Statement) {
        /*
         pub enum StatementKind<'tcx> {
             Assign(Box<(Place<'tcx>, Rvalue<'tcx>)>),
             FakeRead(Box<(FakeReadCause, Place<'tcx>)>),
             SetDiscriminant {
                 place: Box<Place<'tcx>>,
                 variant_index: VariantIdx,
             },
             Deinit(Box<Place<'tcx>>),
             StorageLive(Local),
             StorageDead(Local),
             Retag(RetagKind, Box<Place<'tcx>>),
             AscribeUserType(Box<(Place<'tcx>, UserTypeProjection)>, Variance),
             Coverage(Box<Coverage>),
             CopyNonOverlapping(Box<CopyNonOverlapping<'tcx>>),
             Nop,
         }
        */
        match &statement.kind {
            StatementKind::Assign(_) => {
                log::debug!("Assign: {statement:?}");
            }
            StatementKind::FakeRead(_) => {
                log::debug!("FakeRead: {statement:?}");
            }
            StatementKind::SetDiscriminant { .. } => {
                log::debug!("SetDiscriminant: {statement:?}");
            }
            _ => {
                log::debug!("Other: {statement:?}");
            }
        }
    }
}
