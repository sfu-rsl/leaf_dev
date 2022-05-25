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
    mir::{BasicBlockData, Body, LocalDecls, Place, Rvalue, Statement, StatementKind},
    ty::{InstanceDef, TyCtxt, WithOptConstParam},
};
use std::path::PathBuf;

pub struct RunCompiler;

impl RunCompiler {
    pub fn run(args: &mut Vec<String>, input_path: Option<PathBuf>) -> (i32, Vec<SimpleStatementKind>) {
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

        let mut cb = Callbacks::default();
        rustc_driver::install_ice_hook();
        let exit_code = rustc_driver::catch_with_exit_code(|| rustc_driver::RunCompiler::new(args, &mut cb).run());
        (exit_code, cb.statements)
    }
}

/// A simpler version of
/// [StatementKind](https://doc.rust-lang.org/1.57.0/nightly-rustc/rustc_middle/mir/enum.StatementKind.html)
/// due to issues with the upstream type not being thread-safe (e.g. compiler errors that say
/// "`rustc_span::span_encoding::Span` cannot be shared between threads safely") and how there might
/// be irrelevant info anyway.
/// 
/// TODO: Convert `Place` to a similar version
/// TODO: TerminatorKind
/// TODO: Migrate tests to use the serializable types in the `steveyko` branch
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SimpleStatementKind {
    Assign(SimpleRvalue), // (Box<(Place<'tcx>, Rvalue<'tcx>)>)
    FakeRead, // (Box<(FakeReadCause, Place<'tcx>)>)
    SetDiscriminant, // { place: Box<Place<'tcx>>, variant_index: VariantIdx, },
    StorageLive, // (Local)
    StorageDead, // (Local)
    LlvmInlineAsm, // (Box<LlvmInlineAsm<'tcx>>)
    Retag, // (RetagKind, Box<Place<'tcx>>)
    AscribeUserType, // (Box<(Place<'tcx>, UserTypeProjection)>, Variance)
    Coverage, // (Box<Coverage>)
    CopyNonOverlapping, // (Box<CopyNonOverlapping<'tcx>>)
    Nop,
}

impl SimpleStatementKind {
    fn from_statement_kind(statement_kind: &StatementKind) -> Self {
        // Note: According to https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/mir/enum.StatementKind.html
        // not all StatementKinds are allowed at every MirPhase
        match statement_kind {
            StatementKind::Assign(b) => {
                let (place, rvalue): &(Place<'_>, Rvalue<'_>) = &*b;
                SimpleStatementKind::Assign(SimpleRvalue::from_rvalue(&*rvalue))
            }
            StatementKind::FakeRead(_) => SimpleStatementKind::FakeRead,
            StatementKind::SetDiscriminant { .. } => SimpleStatementKind::SetDiscriminant,
            StatementKind::StorageLive(_) => SimpleStatementKind::StorageLive,
            StatementKind::StorageDead(_) => SimpleStatementKind::StorageDead,
            StatementKind::LlvmInlineAsm(_) => SimpleStatementKind::LlvmInlineAsm,
            StatementKind::Retag(_, _) => SimpleStatementKind::Retag,
            StatementKind::AscribeUserType(_, _) => SimpleStatementKind::AscribeUserType,
            StatementKind::Coverage(_) => SimpleStatementKind::Coverage,
            StatementKind::CopyNonOverlapping(_) => SimpleStatementKind::CopyNonOverlapping,
            StatementKind::Nop => SimpleStatementKind::Nop,
        }

    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SimpleRvalue {
    Use, // (Operand<'tcx>)
    Repeat, // (Operand<'tcx>, ty::Const<'tcx>)
    Ref, // (Region<'tcx>, BorrowKind, Place<'tcx>)
    ThreadLocalRef, // (DefId)
    AddressOf, // (Mutability, Place<'tcx>)
    Len, // (Place<'tcx>)
    Cast, // (CastKind, Operand<'tcx>, Ty<'tcx>)
    BinaryOp, // (BinOp, Box<(Operand<'tcx>, Operand<'tcx>)>)
    CheckedBinaryOp, // (BinOp, Box<(Operand<'tcx>, Operand<'tcx>)>)
    NullaryOp, // (NullOp, Ty<'tcx>)
    UnaryOp, // (UnOp, Operand<'tcx>)
    Discriminant, // (Place<'tcx>)
    Aggregate, // (Box<AggregateKind<'tcx>>, Vec<Operand<'tcx>>)
    ShallowInitBox, // (Operand<'tcx>, Ty<'tcx>)
}

impl SimpleRvalue {
    fn from_rvalue(rvalue: &Rvalue) -> Self {
        match rvalue {
            Rvalue::Use(_) => SimpleRvalue::Use,
            Rvalue::Repeat(_, _) => SimpleRvalue::Repeat,
            Rvalue::Ref(_, _, _) => SimpleRvalue::Ref,
            Rvalue::ThreadLocalRef(_) => SimpleRvalue::ThreadLocalRef,
            Rvalue::AddressOf(_, _) => SimpleRvalue::AddressOf,
            Rvalue::Len(_) => SimpleRvalue::Len,
            Rvalue::Cast(_, _, _) => SimpleRvalue::Cast,
            Rvalue::BinaryOp(_, _) => SimpleRvalue::BinaryOp,
            Rvalue::CheckedBinaryOp(_, _) => SimpleRvalue::CheckedBinaryOp,
            Rvalue::NullaryOp(_, _) => SimpleRvalue::NullaryOp,
            Rvalue::UnaryOp(_, _) => SimpleRvalue::UnaryOp,
            Rvalue::Discriminant(_) => SimpleRvalue::Discriminant,
            Rvalue::Aggregate(_, _) => SimpleRvalue::Aggregate,
            Rvalue::ShallowInitBox(_, _) => SimpleRvalue::ShallowInitBox,
        }
    }
}

struct Callbacks {
    statements: Vec<SimpleStatementKind>
}

impl Default for Callbacks {
    fn default() -> Self {
        Callbacks {
            statements: vec![]
        }
    }
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

    fn visit_basic_blocks(&mut self, body: &Body) {
        // LocalDecls has all the local variable definitions including parameters.
        // https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/mir/type.LocalDecls.html
        self.visit_local_decls(&body.local_decls);

        // Basic blocks have statements.
        for basic_block in body.basic_blocks().iter() {
            self.visit_basic_block(basic_block);
        }
    }

    fn visit_local_decls(&mut self, local_decls: &LocalDecls) {
        for local_decl in local_decls {
            log::debug!("{local_decl:?}");
        }
    }

    // https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/mir/struct.BasicBlockData.html
    fn visit_basic_block(&mut self, basic_block: &BasicBlockData) {
        for statement in &basic_block.statements {
            self.visit_statement(&statement);
        }
    }

    fn visit_statement(&mut self, statement: &Statement) {
        self.statements.push(SimpleStatementKind::from_statement_kind(&statement.kind));
    }
}
