use rustc_middle::{mir, ty::TyCtxt};

use super::{Compilation, CompilationPass};

/// A wrapper pass that logs pass methods.
pub(crate) struct LoggerPass<T> {
    pass: T,
}

macro_rules! target {
    () => {{ std::any::type_name::<T>() }};
}
macro_rules! obj_target {
    () => {
        &format!("{}::{}", target!(), "objects")
    };
}

impl<T> CompilationPass for LoggerPass<T>
where
    T: CompilationPass,
{
    fn visit_ast_before(&mut self, krate: &super::ast::Crate) -> Compilation {
        log::info!(target: target!(), "Visiting AST before transformation");
        log::trace!(target: obj_target!(), "AST: {:#?}", krate);
        self.pass.visit_ast_before(krate)
    }

    fn visit_ast_after(&mut self, krate: &super::ast::Crate) -> Compilation {
        log::info!(target: target!(), "Visiting AST after transformation");
        log::debug!(target: obj_target!(), "AST: {:#?}", krate);
        self.pass.visit_ast_after(krate)
    }

    fn visit_ctxt<'tcx>(
        &mut self,
        tcx: TyCtxt<'tcx>,
        storage: &mut dyn super::Storage,
    ) -> Compilation {
        log::info!(target: target!(), "Visiting TyCtxt");
        self.pass.visit_ctxt(tcx, storage)
    }

    fn visit_mir_body_before<'tcx>(
        tcx: TyCtxt<'tcx>,
        body: &mir::Body<'tcx>,
        storage: &mut dyn super::Storage,
    ) {
        log::info!(target: target!(), "Visiting MIR body before transformation");
        log::debug!(target: obj_target!(), "MIR body: {:#?}", body);
        T::visit_mir_body_before(tcx, body, storage)
    }

    fn visit_mir_body_after<'tcx>(
        _tcx: TyCtxt<'tcx>,
        body: &mir::Body<'tcx>,
        storage: &mut dyn super::Storage,
    ) {
        log::info!(target: target!(), "Visiting MIR body after transformation");
        log::debug!(target: obj_target!(), "MIR body: {:#?}", body);
        T::visit_mir_body_after(_tcx, body, storage)
    }

    fn transform_ast(&mut self, krate: &mut rustc_ast::Crate) {
        log::info!(target: target!(), "Transforming AST");
        log::debug!(target: obj_target!(), "AST to transform: {:#?}", krate);
        self.pass.transform_ast(krate)
    }

    fn transform_mir_body<'tcx>(
        _tcx: rustc_middle::ty::TyCtxt<'tcx>,
        body: &mut rustc_middle::mir::Body<'tcx>,
        storage: &mut dyn super::Storage,
    ) {
        log::info!(target: target!(), "Transforming MIR body");
        log::debug!(target: obj_target!(), "MIR body to transform: {:#?}", body);
        log::debug!(target: obj_target!(), "Storage: {:#?}", storage);
        T::transform_mir_body(_tcx, body, storage)
    }
}

pub(crate) trait CompilationPassLogExt {
    fn into_logged(self) -> LoggerPass<Self>
    where
        Self: Sized;
}
impl<T: CompilationPass> CompilationPassLogExt for T {
    fn into_logged(self) -> LoggerPass<T>
    where
        Self: Sized,
    {
        LoggerPass { pass: self }
    }
}
