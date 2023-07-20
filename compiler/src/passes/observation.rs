use rustc_middle::{mir, ty::TyCtxt};

use super::{Compilation, CompilationPass};

/// A wrapper pass that logs pass methods.
pub(crate) struct LoggerPass<T> {
    pass: T,
}

pub(crate) const OBJECTS_TAG: &str = "pass_objects";

macro_rules! target {
    () => {{ std::any::type_name::<T>() }};
}
macro_rules! obj_target {
    () => {
        &format!("{}::{}", OBJECTS_TAG, target!())
    };
}

impl<T> CompilationPass for LoggerPass<T>
where
    T: CompilationPass,
{
    fn visit_ast_before(&mut self, krate: &super::ast::Crate) -> Compilation {
        log::info!(target: target!(), "Visiting AST before transformation {}", krate.id);
        log::trace!(target: obj_target!(), "AST: {:#?}", krate);
        self.pass.visit_ast_before(krate)
    }

    fn visit_ast_after(&mut self, krate: &super::ast::Crate) -> Compilation {
        log::info!(target: target!(), "Visiting AST after transformation {}", krate.id);
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
        log::info!(
            target: target!(),
            "Visiting MIR body before transformation {:#?}",
            body.source.def_id(),
        );
        log::debug!(target: obj_target!(), "MIR body:\n{}", get_mir_pretty(tcx, body));
        T::visit_mir_body_before(tcx, body, storage)
    }

    fn visit_mir_body_after<'tcx>(
        tcx: TyCtxt<'tcx>,
        body: &mir::Body<'tcx>,
        storage: &mut dyn super::Storage,
    ) {
        log::info!(
            target: target!(),
            "Visiting MIR body after transformation {:#?}",
            body.source.def_id(),
        );
        log::debug!(target: obj_target!(), "MIR body:\n{}", get_mir_pretty(tcx, body));
        T::visit_mir_body_after(tcx, body, storage)
    }

    fn transform_ast(&mut self, krate: &mut rustc_ast::Crate) {
        log::info!(target: target!(), "Transforming AST");
        log::debug!(target: obj_target!(), "AST to transform: {:#?}", krate);
        self.pass.transform_ast(krate)
    }

    fn transform_mir_body<'tcx>(
        tcx: TyCtxt<'tcx>,
        body: &mut mir::Body<'tcx>,
        storage: &mut dyn super::Storage,
    ) {
        log::info!(target: target!(), "Transforming MIR body");
        log::debug!(target: obj_target!(), "MIR body to transform:\n{}", get_mir_pretty(tcx, body));
        log::debug!(target: obj_target!(), "Storage: {:#?}", storage);
        T::transform_mir_body(tcx, body, storage)
    }
}

fn get_mir_pretty<'tcx>(tcx: TyCtxt<'tcx>, body: &mir::Body<'tcx>) -> String {
    let mut buffer = Vec::new();
    mir::pretty::write_mir_fn(tcx, body, &mut |_, _| Ok(()), &mut buffer).unwrap();
    String::from_utf8(buffer).unwrap()
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
