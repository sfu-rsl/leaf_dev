use rustc_middle::mir;

use super::{CompilationPass, Storage};
use crate::mir_transform::instr::passes::LeafPass;

#[derive(Default)]
pub(crate) struct Instrumentator;

impl CompilationPass for Instrumentator {
    fn transform_mir_body<'tcx>(
        tcx: rustc_middle::ty::TyCtxt<'tcx>,
        body: &mut mir::Body<'tcx>,
        storage: &mut dyn Storage,
    ) {
        LeafPass.transform(tcx, body, storage);
    }
}
