use rustc_middle::mir;
use rustc_span::DUMMY_SP;

use super::CompilationPass;
use crate::{constants::CRATE_RUNTIME, mir_transform::instr::passes::LeafPass};

pub(crate) struct Instrumentator;

impl CompilationPass for Instrumentator {
    fn transform_ast(&mut self, krate: &mut rustc_ast::Crate) {
        add_runtime_as_extern_crate(krate);
    }

    fn transform_mir_body<'tcx>(tcx: rustc_middle::ty::TyCtxt<'tcx>, body: &mut mir::Body<'tcx>) {
        use rustc_middle::mir::MirPass;
        (&LeafPass).run_pass(tcx, body);
    }
}

fn add_runtime_as_extern_crate(program_crate: &mut rustc_ast::Crate) {
    use rustc_ast::*;
    use rustc_span::symbol::{Ident, Symbol};

    // extern crate runtime;
    let item = Item {
        attrs: thin_vec::ThinVec::new(),
        id: DUMMY_NODE_ID,
        span: DUMMY_SP,
        vis: Visibility {
            kind: VisibilityKind::Inherited,
            span: DUMMY_SP,
            tokens: None,
        },
        ident: Ident::with_dummy_span(Symbol::intern(CRATE_RUNTIME)),
        kind: ItemKind::ExternCrate(None),
        tokens: None,
    };
    program_crate.items.insert(0, ptr::P(item));
}
