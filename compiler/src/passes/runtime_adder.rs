use rustc_ast::{ptr::P, Item, ItemKind, Visibility, VisibilityKind, DUMMY_NODE_ID};
use rustc_span::{
    symbol::{Ident, Symbol},
    DUMMY_SP,
};

use super::CompilationPass;

/// A pass that adds the runtime library as an extern crate to the program.
#[derive(Clone)]
pub(crate) struct RuntimeAdder {
    crate_name: String,
    enabled: bool,
}

impl RuntimeAdder {
    pub fn new(crate_name: String, enabled: bool) -> Self {
        Self {
            crate_name,
            enabled,
        }
    }
}

impl CompilationPass for RuntimeAdder {
    fn transform_ast(&mut self, krate: &mut rustc_ast::Crate) {
        if !self.enabled {
            return;
        }

        // extern crate runtime;
        let item = Item {
            attrs: Default::default(),
            id: DUMMY_NODE_ID,
            span: DUMMY_SP,
            vis: Visibility {
                kind: VisibilityKind::Inherited,
                span: DUMMY_SP,
                tokens: None,
            },
            ident: Ident::with_dummy_span(Symbol::intern(&self.crate_name)),
            kind: ItemKind::ExternCrate(Some(Symbol::intern(
                *crate::pri_utils::sym::RUNTIME_LIB_CRATE,
            ))),
            tokens: None,
        };
        krate.items.insert(0, P(item));
    }
}
