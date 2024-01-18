use rustc_ast::{ptr::P, Item, ItemKind, Visibility, VisibilityKind, DUMMY_NODE_ID};
use rustc_span::{
    symbol::{Ident, Symbol},
    DUMMY_SP,
};

use crate::pri_utils::sym::RUNTIME_LIB_CRATE;

use super::CompilationPass;

/// A pass that adds the runtime library as an extern crate to the program.
#[derive(Default)]
pub(crate) struct RuntimeAdder;

impl CompilationPass for RuntimeAdder {
    fn transform_ast(&mut self, krate: &mut rustc_ast::Crate) {
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
            ident: Ident::with_dummy_span(Symbol::intern(*RUNTIME_LIB_CRATE)),
            kind: ItemKind::ExternCrate(None),
            tokens: None,
        };
        krate.items.insert(0, P(item));
    }
}
