use rustc_ast::{ast, attr, ptr::P, Item, ItemKind, Visibility, VisibilityKind, DUMMY_NODE_ID};
use rustc_session::Session;
use rustc_span::{
    sym,
    symbol::{Ident, Symbol},
    DUMMY_SP,
};

use crate::pri_utils::sym::RUNTIME_LIB_CRATE;

use super::CompilationPass;
use common::log_info;

/// A pass that adds the runtime library as an extern crate to the program.
#[derive(Clone)]
pub(crate) struct RuntimeExternCrateAdder {
    enabled: bool,
    crate_name: Option<String>,
}

impl RuntimeExternCrateAdder {
    pub fn new(enabled: bool, crate_name: Option<String>) -> Self {
        Self {
            crate_name,
            enabled,
        }
    }
}

impl CompilationPass for RuntimeExternCrateAdder {
    fn transform_ast(
        &mut self,
        _session: &Session,
        krate: &mut rustc_ast::Crate,
        _storage: &mut dyn super::Storage,
    ) {
        if !self.enabled {
            return;
        }

        let crate_name = self
            .crate_name
            .as_ref()
            .map(|n| n.as_str())
            .unwrap_or(*RUNTIME_LIB_CRATE);

        // extern crate runtime as `crate_name`;
        let item = Item {
            attrs: Default::default(),
            id: DUMMY_NODE_ID,
            span: DUMMY_SP,
            vis: Visibility {
                kind: VisibilityKind::Inherited,
                span: DUMMY_SP,
                tokens: None,
            },
            ident: Ident::with_dummy_span(Symbol::intern(crate_name)),
            kind: ItemKind::ExternCrate(Some(Symbol::intern(*RUNTIME_LIB_CRATE))),
            tokens: None,
        };
        krate.items.insert(0, P(item));

        log_info!(
            "Added extern crate statement for the runtime library: extern crate {} as {};",
            *RUNTIME_LIB_CRATE,
            crate_name
        );
    }
}

// FIXME: Make it configurable
const TOOL_NAME: &str = crate::constants::TOOL_LEAF;

#[derive(Debug, Default)]
pub(crate) struct LeafToolAdder;

impl CompilationPass for LeafToolAdder {
    fn transform_ast(
        &mut self,
        session: &Session,
        krate: &mut rustc_ast::Crate,
        _storage: &mut dyn super::Storage,
    ) {
        // #![feature(register_tool)]
        let generator = &session.psess.attr_id_generator;
        krate.attrs.push(attr::mk_attr_nested_word(
            &generator,
            ast::AttrStyle::Inner,
            ast::Safety::Default,
            sym::feature,
            sym::register_tool,
            DUMMY_SP,
        ));
        // #![register_tool(leaf)]
        krate.attrs.push(attr::mk_attr_nested_word(
            generator,
            ast::AttrStyle::Inner,
            ast::Safety::Default,
            sym::register_tool,
            Symbol::intern(TOOL_NAME),
            DUMMY_SP,
        ));

        log_info!("Added tool registration attributes to the crate.");
    }
}
