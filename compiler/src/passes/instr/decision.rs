use const_format::concatcp;
use rustc_hir::def_id::DefId;
use rustc_middle::{mir::Body, ty::TyCtxt};
use rustc_span::Symbol;

use common::{log_info, log_warn};

pub(super) const TAG_INSTR_DECISION: &str = concatcp!(super::TAG_INSTRUMENTATION, "::skip");

const TOOL_NAME: &str = crate::constants::TOOL_LEAF;
const ATTR_NAME: &str = "instrument";

pub(super) fn should_instrument<'tcx>(tcx: TyCtxt<'tcx>, body: &mut Body<'tcx>) -> bool {
    let def_id = body.source.def_id();

    if let Some((explicit, item)) = opt_instrument_attr_inheritable(tcx, def_id) {
        log_info!(
            target: TAG_INSTR_DECISION,
            "Found explicit instrumentation attribute for {:?} on {:?} with value: {}",
            def_id,
            item,
            explicit
        );
        return explicit;
    }

    if tcx.lang_items().start_fn().is_some_and(|id| id == def_id) {
        return false;
    }

    // FIXME: To be replaced with a better-specified list.
    if tcx.def_path_str(def_id).contains("panicking") || tcx.def_path_str(def_id).contains("sync") {
        return false;
    }

    // FIXME: A const function doesn't mean it won't be called at runtime.
    if tcx.is_const_fn(def_id) {
        return false;
    }

    // Some intrinsic functions have body.
    if tcx.intrinsic(def_id).is_some() {
        return false;
    }

    true
}

/// Returns the value of the `instrument` attribute if it is placed on the item or one of its ancestors.
fn opt_instrument_attr_inheritable<'tcx>(
    tcx: TyCtxt<'tcx>,
    def_id: DefId,
) -> Option<(bool, DefId)> {
    let mut current = def_id;
    loop {
        let attr = opt_instrument_attr(tcx, current);
        if attr.is_some() {
            return attr.map(|v| (v, current));
        }

        let parent = tcx.opt_parent(current);
        current = match parent {
            Some(parent) => parent,
            None => return None,
        };
    }
}

/// Returns the value of the `instrument` attribute if it is placed on the item.
/// If the attribute is not found, or the argument passed to the attribute is invalid
/// returns `None`.
fn opt_instrument_attr<'tcx>(tcx: TyCtxt<'tcx>, def_id: DefId) -> Option<bool> {
    tcx.get_attrs_by_path(
        def_id,
        &[Symbol::intern(TOOL_NAME), Symbol::intern(ATTR_NAME)],
    )
    .next()
    .and_then(|attr| match &attr.kind {
        rustc_ast::AttrKind::Normal(attr) => Some(attr),
        _ => None,
    })
    .map(|attr| attr.item.args.inner_tokens())
    .map(|t| t.into_trees().next_ref().cloned())
    .and_then(|token| {
        match token {
            // No argument means it's enabled.
            None => Some(true),
            Some(token) => {
                let as_bool = match &token {
                    rustc_ast::tokenstream::TokenTree::Token(token, ..) => token
                        .is_bool_lit()
                        .then(|| token.ident().unwrap().0.name == rustc_span::symbol::kw::True),
                    _ => None,
                };
                if as_bool.is_none() {
                    log_warn!(
                        "Invalid argument for attribute `{}`: {:?}",
                        ATTR_NAME,
                        token
                    );
                }
                as_bool
            }
        }
    })
}
