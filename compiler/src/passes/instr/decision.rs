use const_format::concatcp;
use rustc_hir::{def_id::DefId, definitions::DefPathData};
use rustc_middle::{mir::Body, ty::TyCtxt};
use rustc_span::Symbol;

use common::{log_info, log_warn};

use crate::utils::mir::TyCtxtExt;

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

    // It is in the module defining lang_start items (std rt module)
    if tcx
        .lang_items()
        .start_fn()
        .map(|id| tcx.module_of(id).collect::<Vec<_>>())
        .zip(Some(tcx.module_of(def_id)))
        .is_some_and(|(start_mod, this_mod)| {
            let n = start_mod.len();
            start_mod.into_iter().eq(this_mod.take(n))
        })
    {
        return false;
    }

    // FIXME: Drops are important for bug detection, however we avoid instrumenting them for now.
    let mut drop_fn_ids = {
        use rustc_hir::LanguageItems as Items;
        [
            Items::drop_in_place_fn,
            Items::async_drop_in_place_fn,
            Items::surface_async_drop_in_place_fn,
            Items::async_drop_surface_drop_in_place_fn,
            Items::async_drop_slice_fn,
            Items::async_drop_chain_fn,
            Items::async_drop_noop_fn,
            Items::async_drop_deferred_drop_in_place_fn,
            Items::async_drop_fuse_fn,
            Items::async_drop_defer_fn,
            Items::async_drop_either_fn,
        ]
        .iter()
        .filter_map(|item| item(tcx.lang_items()))
    };
    if drop_fn_ids.any(|id| id == def_id)
        || tcx
            .lang_items()
            .drop_trait()
            .zip(
                tcx.impl_of_method(def_id)
                    .and_then(|id| tcx.trait_id_of_impl(id)),
            )
            .is_some_and(|(t1, t2)| t1 == t2)
    {
        return false;
    }

    // FIXME: To be replaced with a better-specified list.
    let def_path = &tcx.def_path_debug_str(def_id);
    if def_path.contains("panicking")
        || (def_path.contains("core_arch")
            || (def_path.starts_with("core") && def_path.contains("arch::")))
        || (def_path.starts_with("std")
            && (def_path.contains("thread")
                || def_path.contains("sync")
                || def_path.contains("arch::")))
    {
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
    // Avoid possibly problematic const items.
    // See https://github.com/rust-lang/rust/issues/128145
    if matches!(
        tcx.def_key(def_id).disambiguated_data.data,
        DefPathData::AnonConst
    ) {
        return None;
    }

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
