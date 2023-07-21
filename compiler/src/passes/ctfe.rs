use std::collections::HashSet;

use bimap::BiHashMap;
use rustc_ast::ptr::P;
use rustc_middle::mir::{self};
use rustc_middle::ty::TyCtxt;
use rustc_span::def_id::{DefId, LocalDefId};
use thin_vec::{thin_vec, ThinVec};

use crate::constants::LEAF_AUG_MOD_NAME;
use crate::pri_utils;

use super::{Compilation, CompilationPass, HasResult, Storage, StorageExt};

use utils::*;

#[derive(Debug, Eq, Hash, PartialEq, Clone, Copy)]
pub(crate) enum CtfeId {
    Const(LocalDefId),
    Promoted(LocalDefId, rustc_middle::mir::Promoted),
}

#[derive(Default)]
pub(crate) struct CtfeScanner {
    block_ids: HashSet<CtfeId>,
}

impl CompilationPass for CtfeScanner {
    fn visit_ctxt(&mut self, tcx: TyCtxt, _storage: &mut dyn Storage) -> Compilation {
        for id in find_ctfes(tcx) {
            log::debug!("Found CTFE: {:?}", &id);
            self.block_ids.insert(id);
        }

        Compilation::Stop
    }
}

impl HasResult<HashSet<CtfeId>> for CtfeScanner {
    fn into_result(self) -> HashSet<CtfeId> {
        self.block_ids
    }
}

pub(crate) struct NctfeFunctionAdder {
    ctfe_count: usize,
}

impl NctfeFunctionAdder {
    pub(crate) fn new(ctfe_count: usize) -> Self {
        Self { ctfe_count }
    }
}

pub(super) const KEY_NCTFE_MAP: &str = "ctfe_func_ids";
pub(super) const KEY_FREE_CTFE_IDS: &str = "ctfe_free_ids";
pub(super) const KEY_FREE_NCTFE_FUNC_IDS: &str = "nctfe_free_ids";

impl CompilationPass for NctfeFunctionAdder {
    fn transform_ast(&mut self, krate: &mut rustc_ast::Crate) {
        /* Adding an empty function (NCTFE) for each CTFE.
         * Later, we will fill them with the corresponding MIR of the CTFE.
         * NOTE: Here we set the return type of the function as unit but later it will be changed
         * to the type of the constant. Apparently, this change in the MIR is not a problem.
         */

        if self.ctfe_count == 0 {
            return;
        }

        log::info!("Adding {} NCTFE functions", self.ctfe_count);
        let mut items = ThinVec::with_capacity(self.ctfe_count);
        for i in 0..self.ctfe_count {
            items.push(P(make_unit_fn_item(
                (format!("ctfe_{}", i)).as_str(),
                thin_vec![make_marker_statement()],
            )));
        }

        krate.items.push(P(make_module(LEAF_AUG_MOD_NAME, items)));
    }

    fn visit_ctxt(&mut self, tcx: TyCtxt, storage: &mut dyn Storage) -> Compilation {
        // We have to scan again because DefIds may have changed after the AST transformation.
        get_free_ctfe_ids(storage).extend(find_ctfes(tcx));
        get_free_nctfe_func_ids(storage).extend({
            let nctfes = find_nctfes(tcx);
            log::info!("Retrieved {} NCTFEs", nctfes.len());
            nctfes
        });

        Compilation::Continue
    }

    fn transform_mir_body<'tcx>(
        tcx: TyCtxt<'tcx>,
        body: &mut mir::Body<'tcx>,
        storage: &mut dyn Storage,
    ) {
        if !is_nctfe(tcx, body.source.def_id().expect_local()) {
            return;
        }

        let def_id = body.source.def_id();

        // This NCTFE function is already assigned to some CTFE.
        let ctfe_id = if let Some(id) = get_nctfe_map(storage).get_by_right(&def_id) {
            log::debug!("NCTFE is already associated with {:?}", id);
            *id
        }
        // Find an unassigned CTFE and this NCTFE function to.
        else if let Some(id) = get_free_ctfe_ids(storage).iter().next() {
            let id = *id;
            associate(id, def_id, storage);
            id
        } else {
            log::warn!("Excess of Non-CTFE functions observed, leaving the function as is");
            return;
        };

        let ctfe_body = match ctfe_id {
            CtfeId::Const(def_id) => tcx.mir_for_ctfe(def_id),
            CtfeId::Promoted(def_id, index) => tcx.promoted_mir(def_id).get(index).unwrap(),
        };

        body.local_decls.truncate(0);
        body.local_decls
            .extend(ctfe_body.local_decls.iter().cloned());
        body.basic_blocks_mut().truncate(0);
        body.basic_blocks_mut()
            .extend(ctfe_body.basic_blocks.iter().cloned());
    }
}

pub(crate) fn get_nctfe_func(id: CtfeId, storage: &mut dyn Storage) -> DefId {
    if let Some(def_id) = get_nctfe_map(storage).get_by_left(&id) {
        *def_id
    } else {
        let def_id = *get_free_nctfe_func_ids(storage)
            .iter()
            .next()
            .expect("Not enough Non-CTFE functions");
        associate(id, def_id, storage);
        def_id
    }
}

fn associate(ctfe_id: CtfeId, nctfe_id: DefId, storage: &mut dyn Storage) {
    log::info!("Associating CTFE {ctfe_id:?} with NCTFE {nctfe_id:?}");
    assert!(get_free_ctfe_ids(storage).remove(&ctfe_id));
    assert!(get_free_nctfe_func_ids(storage).remove(&nctfe_id));
    get_nctfe_map(storage).insert(ctfe_id, nctfe_id);
}

fn get_nctfe_map(storage: &mut dyn Storage) -> &mut BiHashMap<CtfeId, DefId> {
    storage.get_or_default::<BiHashMap<CtfeId, DefId>>(KEY_NCTFE_MAP.to_owned())
}

fn get_free_ctfe_ids(storage: &mut dyn Storage) -> &mut HashSet<CtfeId> {
    storage.get_or_default::<HashSet<CtfeId>>(KEY_FREE_CTFE_IDS.to_owned())
}

fn get_free_nctfe_func_ids(storage: &mut dyn Storage) -> &mut HashSet<DefId> {
    storage.get_or_default::<HashSet<DefId>>(KEY_FREE_NCTFE_FUNC_IDS.to_owned())
}

fn find_ctfes<'tcx>(tcx: TyCtxt<'tcx>) -> impl Iterator<Item = CtfeId> + 'tcx {
    let constants = tcx
        .mir_keys(())
        .iter()
        .filter(move |id| {
            use rustc_hir::def::DefKind;
            matches!(
                tcx.def_kind(id.to_def_id()),
                DefKind::Const | DefKind::ConstParam | DefKind::InlineConst
            )
        })
        .cloned()
        // TODO: Handle functions with substitutions
        .map(CtfeId::Const);

    let promoteds = tcx.mir_keys(()).iter().flat_map(move |id| {
        tcx.promoted_mir(id.to_def_id())
            .indices()
            .map(|index| CtfeId::Promoted(*id, index))
    });

    constants.chain(promoteds)
}

fn find_nctfes(tcx: TyCtxt) -> Vec<DefId> {
    find_aug_module(tcx).map_or(Vec::default(), |m| {
        tcx.hir()
            .module_items(m)
            .filter(move |id| is_nctfe(tcx, id.owner_id.def_id))
            .map(|id| id.owner_id.to_def_id())
            .collect()
    })
}

fn find_aug_module(tcx: TyCtxt) -> Option<LocalDefId> {
    tcx.hir_crate_items(())
        .items()
        .find(|id| {
            matches!(tcx.hir().item(*id).kind, rustc_hir::ItemKind::Mod(_))
                && tcx
                    .def_path(id.owner_id.to_def_id())
                    .data
                    .last()
                    .is_some_and(|d| {
                        d.data
                            .get_opt_name()
                            .is_some_and(|name| name.as_str() == LEAF_AUG_MOD_NAME)
                    })
        })
        .map(|id| id.owner_id.def_id)
}

fn is_nctfe(tcx: TyCtxt, def_id: LocalDefId) -> bool {
    use rustc_hir::*;
    let Some(body_id) = tcx.hir().maybe_body_owned_by(def_id) else {
        return false;
    };
    let ExprKind::Block(block, _) = tcx.hir().body(body_id).value.peel_blocks().kind else {
        return false;
    };
    block.stmts.first().is_some_and(|stmt| match stmt.kind {
        StmtKind::Semi(Expr {
            kind:
                ExprKind::Call(
                    Expr {
                        kind: ExprKind::Path(QPath::Resolved(None, path)),
                        ..
                    },
                    _,
                ),
            ..
        }) => path.res.opt_def_id().is_some_and(|id| {
            pri_utils::eq_def_path_str(tcx, &id, pri_utils::helper_item_name!(mark_as_nctfe))
        }),
        _ => false,
    })
}

mod utils {
    use rustc_ast::{ptr::P, *};
    use rustc_span::symbol::{Ident, Symbol};
    use rustc_span::DUMMY_SP;

    use crate::pri_utils;

    use super::*;

    pub(super) fn make_module(name: &str, items: ThinVec<P<Item>>) -> Item {
        Item {
            attrs: Default::default(),
            id: DUMMY_NODE_ID,
            span: DUMMY_SP,
            vis: Visibility {
                kind: VisibilityKind::Public,
                span: DUMMY_SP,
                tokens: None,
            },
            ident: Ident::from_str(name),
            kind: ItemKind::Mod(
                Unsafe::No,
                ModKind::Loaded(items, Inline::No, Default::default()),
            ),
            tokens: None,
        }
    }

    pub(super) fn make_unit_fn_item(name: &str, stmts: ThinVec<Stmt>) -> Item {
        Item {
            attrs: Default::default(),
            id: DUMMY_NODE_ID,
            span: DUMMY_SP,
            vis: Visibility {
                kind: VisibilityKind::Public,
                span: DUMMY_SP,
                tokens: None,
            },
            ident: Ident::with_dummy_span(Symbol::intern(name)),
            kind: ItemKind::Fn(Box::new(make_unit_fn(stmts))),
            tokens: None,
        }
    }

    fn make_unit_fn(stmts: ThinVec<Stmt>) -> Fn {
        Fn {
            defaultness: Defaultness::Final,
            generics: Default::default(),
            sig: FnSig {
                header: Default::default(),
                decl: P(FnDecl {
                    inputs: Default::default(),
                    output: FnRetTy::Default(DUMMY_SP),
                }),
                span: DUMMY_SP,
            },
            body: Some(P(Block {
                id: DUMMY_NODE_ID,
                stmts,
                rules: BlockCheckMode::Default,
                span: DUMMY_SP,
                tokens: None,
                could_be_bare_literal: false,
            })),
        }
    }

    pub(super) fn make_marker_statement() -> Stmt {
        Stmt {
            id: DUMMY_NODE_ID,
            kind: StmtKind::Semi(P(make_dummy_expr(ExprKind::Call(
                P(make_dummy_expr(ExprKind::Path(
                    None,
                    make_pri_helper_item_path(pri_utils::helper_item_name!(mark_as_nctfe)),
                ))),
                Default::default(),
            )))),
            span: DUMMY_SP,
        }
    }

    #[inline]
    fn make_dummy_expr(kind: ExprKind) -> Expr {
        Expr {
            id: DUMMY_NODE_ID,
            kind,
            span: DUMMY_SP,
            attrs: Default::default(),
            tokens: None,
        }
    }

    fn make_pri_helper_item_path(name: &String) -> Path {
        Path {
            span: DUMMY_SP,
            segments: name
                .split("::")
                .map(str::trim)
                .map(Symbol::intern)
                .map(Ident::with_dummy_span)
                .map(PathSegment::from_ident)
                .collect(),
            tokens: None,
        }
    }
}
