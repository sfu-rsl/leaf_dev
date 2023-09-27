use crate::utils::TypeExport;

use super::{CompilationPass, Storage};

use lazy_static::lazy_static;
use rustc_middle::{
    mir::{self, visit::Visitor, HasLocalDecls, LocalDecls, Location, Place},
    ty::TyCtxt,
};
use std::{collections::HashMap, sync::Mutex};

/*
 * Type pass to export type information
 */
#[derive(Default)]
pub(crate) struct TypePass;

lazy_static! {
    static ref MAP: Mutex<HashMap<String, String>> = Mutex::new(HashMap::new());
}

impl CompilationPass for TypePass {
    fn visit_mir_body_before<'tcx>(
        tcx: rustc_middle::ty::TyCtxt<'tcx>,
        body: &mir::Body<'tcx>,
        _storage: &mut dyn Storage,
    ) {
        let mut place_visitor = PlaceVisitor {
            local_decls: body.local_decls(),
            tcx,
        };
        place_visitor.visit_body(body);
        TypeExport::write(MAP.lock().unwrap().clone());
    }
}

struct PlaceVisitor<'tcx, 'b> {
    local_decls: &'b LocalDecls<'tcx>,
    tcx: TyCtxt<'tcx>,
}

impl<'tcx, 'b> Visitor<'tcx> for PlaceVisitor<'tcx, 'b> {
    fn visit_place(
        &mut self,
        place: &Place<'tcx>,
        _context: mir::visit::PlaceContext,
        _location: Location,
    ) {
        let cum_place = Place::from(place.local);
        let cum_ty = cum_place.ty(self.local_decls, self.tcx);
        log::debug!("Visiting Place: {:?} with Ty: {:?}", cum_place, cum_ty);

        if let rustc_middle::ty::TyKind::Adt(def, _) = cum_ty.ty.kind() {
            MAP.lock()
                .unwrap()
                .insert(def.did().index.as_u32().to_string(), cum_ty.ty.to_string());
        }
    }
}
