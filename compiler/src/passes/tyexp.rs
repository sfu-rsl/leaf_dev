use crate::utils::compute_def_id;

use super::{CompilationPass, Storage, StorageExt};

use runtime::tyexp::{TypeExport, TypeInformation, TypeVariant};
use rustc_index::IndexSlice;
use rustc_middle::{
    mir::{self, visit::Visitor, HasLocalDecls, LocalDecls, Location, Place},
    ty::{GenericArg, List, Ty, TyCtxt, VariantDef},
};
use rustc_target::abi::VariantIdx;
use std::collections::HashMap;

/*
 * TypeExporter pass to export type information
 */
#[derive(Default)]
pub(crate) struct TypeExporter;

impl CompilationPass for TypeExporter {
    fn visit_mir_body_before<'tcx>(
        tcx: rustc_middle::ty::TyCtxt<'tcx>,
        body: &mir::Body<'tcx>,
        storage: &mut dyn Storage,
    ) {
        let mut place_visitor = PlaceVisitor {
            local_decls: body.local_decls(),
            tcx,
            storage,
        };
        place_visitor.visit_body(body);
        TypeExport::write(get_type_map(storage).clone());
    }
}

const KEY_TYPE_MAP: &str = "type_ids";

pub(crate) fn get_type_map(storage: &mut dyn Storage) -> &mut HashMap<u64, TypeInformation> {
    storage.get_or_default::<HashMap<u64, TypeInformation>>(KEY_TYPE_MAP.to_owned())
}

struct PlaceVisitor<'tcx, 'b, 's> {
    local_decls: &'b LocalDecls<'tcx>,
    tcx: TyCtxt<'tcx>,
    storage: &'s mut dyn Storage,
}

impl<'tcx, 'b, 's> Visitor<'tcx> for PlaceVisitor<'tcx, 'b, 's> {
    fn visit_place(
        &mut self,
        place: &Place<'tcx>,
        _context: mir::visit::PlaceContext,
        _location: Location,
    ) {
        let place_ty = place.ty(self.local_decls, self.tcx);
        log::debug!("Visiting Place: {:?} with Ty: {:?}", place, place_ty);

        self.add_type_information_to_map(place_ty.ty);
    }
}

impl<'tcx, 'b, 's> PlaceVisitor<'tcx, 'b, 's> {
    fn add_type_information_to_map(&mut self, ty: Ty<'tcx>) {
        // we are only interested in exporting ADT Ty information as of now
        if let rustc_middle::ty::TyKind::Adt(def, subst) = ty.kind() {
            let map = get_type_map(self.storage);
            let def_id = compute_def_id(def.did());
            // skip current ADT Ty if it has been explored
            if !map.contains_key(&def_id) {
                let (variants, variants_tys) =
                    get_variants_and_tys(self.tcx, def.variants(), subst);
                map.insert(
                    def_id,
                    TypeInformation::new(def_id, ty.to_string(), variants),
                );

                // recursively explore other ADT Tys found from variants
                for ty in variants_tys {
                    self.add_type_information_to_map(ty);
                }
            }
        }
    }
}

// get variants information and ADT Tys found from each variant's fields
fn get_variants_and_tys<'tcx>(
    tcx: TyCtxt<'tcx>,
    vars: &'tcx IndexSlice<VariantIdx, VariantDef>,
    subst: &'tcx List<GenericArg<'tcx>>,
) -> (Vec<TypeVariant>, Vec<Ty<'tcx>>) {
    let mut tys: Vec<Ty<'tcx>> = vec![];
    let mut variants: Vec<TypeVariant> = vec![];
    for variant in vars {
        log::debug!(
            "Variant {:#?} with DefId {:#?}",
            variant.name,
            variant.def_id
        );
        let mut fields: Vec<String> = vec![];
        for field in variant.fields.iter() {
            let field_ty = field.ty(tcx, subst);
            if let rustc_middle::ty::TyKind::Adt(field_def, _) = field_ty.kind() {
                let def_id = compute_def_id(field_def.did());
                fields.push(def_id.to_string());
                tys.push(field_ty);
            } else {
                fields.push(field_ty.to_string());
            }
            log::debug!(
                "Field {:#?} with DefId {:#?} and Ty {:#?}",
                field.name,
                field.did.index.as_u32(),
                field_ty
            );
        }

        variants.push(TypeVariant::new(variant.name.to_string(), fields));
    }

    (variants, tys)
}
