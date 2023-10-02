use super::{CompilationPass, Storage, StorageExt};

use runtime::tyexp::{TypeExport, TypeInformation, TypeVariant};
use rustc_abi::VariantIdx;
use rustc_index::IndexSlice;
use rustc_middle::{
    mir::{self, visit::Visitor, HasLocalDecls, LocalDecls, Location, Place},
    ty::{GenericArg, List, Ty, TyCtxt, VariantDef},
};
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
        if tcx.entry_fn(()).expect("No entry function was found").0 == body.source.def_id() {
            let mut place_visitor = PlaceVisitor {
                local_decls: body.local_decls(),
                tcx,
                storage,
            };
            place_visitor.visit_body(body);
            TypeExport::write(get_type_map(storage).clone());
        }
    }
}

const KEY_TYPE_MAP: &str = "type_ids";

fn get_type_map(storage: &mut dyn Storage) -> &mut HashMap<String, TypeInformation> {
    storage.get_or_default::<HashMap<String, TypeInformation>>(KEY_TYPE_MAP.to_owned())
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

        self.get_type_information(place_ty.ty);
    }
}

impl<'tcx, 'b, 's> PlaceVisitor<'tcx, 'b, 's> {
    fn get_type_information(&mut self, ty: Ty<'tcx>) {
        // we are only interested in exporting ADT Ty information as of now
        if let rustc_middle::ty::TyKind::Adt(def, subst) = ty.kind() {
            let map = get_type_map(self.storage);
            // skip current ADT Ty if it has been explored
            if !map.contains_key(&def.did().index.as_u32().to_string()) {
                let (tys, variants) = get_tys_and_variants(self.tcx, def.variants(), subst);
                map.insert(
                    def.did().index.as_u32().to_string(),
                    TypeInformation::new(def.did().index.as_u32(), ty.to_string(), variants),
                );

                // recursively explore other ADT Tys found from variants
                for ty in tys {
                    self.get_type_information(ty);
                }
            }
        }
    }
}

// get variants information and ADT Tys found from each variant's fields
fn get_tys_and_variants<'tcx>(
    tcx: TyCtxt<'tcx>,
    vars: &'tcx IndexSlice<VariantIdx, VariantDef>,
    subst: &'tcx List<GenericArg<'tcx>>,
) -> (Vec<Ty<'tcx>>, Vec<TypeVariant>) {
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
                fields.push(field_def.did().index.as_u32().to_string());
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

    (tys, variants)
}
