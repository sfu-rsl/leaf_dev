use super::{CompilationPass, Storage, StorageExt};

use rustc_abi::{FieldsShape, LayoutS, Variants};
use rustc_index::IndexVec;
use rustc_middle::mir::{self, visit::Visitor};
use rustc_middle::ty::{layout::TyAndLayout, ParamEnv, Ty, TyCtxt, TyKind, TypeVisitableExt};
use rustc_target::abi::{FieldIdx, Layout, VariantIdx};

use std::collections::HashMap;

use runtime::tyexp::*;

const KEY_TYPE_MAP: &str = "type_ids";

/*
 * TypeExporter pass to export type information
 */
#[derive(Default)]
pub(crate) struct TypeExporter;

impl CompilationPass for TypeExporter {
    fn visit_mir_body_before<'tcx>(
        tcx: TyCtxt<'tcx>,
        body: &mir::Body<'tcx>,
        storage: &mut dyn Storage,
    ) {
        let type_map = storage.get_or_default::<HashMap<u128, TypeInfo>>(KEY_TYPE_MAP.to_owned());
        let mut place_visitor = PlaceVisitor {
            tcx,
            type_map,
            param_env: tcx.param_env_reveal_all_normalized(body.source.def_id()),
        };
        place_visitor.visit_body(body);
        TypeExport::write(type_map);
    }
}

struct PlaceVisitor<'tcx, 's> {
    tcx: TyCtxt<'tcx>,
    type_map: &'s mut HashMap<u128, TypeInfo>,
    param_env: ParamEnv<'tcx>,
}

impl<'tcx, 's> Visitor<'tcx> for PlaceVisitor<'tcx, 's> {
    fn visit_ty(&mut self, ty: Ty<'tcx>, _: mir::visit::TyContext) {
        if ty.has_param() {
            return;
        }

        self.add_type_information_to_map(ty);
    }
}

impl<'tcx, 's> PlaceVisitor<'tcx, 's> {
    fn add_type_information_to_map(&mut self, ty: Ty<'tcx>) {
        let layout = match self.tcx.layout_of(self.param_env.and(ty)) {
            Ok(TyAndLayout { layout, .. }) => layout,
            Err(err) => {
                log::warn!("Failed to get layout of type {:?}: {:?}", ty, err);
                return;
            }
        };

        let type_info: TypeInfo = layout.to_runtime(self.tcx, ty);
        self.type_map.insert(type_info.id, type_info);
    }
}

trait ToRuntimeInfo<T> {
    type Def<'tcx>;

    fn to_runtime<'tcx>(self, tcx: TyCtxt<'tcx>, definition: Self::Def<'tcx>) -> T;
}

impl ToRuntimeInfo<TypeInfo> for Layout<'_> {
    type Def<'tcx> = Ty<'tcx>;

    fn to_runtime<'tcx>(self, tcx: TyCtxt<'tcx>, ty: Self::Def<'tcx>) -> TypeInfo {
        let variants = match &self.variants {
            Variants::Single { .. } => vec![self.0.to_runtime(tcx, ty)],
            Variants::Multiple { variants, .. } => {
                variants.iter().map(|v| v.to_runtime(tcx, ty)).collect()
            }
        };

        TypeInfo {
            id: tcx.type_id_hash(ty).as_u128(),
            name: ty.to_string(),
            size: self.size().bytes(),
            align: self.align().abi.bytes(),
            variants,
        }
    }
}

impl ToRuntimeInfo<VariantInfo> for &LayoutS<FieldIdx, VariantIdx> {
    type Def<'tcx> = Ty<'tcx>;

    fn to_runtime<'tcx>(self, tcx: TyCtxt<'tcx>, ty: Self::Def<'tcx>) -> VariantInfo {
        let index = match self.variants {
            Variants::Single { index } => index,
            Variants::Multiple { .. } => panic!("Recursive variants are not expected"),
        };

        let mut field_types = IndexVec::<FieldIdx, Ty<'tcx>>::new();
        match ty.kind() {
            TyKind::Adt(def, subst) => {
                let target_variant = def.variant(index);
                for field in target_variant.fields.iter() {
                    let field_ty = field.ty(tcx, subst);
                    field_types.push(field_ty);
                }
            }
            TyKind::Array(ty, ..) => {
                field_types.push(*ty);
            }
            TyKind::Tuple(tys) => {
                for ty in tys.into_iter() {
                    field_types.push(ty);
                }
            }
            TyKind::Ref(..) => {
                // NOTE: Ref is not an ADT but has an arbitrary fields shape, which may cause panic during type exportation,
                // so we return no field information for simplicity
                return VariantInfo {
                    index: index.as_u32(),
                    fields: FieldsShapeInfo::NoFields,
                };
            }
            _ => ()
        };

        VariantInfo {
            index: index.as_u32(),
            fields: self.fields.to_runtime(tcx, field_types),
        }
    }
}

impl ToRuntimeInfo<FieldsShapeInfo> for &FieldsShape<FieldIdx> {
    type Def<'tcx> = IndexVec<FieldIdx, Ty<'tcx>>;

    fn to_runtime<'tcx>(self, tcx: TyCtxt<'tcx>, field_tys: Self::Def<'tcx>) -> FieldsShapeInfo {
        match self {
            FieldsShape::Primitive => FieldsShapeInfo::NoFields,
            FieldsShape::Union(..) => FieldsShapeInfo::Union,
            FieldsShape::Array { count, .. } => FieldsShapeInfo::Array(ArrayShape {
                len: *count,
                item_ty: tcx
                    .type_id_hash(field_tys[FieldIdx::from_usize(0)])
                    .as_u128(),
            }),
            FieldsShape::Arbitrary { offsets, .. } => {
                let mut fields = vec![];
                for (idx, size) in offsets.clone().into_iter_enumerated() {
                    fields.push(FieldInfo {
                        ty: tcx.type_id_hash(field_tys[idx]).as_u128(),
                        offset: size.bytes(),
                    })
                }
                FieldsShapeInfo::Struct(StructShape { fields })
            }
        }
    }
}
