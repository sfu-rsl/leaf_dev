use super::{CompilationPass, Storage, StorageExt};

use rustc_abi::{FieldsShape, LayoutS, Variants};
use rustc_middle::mir::{self, visit::Visitor};
use rustc_middle::ty::{
    layout::{HasParamEnv, HasTyCtxt, LayoutCx, TyAndLayout},
    ParamEnv, Ty, TyCtxt, TypeVisitableExt,
};
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

fn type_id<'tcx>(tcx: TyCtxt<'tcx>, ty: Ty<'tcx>) -> u128 {
    tcx.type_id_hash(ty).as_u128()
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

        let ty = self.tcx.normalize_erasing_regions(self.param_env, ty);
        if self.type_map.contains_key(&type_id(self.tcx, ty)) {
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

        log::debug!(target: "TypeExport", "Generating type information for {:?}", ty);
        let cx = LayoutCx {
            tcx: self.tcx,
            param_env: self.param_env,
        };
        let type_info: TypeInfo = layout.to_runtime(&cx, ty);
        self.type_map.insert(type_info.id, type_info);
    }
}

trait ToRuntimeInfo<'tcx, Cx, T> {
    type Def;

    fn to_runtime(self, cx: &Cx, ty: Self::Def) -> T
    where
        Cx: 'tcx;
}

impl<'tcx, Cx> ToRuntimeInfo<'tcx, Cx, TypeInfo> for Layout<'tcx>
where
    Cx: HasTyCtxt<'tcx> + HasParamEnv<'tcx>,
{
    type Def = Ty<'tcx>;

    fn to_runtime(self, cx: &Cx, ty: Self::Def) -> TypeInfo
    where
        Cx: 'tcx,
    {
        // FIXME: Reconstruction of deconstructed layout.
        /* FIXME: As all conversions now get TyAndLayout as definition,
         * we can move it inside `cx`. */
        let ty_layout = TyAndLayout { ty, layout: self };

        let variants = match &self.variants {
            Variants::Single { .. } => vec![self.0.to_runtime(cx, ty_layout)],
            Variants::Multiple { variants, .. } => variants
                .iter_enumerated()
                .map(|(i, v)| v.to_runtime(cx, ty_layout.for_variant(cx, i)))
                .collect(),
        };

        TypeInfo {
            id: type_id(cx.tcx(), ty),
            name: ty.to_string(),
            size: self.size().bytes(),
            align: self.align().abi.bytes(),
            variants,
        }
    }
}

impl<'tcx, Cx> ToRuntimeInfo<'tcx, Cx, VariantInfo> for &LayoutS<FieldIdx, VariantIdx>
where
    Cx: HasTyCtxt<'tcx> + HasParamEnv<'tcx>,
{
    type Def = TyAndLayout<'tcx>;

    fn to_runtime(self, cx: &Cx, ty_layout: Self::Def) -> VariantInfo
    where
        Cx: 'tcx,
    {
        let index = match self.variants {
            Variants::Single { index } => index,
            Variants::Multiple { .. } => panic!("Recursive variants are not expected"),
        };

        VariantInfo {
            index: index.as_u32(),
            fields: self.fields.to_runtime(cx, ty_layout),
        }
    }
}

impl<'tcx, Cx> ToRuntimeInfo<'tcx, Cx, FieldsShapeInfo> for &FieldsShape<FieldIdx>
where
    Cx: HasTyCtxt<'tcx> + HasParamEnv<'tcx>,
{
    type Def = TyAndLayout<'tcx>;

    fn to_runtime(self, cx: &Cx, ty_layout: Self::Def) -> FieldsShapeInfo
    where
        Cx: 'tcx,
    {
        let tcx = cx.tcx();
        match self {
            FieldsShape::Primitive => FieldsShapeInfo::NoFields,
            FieldsShape::Union(..) => FieldsShapeInfo::Union,
            FieldsShape::Array { count, .. } => FieldsShapeInfo::Array(ArrayShape {
                len: *count,
                item_ty: type_id(tcx, field_ty(ty_layout, cx, FieldIdx::from_usize(0))),
            }),
            FieldsShape::Arbitrary { offsets, .. } => {
                let mut fields = vec![];
                for (idx, size) in offsets.clone().into_iter_enumerated() {
                    let ty = field_ty(ty_layout, cx, idx);
                    fields.push(FieldInfo {
                        ty: type_id(tcx, ty),
                        offset: size.bytes(),
                    })
                }
                FieldsShapeInfo::Struct(StructShape { fields })
            }
        }
    }
}

fn field_ty<'tcx, Cx>(ty_layout: TyAndLayout<'tcx>, cx: &Cx, index: FieldIdx) -> Ty<'tcx>
where
    Cx: HasTyCtxt<'tcx> + HasParamEnv<'tcx>,
{
    /* NOTE: Guarantee on functionality correctness.
     * This method is obtained by checking the compiler's source code.
     * There are two places found so far that we can rely on to map the index
     * available in the layout to the type.
     * 1. `layout_of_uncached` in `rustc_ty_utils::layout`: The main function
     * that computes the layout of a type. It is called by `layout_of` in `TyCtxt`.
     * 2. `ty_and_layout_field` provided by `TyAbiInterface` trait.
     * The latter returns what we want directly and it takes shorter paths in
     * some cases. The former provides no direct method, and we probably can only
     * use the source to implement it.
     * The sources are checked manually and match for what we want.
     */
    use rustc_target::abi::TyAbiInterface;
    Ty::ty_and_layout_field(ty_layout, cx, index.as_usize()).ty
}
