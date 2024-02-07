use super::{CompilationPass, Storage, StorageExt};

use rustc_abi::{FieldsShape, LayoutS, Variants};
use rustc_middle::mir::{self, visit::Visitor};
use rustc_middle::ty::EarlyBinder;
use rustc_middle::ty::{
    layout::{HasParamEnv, HasTyCtxt, LayoutCx, TyAndLayout},
    GenericArgsRef, ParamEnv, Ty, TyCtxt,
};
use rustc_target::abi::{FieldIdx, Layout, VariantIdx};

use std::collections::HashMap;
use std::env::{self};
use std::path::Path;

use glob::glob;

use common::tyexp::*;

const KEY_TYPE_MAP: &str = "type_ids";

const TAG_TYPE_EXPORT: &str = "type_export";

/*
 * TypeExporter pass to export type information
 */
#[derive(Default)]
pub(crate) struct TypeExporter;

impl CompilationPass for TypeExporter {
    fn visit_tcx_at_codegen_after(
        &mut self,
        tcx: rustc_middle::ty::TyCtxt,
        storage: &mut dyn Storage,
    ) {
        let type_map = storage.get_or_default::<HashMap<u128, TypeInfo>>(KEY_TYPE_MAP.to_owned());
        tcx.collect_and_partition_mono_items(())
            .1
            .iter()
            .for_each(|unit| {
                unit.items().iter().for_each(|(item, _)| match item {
                    mir::mono::MonoItem::Fn(instance) => {
                        let body = tcx.instance_mir(instance.def);
                        log::debug!(target: TAG_TYPE_EXPORT, "Exporting types in {:?}", instance);
                        let mut place_visitor = PlaceVisitor {
                            tcx,
                            type_map,
                            args: instance.args,
                            param_env: tcx.param_env_reveal_all_normalized(body.source.def_id()),
                        };
                        place_visitor.visit_body(body);
                    }
                    _ => {}
                })
            });

        // TODO: #379
        let output_filenames = tcx.output_filenames(());
        let file_path = if output_filenames.out_directory.as_os_str().is_empty()
            || env::var("CARGO_PRIMARY_PACKAGE").is_ok()
        {
            aggregate_type_info(type_map);

            output_filenames
                .out_directory
                .parent()
                .unwrap_or(Path::new(""))
                .join("types.json")
                .display()
                .to_string()
        } else {
            output_filenames
                .out_directory
                .join(format!(
                    "types-{}{}.json",
                    env::var("CARGO_PKG_NAME").unwrap().replace("-", "_"), // to follow the naming convention of the metadata output file
                    tcx.sess.opts.cg.extra_filename
                ))
                .display()
                .to_string()
        };
        TypeExport::write(type_map.values(), file_path);
    }
}

fn type_id<'tcx>(tcx: TyCtxt<'tcx>, ty: Ty<'tcx>) -> u128 {
    tcx.type_id_hash(ty).as_u128()
}

fn aggregate_type_info(type_map: &mut HashMap<u128, TypeInfo>) {
    let dep_types_file_pattern = format!(
        "{}/**/types-*.json",
        env::var("CARGO_MANIFEST_DIR").unwrap()
    );
    for entry in glob(dep_types_file_pattern.as_str())
        .expect(format!("Failed to read glob pattern: {}", dep_types_file_pattern).as_str())
    {
        let file_path = entry
            .expect("Failed to read glob entry")
            .display()
            .to_string();
        let type_infos = TypeExport::get_type_info(file_path).unwrap();
        for type_info in type_infos {
            if !type_map.contains_key(&type_info.id) {
                type_map.insert(type_info.id, type_info);
            }
        }
    }
}

struct PlaceVisitor<'tcx, 's> {
    tcx: TyCtxt<'tcx>,
    type_map: &'s mut HashMap<u128, TypeInfo>,
    args: GenericArgsRef<'tcx>,
    param_env: ParamEnv<'tcx>,
}

impl<'tcx, 's> Visitor<'tcx> for PlaceVisitor<'tcx, 's> {
    fn visit_ty(&mut self, ty: Ty<'tcx>, _: mir::visit::TyContext) {
        let normalized_ty = self.tcx.instantiate_and_normalize_erasing_regions(
            self.args,
            self.param_env,
            EarlyBinder::bind(ty),
        );
        log::debug!(target: TAG_TYPE_EXPORT, "Normalized ty with param: {} -> {}", ty, normalized_ty);

        if self
            .type_map
            .contains_key(&type_id(self.tcx, normalized_ty))
        {
            return;
        }

        self.add_type_information_to_map(normalized_ty);
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

        log::debug!(target: TAG_TYPE_EXPORT, "Generating type information for {:?}", ty);
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
