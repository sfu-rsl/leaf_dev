use super::{CompilationPass, Storage, StorageExt};

use rustc_abi::{FieldsShape, LayoutS, Variants};
use rustc_middle::mir::{self, visit::Visitor};
use rustc_middle::ty::EarlyBinder;
use rustc_middle::ty::{
    layout::{HasParamEnv, HasTyCtxt, LayoutCx, TyAndLayout},
    GenericArgsRef, ParamEnv, Ty, TyCtxt, TyKind,
};
use rustc_target::abi::{FieldIdx, Layout, VariantIdx};

use common::{log_debug, log_warn};
use std::collections::HashMap;
use std::env::{self};
use std::ops::DerefMut;

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
    fn override_flags() -> super::OverrideFlags {
        super::OverrideFlags::MAKE_CODEGEN_BACKEND
    }

    fn visit_tcx_at_codegen_after(
        &mut self,
        tcx: rustc_middle::ty::TyCtxt,
        storage: &mut dyn Storage,
    ) {
        let mut type_map = capture_all_types(storage, tcx);

        // TODO: #379
        let sample_file_path = tcx.output_filenames(()).with_extension(".json");
        let out_dir = &sample_file_path.parent().unwrap();
        let is_single_file_program =
            out_dir.as_os_str().is_empty() || !rustc_session::utils::was_invoked_from_cargo();
        let file_path = if is_single_file_program {
            out_dir.join(FINAL_TYPE_EXPORT_FILE)
        }
        else if env::var("CARGO_PRIMARY_PACKAGE").is_ok() {
            aggregate_type_info(&mut type_map, Some(out_dir.display().to_string()));

            /* For compiling a single file program, the final type export file is placed in the same directory as the program file.
             * For compiling a project, the final type export file is placed in the output directory (i.e. "./target/debug/") */
            out_dir
                .parent()
                .unwrap()
                .join(FINAL_TYPE_EXPORT_FILE)
        } else {
            out_dir.join(format!(
                "types-{}{}.json",
                env::var("CARGO_PKG_NAME").unwrap().replace("-", "_"), // to follow the naming convention of the metadata output file
                tcx.sess.opts.cg.extra_filename
            ))
        }
        .display()
        .to_string();
        TypeExport::write(type_map.values(), file_path);
    }
}

fn capture_all_types<'s>(
    storage: &'s mut dyn Storage,
    tcx: TyCtxt,
) -> impl DerefMut<Target = HashMap<TypeId, TypeInfo>> + 's {
    let mut type_map = storage.get_or_default::<HashMap<TypeId, TypeInfo>>(KEY_TYPE_MAP.to_owned());

    tcx.collect_and_partition_mono_items(())
        .1
        .iter()
        .for_each(|unit| {
            unit.items().iter().for_each(|(item, _)| match item {
                mir::mono::MonoItem::Fn(instance) => {
                    let body = tcx.instance_mir(instance.def);
                    log_debug!(target: TAG_TYPE_EXPORT, "Exporting types in {:?}", instance);
                    let mut place_visitor = PlaceVisitor {
                        tcx,
                        type_map: &mut type_map,
                        args: instance.args,
                        param_env: tcx.param_env_reveal_all_normalized(body.source.def_id()),
                        local_decls: &body.local_decls,
                    };
                    place_visitor.visit_body(body);
                }
                _ => {}
            })
        });

    let special_types = vec![Ty::new_mut_ptr(tcx, tcx.types.unit)];
    for ty in special_types {
        add_type_information_to_map(&mut type_map, tcx, ty, ParamEnv::empty());
    }

    type_map
}

fn add_type_information_to_map<'tcx>(
    type_map: &mut HashMap<TypeId, TypeInfo>,
    tcx: TyCtxt<'tcx>,
    ty: Ty<'tcx>,
    param_env: ParamEnv<'tcx>,
) {
    let layout = match tcx.layout_of(param_env.and(ty)) {
        Ok(TyAndLayout { layout, .. }) => layout,
        Err(err) => {
            log_warn!("Failed to get layout of type {:?}: {:?}", ty, err);
            return;
        }
    };
    log_debug!(target: TAG_TYPE_EXPORT, "Generating type information for {:?}", ty);
    let cx = LayoutCx { tcx, param_env };
    let type_info: TypeInfo = layout.to_runtime(&cx, ty);
    type_map.insert(type_info.id, type_info);
}

fn type_id<'tcx>(tcx: TyCtxt<'tcx>, ty: Ty<'tcx>) -> TypeId {
    TypeId::new(tcx.type_id_hash(ty).as_u128()).unwrap()
}

fn aggregate_type_info(type_map: &mut HashMap<TypeId, TypeInfo>, path_prefix: Option<String>) {
    let path_prefix = path_prefix.unwrap_or_else(|| ".".to_string());
    let dep_types_file_pattern = format!("{}/**/types-*.json", path_prefix);
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

struct PlaceVisitor<'tcx, 's, 'b> {
    tcx: TyCtxt<'tcx>,
    type_map: &'s mut HashMap<TypeId, TypeInfo>,
    args: GenericArgsRef<'tcx>,
    param_env: ParamEnv<'tcx>,
    local_decls: &'b mir::LocalDecls<'tcx>,
}

impl<'tcx, 's, 'b> Visitor<'tcx> for PlaceVisitor<'tcx, 's, 'b> {
    fn visit_ty(&mut self, ty: Ty<'tcx>, context: mir::visit::TyContext) {
        let normalized_ty = self.tcx.instantiate_and_normalize_erasing_regions(
            self.args,
            self.param_env,
            EarlyBinder::bind(ty),
        );
        log_debug!(target: TAG_TYPE_EXPORT, "Normalized ty with param: {} -> {}", ty, normalized_ty);

        if self
            .type_map
            .contains_key(&type_id(self.tcx, normalized_ty))
        {
            return;
        }

        add_type_information_to_map(self.type_map, self.tcx, ty, self.param_env);

        // For pointee
        ty.builtin_deref(true)
            .inspect(|t| self.visit_ty(*t, context));
    }

    fn visit_place(
        &mut self,
        place: &mir::Place<'tcx>,
        context: mir::visit::PlaceContext,
        location: mir::Location,
    ) {
        self.super_place(place, context, location);
        // Process intermediate types as well
        place.iter_projections().fold(
            mir::tcx::PlaceTy::from_ty(self.local_decls[place.local].ty),
            |p_ty, x| {
                let p_ty = p_ty.projection_ty(self.tcx, x.1);
                self.visit_ty(p_ty.ty, mir::visit::TyContext::Location(location));
                p_ty
            },
        );
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
        let tcx = cx.tcx();
        // FIXME: Reconstruction of deconstructed layout.
        /* FIXME: As all conversions now get TyAndLayout as definition,
         * we can move it inside `cx`. */
        let ty_layout = TyAndLayout { ty, layout: self };
        let size = if ty.is_sized(tcx, cx.param_env()) {
            self.size().bytes()
        } else {
            TypeInfo::SIZE_UNSIZED
        };

        let variants = match &self.variants {
            Variants::Single { .. } => vec![self.0.to_runtime(cx, ty_layout)],
            Variants::Multiple { variants, .. } => variants
                .iter_enumerated()
                .map(|(i, v)| v.to_runtime(cx, ty_layout.for_variant(cx, i)))
                .collect(),
        };

        TypeInfo {
            id: type_id(tcx, ty),
            name: ty.to_string(),
            size,
            align: self.align().abi.bytes(),
            variants,
            // NOTE: This also includes `Box` which may not be desired.
            pointee_ty: ty.builtin_deref(true).map(|t| type_id(tcx, t)),
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
            FieldsShape::Union(count) => FieldsShapeInfo::Union(StructShape {
                fields: (0..(*count).into())
                    .into_iter()
                    .map(|idx| {
                        let ty = field_ty(ty_layout, cx, FieldIdx::from_usize(idx));
                        FieldInfo {
                            ty: type_id(tcx, ty),
                            offset: 0,
                        }
                    })
                    .collect(),
            }),
            FieldsShape::Array { count, .. } => FieldsShapeInfo::Array(ArrayShape {
                len: *count,
                item_ty: type_id(tcx, field_ty(ty_layout, cx, FieldIdx::from_usize(0))),
            }),
            FieldsShape::Arbitrary { offsets, .. } => FieldsShapeInfo::Struct(StructShape {
                fields: {
                    offsets
                        .clone()
                        .into_iter_enumerated()
                        .map(|(idx, offset)| {
                            let ty = field_ty(ty_layout, cx, idx);
                            FieldInfo {
                                ty: type_id(tcx, ty),
                                offset: offset.bytes(),
                            }
                        })
                        .collect()
                },
            }),
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
    let result = Ty::ty_and_layout_field(ty_layout, cx, index.as_usize()).ty;

    if matches!(ty_layout.ty.kind(), TyKind::Ref(..) | TyKind::RawPtr(..)) && index.as_usize() == 0
    {
        // NOTE: The function returns the fat pointer type itself for the first
        // field if this is a fat pointer. We change it to the type of a pointer to unit.
        debug_assert_eq!(result, ty_layout.ty, "The assumption is not true.");
        Ty::new_mut_ptr(cx.tcx(), cx.tcx().types.unit)
    } else {
        result
    }
}
