use std::collections::HashMap;

use rustc_hir::def::DefKind;
use rustc_middle::{
    middle::exported_symbols,
    mir::{self, BasicBlock, BasicBlockData, HasLocalDecls, Local},
    ty::{Ty, TyCtxt},
};
use rustc_span::def_id::DefId;

use crate::mir_transform::modification::{
    self, BodyBlockManager, BodyLocalManager, BodyModificationUnit, JumpTargetModifier,
};

use super::PlaceRef;

pub trait TyContextProvider<'tcx> {
    fn tcx(&self) -> TyCtxt<'tcx>;
}

pub trait BodyProvider<'tcx> {
    fn body(&self) -> &mir::Body<'tcx>;
}

pub trait InEntryFunction {}

pub struct FunctionInfo<'tcx> {
    pub def_id: DefId,
    pub ret_ty: Ty<'tcx>,
}

pub trait FunctionInfoProvider<'tcx> {
    fn get_pri_func_info(&self, func_name: &str) -> &FunctionInfo<'tcx>;
}

pub struct SpecialTypes<'tcx> {
    pub place_ref: Ty<'tcx>,
    pub operand_ref: Ty<'tcx>,
    pub binary_op: Ty<'tcx>,
    pub unary_op: Ty<'tcx>,
}

pub trait SpecialTypesProvider<'tcx> {
    fn pri_special_types(&self) -> &SpecialTypes<'tcx>;
}

pub trait LocationProvider {
    fn location(&self) -> BasicBlock;
}

pub trait DestinationReferenceProvider {
    fn dest_ref(&self) -> PlaceRef;
}

#[derive(Clone, Copy)]
pub struct SwitchInfo<'tcx> {
    pub(super) node_location: BasicBlock,
    pub(super) discr_ty: Ty<'tcx>,
    pub(super) runtime_info_store_var: Local,
}

pub trait SwitchInfoProvider<'tcx> {
    fn switch_info(&self) -> SwitchInfo<'tcx>;
}

/*
 * This is the minimal required context that we expect for call adding.
 *(Otherwise, we will not be able to add new call blocks to the MIR.)
 */
pub trait BaseContext<'tcx>
where
    Self: TyContextProvider<'tcx>
        + BodyLocalManager<'tcx>
        + BodyBlockManager<'tcx>
        + FunctionInfoProvider<'tcx>
        + SpecialTypesProvider<'tcx>
        + HasLocalDecls<'tcx>,
{
}

impl<'tcx, C> BaseContext<'tcx> for C where
    Self: TyContextProvider<'tcx>
        + BodyLocalManager<'tcx>
        + BodyBlockManager<'tcx>
        + FunctionInfoProvider<'tcx>
        + SpecialTypesProvider<'tcx>
        + HasLocalDecls<'tcx>
{
}

pub struct DefaultContext<'tcx, 'm> {
    tcx: TyCtxt<'tcx>,
    modification_unit: &'m mut BodyModificationUnit<'tcx>,
    pri_functions: HashMap<String, FunctionInfo<'tcx>>,
    pri_special_types: SpecialTypes<'tcx>,
}

impl<'tcx, 'm> DefaultContext<'tcx, 'm> {
    pub fn new(tcx: TyCtxt<'tcx>, modification_unit: &'m mut BodyModificationUnit<'tcx>) -> Self {
        Self {
            tcx,
            modification_unit,
            pri_functions: DefaultContext::extract_functions(tcx), // FIXME: Perform caching
            pri_special_types: DefaultContext::find_special_types(tcx),
        }
    }

    fn extract_functions(tcx: TyCtxt<'tcx>) -> HashMap<String, FunctionInfo<'tcx>> {
        DefaultContext::get_exported_symbols_of_pri(tcx)
            .into_iter()
            .filter(|def_id| matches!(tcx.def_kind(def_id), DefKind::Fn | DefKind::AssocFn))
            .map(|def_id| {
                (
                    tcx.def_path_str(def_id),
                    FunctionInfo {
                        def_id,
                        ret_ty: tcx.fn_sig(def_id).output().skip_binder(),
                    },
                )
            })
            .collect()
    }

    fn find_special_types(tcx: TyCtxt<'tcx>) -> SpecialTypes<'tcx> {
        /*
         * FIXME: The desired enums and type aliases don't show up in the exported symbols.
         * It may be because of the MIR phases that clean up/optimize/unify things,
         * the way that the library is added (using the compiled file), or
         * that enums and type aliases are not included at all in the exported_symbols.
         * As a workaround, we have defined some static variables having those desired
         * types and are accessible.
         * However, there should be some functions in TyCtxt that will list these items for us.
         */
        let def_ids: HashMap<String, DefId> = DefaultContext::get_exported_symbols_of_pri(tcx)
            .into_iter()
            .filter(|def_id| matches!(tcx.def_kind(def_id), DefKind::Static(_)))
            .map(|def_id| (tcx.def_path_str(def_id), def_id))
            .collect();

        let get_ty =
            |name: &str| -> Ty<'tcx> { tcx.type_of(def_ids.get(&name.replace(' ', "")).unwrap()) };
        SpecialTypes {
            place_ref: get_ty(stringify!(runtime::pri::PLACE_REF_TYPE_HOLDER)),
            operand_ref: get_ty(stringify!(runtime::pri::OPERAND_REF_TYPE_HOLDER)),
            binary_op: get_ty(stringify!(runtime::pri::BINARY_OP_TYPE_HOLDER)),
            unary_op: get_ty(stringify!(runtime::pri::UNARY_OP_TYPE_HOLDER)),
        }
    }

    fn get_exported_symbols_of_pri(tcx: TyCtxt<'tcx>) -> Vec<DefId> {
        let crate_num = *tcx
            .crates(())
            .iter()
            .find(|cnum| tcx.crate_name(**cnum).as_str() == stringify!(runtime))
            .unwrap_or_else(|| {
                panic!(
                    "{} crate is not added as a dependency.",
                    stringify!(runtime)
                )
            });

        tcx.exported_symbols(crate_num)
            .iter()
            .filter_map(|(exported_symbol, _)| match exported_symbol {
                exported_symbols::ExportedSymbol::NonGeneric(def_id)
                | exported_symbols::ExportedSymbol::Generic(def_id, _) => Some(def_id),
                _ => None,
            })
            .cloned()
            .collect()
    }
}

impl<'tcx> TyContextProvider<'tcx> for DefaultContext<'tcx, '_> {
    fn tcx(&self) -> TyCtxt<'tcx> {
        self.tcx
    }
}

impl<'tcx, 'm> BodyLocalManager<'tcx> for DefaultContext<'tcx, 'm> {
    fn add_local<T>(&mut self, decl_info: T) -> Local
    where
        T: Into<modification::NewLocalDecl<'tcx>>,
    {
        self.modification_unit.add_local(decl_info)
    }
}

impl<'tcx, 'm> BodyBlockManager<'tcx> for DefaultContext<'tcx, 'm> {
    fn insert_blocks_before<I>(
        &mut self,
        index: BasicBlock,
        blocks: I,
        sticky: bool,
    ) -> Vec<BasicBlock>
    where
        I: IntoIterator<Item = BasicBlockData<'tcx>>,
    {
        self.modification_unit
            .insert_blocks_before(index, blocks, sticky)
    }

    fn insert_blocks_after<I>(
        &mut self,
        index: BasicBlock,
        blocks: I,
        sticky: bool,
    ) -> Vec<BasicBlock>
    where
        I: IntoIterator<Item = BasicBlockData<'tcx>>,
    {
        self.modification_unit
            .insert_blocks_after(index, blocks, sticky)
    }
}

impl JumpTargetModifier for DefaultContext<'_, '_> {
    fn modify_jump_target_where(
        &mut self,
        terminator_location: BasicBlock,
        from: BasicBlock,
        to: BasicBlock,
        constraint: modification::JumpModificationConstraint,
    ) {
        self.modification_unit
            .modify_jump_target_where(terminator_location, from, to, constraint)
    }
}

impl<'tcx> FunctionInfoProvider<'tcx> for DefaultContext<'tcx, '_> {
    fn get_pri_func_info(&self, func_name: &str) -> &FunctionInfo<'tcx> {
        self.pri_functions
            .get(&("runtime::".to_owned() + &func_name.replace(' ', ""))) // FIXME
            .unwrap_or_else(|| panic!("Invalid pri function name: `{func_name}`."))
    }
}

impl<'tcx> SpecialTypesProvider<'tcx> for DefaultContext<'tcx, '_> {
    fn pri_special_types(&self) -> &SpecialTypes<'tcx> {
        &self.pri_special_types
    }
}

/*
 * Makes it possible to borrow another context while owning itself.
 */
pub struct TransparentContext<'b, B> {
    pub(super) base: &'b mut B,
}

pub struct InBodyContext<'b, 'tcx, 'bd, B> {
    pub(super) base: &'b mut B,
    pub(super) body: &'bd mir::Body<'tcx>,
}

impl<'tcx, B> BodyProvider<'tcx> for InBodyContext<'_, 'tcx, '_, B> {
    fn body(&self) -> &mir::Body<'tcx> {
        self.body
    }
}

impl<'tcx, B> mir::HasLocalDecls<'tcx> for InBodyContext<'_, 'tcx, '_, B> {
    fn local_decls(&self) -> &mir::LocalDecls<'tcx> {
        self.body().local_decls()
    }
}

pub struct EntryFunctionMarkerContext<'b, B> {
    pub(super) base: &'b mut B,
}

impl<'b, B> InEntryFunction for EntryFunctionMarkerContext<'b, B> {}

pub struct AtLocationContext<'b, B> {
    pub(super) base: &'b mut B,
    pub(super) location: BasicBlock,
}

impl<B> LocationProvider for AtLocationContext<'_, B> {
    fn location(&self) -> BasicBlock {
        self.location
    }
}

pub struct AssignmentContext<'b, B> {
    pub(super) base: &'b mut B,
    pub(super) dest_ref: PlaceRef,
}

impl<B> DestinationReferenceProvider for AssignmentContext<'_, B> {
    fn dest_ref(&self) -> PlaceRef {
        self.dest_ref
    }
}

pub struct BranchingContext<'b, 'tcx, B> {
    pub(super) base: &'b mut B,
    pub(super) switch_info: SwitchInfo<'tcx>,
}

impl<'tcx, B> SwitchInfoProvider<'tcx> for BranchingContext<'_, 'tcx, B> {
    fn switch_info(&self) -> SwitchInfo<'tcx> {
        self.switch_info
    }
}

/*
 * We make inheritance of traits from the base context possible through generics.
 */

macro_rules! impl_func_info_provider {
    ($generic_context_type:ident, $($extra_lifetime_param:lifetime)*, $($extra_generic_param:ident)*) => {
        impl<'b, 'tcx$(, $extra_lifetime_param)*, B: FunctionInfoProvider<'tcx>$(, $extra_generic_param)*> FunctionInfoProvider<'tcx>
            for $generic_context_type<'b$(, $extra_lifetime_param)*, B$(, $extra_generic_param)*>
        {
            fn get_pri_func_info(&self, func_name: &str) -> &FunctionInfo<'tcx> {
                self.base.get_pri_func_info(func_name)
            }
        }
    };
}

macro_rules! impl_special_types_provider {
    ($generic_context_type:ident, $($extra_lifetime_param:lifetime)*, $($extra_generic_param:ident)*) => {
        impl<'b, 'tcx$(, $extra_lifetime_param)*, B: SpecialTypesProvider<'tcx>$(, $extra_generic_param)*> SpecialTypesProvider<'tcx>
            for $generic_context_type<'b$(, $extra_lifetime_param)*, B$(, $extra_generic_param)*>
        {
            fn pri_special_types(&self) -> &SpecialTypes<'tcx> {
                self.base.pri_special_types()
            }
        }
    };
}

macro_rules! impl_local_manager {
    ($generic_context_type:ident, $($extra_lifetime_param:lifetime)*, $($extra_generic_param:ident)*) => {
        impl<'b, 'tcx$(, $extra_lifetime_param)*, B: BodyLocalManager<'tcx>$(, $extra_generic_param)*> BodyLocalManager<'tcx>
            for $generic_context_type<'b$(, $extra_lifetime_param)*, B$(, $extra_generic_param)*>
        {
            fn add_local<T>(&mut self, decl_info: T) -> Local
            where
                T: Into<modification::NewLocalDecl<'tcx>>,
            {
                self.base.add_local(decl_info)
            }
        }
    };
}

macro_rules! impl_block_manager {
    ($generic_context_type:ident, $($extra_lifetime_param:lifetime)*, $($extra_generic_param:ident)*) => {
        impl<'b, 'tcx$(, $extra_lifetime_param)*, B: BodyBlockManager<'tcx>$(, $extra_generic_param)*> BodyBlockManager<'tcx>
            for $generic_context_type<'b$(, $extra_lifetime_param)*, B>$(, $extra_generic_param)*
        {
            fn insert_blocks_before<I>(&mut self, index: BasicBlock, blocks: I, sticky: bool) -> Vec<BasicBlock>
            where
                I: IntoIterator<Item = BasicBlockData<'tcx>>,
            {
                self.base.insert_blocks_before(index, blocks, sticky)
            }

            fn insert_blocks_after<I>(
                &mut self,
                index: BasicBlock,
                blocks: I,
                sticky: bool,
            ) -> Vec<BasicBlock>
            where
                I: IntoIterator<Item = BasicBlockData<'tcx>>,
            {
                self.base
                    .insert_blocks_after(index, blocks, sticky)
            }
        }
    };
}

macro_rules! impl_jump_target_modifier {
    ($generic_context_type:ident, $($extra_lifetime_param:lifetime)*, $($extra_generic_param:ident)*) => {
        impl<'b$(, $extra_lifetime_param)*, B: JumpTargetModifier$(, $extra_generic_param)*> JumpTargetModifier for $generic_context_type<'b$(, $extra_lifetime_param)*, B$(, $extra_generic_param)*> {
            fn modify_jump_target_where(
                &mut self,
                terminator_location: BasicBlock,
                from: BasicBlock,
                to: BasicBlock,
                constraint: modification::JumpModificationConstraint,
            ) {
                self.base
                    .modify_jump_target_where(terminator_location, from, to, constraint)
            }
        }
    };
}

macro_rules! impl_ty_ctxt_provider {
    ($generic_context_type:ident, $($extra_lifetime_param:lifetime)*, $($extra_generic_param:ident)*) => {
        impl<'b, 'tcx$(, $extra_lifetime_param)*, B: TyContextProvider<'tcx>$(, $extra_generic_param)*> TyContextProvider<'tcx>
            for $generic_context_type<'b$(, $extra_lifetime_param)*, B$(, $extra_generic_param)*>
        {
            fn tcx(&self) -> TyCtxt<'tcx> {
                self.base.tcx()
            }
        }
    };
}

macro_rules! impl_body_provider {
    ($generic_context_type:ident, $($extra_lifetime_param:lifetime)*, $($extra_generic_param:ident)*) => {
        impl<'b, 'tcx$(, $extra_lifetime_param)*, B: BodyProvider<'tcx>$(, $extra_generic_param)*> BodyProvider<'tcx> for $generic_context_type<'b$(, $extra_lifetime_param)*, B$(, $extra_generic_param)*> {
            fn body(&self) -> &mir::Body<'tcx> {
                self.base.body()
            }
        }
    };
}

macro_rules! impl_in_entry_function {
    ($generic_context_type:ident, $($extra_lifetime_param:lifetime)*, $($extra_generic_param:ident)*) => {
        impl<'b$(, $extra_lifetime_param)*, B: InEntryFunction$(, $extra_generic_param)*> InEntryFunction for $generic_context_type<'b$(, $extra_lifetime_param)*, B$(, $extra_generic_param)*> {
        }
    };
}

macro_rules! impl_has_local_decls {
    ($generic_context_type:ident, $($extra_lifetime_param:lifetime)*, $($extra_generic_param:ident)*) => {
        impl<'b, 'tcx$(, $extra_lifetime_param)*, B: mir::HasLocalDecls<'tcx>$(, $extra_generic_param)*> mir::HasLocalDecls<'tcx> for $generic_context_type<'b$(, $extra_lifetime_param)*, B$(, $extra_generic_param)*> {
            fn local_decls(&self) -> &mir::LocalDecls<'tcx>  {
                self.base.local_decls()
            }
        }
    };
}

macro_rules! impl_location_provider {
    ($generic_context_type:ident, $($extra_lifetime_param:lifetime)*, $($extra_generic_param:ident)*) => {
        impl<'b$(, $extra_lifetime_param)*, B: LocationProvider$(, $extra_generic_param)*> LocationProvider for $generic_context_type<'b$(, $extra_lifetime_param)*, B$(, $extra_generic_param)*> {
            fn location(&self) -> BasicBlock {
                self.base.location()
            }
        }
    };
}

macro_rules! impl_dest_ref_provider {
    ($generic_context_type:ident, $($extra_lifetime_param:lifetime)*, $($extra_generic_param:ident)*) => {
        impl<'b$(, $extra_lifetime_param)*, B: DestinationReferenceProvider$(, $extra_generic_param)*> DestinationReferenceProvider
            for $generic_context_type<'b$(, $extra_lifetime_param)*, B$(, $extra_generic_param)*>
        {
            fn dest_ref(&self) -> PlaceRef {
                self.base.dest_ref()
            }
        }
    };
}

macro_rules! impl_discr_info_provider {
    ($generic_context_type:ident, $($extra_lifetime_param:lifetime)*, $($extra_generic_param:ident)*) => {
        impl<'b, 'tcx$(, $extra_lifetime_param)*, B: SwitchInfoProvider<'tcx>$(, $extra_generic_param)*> SwitchInfoProvider<'tcx>
            for $generic_context_type<'b$(, $extra_lifetime_param)*, B$(, $extra_generic_param)*>
        {
            fn switch_info(&self) -> SwitchInfo<'tcx> {
                self.base.switch_info()
            }
        }
    };
}

impl_func_info_provider!(TransparentContext,,);
impl_special_types_provider!(TransparentContext,,);
impl_local_manager!(TransparentContext,,);
impl_block_manager!(TransparentContext,,);
impl_jump_target_modifier!(TransparentContext,,);
impl_ty_ctxt_provider!(TransparentContext,,);
impl_body_provider!(TransparentContext,,);
impl_in_entry_function!(TransparentContext,,);
impl_has_local_decls!(TransparentContext,,);
impl_location_provider!(TransparentContext,,);
impl_dest_ref_provider!(TransparentContext,,);
impl_discr_info_provider!(TransparentContext,,);

impl_func_info_provider!(InBodyContext, 'tcxb 'bd,);
impl_special_types_provider!(InBodyContext, 'tcxb 'bd,);
impl_local_manager!(InBodyContext, 'tcxb 'bd,);
impl_block_manager!(InBodyContext, 'tcxb 'bd,);
impl_jump_target_modifier!(InBodyContext, 'tcxb 'bd,);
impl_ty_ctxt_provider!(InBodyContext, 'tcxb 'bd,);
impl_in_entry_function!(InBodyContext, 'tcxb 'bd,);
impl_location_provider!(InBodyContext, 'tcxb 'bd,);
impl_dest_ref_provider!(InBodyContext, 'tcxb 'bd,);
impl_discr_info_provider!(InBodyContext, 'tcxb 'bd,);

impl_func_info_provider!(EntryFunctionMarkerContext,,);
impl_special_types_provider!(EntryFunctionMarkerContext,,);
impl_local_manager!(EntryFunctionMarkerContext,,);
impl_block_manager!(EntryFunctionMarkerContext,,);
impl_jump_target_modifier!(EntryFunctionMarkerContext,,);
impl_ty_ctxt_provider!(EntryFunctionMarkerContext,,);
impl_body_provider!(EntryFunctionMarkerContext,,);
impl_has_local_decls!(EntryFunctionMarkerContext,,);
impl_location_provider!(EntryFunctionMarkerContext,,);
impl_dest_ref_provider!(EntryFunctionMarkerContext,,);
impl_discr_info_provider!(EntryFunctionMarkerContext,,);

impl_func_info_provider!(AtLocationContext,,);
impl_special_types_provider!(AtLocationContext,,);
impl_local_manager!(AtLocationContext,,);
impl_block_manager!(AtLocationContext,,);
impl_jump_target_modifier!(AtLocationContext,,);
impl_ty_ctxt_provider!(AtLocationContext,,);
impl_body_provider!(AtLocationContext,,);
impl_in_entry_function!(AtLocationContext,,);
impl_has_local_decls!(AtLocationContext,,);
impl_dest_ref_provider!(AtLocationContext,,);
impl_discr_info_provider!(AtLocationContext,,);

impl_func_info_provider!(AssignmentContext,,);
impl_special_types_provider!(AssignmentContext,,);
impl_local_manager!(AssignmentContext,,);
impl_block_manager!(AssignmentContext,,);
impl_jump_target_modifier!(AssignmentContext,,);
impl_ty_ctxt_provider!(AssignmentContext,,);
impl_body_provider!(AssignmentContext,,);
impl_in_entry_function!(AssignmentContext,,);
impl_has_local_decls!(AssignmentContext,,);
impl_location_provider!(AssignmentContext,,);
impl_discr_info_provider!(AssignmentContext,,);

impl_func_info_provider!(BranchingContext, 'tcxd,);
impl_special_types_provider!(BranchingContext, 'tcxd,);
impl_local_manager!(BranchingContext, 'tcxd,);
impl_block_manager!(BranchingContext, 'tcxd,);
impl_jump_target_modifier!(BranchingContext, 'tcxd,);
impl_ty_ctxt_provider!(BranchingContext, 'tcxd,);
impl_body_provider!(BranchingContext, 'tcxd,);
impl_in_entry_function!(BranchingContext, 'tcxd,);
impl_has_local_decls!(BranchingContext, 'tcxd,);
impl_location_provider!(BranchingContext, 'tcxd,);
impl_dest_ref_provider!(BranchingContext, 'tcxd,);
