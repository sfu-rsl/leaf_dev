use std::collections::HashMap;

use rustc_hir::def::DefKind;
use rustc_middle::{
    middle::exported_symbols,
    mir::{BasicBlock, BasicBlockData, Local},
    ty::{Ty, TyCtxt},
};
use rustc_span::def_id::DefId;

use crate::mir_transform::modification::{
    self, BodyBlockManager, BodyLocalManager, BodyModificationUnit,
};

pub struct FunctionInfo<'tcx> {
    pub def_id: DefId,
    pub ret_ty: Ty<'tcx>,
}

pub trait TyContextProvider<'tcx> {
    fn tcx(&self) -> TyCtxt<'tcx>;
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
    fn dest_ref(&self) -> Local;
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
        + SpecialTypesProvider<'tcx>,
{
}

impl<'tcx, C> BaseContext<'tcx> for C where
    Self: TyContextProvider<'tcx>
        + BodyLocalManager<'tcx>
        + BodyBlockManager<'tcx>
        + FunctionInfoProvider<'tcx>
        + SpecialTypesProvider<'tcx>
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
            .filter(|def_id| matches!(tcx.def_kind(def_id), DefKind::Fn))
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
            |name: &str| -> Ty<'tcx> { tcx.type_of(def_ids.get(&name.replace(" ", "")).unwrap()) };
        SpecialTypes {
            place_ref: get_ty(stringify!(pri::PLACE_REF_TYPE_HOLDER)),
            operand_ref: get_ty(stringify!(pri::OPERAND_REF_TYPE_HOLDER)),
            binary_op: get_ty(stringify!(pri::BINARY_OP_TYPE_HOLDER)),
            unary_op: get_ty(stringify!(pri::UNARY_OP_TYPE_HOLDER)),
        }
    }

    fn get_exported_symbols_of_pri(tcx: TyCtxt<'tcx>) -> Vec<DefId> {
        let crate_num = tcx
            .crates(())
            .iter()
            .find(|cnum| tcx.crate_name(**cnum).as_str() == stringify!(pri))
            .expect(format!("{} crate is not added as a dependency.", stringify!(pri)).as_str())
            .clone();

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
    fn insert_blocks_before<I>(&mut self, index: BasicBlock, blocks: I) -> Vec<BasicBlock>
    where
        I: IntoIterator<Item = BasicBlockData<'tcx>>,
    {
        self.modification_unit.insert_blocks_before(index, blocks)
    }
}

impl<'tcx> FunctionInfoProvider<'tcx> for DefaultContext<'tcx, '_> {
    fn get_pri_func_info(&self, func_name: &str) -> &FunctionInfo<'tcx> {
        self.pri_functions
            .get(&func_name.replace(" ", "")) // FIXME
            .expect("Invalid pri function name.")
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
    pub(super) dest_ref: Local,
}

impl<B> DestinationReferenceProvider for AssignmentContext<'_, B> {
    fn dest_ref(&self) -> Local {
        self.dest_ref
    }
}

/*
 * We make inheritance of traits from the base context possible through generics.
 */

macro_rules! impl_func_info_provider {
    ($generic_context_type:ident) => {
        impl<'tcx, 'b, B: FunctionInfoProvider<'tcx>> FunctionInfoProvider<'tcx>
            for $generic_context_type<'b, B>
        {
            fn get_pri_func_info(&self, func_name: &str) -> &FunctionInfo<'tcx> {
                self.base.get_pri_func_info(func_name)
            }
        }
    };
}

macro_rules! impl_special_types_provider {
    ($generic_context_type:ident) => {
        impl<'tcx, 'b, B: SpecialTypesProvider<'tcx>> SpecialTypesProvider<'tcx>
            for $generic_context_type<'b, B>
        {
            fn pri_special_types(&self) -> &SpecialTypes<'tcx> {
                self.base.pri_special_types()
            }
        }
    };
}

macro_rules! impl_local_manager {
    ($generic_context_type:ident) => {
        impl<'tcx, 'b, B: BodyLocalManager<'tcx>> BodyLocalManager<'tcx>
            for $generic_context_type<'b, B>
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
    ($generic_context_type:ident) => {
        impl<'tcx, 'b, B: BodyBlockManager<'tcx>> BodyBlockManager<'tcx>
            for $generic_context_type<'b, B>
        {
            fn insert_blocks_before<I>(&mut self, index: BasicBlock, blocks: I) -> Vec<BasicBlock>
            where
                I: IntoIterator<Item = BasicBlockData<'tcx>>,
            {
                self.base.insert_blocks_before(index, blocks)
            }
        }
    };
}

macro_rules! impl_ty_ctxt_provider {
    ($generic_context_type:ident) => {
        impl<'tcx, 'b, B: TyContextProvider<'tcx>> TyContextProvider<'tcx>
            for $generic_context_type<'b, B>
        {
            fn tcx(&self) -> TyCtxt<'tcx> {
                self.base.tcx()
            }
        }
    };
}

macro_rules! impl_location_provider {
    ($generic_context_type:ident) => {
        impl<'b, B: LocationProvider> LocationProvider for $generic_context_type<'b, B> {
            fn location(&self) -> BasicBlock {
                self.base.location()
            }
        }
    };
}

macro_rules! impl_dest_ref_provider {
    ($generic_context_type:ident) => {
        impl<'b, B: DestinationReferenceProvider> DestinationReferenceProvider
            for $generic_context_type<'b, B>
        {
            fn dest_ref(&self) -> Local {
                self.base.dest_ref()
            }
        }
    };
}

impl_func_info_provider!(TransparentContext);
impl_special_types_provider!(TransparentContext);
impl_local_manager!(TransparentContext);
impl_block_manager!(TransparentContext);
impl_ty_ctxt_provider!(TransparentContext);
impl_location_provider!(TransparentContext);
impl_dest_ref_provider!(TransparentContext);

impl_func_info_provider!(AtLocationContext);
impl_special_types_provider!(AtLocationContext);
impl_local_manager!(AtLocationContext);
impl_block_manager!(AtLocationContext);
impl_ty_ctxt_provider!(AtLocationContext);
impl_dest_ref_provider!(AtLocationContext);

impl_func_info_provider!(AssignmentContext);
impl_special_types_provider!(AssignmentContext);
impl_local_manager!(AssignmentContext);
impl_block_manager!(AssignmentContext);
impl_ty_ctxt_provider!(AssignmentContext);
impl_location_provider!(AssignmentContext);
