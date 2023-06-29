use delegate::delegate;

use std::collections::HashMap;

use rustc_middle::{
    mir::{self, BasicBlock, BasicBlockData, HasLocalDecls, Local, LocalDecls},
    ty::{Ty, TyCtxt},
};
use rustc_span::def_id::DefId;

use crate::mir_transform::modification::{
    self, BodyBlockManager, BodyLocalManager, BodyModificationUnit, JumpTargetModifier,
};

use super::{pri_utils, OperandRef, PlaceRef};

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

/// Contains types that are used in PRI functions along with primitive types.
pub struct PriTypes<'tcx> {
    pub place_ref: Ty<'tcx>,
    pub operand_ref: Ty<'tcx>,
    pub binary_op: Ty<'tcx>,
    pub unary_op: Ty<'tcx>,
}

pub struct PriHelperFunctions {
    pub f32_to_bits: DefId,
    pub f64_to_bits: DefId,
}

pub trait PriItemsProvider<'tcx> {
    fn get_pri_func_info(&self, func_name: &str) -> &FunctionInfo<'tcx>;
    fn pri_types(&self) -> &PriTypes<'tcx>;
    fn pri_helper_funcs(&self) -> &PriHelperFunctions;
}

pub trait LocationProvider {
    fn location(&self) -> BasicBlock;
}

pub trait DestinationReferenceProvider {
    fn dest_ref(&self) -> PlaceRef;
}

pub trait CastOperandProvider {
    fn operand_ref(&self) -> OperandRef;
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
pub(crate) trait BaseContext<'tcx>
where
    Self: TyContextProvider<'tcx>
        + BodyLocalManager<'tcx>
        + BodyBlockManager<'tcx>
        + PriItemsProvider<'tcx>
        + HasLocalDecls<'tcx>,
{
}

impl<'tcx, C> BaseContext<'tcx> for C where
    Self: TyContextProvider<'tcx>
        + BodyLocalManager<'tcx>
        + BodyBlockManager<'tcx>
        + PriItemsProvider<'tcx>
        + HasLocalDecls<'tcx>
{
}

pub(crate) struct DefaultContext<'tcx, 'm> {
    tcx: TyCtxt<'tcx>,
    modification_unit: &'m mut BodyModificationUnit<'tcx>,
    pri: PriItems<'tcx>,
}

pub(crate) struct PriItems<'tcx> {
    pub funcs: HashMap<String, FunctionInfo<'tcx>>,
    pub types: PriTypes<'tcx>,
    pub helper_funcs: PriHelperFunctions,
}

impl<'tcx, 'm> DefaultContext<'tcx, 'm> {
    pub fn new(tcx: TyCtxt<'tcx>, modification_unit: &'m mut BodyModificationUnit<'tcx>) -> Self {
        use pri_utils::*;
        let pri_symbols = find_pri_exported_symbols(tcx);
        if cfg!(debug_assertions) {
            for def_id in &pri_symbols {
                log::debug!("Found PRI symbol: {:?}", tcx.def_path_str(*def_id));
            }
        }

        Self {
            tcx,
            modification_unit,
            pri: PriItems {
                funcs: find_pri_funcs(&pri_symbols, tcx), // FIXME: Perform caching
                types: find_pri_types(&pri_symbols, tcx),
                helper_funcs: find_helper_funcs(&pri_symbols, tcx),
            },
        }
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

    fn insert_blocks_after<I>(&mut self, index: BasicBlock, blocks: I) -> Vec<BasicBlock>
    where
        I: IntoIterator<Item = BasicBlockData<'tcx>>,
    {
        self.modification_unit.insert_blocks_after(index, blocks)
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

impl<'tcx> PriItemsProvider<'tcx> for PriItems<'tcx> {
    fn get_pri_func_info(&self, func_name: &str) -> &FunctionInfo<'tcx> {
        self.funcs
            .get(&("runtime::".to_owned() + &func_name.replace(' ', ""))) // FIXME
            .unwrap_or_else(|| panic!("Invalid pri function name: `{func_name}`."))
    }

    fn pri_types(&self) -> &PriTypes<'tcx> {
        &self.types
    }

    fn pri_helper_funcs(&self) -> &PriHelperFunctions {
        &self.helper_funcs
    }
}

impl<'tcx> PriItemsProvider<'tcx> for DefaultContext<'tcx, '_> {
    delegate! {
        to self.pri {
            fn get_pri_func_info(&self, func_name: &str) -> &FunctionInfo<'tcx>;
            fn pri_types(&self) -> &PriTypes<'tcx>;
            fn pri_helper_funcs(&self) -> &PriHelperFunctions;
        }
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

pub struct CastAssignmentContext<'b, B> {
    pub(super) base: &'b mut B,
    pub(super) operand_ref: OperandRef,
}

impl<B> CastOperandProvider for CastAssignmentContext<'_, B> {
    fn operand_ref(&self) -> OperandRef {
        self.operand_ref
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

/* We make inheritance of traits from the base context possible through generics.
 * NOTE: From this point we utilize macros as much as possible to prevent boilerplate codes.
 * Probably the best way to understand or fix the following code is to check for
 * the expanded version of it. For example, rust-analyzer provides such an option
 * through the "Expand macro recursively" command in vscode.
 */

/* NOTE: "self" has to be passed as a parameter to not get errors from the hygiene checker. */
macro_rules! delegate_to_base {
    ($self:ident $($fn_def:item)*) => {
        delegate! {
            to $self.base {
                $($fn_def)*
            }
        }
    };
}

/// A meta macro that creates macros for implementing traits through delegation
/// to the base context.
macro_rules! make_impl_macro {
    ($name:ident, $trait:ident$(<$($trait_lifetime:lifetime)*>)?, $self:ident, $($fn_def:item)*) => {
        macro_rules! $name {
            ($$context_name:ident$$(<$$($$context_lifetime:lifetime),* $$($$context_generic:ident),*>)?) => {
                impl<
                    'b
                    $(, $($trait_lifetime)*)?
                    $$(
                        $$(, $context_lifetime)*
                        $$(, $$context_generic)*
                    )?
                    , B: $trait$(<$($trait_lifetime),*>)?
                > $trait$(<$($trait_lifetime),*>)?
                    for $$context_name<'b$$($$(, $$context_lifetime)*$$(, $$context_generic)*)?, B>
                {
                    delegate_to_base!{ $self $($fn_def)* }
                }
            };
        }
    };
}

make_impl_macro! {
    impl_pri_items_provider,
    PriItemsProvider<'tcx>,
    self,
    fn get_pri_func_info(&self, func_name: &str) -> &FunctionInfo<'tcx>;
    fn pri_types(&self) -> &PriTypes<'tcx>;
    fn pri_helper_funcs(&self) -> &PriHelperFunctions;
}

make_impl_macro! {
    impl_local_manager,
    BodyLocalManager<'tcx>,
    self,
    fn add_local<T>(&mut self, decl_info: T) -> Local
    where
        T: Into<modification::NewLocalDecl<'tcx>>;
}

make_impl_macro! {
    impl_block_manager,
    BodyBlockManager<'tcx>,
    self,
    fn insert_blocks_before<I>(&mut self, index: BasicBlock, blocks: I, sticky: bool) -> Vec<BasicBlock>
    where
        I: IntoIterator<Item = BasicBlockData<'tcx>>;

    fn insert_blocks_after<I>(&mut self, index: BasicBlock, blocks: I) -> Vec<BasicBlock>
    where
        I: IntoIterator<Item = BasicBlockData<'tcx>>;
}

make_impl_macro! {
    impl_jump_target_modifier,
    JumpTargetModifier,
    self,
    fn modify_jump_target_where(
        &mut self,
        terminator_location: BasicBlock,
        from: BasicBlock,
        to: BasicBlock,
        constraint: modification::JumpModificationConstraint,
    );
}

make_impl_macro! {
    impl_ty_ctxt_provider,
    TyContextProvider<'tcx>,
    self,
    fn tcx(&self) -> TyCtxt<'tcx>;
}

make_impl_macro! {
    impl_body_provider,
    BodyProvider<'tcx>,
    self,
    fn body(&self) -> &mir::Body<'tcx>;
}

make_impl_macro! {
    impl_in_entry_function,
    InEntryFunction,
    self,
    // No functions to implement
}

make_impl_macro! {
    impl_has_local_decls,
    HasLocalDecls<'tcx>,
    self,
    fn local_decls(&self) -> &LocalDecls<'tcx>;
}

make_impl_macro! {
    impl_location_provider,
    LocationProvider,
    self,
    fn location(&self) -> BasicBlock;
}

make_impl_macro! {
    impl_dest_ref_provider,
    DestinationReferenceProvider,
    self,
    fn dest_ref(&self) -> PlaceRef;
}

make_impl_macro! {
    impl_cast_operand_provider,
    CastOperandProvider,
    self,
    fn operand_ref(&self) -> OperandRef;
}

make_impl_macro! {
    impl_discr_info_provider,
    SwitchInfoProvider<'tcx>,
    self,
    fn switch_info(&self) -> SwitchInfo<'tcx>;
}

/// A meta macro that creates a macro able to call a list of macros with exclusions for some input.
/// Note that the excluded macros must appear in the same order as in the original "all" list.
macro_rules! make_caller_macro {
    ($name:ident, [$($impl_macro:ident),+$(,)?]) => {
        macro_rules! $name {
            (all for $$($$target:tt)+) => {
                $name!([$($impl_macro)*] for $$($$target)+);
            };
            (all - [$$to_skip_head:ident $$($$to_skip_tail:ident)*] for $$($$target:tt)+) => {
                $name!(
                    [$($impl_macro)*] - [$$to_skip_head $$($$to_skip_tail)*]
                    for $$($$target)+
                );
            };
            $(
                ([$impl_macro $$($$to_impl:ident)*] - [$impl_macro $$($$to_skip_tail:ident)*] for $$($$target:tt)+) => {
                    $name!(
                        [$$($$to_impl)*] - [$$($$to_skip_tail)*]
                        for $$($$target)+
                    );
                };
            )+
            ([$$($$to_impl:ident)*] - [] for $$($$target:tt)+) => {
                $name!(
                    [$$($$to_impl)*]
                    for $$($$target)+
                );
            };
            ([$$to_impl_head:ident $$($$to_impl_tail:ident)*] - [$$to_skip_head:ident $$($$to_skip_tail:ident)* ] for $$($$target:tt)+) => {
                $$to_impl_head!($$($$target)+);
                $name!(
                    [$$($$to_impl_tail)*] - [$$to_skip_head $$($$to_skip_tail)*]
                    for $$($$target)+
                );
            };
            ([$$to_impl_head:ident $$($$to_impl_tail:ident)*] for $$($$target:tt)+) => {
                $$to_impl_head!($$($$target)+);
                $name!(
                    [$$($$to_impl_tail)*]
                    for $$($$target)+
                );
            };
            ([] for $$($$target:tt)+) => {
            };
        }
    };
}

// A macro that calls all "impl" macros defined above for the target type
// except the ones to be excluded.
make_caller_macro!(
    impl_traits,
    [
        impl_pri_items_provider,
        impl_local_manager,
        impl_block_manager,
        impl_jump_target_modifier,
        impl_ty_ctxt_provider,
        impl_body_provider,
        impl_in_entry_function,
        impl_has_local_decls,
        impl_location_provider,
        impl_dest_ref_provider,
        impl_cast_operand_provider,
        impl_discr_info_provider,
    ]
);

impl_traits!(all for TransparentContext);
impl_traits!(all - [ impl_body_provider impl_has_local_decls ] for InBodyContext<'tcxb, 'bd>);
impl_traits!(all - [ impl_in_entry_function ] for EntryFunctionMarkerContext);
impl_traits!(all - [ impl_location_provider ] for AtLocationContext);
impl_traits!(all - [ impl_dest_ref_provider ] for AssignmentContext);
impl_traits!(all - [ impl_cast_operand_provider ] for CastAssignmentContext);
impl_traits!(all - [ impl_discr_info_provider ] for BranchingContext<'tcxd>);
