use delegate::delegate;

use std::collections::HashMap;

use rustc_hir::def::DefKind;
use rustc_middle::{
    mir::{self, BasicBlock, BasicBlockData, HasLocalDecls, Local, LocalDecls},
    ty::{Ty, TyCtxt},
};
use rustc_span::def_id::DefId;

use crate::mir_transform::modification::{
    self, BodyBlockManager, BodyLocalManager, BodyModificationUnit, JumpTargetModifier,
};

use super::{OperandRef, PlaceRef};

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

pub(crate) struct DefaultContext<'tcx, 'm> {
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
        DefaultContext::get_pri_exported_symbols(tcx)
            .into_iter()
            .filter(|def_id| matches!(tcx.def_kind(def_id), DefKind::Fn | DefKind::AssocFn))
            .map(|def_id| {
                (
                    tcx.def_path_str(def_id),
                    FunctionInfo {
                        def_id,
                        ret_ty: tcx
                            .fn_sig(def_id)
                            .skip_binder()
                            .output()
                            .no_bound_vars()
                            .expect(
                                "PRI functions are not expected to have bound vars (generics).",
                            ),
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
        let def_ids: HashMap<String, DefId> = DefaultContext::get_pri_exported_symbols(tcx)
            .into_iter()
            .filter(|def_id| matches!(tcx.def_kind(def_id), DefKind::Static(_)))
            .map(|def_id| (tcx.def_path_str(def_id), def_id))
            .collect();

        let get_ty = |name: &str| -> Ty<'tcx> {
            tcx.type_of(def_ids.get(&name.replace(' ', "")).unwrap())
                .no_bound_vars()
                .expect("PRI special types are not expected to have bound vars (generics).")
        };
        SpecialTypes {
            place_ref: get_ty(stringify!(runtime::pri::PLACE_REF_TYPE_HOLDER)),
            operand_ref: get_ty(stringify!(runtime::pri::OPERAND_REF_TYPE_HOLDER)),
            binary_op: get_ty(stringify!(runtime::pri::BINARY_OP_TYPE_HOLDER)),
            unary_op: get_ty(stringify!(runtime::pri::UNARY_OP_TYPE_HOLDER)),
        }
    }

    fn get_pri_exported_symbols(tcx: TyCtxt<'tcx>) -> Vec<DefId> {
        use rustc_hir::definitions::{DefPathData, DisambiguatedDefPathData};
        use rustc_middle::middle::exported_symbols::ExportedSymbol;

        fn def_id<'a>(symbol: &'a ExportedSymbol) -> Option<&'a DefId> {
            match symbol {
                ExportedSymbol::NonGeneric(def_id) | ExportedSymbol::Generic(def_id, _) => {
                    Some(def_id)
                }
                _ => None,
            }
        }

        fn module(tcx: TyCtxt, def_id: &DefId) -> impl Iterator<Item = DisambiguatedDefPathData> {
            tcx.def_path(*def_id)
                .data
                .into_iter()
                .take_while(|p| matches!(p.data, DefPathData::TypeNs(_)))
        }

        // Finding the runtime crate.
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

        let runtime_symbols = tcx
            .exported_symbols(crate_num)
            .iter()
            .filter_map(|(s, _)| def_id(s))
            .cloned()
            .filter(|def_id| def_id.krate == crate_num);

        // Finding the pri module.
        let pri_module = {
            let name = stringify!(runtime::pri::MODULE_MARKER).replace(' ', "");
            let marker_id = runtime_symbols
                .clone()
                .find(|def_id| tcx.def_path_str(def_id) == name)
                .expect("The pri module should contain the marker symbol.");
            module(tcx, &marker_id).collect::<Vec<_>>()
        };

        runtime_symbols
            .filter(|def_id| module(tcx, def_id).eq_by(&pri_module, |a, b| &a == b))
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
    impl_func_info_provider,
    FunctionInfoProvider<'tcx>,
    self,
    fn get_pri_func_info(&self, func_name: &str) -> &FunctionInfo<'tcx>;
}

make_impl_macro! {
    impl_special_types_provider,
    SpecialTypesProvider<'tcx>,
    self,
    fn pri_special_types(&self) -> &SpecialTypes<'tcx>;
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
            (all for $$($$xxxxxx:tt)+) => {
                $name!([$($impl_macro)*] for $$($$xxxxxx)+);
            };
            (all - [$$to_skip_head:ident $$($$to_skip_tail:ident)*] for $$($$xxxxxx:tt)+) => {
                $name!(
                    [$($impl_macro)*] - [$$to_skip_head $$($$to_skip_tail)*]
                    for $$($$xxxxxx)+
                );
            };
            $(
                ([$impl_macro $$($$to_impl:ident)*] - [$impl_macro $$($$to_skip_tail:ident)*] for $$($$xxxxxx:tt)+) => {
                    $name!(
                        [$$($$to_impl)*] - [$$($$to_skip_tail)*]
                        for $$($$xxxxxx)+
                    );
                };
            )+
            ([$$($$to_impl:ident)*] - [] for $$($$xxxxxx:tt)+) => {
                $name!(
                    [$$($$to_impl)*]
                    for $$($$xxxxxx)+
                );
            };
            ([$$to_impl_head:ident $$($$to_impl_tail:ident)*] - [$$to_skip_head:ident $$($$to_skip_tail:ident)* ] for $$($$xxxxxx:tt)+) => {
                $$to_impl_head!($$($$xxxxxx)+);
                $name!(
                    [$$($$to_impl_tail)*] - [$$to_skip_head $$($$to_skip_tail)*]
                    for $$($$xxxxxx)+
                );
            };
            ([$$to_impl_head:ident $$($$to_impl_tail:ident)*] for $$($$xxxxxx:tt)+) => {
                $$to_impl_head!($$($$xxxxxx)+);
                $name!(
                    [$$($$to_impl_tail)*]
                    for $$($$xxxxxx)+
                );
            };
            ([] for $$($$xxxxxx:tt)+) => {
            };
        }
    };
}

// A macro that calls all "impl" macros defined above for the target type
// except the ones to be excluded.
make_caller_macro!(
    impl_traits,
    [
        impl_func_info_provider,
        impl_special_types_provider,
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
