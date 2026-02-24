use rustc_middle::mir::ProjectionElem;

use common::log_warn;

use super::{
    BodyProvider, PlaceReferencer,
    ctxt_reqs::ForPlaceRef,
    prelude::{mir::*, *},
};

impl<'tcx, C> PlaceReferencer<'tcx> for RuntimeCallAdder<C>
where
    Self: MirCallAdder<'tcx> + BlockInserter<'tcx>,
    C: ForPlaceRef<'tcx>,
{
    fn reference_place(&mut self, place: &Place<'tcx>) -> PlaceRef {
        let BlocksAndResult(new_blocks, reference) = self.internal_reference_place(place);
        self.insert_blocks(new_blocks);
        reference.into()
    }
}
impl<'tcx, C> RuntimeCallAdder<C>
where
    Self: MirCallAdder<'tcx> + BlockInserter<'tcx>,
    C: ForPlaceRef<'tcx>,
{
    pub(super) fn internal_reference_place(
        &mut self,
        place: &Place<'tcx>,
    ) -> BlocksAndResult<'tcx> {
        let BlocksAndResult(mut blocks, place_ref) = self.reference_place_local(place.local);

        let tcx = self.tcx();

        // For setting addresses we have to remake a cumulative place up to each projection.
        let mut cum_place = Place::from(place.local);
        let mut cum_ty = cum_place.ty(&self.context, tcx);

        for (_, proj) in place.iter_projections() {
            let added_blocks = self.reference_place_projection(place_ref, proj);
            blocks.extend(added_blocks);

            cum_place = cum_place.project_deeper(&[proj], tcx);
            cum_ty = cum_ty.projection_ty(tcx, proj);

            blocks.extend(self.set_place_type(place_ref, cum_ty.ty));
            blocks.push(self.set_place_addr(place_ref, &cum_place, cum_ty.ty));
        }

        BlocksAndResult(blocks, place_ref)
    }

    pub(super) fn reference_place_local(&mut self, local: Local) -> BlocksAndResult<'tcx> {
        let kind = self.body().local_kind(local);
        use rustc_middle::mir::LocalKind::*;
        let func_name = match kind {
            Temp => sym::ref_place_local,
            Arg => sym::ref_place_argument,
            ReturnPointer => sym::ref_place_return_value,
        };
        let args = match kind {
            ReturnPointer => vec![],
            _ => vec![operand::const_from_uint(self.context.tcx(), local.as_u32())],
        };

        let (block, place_ref) = self.make_bb_for_place_ref_call(func_name, args);
        let mut blocks = vec![block];

        let ty = self.local_decls()[local].ty;
        blocks.extend(self.set_place_type(place_ref, ty));
        blocks.push(self.set_place_addr(place_ref, &local.into(), ty));

        BlocksAndResult(blocks, place_ref)
    }

    fn reference_place_projection<T>(
        &mut self,
        current_ref: Local,
        proj: ProjectionElem<Local, T>,
    ) -> Vec<BasicBlockData<'tcx>> {
        let mut new_blocks = Vec::new();

        let (func_name, additional_args) = match proj {
            ProjectionElem::Deref => (sym::ref_place_deref, vec![]),
            ProjectionElem::Field(index, _) => (
                sym::ref_place_field,
                vec![operand::const_from_uint(
                    self.context.tcx(),
                    u32::from(index),
                )],
            ),
            ProjectionElem::Index(index) => {
                let BlocksAndResult(additional_blocks, index_ref) =
                    self.internal_reference_place(&Place::from(index));
                new_blocks.extend(additional_blocks);
                (
                    sym::ref_place_index,
                    vec![operand::copy_for_local(index_ref)],
                )
            }
            ProjectionElem::ConstantIndex {
                offset,
                min_length,
                from_end,
            } => (
                sym::ref_place_constant_index,
                vec![
                    operand::const_from_uint(self.context.tcx(), offset),
                    operand::const_from_uint(self.context.tcx(), min_length),
                    operand::const_from_bool(self.context.tcx(), from_end),
                ],
            ),
            ProjectionElem::Subslice { from, to, from_end } => (
                sym::ref_place_subslice,
                vec![
                    operand::const_from_uint(self.context.tcx(), from),
                    operand::const_from_uint(self.context.tcx(), to),
                    operand::const_from_bool(self.context.tcx(), from_end),
                ],
            ),
            ProjectionElem::Downcast(_, index) => (
                sym::ref_place_downcast,
                vec![operand::const_from_uint(
                    self.context.tcx(),
                    u32::from(index),
                )],
            ),
            ProjectionElem::OpaqueCast(_) => (sym::ref_place_opaque_cast, vec![]),
            ProjectionElem::UnwrapUnsafeBinder(..) => (sym::ref_place_unwrap_unsafe_binder, vec![]),
        };

        new_blocks.push(
            self.make_bb_for_place_ref_call(
                func_name,
                [vec![operand::copy_for_local(current_ref)], additional_args].concat(),
            )
            .0, // The result is unit.
        );
        new_blocks
    }

    fn make_bb_for_place_ref_call(
        &mut self,
        func_name: LeafSymbol,
        args: Vec<Operand<'tcx>>,
    ) -> (BasicBlockData<'tcx>, Local) {
        self.make_bb_for_call_with_ret(func_name, args)
    }

    fn set_place_addr(
        &mut self,
        place_ref: Local,
        place: &Place<'tcx>,
        place_ty: Ty<'tcx>,
    ) -> BasicBlockData<'tcx> {
        let (stmt, ptr_local) = utils::ptr_to_place(self.tcx(), &mut self.context, place, place_ty);
        let (mut block, _) = {
            self.make_bb_for_helper_call_with_all(
                self.context.pri_helper_funcs().set_place_address_typed,
                vec![place_ty.into()],
                vec![
                    operand::copy_for_local(place_ref),
                    operand::move_for_local(ptr_local),
                ],
                None,
            )
        };
        block.statements.push(stmt);
        block
    }

    pub(super) fn set_place_size(
        &mut self,
        place_ref: Local,
        place_ty: Ty<'tcx>,
    ) -> Vec<BasicBlockData<'tcx>> {
        if !place_ty.is_sized(self.tcx(), self.current_typing_env()) {
            log_warn!("Encountered unsized type. Skipping size setting.");
            return vec![BasicBlockData::new(Some(terminator::goto(None)), false)];
        }

        let (get_call_block, size_local) = self.make_bb_for_helper_call_with_all(
            self.context.pri_helper_funcs().size_of,
            vec![place_ty.into()],
            Vec::default(),
            None,
        );

        let set_call_block = self.make_bb_for_call(
            sym::set_place_size,
            vec![
                operand::copy_for_local(place_ref),
                operand::move_for_local(size_local),
            ],
        );

        vec![get_call_block, set_call_block]
    }

    fn set_place_type(&mut self, place_ref: Local, ty: Ty<'tcx>) -> Vec<BasicBlockData<'tcx>> {
        let mut blocks = vec![];

        let tcx = self.context.tcx();
        // FIXME: To be removed when type information passing is complete.
        if let Some((func_name, additional_args)) = if ty.is_bool() {
            Some((sym::set_place_type_bool, vec![]))
        } else if ty.is_char() {
            Some((sym::set_place_type_char, vec![]))
        } else if ty.is_integral() {
            Some((
                sym::set_place_type_int,
                vec![
                    operand::const_from_uint(tcx, ty.primitive_size(tcx).bits()),
                    operand::const_from_bool(tcx, ty.is_signed()),
                ],
            ))
        } else if ty.is_floating_point() {
            let (e_bits, s_bits) = ty::ebit_sbit_size(ty);
            Some((
                sym::set_place_type_float,
                vec![
                    operand::const_from_uint(tcx, e_bits),
                    operand::const_from_uint(tcx, s_bits),
                ],
            ))
        } else {
            None
        } {
            blocks.push(self.make_bb_for_call(
                func_name,
                [vec![operand::copy_for_local(place_ref)], additional_args].concat(),
            ));
        }

        let id_local = {
            let (block, id_local) = self.make_type_id_of_bb(ty);
            blocks.push(block);
            id_local
        };

        blocks.push(self.make_bb_for_call(
            sym::set_place_type_id,
            vec![
                operand::copy_for_local(place_ref),
                operand::move_for_local(id_local),
            ],
        ));

        blocks
    }
}

mod utils {
    use rustc_middle::mir::RawPtrKind;

    pub(super) use super::super::prelude::{mir::*, *};

    pub(super) use super::super::utils::{assignment, operand, terminator, ty};

    pub(super) fn ptr_to_place<'tcx>(
        tcx: TyCtxt<'tcx>,
        local_manager: &mut impl BodyLocalManager<'tcx>,
        place: &Place<'tcx>,
        place_ty: Ty<'tcx>,
    ) -> (Statement<'tcx>, Local) {
        let ptr_local = local_manager.add_local(Ty::new_imm_ptr(tcx, place_ty));
        let ptr_assignment = assignment::create(
            Place::from(ptr_local),
            Rvalue::RawPtr(RawPtrKind::Const, place.clone()),
        );

        (ptr_assignment, ptr_local)
    }
}
use utils::*;
