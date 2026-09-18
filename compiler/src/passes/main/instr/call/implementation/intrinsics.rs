use rustc_span::Spanned;

use common::pri::{AtomicBinaryOp, AtomicOrdering};

use super::{
    AssignmentInfoProvider, AtomicIntrinsicHandler, IntrinsicHandler,
    context::PointerParamProvider,
    ctxt_reqs::{Basic, ForAssignment, ForAtomicIntrinsic, ForMemoryIntrinsic, ForOperandRef},
    prelude::{mir::*, *},
    pri::sym::intrinsics::{
        LeafIntrinsicSymbol, atomic::LeafAtomicIntrinsicSymbol, memory::LeafMemoryIntrinsicSymbol,
    },
    utils::operand,
};

impl<'tcx, C> IntrinsicHandler<'tcx> for RuntimeCallAdder<C>
where
    Self: MirCallAdder<'tcx> + BlockInserter<'tcx>,
    C: ForAssignment<'tcx>,
{
    fn intrinsic_one_to_one_by(
        &mut self,
        intrinsic_func: DefId,
        pri_func: LeafIntrinsicSymbol,
        args: impl Iterator<Item = OperandRef>,
    ) {
        self.assert_pri_intrinsic_consistency(intrinsic_func, pri_func);

        let pri_name = *pri_func;
        let args = args.map(Into::into).map(operand::move_for_local).collect();
        let block = self.make_bb_for_assign_call(pri_name, args);
        self.insert_blocks([block]);
    }
}

impl<'tcx, C> RuntimeCallAdder<C>
where
    C: Basic<'tcx>,
{
    fn assert_pri_intrinsic_consistency(
        &mut self,
        intrinsic_func: DefId,
        pri_func: LeafIntrinsicSymbol,
    ) {
        let tcx = self.tcx();
        let arg_num = |def_id| {
            tcx.fn_sig(def_id)
                .instantiate_identity()
                .skip_normalization()
                .inputs()
                .iter()
                .count()
        };
        let pri_func_info = self.get_pri_func_info(*pri_func);

        let pri_func_arg_num = arg_num(pri_func_info.def_id);
        let intrinsic_arg_num = arg_num(intrinsic_func);

        assert_eq!(
            pri_func_arg_num - (1/* assignment_id */) - (1/* dest */),
            intrinsic_arg_num,
            "Inconsistent number of arguments between intrinsic and its corresponding PRI function. {:?} -x-> {:?}",
            intrinsic_func,
            pri_func_info.def_id
        );
    }
}

impl<'tcx, C> MemoryIntrinsicHandler<'tcx> for RuntimeCallAdder<C>
where
    Self: MirCallAdder<'tcx> + BlockInserter<'tcx>,
    C: ForMemoryIntrinsic<'tcx>,
{
    fn load(&mut self, is_ptr_aligned: bool) {
        self.add_bb_for_memory_op_intrinsic_call(
            sym::intrinsics::memory::intrinsic_memory_load,
            vec![
                operand::move_for_local(self.dest_ref().into()),
                operand::const_from_bool(self.tcx(), self.context.is_volatile()),
                operand::const_from_bool(self.tcx(), is_ptr_aligned),
            ],
            Default::default(),
            Default::default(),
        );
    }

    fn store(&mut self, val: &Spanned<Operand<'tcx>>, is_ptr_aligned: bool) {
        let val_ref = self.reference_operand_spanned(val);
        self.add_bb_for_memory_op_intrinsic_call(
            sym::intrinsics::memory::intrinsic_memory_store,
            vec![
                operand::move_for_local(val_ref.into()),
                operand::const_from_bool(self.tcx(), self.context.is_volatile()),
                operand::const_from_bool(self.tcx(), is_ptr_aligned),
            ],
            Default::default(),
            Default::default(),
        )
    }

    fn copy(
        &mut self,
        dst: &Spanned<Operand<'tcx>>,
        count: &Spanned<Operand<'tcx>>,
        is_overlapping: bool,
    ) {
        let mut stmts = Vec::new();
        let dst_ref = self.reference_operand_spanned(dst);
        let count_ref = self.reference_operand_spanned(count);

        let conc_dst_ptr_local = {
            let (ptr_stmts, id_local) = self.make_conc_ptr_assignment(dst.node.to_copy());
            stmts.extend(ptr_stmts);
            id_local
        };

        self.add_bb_for_memory_op_intrinsic_call(
            sym::intrinsics::memory::intrinsic_memory_copy,
            vec![
                operand::move_for_local(dst_ref.into()),
                operand::move_for_local(conc_dst_ptr_local),
                operand::move_for_local(count_ref.into()),
                count.node.to_copy(),
                operand::const_from_bool(self.tcx(), self.context.is_volatile()),
                operand::const_from_bool(self.tcx(), is_overlapping),
            ],
            stmts,
            Default::default(),
        )
    }

    fn set(&mut self, val: &Spanned<Operand<'tcx>>, count: &Spanned<Operand<'tcx>>) {
        let val_ref = self.reference_operand_spanned(val);
        let count_ref = self.reference_operand_spanned(count);
        self.add_bb_for_memory_op_intrinsic_call(
            sym::intrinsics::memory::intrinsic_memory_set,
            vec![
                operand::move_for_local(val_ref.into()),
                operand::move_for_local(count_ref.into()),
                count.node.to_copy(),
                operand::const_from_bool(self.tcx(), self.context.is_volatile()),
            ],
            Default::default(),
            Default::default(),
        )
    }

    fn swap(&mut self, second: &Spanned<Operand<'tcx>>) {
        let mut stmts = Vec::new();
        let second_ref = self.reference_operand_spanned(second);

        let conc_second_ptr_local = {
            let (ptr_stmts, id_local) = self.make_conc_ptr_assignment(second.node.to_copy());
            stmts.extend(ptr_stmts);
            id_local
        };

        self.add_bb_for_memory_op_intrinsic_call(
            sym::intrinsics::memory::intrinsic_memory_swap,
            vec![
                operand::move_for_local(second_ref.into()),
                operand::move_for_local(conc_second_ptr_local),
            ],
            stmts,
            Default::default(),
        )
    }

    fn raw_eq(&mut self, second: &Spanned<Operand<'tcx>>) {
        let mut stmts = Vec::new();
        let second_ref = self.reference_operand_spanned(second);

        let conc_second_ptr_local = {
            let (ptr_stmts, id_local) = self.make_conc_ptr_assignment(second.node.to_copy());
            stmts.extend(ptr_stmts);
            id_local
        };

        self.add_bb_for_memory_op_intrinsic_call(
            sym::intrinsics::memory::intrinsic_assign_raw_eq,
            vec![
                operand::move_for_local(self.dest_ref().into()),
                operand::move_for_local(second_ref.into()),
                operand::move_for_local(conc_second_ptr_local),
            ],
            stmts,
            Default::default(),
        )
    }

    fn compare_bytes(&mut self, second: &Spanned<Operand<'tcx>>, count: &Spanned<Operand<'tcx>>) {
        let mut stmts = Vec::new();
        let second_ref = self.reference_operand_spanned(second);
        let count_ref = self.reference_operand_spanned(count);

        let conc_second_ptr_local = {
            let (ptr_stmts, id_local) = self.make_conc_ptr_assignment(second.node.to_copy());
            stmts.extend(ptr_stmts);
            id_local
        };
        self.add_bb_for_memory_op_intrinsic_call(
            sym::intrinsics::memory::intrinsic_assign_compare_bytes,
            vec![
                operand::move_for_local(self.dest_ref().into()),
                operand::move_for_local(second_ref.into()),
                operand::move_for_local(conc_second_ptr_local),
                operand::move_for_local(count_ref.into()),
                count.node.to_copy(),
            ],
            stmts,
            Default::default(),
        )
    }
}

impl<'tcx, C> RuntimeCallAdder<C>
where
    Self: MirCallAdder<'tcx> + BlockInserter<'tcx>,
    C: ForMemoryIntrinsic<'tcx>,
{
    fn add_bb_for_memory_op_intrinsic_call(
        &mut self,
        func: LeafMemoryIntrinsicSymbol,
        additional_args: Vec<Operand<'tcx>>,
        additional_stmts: Vec<Statement<'tcx>>,
        additional_blocks: Vec<BasicBlockData<'tcx>>,
    ) {
        let mut stmts = additional_stmts;
        let mut blocks = additional_blocks;

        let PointerParamInstrPack {
            type_id_block,
            conc_ptr_stmts,
            pri_args: [ptr_ref, ptr_value, ptr_type_id],
        } = self.reference_ptr_param();
        stmts.extend(conc_ptr_stmts);
        blocks.push(type_id_block);

        let mut block = self.make_bb_for_call(
            **func,
            [
                vec![
                    operand::const_from_uint(self.tcx(), self.assignment_id()),
                    ptr_ref,
                    ptr_value,
                    ptr_type_id,
                ],
                additional_args,
            ]
            .concat(),
        );
        block.statements.extend(stmts);
        blocks.push(block);

        self.insert_blocks(blocks);
    }
}

impl<'tcx, C> AtomicIntrinsicHandler<'tcx> for RuntimeCallAdder<C>
where
    Self: MirCallAdder<'tcx> + BlockInserter<'tcx>,
    C: ForAtomicIntrinsic<'tcx>,
{
    fn load(&mut self)
    where
        Self: AssignmentInfoProvider,
    {
        self.add_bb_for_atomic_intrinsic_call_with_ptr(
            sym::intrinsics::atomic::intrinsic_atomic_load,
            vec![operand::move_for_local(self.dest_ref().into())],
            Default::default(),
        );
    }

    fn store(&mut self, val: &Spanned<Operand<'tcx>>)
    where
        Self: AssignmentInfoProvider,
    {
        let val_ref = self.reference_operand_spanned(val);
        self.add_bb_for_atomic_intrinsic_call_with_ptr(
            sym::intrinsics::atomic::intrinsic_atomic_store,
            vec![operand::move_for_local(val_ref.into())],
            Default::default(),
        )
    }

    fn exchange(&mut self, val: &Spanned<Operand<'tcx>>)
    where
        Self: AssignmentInfoProvider,
    {
        let val_ref = self.reference_operand_spanned(val);
        self.add_bb_for_atomic_intrinsic_call_with_ptr(
            sym::intrinsics::atomic::intrinsic_atomic_xchg,
            vec![
                operand::move_for_local(val_ref.into()),
                operand::move_for_local(self.dest_ref().into()),
            ],
            Default::default(),
        );
    }

    fn compare_exchange(
        &mut self,
        failure_ordering: AtomicOrdering,
        weak: bool,
        old: &Spanned<Operand<'tcx>>,
        src: &Spanned<Operand<'tcx>>,
    ) where
        Self: AssignmentInfoProvider,
    {
        let mut additional_blocks = vec![];
        let old_ref = self.reference_operand_spanned(old);
        let src_ref = self.reference_operand_spanned(src);

        let failure_ordering_local = {
            let bb = self.make_bb_for_atomic_ordering(failure_ordering);
            additional_blocks.extend(bb.0);
            bb.1
        };

        self.add_bb_for_atomic_intrinsic_call_with_ptr(
            sym::intrinsics::atomic::intrinsic_atomic_cxchg,
            vec![
                operand::move_for_local(failure_ordering_local),
                operand::const_from_bool(self.tcx(), weak),
                operand::move_for_local(old_ref.into()),
                operand::move_for_local(src_ref.into()),
                operand::move_for_local(self.dest_ref().into()),
            ],
            additional_blocks,
        )
    }

    fn binary_op(&mut self, operator: AtomicBinaryOp, src: &Spanned<Operand<'tcx>>)
    where
        Self: AssignmentInfoProvider,
    {
        let tcx = self.tcx();
        let mut additional_blocks = vec![];
        let src_ref = self.reference_operand_spanned(src);

        let operator_local = {
            let (block, local) = self.make_bb_for_helper_call_with_all(
                self.pri_helper_funcs().const_atomic_binary_op_of,
                vec![],
                vec![operand::const_from_uint(tcx, operator.to_raw())],
                Default::default(),
            );
            additional_blocks.push(block);
            local
        };

        let prev_dest = self.dest_ref();

        self.add_bb_for_atomic_intrinsic_call_with_ptr(
            sym::intrinsics::atomic::intrinsic_atomic_binary_op,
            vec![
                operand::move_for_local(operator_local),
                operand::move_for_local(src_ref.into()),
                operand::move_for_local(prev_dest.into()),
            ],
            additional_blocks,
        )
    }

    fn fence(&mut self, single_thread: bool) {
        self.add_bb_for_atomic_intrinsic_call(
            sym::intrinsics::atomic::intrinsic_atomic_fence,
            vec![operand::const_from_bool(self.tcx(), single_thread)],
            Default::default(),
            Default::default(),
        );
    }
}

impl<'tcx, C> RuntimeCallAdder<C>
where
    C: ForAtomicIntrinsic<'tcx>,
{
    fn add_bb_for_atomic_intrinsic_call_with_ptr(
        &mut self,
        func: LeafAtomicIntrinsicSymbol,
        additional_args: Vec<Operand<'tcx>>,
        additional_blocks: Vec<BasicBlockData<'tcx>>,
    ) where
        Self: AssignmentInfoProvider,
    {
        let mut stmts = Vec::new();
        let mut blocks = additional_blocks;

        let PointerParamInstrPack {
            type_id_block,
            conc_ptr_stmts,
            pri_args: [ptr_ref, ptr_value, ptr_type_id],
        } = self.reference_ptr_param();
        stmts.extend(conc_ptr_stmts);
        blocks.push(type_id_block);

        self.add_bb_for_atomic_intrinsic_call(
            func,
            vec![
                vec![
                    operand::const_from_uint(self.tcx(), self.assignment_id()),
                    ptr_ref,
                    ptr_value,
                    ptr_type_id,
                ],
                additional_args,
            ]
            .concat(),
            stmts,
            blocks,
        );
    }

    fn add_bb_for_atomic_intrinsic_call(
        &mut self,
        func: LeafAtomicIntrinsicSymbol,
        additional_args: Vec<Operand<'tcx>>,
        additional_stmts: Vec<Statement<'tcx>>,
        additional_blocks: Vec<BasicBlockData<'tcx>>,
    ) {
        let mut blocks = additional_blocks;

        let ordering_local = {
            let bb = self.make_bb_for_atomic_ordering(self.context.ordering());
            blocks.extend(bb.0);
            bb.1
        };

        let mut block = self.make_bb_for_call(
            **func,
            [
                vec![operand::move_for_local(ordering_local)],
                additional_args,
            ]
            .concat(),
        );
        block.statements.extend(additional_stmts);
        blocks.push(block);

        self.insert_blocks(blocks);
    }
}

pub(super) struct PointerParamInstrPack<'tcx> {
    pub type_id_block: BasicBlockData<'tcx>,
    pub conc_ptr_stmts: Vec<Statement<'tcx>>,
    pub pri_args: [Operand<'tcx>; 3],
}

impl<'tcx, C> RuntimeCallAdder<C>
where
    Self: MirCallAdder<'tcx>,
    C: Basic<'tcx>,
{
    fn reference_ptr_param(&mut self) -> PointerParamInstrPack<'tcx>
    where
        C: ForOperandRef<'tcx> + PointerParamProvider<'tcx>,
    {
        self.reference_and_pack_ptr_operand(&self.context.ptr_operand().clone())
    }

    /// References a pointer operand and makes the three arguments passed to PRI:
    /// the operand reference, the concrete pointer value, and the pointer type id.
    ///
    /// # Returns
    /// The type id block, the concrete pointer assignment, and the three arguments as operand array.
    pub(super) fn reference_and_pack_ptr_operand(
        &mut self,
        ptr_operand: &Spanned<Operand<'tcx>>,
    ) -> PointerParamInstrPack<'tcx>
    where
        C: ForOperandRef<'tcx>,
    {
        let operand_ref = self.reference_operand_spanned(ptr_operand);
        let ptr_value = ptr_operand.node.to_copy();
        let ptr_ty = ptr_operand.node.ty(self, self.tcx());
        let (conc_ptr_stmts, conc_ptr_local) = self.make_conc_ptr_assignment(ptr_value);

        let (ptr_type_id_block, ptr_type_id_local) = self.make_type_id_of_bb(ptr_ty);

        PointerParamInstrPack {
            type_id_block: ptr_type_id_block,
            conc_ptr_stmts,
            pri_args: [
                operand::move_for_local(operand_ref.into()),
                operand::move_for_local(conc_ptr_local),
                operand::move_for_local(ptr_type_id_local),
            ],
        }
    }

    fn make_conc_ptr_assignment(
        &mut self,
        ptr_or_ref_value: Operand<'tcx>,
    ) -> (Vec<Statement<'tcx>>, Local) {
        let mut stmts = Vec::new();
        let tcx = self.tcx();

        let ty = ptr_or_ref_value.ty(self, tcx);
        assert!(ty.is_raw_ptr() || ty.is_ref());
        let ptr_ty = if ty.is_raw_ptr() {
            ty
        } else {
            Ty::new_imm_ptr(tcx, ty.peel_refs())
        };

        let ptr_or_ref_place = if let Some(place) = ptr_or_ref_value.place() {
            place
        } else {
            // let a = ptr_or_ref_value;
            let ptr_or_ref_local = self.add_local(ty);
            stmts.push(assignment::create(
                Place::from(ptr_or_ref_local),
                Rvalue::Use(ptr_or_ref_value, rustc_middle::mir::WithRetag::No),
            ));
            Place::from(ptr_or_ref_local)
        };

        // let b: *const T = &raw (*a);
        let raw_ptr_local = self.add_local(ptr_ty);
        stmts.push(assignment::create(
            Place::from(raw_ptr_local),
            Rvalue::RawPtr(
                rustc_middle::mir::RawPtrKind::Const,
                ptr_or_ref_place.project_deeper(&[ProjectionElem::Deref], tcx),
            ),
        ));

        // let c: *const () = b as *const ();
        let raw_addr_ty = Ty::new_imm_ptr(tcx, tcx.types.unit);
        let raw_addr_local = self.add_local(raw_addr_ty);
        stmts.push(assignment::create(
            Place::from(raw_addr_local),
            rvalue::cast_ptr_to_ptr(operand::move_for_local(raw_ptr_local), raw_addr_ty),
        ));

        (stmts, raw_addr_local)
    }
}

impl<'tcx, C> RuntimeCallAdder<C>
where
    Self: MirCallAdder<'tcx>,
    C: Basic<'tcx>,
{
    fn make_bb_for_atomic_ordering(&mut self, ordering: AtomicOrdering) -> BlocksAndResult<'tcx> {
        let tcx = self.tcx();

        self.make_bb_for_helper_call_with_all(
            self.pri_helper_funcs().const_atomic_ord_of,
            vec![],
            vec![operand::const_from_uint(tcx, ordering.to_raw())],
            Default::default(),
        )
        .into()
    }
}

mod utils {
    pub(super) use super::super::utils::{assignment, rvalue};
}
use utils::*;
