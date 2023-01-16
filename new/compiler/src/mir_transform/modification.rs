use std::{collections::HashMap, marker::PhantomData};

use crate::visit::{self, TerminatorKindMutVisitor};
use rustc_ast::Mutability;
use rustc_index::vec::IndexVec;
use rustc_middle::{
    mir::{
        BasicBlock, BasicBlockData, Body, Local, LocalDecl, LocalDecls, Operand, Place, SourceInfo,
        Terminator,
    },
    ty::Ty,
};
use rustc_span::Span;

pub const NEXT_BLOCK: BasicBlock = BasicBlock::MAX;

pub struct BodyModificationUnit<'tcx> {
    nex_local_index: Local,
    new_locals: Vec<NewLocalDecl<'tcx>>,
    new_blocks: HashMap<BasicBlock, Vec<(BasicBlock, BasicBlockData<'tcx>)>>,
    new_block_count: u32,
}

impl<'tcx> BodyModificationUnit<'tcx> {
    pub fn new(nex_local_index: Local) -> Self {
        Self {
            nex_local_index: nex_local_index,
            new_locals: Vec::new(),
            new_blocks: HashMap::new(),
            new_block_count: 0,
        }
    }
}

pub struct NewLocalDecl<'tcx>(LocalDecl<'tcx>);

impl<'tcx> From<LocalDecl<'tcx>> for NewLocalDecl<'tcx> {
    fn from(value: LocalDecl<'tcx>) -> Self {
        NewLocalDecl(value)
    }
}

impl<'tcx> From<(Mutability, Ty<'tcx>, SourceInfo)> for NewLocalDecl<'tcx> {
    fn from(value: (Mutability, Ty<'tcx>, SourceInfo)) -> Self {
        LocalDecl {
            mutability: value.0,
            local_info: None,
            internal: true,
            is_block_tail: None,
            ty: value.1,
            user_ty: None,
            source_info: value.2,
        }
        .into()
    }
}

impl<'tcx> From<Ty<'tcx>> for NewLocalDecl<'tcx> {
    fn from(value: Ty<'tcx>) -> Self {
        (
            Mutability::Not,
            value,
            SourceInfo::outermost(rustc_span::DUMMY_SP),
        )
            .into()
    }
}

impl<'tcx> BodyModificationUnit<'tcx> {
    pub fn add_local<T>(&mut self, decl_info: T) -> Local
    where
        T: Into<NewLocalDecl<'tcx>>,
    {
        self.new_locals.push(decl_info.into());
        Local::from(self.nex_local_index + (self.new_locals.len() - 1))
    }
}

impl<'tcx> BodyModificationUnit<'tcx> {
    pub fn insert_blocks_before<I>(&mut self, index: BasicBlock, blocks: I) -> Vec<BasicBlock>
    where
        I: IntoIterator<Item = BasicBlockData<'tcx>>,
    {
        let chunk = self.new_blocks.entry(index).or_insert_with(Vec::new);
        let block_count: u32 = {
            let count_before = chunk.len();
            // Associating temporary indices to the new blocks, so they can be referenced if needed.
            chunk.extend(blocks.into_iter().enumerate().map(|(i, b)| {
                (
                    BasicBlock::from(BasicBlock::MAX_AS_U32 - 1 - self.new_block_count - i as u32),
                    b,
                )
            }));
            (chunk.len() - count_before).try_into().unwrap()
        };
        self.new_block_count += block_count;
        Vec::from_iter(
            chunk[(chunk.len() - block_count as usize)..]
                .iter()
                .map(|x| x.0),
        )
    }
}

impl<'tcx> BodyModificationUnit<'tcx> {
    pub fn commit(self, body: &mut Body<'tcx>) {
        Self::add_new_locals(&mut body.local_decls, self.new_locals);

        if !self.new_blocks.is_empty() {
            let index_mapping = Self::insert_new_blocks(body.basic_blocks_mut(), self.new_blocks);
            Self::update_jumps(body.basic_blocks_mut(), index_mapping);
        }
    }

    fn add_new_locals(locals: &mut LocalDecls<'tcx>, new_locals: Vec<NewLocalDecl<'tcx>>) {
        let first_index = locals.len();
        for (i, local) in new_locals.into_iter().enumerate() {
            let index = locals.push(local.0);
            // Asserting that the indices that we have given are correct.
            assert_eq!(index, (i + first_index).into());
        }
    }

    fn insert_new_blocks<I>(
        blocks: &mut IndexVec<BasicBlock, BasicBlockData<'tcx>>,
        new_blocks: I,
    ) -> HashMap<BasicBlock, BasicBlock>
    where
        I: IntoIterator<Item = (BasicBlock, Vec<(BasicBlock, BasicBlockData<'tcx>)>)>,
    {
        let mut index_mapping = HashMap::<BasicBlock, BasicBlock>::new();
        let mut push = |blocks: &mut IndexVec<BasicBlock, BasicBlockData<'tcx>>,
                        block: BasicBlockData<'tcx>,
                        current_index: BasicBlock| {
            let new_index = blocks.push(block);
            if new_index != current_index {
                index_mapping.insert(current_index, new_index);
            }
        };

        let current_blocks = Vec::from_iter(blocks.drain(..));

        let mut new_blocks = Vec::from_iter(new_blocks);
        new_blocks.sort_by_key(|p| p.0);
        let mut new_blocks = new_blocks.into_iter().peekable();

        for (i, block) in current_blocks.into_iter().enumerate() {
            let i = BasicBlock::from(i);
            if new_blocks.peek().is_some_and(|(index, _)| *index == i) {
                let (_, chunk) = new_blocks.next().unwrap();
                blocks.extend_reserve(chunk.len());
                for (pseudo_index, block) in chunk {
                    push(blocks, block, pseudo_index);
                }
            }

            push(blocks, block, i);
        }

        // Currently, we only consider insertion of blocks before original blocks.
        assert!(new_blocks.peek().is_none());

        index_mapping
    }

    fn update_jumps(
        blocks: &mut IndexVec<BasicBlock, BasicBlockData<'tcx>>,
        index_mapping: HashMap<BasicBlock, BasicBlock>,
    ) {
        let mut updater = JumpUpdater {
            index_mapping: index_mapping,
            next_index: NEXT_BLOCK,
            phantom: PhantomData,
        };
        for (index, block) in blocks
            .iter_enumerated_mut()
            .filter(|(_, b)| b.terminator.is_some())
        {
            updater.next_index = index + 1;
            updater.update_terminator(block.terminator_mut());
        }
    }
}

struct JumpUpdater<'tcx> {
    index_mapping: HashMap<BasicBlock, BasicBlock>,
    next_index: BasicBlock,
    phantom: PhantomData<&'tcx ()>,
}

impl<'tcx> visit::TerminatorKindMutVisitor<'tcx, ()> for JumpUpdater<'tcx> {
    fn visit_goto(&mut self, target: &mut BasicBlock) {
        self.update(target);
    }

    fn visit_switch_int(
        &mut self,
        _discr: &mut rustc_middle::mir::Operand<'tcx>,
        targets: &mut rustc_middle::mir::SwitchTargets,
    ) {
        for target in targets.all_targets_mut() {
            self.update(&mut *target);
        }
    }

    fn visit_drop(
        &mut self,
        _place: &mut rustc_middle::mir::Place<'tcx>,
        target: &mut BasicBlock,
        unwind: &mut Option<BasicBlock>,
    ) {
        self.update(target);
        self.update_maybe(unwind);
    }

    fn visit_drop_and_replace(
        &mut self,
        _place: &mut rustc_middle::mir::Place<'tcx>,
        _value: &mut rustc_middle::mir::Operand<'tcx>,
        target: &mut BasicBlock,
        unwind: &mut Option<BasicBlock>,
    ) {
        self.update(target);
        self.update_maybe(unwind);
    }

    fn visit_call(
        &mut self,
        _func: &mut Operand<'tcx>,
        _args: &mut Vec<Operand<'tcx>>,
        _destination: &mut Place<'tcx>,
        target: &mut Option<BasicBlock>,
        _cleanup: &mut Option<BasicBlock>,
        _from_hir_call: bool,
        _fn_span: Span,
    ) -> () {
        self.update_maybe(target);
    }

    fn visit_assert(
        &mut self,
        _cond: &mut rustc_middle::mir::Operand<'tcx>,
        _expected: &mut bool,
        _msg: &mut rustc_middle::mir::AssertMessage<'tcx>,
        target: &mut BasicBlock,
        cleanup: &mut Option<BasicBlock>,
    ) {
        self.update(target);
        self.update_maybe(cleanup);
    }

    fn visit_yield(
        &mut self,
        _value: &mut rustc_middle::mir::Operand<'tcx>,
        resume: &mut BasicBlock,
        _resume_arg: &mut rustc_middle::mir::Place<'tcx>,
        drop: &mut Option<BasicBlock>,
    ) {
        self.update(resume);
        self.update_maybe(drop);
    }

    fn visit_false_edge(
        &mut self,
        real_target: &mut BasicBlock,
        imaginary_target: &mut BasicBlock,
    ) {
        self.update(real_target);
        self.update(imaginary_target);
    }

    fn visit_false_unwind(
        &mut self,
        real_target: &mut BasicBlock,
        unwind: &mut Option<BasicBlock>,
    ) {
        self.update(real_target);
        self.update_maybe(unwind);
    }

    fn visit_inline_asm(
        &mut self,
        _template: &mut &[rustc_ast::InlineAsmTemplatePiece],
        _operands: &mut Vec<rustc_middle::mir::InlineAsmOperand<'tcx>>,
        _options: &mut rustc_ast::InlineAsmOptions,
        _line_spans: &'tcx [Span],
        destination: &mut Option<BasicBlock>,
        cleanup: &mut Option<BasicBlock>,
    ) {
        self.update_maybe(destination);
        self.update_maybe(cleanup);
    }
}

impl<'tcx> JumpUpdater<'tcx> {
    pub fn update_terminator(&mut self, terminator: &mut Terminator<'tcx>) {
        Self::visit_terminator_kind(self, &mut terminator.kind)
    }

    fn update(&self, target: &mut BasicBlock) {
        log::debug!("Updating jump target from {:?}", target);
        if *target == NEXT_BLOCK {
            *target = self.next_index;
        } else if let Some(new_index) = self.index_mapping.get(target) {
            *target = *new_index;
        }
        log::debug!("Updated jump target to {:?}", target);
    }

    fn update_maybe(&self, target: &mut Option<BasicBlock>) {
        if let Some(t) = target.as_mut() {
            self.update(t);
        }
    }
}
