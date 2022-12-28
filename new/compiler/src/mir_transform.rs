use std::{collections::HashMap, marker::PhantomData};

use rustc_ast::Mutability;
use rustc_index::vec::IndexVec;
use rustc_middle::{
    mir::{
        BasicBlock, BasicBlockData, Body, HasLocalDecls, Local, LocalDecl, LocalDecls, SourceInfo,
        Terminator,
    },
    ty::Ty,
};
use rustc_span::Span;

use crate::visit::{self, TerminatorKindMutVisitor};

struct BodyModificationUnit<'tcx> {
    body: &'tcx mut Body<'tcx>,

    new_locals: Vec<NewLocalDecl<'tcx>>,
    new_blocks: HashMap<BasicBlock, Vec<(BasicBlock, BasicBlockData<'tcx>)>>,
    new_block_count: u32,
}

impl<'tcx> BodyModificationUnit<'tcx> {
    fn new(body: &'tcx mut Body<'tcx>) -> Self {
        Self {
            body: body,
            new_locals: Vec::new(),
            new_blocks: HashMap::new(),
            new_block_count: 0,
        }
    }
}

struct NewLocalDecl<'tcx>(LocalDecl<'tcx>);

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
    pub fn add_local<T>(&mut self, data: T) -> Local
    where
        T: Into<NewLocalDecl<'tcx>>,
    {
        self.new_locals.push(data.into());
        Local::from(self.body.local_decls().next_index() + (self.new_locals.len() - 1))
    }
}

impl<'tcx> BodyModificationUnit<'tcx> {
    pub fn insert_blocks_before<I>(&mut self, index: BasicBlock, blocks: I) -> Vec<BasicBlock>
    where
        I: IntoIterator<Item = BasicBlockData<'tcx>>,
    {
        if !self.new_blocks.contains_key(&index) {
            self.new_blocks.insert(index, Vec::new());
        }

        let chunk = self.new_blocks.get_mut(&index).unwrap();
        let block_count: u32 = {
            let count_before = chunk.len();
            // Associating temporary indices to the new blocks, so they can be referenced if needed.
            chunk.extend(blocks.into_iter().enumerate().map(|(i, b)| {
                (
                    BasicBlock::from(u32::MAX - self.new_block_count - i as u32),
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
    pub fn commit(self) {
        BodyModificationUnit::add_new_locals(&mut self.body.local_decls, self.new_locals);

        if !self.new_blocks.is_empty() {
            let index_mapping = BodyModificationUnit::insert_new_blocks(
                &mut self.body.basic_blocks_mut(),
                self.new_blocks,
            );
            BodyModificationUnit::update_jumps(
                self.body.basic_blocks_mut().iter_mut(),
                index_mapping,
            );
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

    fn update_jumps<I>(blocks: I, index_mapping: HashMap<BasicBlock, BasicBlock>)
    where
        I: Iterator<Item = &'tcx mut BasicBlockData<'tcx>>,
    {
        let mut updater = JumpUpdater {
            index_mapping: index_mapping,
            phantom: PhantomData,
        };
        for block in blocks.filter(|b| b.terminator.is_some()) {
            updater.update_terminator(block.terminator_mut());
        }
    }
}

struct JumpUpdater<'tcx> {
    index_mapping: HashMap<BasicBlock, BasicBlock>,
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
        TerminatorKindMutVisitor::<'tcx, ()>::visit_terminator_kind(self, &mut terminator.kind)
    }

    fn update(&self, target: &mut BasicBlock) {
        if let Some(new_index) = self.index_mapping.get(target) {
            *target = *new_index;
        }
    }

    fn update_maybe(&self, target: &mut Option<BasicBlock>) {
        if let Some(t) = target.as_mut() {
            self.update(t);
        }
    }
}
