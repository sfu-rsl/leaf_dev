use std::{cell::RefCell, collections::HashMap, marker::PhantomData};

use crate::visit::{self, TerminatorKindMutVisitor};
use rustc_ast::Mutability;
use rustc_index::vec::{Idx, IndexVec};
use rustc_middle::{
    mir::{
        BasicBlock, BasicBlockData, Body, Local, LocalDecl, LocalDecls, Operand, Place, SourceInfo,
        Terminator,
    },
    ty::Ty,
};
use rustc_span::Span;

pub const NEXT_BLOCK: BasicBlock = BasicBlock::MAX;

struct NewBasicBlock<'tcx> {
    pseudo_index: BasicBlock,
    data: BasicBlockData<'tcx>,
    is_sticky: bool,
}

pub struct BodyModificationUnit<'tcx> {
    next_local_index: Local,
    new_locals: Vec<NewLocalDecl<'tcx>>,
    // new_blocks_before maps BasicBlocks from MIR already in the AST to a list of new basic blocks
    // we'll insert just before it.
    new_blocks_before: HashMap<BasicBlock, Vec<NewBasicBlock<'tcx>>>,
    new_blocks_before_count: u32, // the sum of lengths of Vectors in its Hashmap
    new_blocks_after: HashMap<BasicBlock, Vec<NewBasicBlock<'tcx>>>,
    new_blocks_after_count: u32,
    jump_modifications:
        HashMap<BasicBlock, Vec<(BasicBlock, JumpModificationConstraint, BasicBlock)>>,
}

impl<'tcx> BodyModificationUnit<'tcx> {
    pub fn new(nex_local_index: Local) -> Self {
        Self {
            next_local_index: nex_local_index,
            new_locals: Vec::new(),
            new_blocks_before: HashMap::new(),
            new_blocks_before_count: 0,
            new_blocks_after: HashMap::new(),
            new_blocks_after_count: 0,
            jump_modifications: HashMap::new(),
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

pub trait BodyLocalManager<'tcx> {
    fn add_local<T>(&mut self, decl_info: T) -> Local
    where
        T: Into<NewLocalDecl<'tcx>>;
}

pub trait BodyBlockManager<'tcx> {
    fn insert_blocks_before<I>(
        &mut self,
        index: BasicBlock,
        blocks: I,
        sticky: bool,
    ) -> Vec<BasicBlock>
    where
        I: IntoIterator<Item = BasicBlockData<'tcx>>;

    fn insert_block_after(&mut self, index: BasicBlock, block: BasicBlockData<'tcx>) -> BasicBlock;
}

pub trait JumpTargetModifier {
    fn modify_jump_target(
        &mut self,
        terminator_location: BasicBlock,
        from: BasicBlock,
        to: BasicBlock,
    ) {
        self.modify_jump_target_where(
            terminator_location,
            from,
            to,
            JumpModificationConstraint::None,
        )
    }

    fn modify_jump_target_where(
        &mut self,
        terminator_location: BasicBlock,
        from: BasicBlock,
        to: BasicBlock,
        constraint: JumpModificationConstraint,
    );
}

#[derive(PartialEq, Eq)]
pub enum JumpModificationConstraint {
    None,
    SwitchValue(u128),
    SwitchOtherwise,
}

type JumpTargetAttribute = JumpModificationConstraint;

impl JumpModificationConstraint {
    /// Checks if this constraint satisfies a target situation.
    /// Returns `None` if the target is not consistent with this constraint;
    /// otherwise, a number representing the satisfaction score is returned.
    /// For example, an exact match will get a `MAX` score while the most general
    /// constraint (`None`) will get a 0 score.
    /// In simpler words, if self is less constraining it is considered more
    /// general and satisfying for a target.
    fn sat_score(&self, target: &JumpTargetAttribute) -> Option<u32> {
        match self {
            JumpModificationConstraint::None => Some(0),
            _ => self.eq(target).then_some(u32::MAX),
        }
    }
}

impl<'tcx> BodyLocalManager<'tcx> for BodyModificationUnit<'tcx> {
    fn add_local<T>(&mut self, decl_info: T) -> Local
    where
        T: Into<NewLocalDecl<'tcx>>,
    {
        self.new_locals.push(decl_info.into());
        self.next_local_index + (self.new_locals.len() - 1)
    }
}

impl<'tcx> BodyBlockManager<'tcx> for BodyModificationUnit<'tcx> {
    fn insert_blocks_before<I>(
        &mut self,
        index: BasicBlock,
        blocks: I,
        sticky: bool,
    ) -> Vec<BasicBlock>
    where
        I: IntoIterator<Item = BasicBlockData<'tcx>>,
    {
        let chunk = self.new_blocks_before.entry(index).or_insert_with(Vec::new);
        let block_count: u32 = {
            let starting_count = chunk.len();
            // Associating temporary indices to the new blocks, so they can be referenced if needed.
            chunk.extend(blocks.into_iter().enumerate().map(|(i, b)| NewBasicBlock {
                pseudo_index: BasicBlock::from(
                    BasicBlock::MAX_AS_U32 - 1 - self.new_blocks_before_count - i as u32,
                ),
                data: b,
                is_sticky: sticky,
            }));
            (chunk.len() - starting_count).try_into().unwrap()
        };
        self.new_blocks_before_count += block_count;
        Vec::from_iter(
            chunk[(chunk.len() - block_count as usize)..]
                .iter()
                .map(|nbb| nbb.pseudo_index),
        )
    }

    // TODO: either raise error or add support for when the index bb has two or more targets
    // TODO: update block to blocks & add support for vector inputs
    // blocks will be inserted directly after the block & in a row
    fn insert_block_after(&mut self, index: BasicBlock, block: BasicBlockData<'tcx>) -> BasicBlock {
        // TODO: fill this out with all the blocks & turn into its own function?
        let maybe_target = match block.terminator.as_ref().unwrap().kind {
            rustc_middle::mir::TerminatorKind::Call {
                func: _,
                args: _,
                destination: _,
                target,
                cleanup: _,
                from_hir_call: _,
                fn_span: _,
            } => target,
            _ => None,
        };

        let chunk = self.new_blocks_after.entry(index).or_insert_with(Vec::new);
        let new_block_count: u32 = {
            let starting_count = chunk.len();
            // Associating temporary indices to the new blocks, so they can be referenced if needed.
            chunk.push(NewBasicBlock {
                pseudo_index: BasicBlock::from(
                    BasicBlock::MAX_AS_U32 - 1 - self.new_blocks_after_count,
                ),
                data: block,
                is_sticky: false, // sticky means nothing in this context
            });
            (chunk.len() - starting_count).try_into().unwrap()
        };
        let pseudo_index = chunk[chunk.len() - 1].pseudo_index;

        if let Some(target) = maybe_target {
            self.jump_modifications
                .entry(index)
                .or_insert_with(|| Vec::with_capacity(1))
                .push((target, JumpModificationConstraint::None, pseudo_index));
        } else {
            todo!("should the lack of existance of a target be an error or not?")
        }

        self.new_blocks_after_count += new_block_count;
        pseudo_index
    }
}

impl JumpTargetModifier for BodyModificationUnit<'_> {
    fn modify_jump_target_where(
        &mut self,
        terminator_location: BasicBlock,
        from: BasicBlock,
        to: BasicBlock,
        constraint: JumpModificationConstraint,
    ) {
        if from == to {
            log::warn!(
                "Ignoring modification of jump target to the same index. from == to == {:?}",
                from
            );
            return;
        }

        self.jump_modifications
            .entry(terminator_location)
            .or_insert_with(|| Vec::with_capacity(1))
            .push((from, constraint, to));
    }
}

impl<'tcx> BodyModificationUnit<'tcx> {
    // No blocks actually get added to the MIR of the current body until this function gets called.
    pub fn commit(mut self, body: &mut Body<'tcx>) {
        Self::add_new_locals(&mut body.local_decls, self.new_locals);

        // this function applies any jump modifications to terminators of blocks as specified
        Self::update_jumps_pre_insert(
            Iterator::chain(
                body.basic_blocks_mut().iter_enumerated_mut(),
                self.new_blocks_before
                    .values_mut()
                    .flatten()
                    .map(|p| (p.pseudo_index, &mut p.data)),
            ),
            &self.jump_modifications,
        );

        if !self.new_blocks_before.is_empty() {
            let index_mapping = Self::insert_new_blocks(
                body.basic_blocks_mut(),
                self.new_blocks_before,
                self.new_blocks_after,
            );
            Self::update_jumps_post_insert(body.basic_blocks_mut(), index_mapping);
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

    fn insert_new_blocks(
        blocks: &mut IndexVec<BasicBlock, BasicBlockData<'tcx>>,
        new_blocks_before: impl IntoIterator<Item = (BasicBlock, Vec<NewBasicBlock<'tcx>>)>,
        new_blocks_after: impl IntoIterator<Item = (BasicBlock, Vec<NewBasicBlock<'tcx>>)>,
    ) -> HashMap<BasicBlock, BasicBlock> {
        let mut index_mapping = HashMap::<BasicBlock, BasicBlock>::new();
        let mut push = |blocks: &mut IndexVec<BasicBlock, BasicBlockData<'tcx>>,
                        block: BasicBlockData<'tcx>,
                        before_index: BasicBlock,
                        after_index: Option<BasicBlock>| {
            let new_index = blocks.push(block);
            let new_index = after_index.unwrap_or(new_index);
            if new_index != before_index {
                index_mapping.insert(before_index, new_index);
            }
        };

        let current_blocks = Vec::from_iter(blocks.drain(..));

        let mut new_blocks_before = {
            let mut new_blocks_before = Vec::from_iter(new_blocks_before);
            new_blocks_before.sort_by_key(|p| p.0);
            new_blocks_before.into_iter().peekable()
        };
        let mut new_blocks_after = {
            let mut new_blocks_after = Vec::from_iter(new_blocks_after);
            new_blocks_after.sort_by_key(|p| p.0);
            new_blocks_after.into_iter().peekable()
        };

        for (i, block) in current_blocks.into_iter().enumerate() {
            let i = BasicBlock::from(i);
            let mut top_index = blocks.next_index();

            if new_blocks_before
                .peek()
                .is_some_and(|(index, _)| *index == i)
            {
                let (_, mut chunk) = new_blocks_before.next().unwrap();
                blocks.extend_reserve(chunk.len());
                for non_sticky in chunk.drain_filter(|b| !b.is_sticky) {
                    push(blocks, non_sticky.data, non_sticky.pseudo_index, None);
                }
                top_index = blocks.next_index();
                for sticky in chunk.drain_filter(|b| b.is_sticky) {
                    push(blocks, sticky.data, sticky.pseudo_index, None);
                }
                debug_assert!(chunk.is_empty());
            }

            // after_index is the new place that any jumps will target instead of i
            push(blocks, block, i, Some(top_index));

            // TODO: double check that this works as expected when more than one block is added
            if new_blocks_after
                .peek()
                .is_some_and(|(index, _)| *index == i)
            {
                let (_, mut chunk) = new_blocks_after.next().unwrap();
                blocks.extend_reserve(chunk.len());
                for bb in chunk.drain(..) {
                    push(blocks, bb.data, bb.pseudo_index, None);
                }
            }
        }

        // We only consider the insertion of blocks before the last block in this body (usually a return)
        assert!(
            new_blocks_before.peek().is_none() && new_blocks_after.peek().is_none(),
            "Found unexpected blocks that would be inserted after the last basic block"
        );

        index_mapping
    }

    fn update_jumps_pre_insert<'b>(
        blocks: impl Iterator<Item = (BasicBlock, &'b mut BasicBlockData<'tcx>)>,
        jump_modifications: &HashMap<
            BasicBlock,
            Vec<(BasicBlock, JumpModificationConstraint, BasicBlock)>,
        >,
    ) where
        'tcx: 'b,
    {
        let blocks = blocks.filter(|(i, _)| jump_modifications.contains_key(i));

        Self::update_jumps(
            blocks,
            |i, target, attr| {
                jump_modifications
                    .get(&i)
                    .unwrap()
                    .iter()
                    .filter(|(from, _, _)| *from == target)
                    .filter_map(|(_, c, to)| c.sat_score(attr).map(|s| (s, to)))
                    .max_by_key(|(score, _)| *score)
                    .map(|(_, to)| to)
                    .cloned()
            },
            false,
            |i, c| jump_modifications.get(&i).unwrap().len() == c,
            true,
        );
    }

    fn update_jumps_post_insert(
        blocks: &mut IndexVec<BasicBlock, BasicBlockData<'tcx>>,
        index_mapping: HashMap<BasicBlock, BasicBlock>,
    ) {
        Self::update_jumps(
            blocks.iter_enumerated_mut(),
            |_, target, _| index_mapping.get(&target).cloned(),
            true,
            |_, _| true,
            false,
        );
    }

    fn update_jumps<'b, 'm>(
        blocks: impl Iterator<Item = (BasicBlock, &'b mut BasicBlockData<'tcx>)>,
        index_mapping: impl Fn(BasicBlock, BasicBlock, &JumpTargetAttribute) -> Option<BasicBlock>,
        update_next: bool,
        sanity_check: impl Fn(BasicBlock, usize) -> bool,
        recursive: bool,
    ) where
        'tcx: 'b,
    {
        let index_rc = RefCell::new(BasicBlock::from(0_u32));
        let map = |target: BasicBlock, attr: &JumpTargetAttribute| -> Option<BasicBlock> {
            if update_next && target == NEXT_BLOCK {
                Some(*index_rc.borrow() + 1)
            } else {
                index_mapping(*index_rc.borrow(), target, attr)
            }
        };
        let mut updater = JumpUpdater::new(Box::new(map), recursive);
        for (index, block) in blocks.filter(|(_, b)| b.terminator.is_some()) {
            *index_rc.borrow_mut() = index;
            let update_count = updater.update_terminator(block.terminator_mut());
            if !sanity_check(index, update_count) {
                panic!("Update count of {update_count} was not acceptable at index {index:?}");
            }
        }
    }
}

trait MapFunc: Fn(BasicBlock, &JumpModificationConstraint) -> Option<BasicBlock> {}
impl<T: Fn(BasicBlock, &JumpModificationConstraint) -> Option<BasicBlock>> MapFunc for T {}

struct JumpUpdater<'tcx, M>
where
    M: MapFunc,
{
    index_mapping: M,
    next_index: BasicBlock,
    count: usize,
    recursive: bool,
    phantom: PhantomData<&'tcx ()>,
}

impl<'tcx, M> JumpUpdater<'tcx, M>
where
    M: MapFunc,
{
    fn new(index_mapping: M, recursive: bool) -> Self {
        Self {
            index_mapping,
            next_index: NEXT_BLOCK,
            count: 0,
            recursive,
            phantom: PhantomData,
        }
    }
}

impl<'tcx, M> visit::TerminatorKindMutVisitor<'tcx, ()> for JumpUpdater<'tcx, M>
where
    M: MapFunc,
{
    fn visit_goto(&mut self, target: &mut BasicBlock) {
        self.update(target);
    }

    fn visit_switch_int(
        &mut self,
        _discr: &mut rustc_middle::mir::Operand<'tcx>,
        targets: &mut rustc_middle::mir::SwitchTargets,
    ) {
        // Because of API limitations we have to take this weird approach.
        let values: Vec<u128> = targets.iter().map(|(v, _)| v).collect();
        for (index, target) in targets.all_targets_mut().iter_mut().enumerate() {
            if index < values.len() {
                self.update_with_attr(
                    &mut *target,
                    JumpTargetAttribute::SwitchValue(values[index]),
                );
            } else {
                self.update_with_attr(&mut *target, JumpTargetAttribute::SwitchOtherwise);
            }
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
        _args: &mut [Operand<'tcx>],
        _destination: &mut Place<'tcx>,
        target: &mut Option<BasicBlock>,
        _cleanup: &mut Option<BasicBlock>,
        _from_hir_call: bool,
        _fn_span: Span,
    ) {
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
        _operands: &mut [rustc_middle::mir::InlineAsmOperand<'tcx>],
        _options: &mut rustc_ast::InlineAsmOptions,
        _line_spans: &'tcx [Span],
        destination: &mut Option<BasicBlock>,
        cleanup: &mut Option<BasicBlock>,
    ) {
        self.update_maybe(destination);
        self.update_maybe(cleanup);
    }
}

impl<'tcx, M> JumpUpdater<'tcx, M>
where
    M: MapFunc,
{
    pub fn update_terminator(&mut self, terminator: &mut Terminator<'tcx>) -> usize {
        self.count = 0;
        Self::visit_terminator_kind(self, &mut terminator.kind);
        self.count
    }

    fn update(&mut self, target: &mut BasicBlock) {
        self.update_with_attr(target, JumpTargetAttribute::None)
    }

    fn update_maybe(&mut self, target: &mut Option<BasicBlock>) {
        self.update_maybe_with_attr(target, JumpTargetAttribute::None)
    }

    fn update_with_attr(&mut self, target: &mut BasicBlock, target_attr: JumpTargetAttribute) {
        let new_index = (self.index_mapping)(*target, &target_attr);
        if let Some(new_index) = new_index {
            log::debug!("Updating jump target from {:?} to {:?}", target, new_index);
            *target = new_index;
            self.count += 1;
            if self.recursive {
                self.update(target);
            }
        }
    }

    fn update_maybe_with_attr(
        &mut self,
        target: &mut Option<BasicBlock>,
        target_attr: JumpTargetAttribute,
    ) {
        if let Some(t) = target.as_mut() {
            self.update_with_attr(t, target_attr);
        }
    }
}
