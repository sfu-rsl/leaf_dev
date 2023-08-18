use std::collections::HashMap;

use rustc_middle::mir::{BasicBlock, BasicBlockData, Body, Statement, Terminator, TerminatorKind};

use super::update_jumps;

const NEXT_BLOCK: BasicBlock = super::NEXT_BLOCK;

pub(crate) fn split_blocks(body: &mut Body, predicate: impl Fn(&Statement) -> bool) {
    let blocks = body.basic_blocks_mut();
    let original_blocks = Vec::from_iter(blocks.drain(..));
    let mut jump_map = HashMap::new();
    for (i, block) in original_blocks.into_iter().enumerate() {
        jump_map.insert(BasicBlock::from(i), blocks.next_index());

        let new_blocks = block.split_with(&predicate);
        for block in new_blocks {
            blocks.push(block);
        }
    }

    update_jumps(
        blocks.iter_enumerated_mut(),
        |_, target, _| jump_map.get(&target).cloned(),
        true,
        None,
        false,
    );
}

pub(crate) trait BasicBlockDataSplitExt<'tcx> {
    fn split_with(self, predicate: impl Fn(&Statement) -> bool) -> Vec<BasicBlockData<'tcx>>;
}

impl<'tcx> BasicBlockDataSplitExt<'tcx> for BasicBlockData<'tcx> {
    fn split_with(self, predicate: impl Fn(&Statement) -> bool) -> Vec<BasicBlockData<'tcx>> {
        let mut result = Vec::new();

        let mut new_block = BasicBlockData::new(None);
        for statement in self.statements {
            new_block.statements.push(statement);
            let statement = new_block.statements.last().unwrap();
            if predicate(&statement) {
                new_block.terminator = Some(Terminator {
                    kind: TerminatorKind::Goto { target: NEXT_BLOCK },
                    source_info: (&statement.source_info).clone(),
                });
                result.push(new_block);
                new_block = BasicBlockData::new(None);
            }
        }

        new_block.terminator = self.terminator;
        result.push(new_block);

        result
    }
}
