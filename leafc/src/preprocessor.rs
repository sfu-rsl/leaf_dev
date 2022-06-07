extern crate rustc_middle;
extern crate rustc_span;

use rustc_middle::mir::{terminator::TerminatorKind::*, BasicBlock, BasicBlockData, Body};
use std::{collections::HashMap, ops::IndexMut};

pub fn clear_and_get_reverse<'tcx>(body: &mut Body<'tcx>) -> Vec<BasicBlockData<'tcx>> {
    let basic_blocks = body.basic_blocks_mut();
    let mut reverse = vec![];
    while let Some(basic_block) = basic_blocks.pop() {
        reverse.push(basic_block);
    }

    reverse
}

pub fn repopulate_basic_blocks<'tcx>(
    body: &mut Body<'tcx>,
    reverse: &mut Vec<BasicBlockData<'tcx>>,
) -> Vec<usize> {
    let mut index_counts = vec![];
    while let Some(basic_block) = reverse.pop() {
        create_new_blocks(body, basic_block, &mut index_counts);
    }

    index_counts
}

pub fn create_new_blocks<'tcx>(
    body: &mut Body<'tcx>,
    mut basic_block: BasicBlockData<'tcx>,
    index_counts: &mut Vec<usize>,
) {
    let mut new_basic_blocks = vec![];

    // # of statements + block for teminator call + terminator
    // We do this before we pop basic_block.statements.
    index_counts.push(basic_block.statements.len() + 2);

    // Create a new block with the original terminator and no statements.
    let mut new_basic_block = BasicBlockData::new(basic_block.terminator);
    new_basic_block.is_cleanup = basic_block.is_cleanup;
    new_basic_blocks.push(new_basic_block);

    // Create a new block that will include a leafrt call for the terminator
    new_basic_blocks.push(BasicBlockData::new(None));

    while let Some(statement) = basic_block.statements.pop() {
        // Process from the last statement in the block
        let mut new_basic_block = BasicBlockData::new(None);
        new_basic_block.statements = Vec::from([statement]);
        new_basic_blocks.push(new_basic_block);
    }

    let basic_blocks = body.basic_blocks_mut();
    while let Some(new_basic_block) = new_basic_blocks.pop() {
        basic_blocks.push(new_basic_block);
    }
}

pub fn create_index_mappings<'tcx>(index_counts: &Vec<usize>) -> HashMap<BasicBlock, BasicBlock> {
    let mut index_map = HashMap::<BasicBlock, BasicBlock>::new();
    let mut origin_index: usize = 0;
    let mut new_index = 0;

    for cnt in index_counts {
        // At this point, index_counts contains a list of how many basic blocks replace each of
        // the original basic blocks
        index_map.insert(
            BasicBlock::from_usize(origin_index),
            BasicBlock::from_usize(new_index),
        );
        origin_index += 1;
        new_index += cnt;
    }

    index_map
}

pub fn remap_jump_targets<'tcx>(
    body: &mut Body<'tcx>,
    basic_block_idx: BasicBlock,
    index_map: &HashMap<BasicBlock, BasicBlock>,
) {
    // Adjust jump targets
    match &mut body.index_mut(basic_block_idx).terminator_mut().kind {
        Goto { target } => {
            // target has to exist in index_map
            *target = index_map.get(&target).unwrap().to_owned();
        }
        SwitchInt {
            discr: _,     //: Operand<'tcx>,
            switch_ty: _, //: Ty<'tcx>,
            targets,      //: SwitchTargets,
        } => {
            for target in targets.all_targets_mut() {
                *target = index_map.get(&target).unwrap().to_owned();
            }
        }
        ////Resume,
        ////Abort,
        ////Return,
        ////Unreachable,
        Drop {
            place: _, //: Place<'tcx>,
            target,   //: BasicBlock,
            unwind,   //: Option<BasicBlock>,
        } => {
            *target = index_map.get(&target).unwrap().to_owned();
            *unwind = unwind.map(|idx| index_map.get(&idx).unwrap().to_owned());
        }
        DropAndReplace {
            place: _, //: Place<'tcx>,
            value: _, //: Operand<'tcx>,
            target,   //: BasicBlock,
            unwind,   //: Option<BasicBlock>,
        } => {
            *target = index_map.get(&target).unwrap().to_owned();
            *unwind = unwind.map(|idx| index_map.get(&idx).unwrap().to_owned());
        }
        Call {
            func: _,          //: Operand<'tcx>,
            args: _,          //: Vec<Operand<'tcx>>,
            destination,      //: Option<(Place<'tcx>, BasicBlock)>,
            cleanup,          //: Option<BasicBlock>,
            from_hir_call: _, //: bool,
            fn_span: _,       //: Span,
        } => {
            *destination =
                destination.map(|(place, idx)| (place, index_map.get(&idx).unwrap().to_owned()));
            *cleanup = cleanup.map(|idx| index_map.get(&idx).unwrap().to_owned());
        }
        Assert {
            cond: _,     //: Operand<'tcx>,
            expected: _, //: bool,
            msg: _,      //: AssertMessage<'tcx>,
            target,      //: BasicBlock,
            cleanup,     //: Option<BasicBlock>,
        } => {
            *target = index_map.get(&target).unwrap().to_owned();
            *cleanup = cleanup.map(|idx| index_map.get(&idx).unwrap().to_owned());
        }
        Yield {
            value: _,      //: Operand<'tcx>,
            resume,        //: BasicBlock,
            resume_arg: _, //: Place<'tcx>,
            drop,          //: Option<BasicBlock>,
        } => {
            *resume = index_map.get(&resume).unwrap().to_owned();
            *drop = drop.map(|idx| index_map.get(&idx).unwrap().to_owned());
        }
        //GeneratorDrop,
        FalseEdge {
            real_target,      //: BasicBlock,
            imaginary_target, //: BasicBlock,
        } => {
            *real_target = index_map.get(&real_target).unwrap().to_owned();
            *imaginary_target = index_map.get(&imaginary_target).unwrap().to_owned();
        }
        FalseUnwind {
            real_target, //: BasicBlock,
            unwind,      //: Option<BasicBlock>,
        } => {
            *real_target = index_map.get(&real_target).unwrap().to_owned();
            *unwind = unwind.map(|idx| index_map.get(&idx).unwrap().to_owned());
        }
        InlineAsm {
            template: _,   //: &'tcx [InlineAsmTemplatePiece],
            operands: _,   //: Vec<InlineAsmOperand<'tcx>>,
            options: _,    //: InlineAsmOptions,
            line_spans: _, //: &'tcx [Span],
            destination,   //: Option<BasicBlock>,
            cleanup,       //: Option<BasicBlock>,
        } => {
            *destination = destination.map(|idx| index_map.get(&idx).unwrap().to_owned());
            *cleanup = cleanup.map(|idx| index_map.get(&idx).unwrap().to_owned());
        }
        _ => (),
    }
}
