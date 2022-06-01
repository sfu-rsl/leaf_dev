extern crate rustc_middle;
extern crate rustc_span;

use log::debug;
use rustc_middle::{
    mir::{self, terminator::TerminatorKind::*},
    ty,
};
use rustc_span::def_id;
use std::{collections, ops::IndexMut};

pub fn separate_consts<'tcx>(body: &mut mir::Body<'tcx>) {
    let (basic_blocks, local_decls) = body.basic_blocks_and_local_decls_mut();

    basic_blocks.iter_mut().for_each(|basic_block| {
        separate_consts_in_bb(basic_block, local_decls);
    });
}

pub fn separate_consts_in_bb<'tcx>(
    basic_block: &mut mir::BasicBlockData<'tcx>,
    local_decls: &mut mir::LocalDecls<'tcx>,
) {
    let mut new_statements: Vec<(usize, Vec<mir::Statement>)> = vec![];

    (0..basic_block.statements.len()).for_each(|i| {
        let kind = &mut basic_block.statements[i].kind;
        if let mir::StatementKind::Assign(b) = kind {
            let option = separate_consts_in_assign(b, local_decls);
            if let Some(statements_to_push) = option {
                new_statements.push((i, statements_to_push));
            }
        }
    });

    for (i, statements_to_push) in &mut new_statements.pop() {
        for statement in statements_to_push.pop() {
            basic_block.statements.insert(*i, statement);
        }
    }
}

pub fn separate_consts_in_assign<'tcx>(
    b: &mut Box<(mir::Place<'tcx>, mir::Rvalue<'tcx>)>,
    local_decls: &mut mir::LocalDecls<'tcx>,
) -> Option<Vec<mir::Statement<'tcx>>> {
    let (_, r) = &mut **b;
    match r {
        mir::Rvalue::Repeat(mir::Operand::Constant(c), co) => {
            debug!("Repeat");
            let new_local_decl_idx = insert_new_local_decl(**c, local_decls);
            let new_statement = get_new_statement(**c, new_local_decl_idx);
            b.1 = mir::Rvalue::Repeat(mir::Operand::Move(new_local_decl_idx.into()), *co);
            Some(vec![new_statement])
        }
        mir::Rvalue::Cast(ck, mir::Operand::Constant(c), t) => {
            debug!("Cast");
            let new_local_decl_idx = insert_new_local_decl(**c, local_decls);
            let new_statement = get_new_statement(**c, new_local_decl_idx);
            b.1 = mir::Rvalue::Cast(*ck, mir::Operand::Move(new_local_decl_idx.into()), *t);
            Some(vec![new_statement])
        }
        mir::Rvalue::BinaryOp(_, box (mir::Operand::Constant(_), _)) => {
            debug!("BinaryOp");
            let (new_local_decl_idx, new_statement) =
                if let mir::Rvalue::BinaryOp(_, box (mir::Operand::Constant(c), _)) = r {
                    let new_local_decl_idx = insert_new_local_decl(**c, local_decls);
                    (
                        new_local_decl_idx,
                        get_new_statement(**c, new_local_decl_idx),
                    )
                } else {
                    unreachable!();
                };
            if let mir::Rvalue::BinaryOp(_, ib) = r {
                ib.0 = mir::Operand::Move(new_local_decl_idx.into());
            } else {
                unreachable!();
            }
            Some(vec![new_statement])
        }
        mir::Rvalue::BinaryOp(_, box (_, mir::Operand::Constant(_))) => {
            debug!("BinaryOp");
            let (new_local_decl_idx, new_statement) =
                if let mir::Rvalue::BinaryOp(_, box (_, mir::Operand::Constant(c))) = r {
                    let new_local_decl_idx = insert_new_local_decl(**c, local_decls);
                    (
                        new_local_decl_idx,
                        get_new_statement(**c, new_local_decl_idx),
                    )
                } else {
                    unreachable!();
                };
            if let mir::Rvalue::BinaryOp(_, ib) = r {
                ib.1 = mir::Operand::Move(new_local_decl_idx.into());
            } else {
                unreachable!();
            }
            Some(vec![new_statement])
        }
        mir::Rvalue::CheckedBinaryOp(_, box (mir::Operand::Constant(_), _)) => {
            debug!("CheckedBinaryOp");
            let (new_local_decl_idx, new_statement) =
                if let mir::Rvalue::CheckedBinaryOp(_, box (mir::Operand::Constant(c), _)) = r {
                    let new_local_decl_idx = insert_new_local_decl(**c, local_decls);
                    (
                        new_local_decl_idx,
                        get_new_statement(**c, new_local_decl_idx),
                    )
                } else {
                    unreachable!();
                };
            if let mir::Rvalue::CheckedBinaryOp(_, ib) = r {
                ib.0 = mir::Operand::Move(new_local_decl_idx.into());
            } else {
                unreachable!();
            }
            Some(vec![new_statement])
        }
        mir::Rvalue::CheckedBinaryOp(_, box (_, mir::Operand::Constant(_))) => {
            debug!("CheckedBinaryOp");
            let (new_local_decl_idx, new_statement) =
                if let mir::Rvalue::CheckedBinaryOp(_, box (_, mir::Operand::Constant(c))) = r {
                    let new_local_decl_idx = insert_new_local_decl(**c, local_decls);
                    (
                        new_local_decl_idx,
                        get_new_statement(**c, new_local_decl_idx),
                    )
                } else {
                    unreachable!();
                };
            if let mir::Rvalue::CheckedBinaryOp(_, ib) = r {
                ib.1 = mir::Operand::Move(new_local_decl_idx.into());
            } else {
                unreachable!();
            }
            Some(vec![new_statement])
        }
        mir::Rvalue::UnaryOp(u, mir::Operand::Constant(c)) => {
            debug!("UnaryOp");
            let new_local_decl_idx = insert_new_local_decl(**c, local_decls);
            let new_statement = get_new_statement(**c, new_local_decl_idx);
            b.1 = mir::Rvalue::UnaryOp(*u, mir::Operand::Move(new_local_decl_idx.into()));
            Some(vec![new_statement])
        }
        mir::Rvalue::Aggregate(box mir::AggregateKind::Array(_), v) => {
            debug!("Aggregate");
            let mut new_statements = vec![];
            for i in 0..v.len() {
                let item = &v[i];
                if let mir::Operand::Constant(c) = item {
                    let new_local_decl_idx = insert_new_local_decl(**c, local_decls);
                    new_statements.push(get_new_statement(**c, new_local_decl_idx));
                    let new_op = mir::Operand::Move(new_local_decl_idx.into());
                    v.remove(i);
                    v.insert(i, new_op);
                }
            }
            Some(new_statements)
        }
        _ => {
            debug!("Not supported: {r:?}");
            None
        }
    }
}

pub fn insert_new_local_decl<'tcx>(
    c: mir::Constant<'tcx>,
    local_decls: &mut mir::LocalDecls<'tcx>,
) -> mir::Local {
    let new_local_decl = mir::LocalDecl::new(c.ty(), rustc_span::DUMMY_SP);
    local_decls.push(new_local_decl)
}

pub fn get_new_statement<'tcx>(
    c: mir::Constant<'tcx>,
    local_decl: mir::Local,
) -> mir::Statement<'tcx> {
    mir::Statement {
        source_info: mir::SourceInfo::outermost(rustc_span::DUMMY_SP),
        kind: mir::StatementKind::Assign(Box::new((
            local_decl.into(),
            mir::Rvalue::Use(mir::Operand::Constant(Box::new(c))),
        ))),
    }
}

pub fn clear_and_get_reverse<'tcx>(body: &mut mir::Body<'tcx>) -> Vec<mir::BasicBlockData<'tcx>> {
    let basic_blocks = body.basic_blocks_mut();
    let mut reverse = vec![];
    while let Some(basic_block) = basic_blocks.pop() {
        reverse.push(basic_block);
    }

    reverse
}

pub fn repopulate_basic_blocks<'tcx>(
    body: &mut mir::Body<'tcx>,
    reverse: &mut Vec<mir::BasicBlockData<'tcx>>,
) -> Vec<usize> {
    let mut index_counts = vec![];
    while let Some(basic_block) = reverse.pop() {
        create_new_blocks(body, basic_block, &mut index_counts);
    }

    index_counts
}

pub fn create_new_blocks<'tcx>(
    body: &mut mir::Body<'tcx>,
    mut basic_block: mir::BasicBlockData<'tcx>,
    index_counts: &mut Vec<usize>,
) {
    let mut new_basic_blocks = vec![];

    // # of statements + block for teminator call + terminator
    // We do this before we pop basic_block.statements.
    index_counts.push(basic_block.statements.len() + 2);

    // Create a new block with the original terminator and no statements.
    let mut new_basic_block = mir::BasicBlockData::new(basic_block.terminator);
    new_basic_block.is_cleanup = basic_block.is_cleanup;
    new_basic_blocks.push(new_basic_block);

    // Create a new block that will include a rc0lib call for the terminator
    new_basic_blocks.push(mir::BasicBlockData::new(None));

    while let Some(statement) = basic_block.statements.pop() {
        // Process from the last statement in the block
        let mut new_basic_block = mir::BasicBlockData::new(None);
        new_basic_block.statements = Vec::from([statement]);
        new_basic_blocks.push(new_basic_block);
    }

    let basic_blocks = body.basic_blocks_mut();
    while let Some(new_basic_block) = new_basic_blocks.pop() {
        basic_blocks.push(new_basic_block);
    }
}

pub fn create_index_mappings<'tcx>(
    index_counts: &Vec<usize>,
) -> collections::HashMap<mir::BasicBlock, mir::BasicBlock> {
    let mut index_map = collections::HashMap::<mir::BasicBlock, mir::BasicBlock>::new();
    let mut origin_index: usize = 0;
    let mut new_index = 0;

    for cnt in index_counts {
        // At this point, index_counts contains a list of how many basic blocks replace each of
        // the original basic blocks
        index_map.insert(
            mir::BasicBlock::from_usize(origin_index),
            mir::BasicBlock::from_usize(new_index),
        );
        origin_index += 1;
        new_index += cnt;
    }

    index_map
}

pub fn remap_jump_targets<'tcx>(
    body: &mut mir::Body<'tcx>,
    basic_block_idx: mir::BasicBlock,
    index_map: &collections::HashMap<mir::BasicBlock, mir::BasicBlock>,
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

pub fn get_unevaluated_promoted<'tcx>(
    op: &mir::Operand<'tcx>,
) -> Option<(ty::WithOptConstParam<def_id::DefId>, mir::Promoted)> {
    if let mir::Operand::Constant(box mir::Constant {
        span: _,
        user_ty: _,
        literal: mir::ConstantKind::Ty(c),
    }) = op
    {
        if let ty::ConstKind::Unevaluated(ty::Unevaluated {
            def: did,
            substs: _,
            promoted: Some(p),
        }) = c.val()
        {
            return Some((did, p));
        }
    }
    None
}

pub fn get_const_op<'tcx>(
    statement: &'tcx mir::Statement<'tcx>,
) -> Option<&'tcx mir::Operand<'tcx>> {
    if let mir::Statement {
        source_info: _,
        kind:
            mir::StatementKind::Assign(box (
                _,
                mir::Rvalue::Use(
                    op @ mir::Operand::Constant(box mir::Constant {
                        span: _,
                        user_ty: _,
                        literal: _,
                    }),
                ),
            )),
    } = statement
    {
        return Some(op);
    }
    None
}

pub fn get_fn_name(ty_kind: &ty::TyKind) -> Option<String> {
    let fn_name_suffix = match ty_kind {
        ty::TyKind::Bool => "const_bool",
        ty::TyKind::Char => "const_char",
        ty::TyKind::Int(ty::IntTy::Isize) => "const_isize",
        ty::TyKind::Int(ty::IntTy::I8) => "const_i8",
        ty::TyKind::Int(ty::IntTy::I16) => "const_i16",
        ty::TyKind::Int(ty::IntTy::I32) => "const_i32",
        ty::TyKind::Int(ty::IntTy::I64) => "const_i64",
        ty::TyKind::Int(ty::IntTy::I128) => "const_i128",
        ty::TyKind::Uint(ty::UintTy::Usize) => "const_usize",
        ty::TyKind::Uint(ty::UintTy::U8) => "const_u8",
        ty::TyKind::Uint(ty::UintTy::U16) => "const_u16",
        ty::TyKind::Uint(ty::UintTy::U32) => "const_u32",
        ty::TyKind::Uint(ty::UintTy::U64) => "const_u64",
        ty::TyKind::Uint(ty::UintTy::U128) => "const_u128",
        ty::TyKind::Float(ty::FloatTy::F32) => "const_f32",
        ty::TyKind::Float(ty::FloatTy::F64) => "const_f64",
        //ty::TyKind::Str => "const_str",
        ty::TyKind::Ref(_, ty, _) => {
            let tk = ty.kind();
            if let ty::TyKind::Str = tk {
                "const_str"
            } else {
                return None;
            }
        }
        _ => {
            return None;
        }
    };
    Some(format!("{}{}", "rc0lib::assign::", fn_name_suffix))
}
