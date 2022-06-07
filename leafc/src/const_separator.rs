extern crate rustc_middle;
extern crate rustc_span;

use log::debug;
use rustc_middle::mir::{
    AggregateKind, BasicBlockData, Body, Constant, Local, LocalDecl, LocalDecls, Operand, Place,
    Rvalue, SourceInfo, Statement, StatementKind,
};

pub fn separate_consts<'tcx>(body: &mut Body<'tcx>) {
    let (basic_blocks, local_decls) = body.basic_blocks_and_local_decls_mut();

    basic_blocks.iter_mut().for_each(|basic_block| {
        separate_consts_in_bb(basic_block, local_decls);
    });
}

pub fn separate_consts_in_bb<'tcx>(
    basic_block: &mut BasicBlockData<'tcx>,
    local_decls: &mut LocalDecls<'tcx>,
) {
    let mut new_statements: Vec<(usize, Vec<Statement>)> = vec![];

    (0..basic_block.statements.len()).for_each(|i| {
        let kind = &mut basic_block.statements[i].kind;
        if let StatementKind::Assign(b) = kind {
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
    b: &mut Box<(Place<'tcx>, Rvalue<'tcx>)>,
    local_decls: &mut LocalDecls<'tcx>,
) -> Option<Vec<Statement<'tcx>>> {
    let (_, r) = &mut **b;
    match r {
        Rvalue::Repeat(Operand::Constant(c), co) => {
            debug!("Repeat");
            let new_local_decl_idx = insert_new_local_decl(**c, local_decls);
            let new_statement = get_new_statement(**c, new_local_decl_idx);
            b.1 = Rvalue::Repeat(Operand::Move(new_local_decl_idx.into()), *co);
            Some(vec![new_statement])
        }
        Rvalue::Cast(ck, Operand::Constant(c), t) => {
            debug!("Cast");
            let new_local_decl_idx = insert_new_local_decl(**c, local_decls);
            let new_statement = get_new_statement(**c, new_local_decl_idx);
            b.1 = Rvalue::Cast(*ck, Operand::Move(new_local_decl_idx.into()), *t);
            Some(vec![new_statement])
        }
        Rvalue::BinaryOp(_, box (Operand::Constant(_), _)) => {
            debug!("BinaryOp");
            let (new_local_decl_idx, new_statement) =
                if let Rvalue::BinaryOp(_, box (Operand::Constant(c), _)) = r {
                    let new_local_decl_idx = insert_new_local_decl(**c, local_decls);
                    (
                        new_local_decl_idx,
                        get_new_statement(**c, new_local_decl_idx),
                    )
                } else {
                    unreachable!();
                };
            if let Rvalue::BinaryOp(_, ib) = r {
                ib.0 = Operand::Move(new_local_decl_idx.into());
            } else {
                unreachable!();
            }
            Some(vec![new_statement])
        }
        Rvalue::BinaryOp(_, box (_, Operand::Constant(_))) => {
            debug!("BinaryOp");
            let (new_local_decl_idx, new_statement) =
                if let Rvalue::BinaryOp(_, box (_, Operand::Constant(c))) = r {
                    let new_local_decl_idx = insert_new_local_decl(**c, local_decls);
                    (
                        new_local_decl_idx,
                        get_new_statement(**c, new_local_decl_idx),
                    )
                } else {
                    unreachable!();
                };
            if let Rvalue::BinaryOp(_, ib) = r {
                ib.1 = Operand::Move(new_local_decl_idx.into());
            } else {
                unreachable!();
            }
            Some(vec![new_statement])
        }
        Rvalue::CheckedBinaryOp(_, box (Operand::Constant(_), _)) => {
            debug!("CheckedBinaryOp");
            let (new_local_decl_idx, new_statement) =
                if let Rvalue::CheckedBinaryOp(_, box (Operand::Constant(c), _)) = r {
                    let new_local_decl_idx = insert_new_local_decl(**c, local_decls);
                    (
                        new_local_decl_idx,
                        get_new_statement(**c, new_local_decl_idx),
                    )
                } else {
                    unreachable!();
                };
            if let Rvalue::CheckedBinaryOp(_, ib) = r {
                ib.0 = Operand::Move(new_local_decl_idx.into());
            } else {
                unreachable!();
            }
            Some(vec![new_statement])
        }
        Rvalue::CheckedBinaryOp(_, box (_, Operand::Constant(_))) => {
            debug!("CheckedBinaryOp");
            let (new_local_decl_idx, new_statement) =
                if let Rvalue::CheckedBinaryOp(_, box (_, Operand::Constant(c))) = r {
                    let new_local_decl_idx = insert_new_local_decl(**c, local_decls);
                    (
                        new_local_decl_idx,
                        get_new_statement(**c, new_local_decl_idx),
                    )
                } else {
                    unreachable!();
                };
            if let Rvalue::CheckedBinaryOp(_, ib) = r {
                ib.1 = Operand::Move(new_local_decl_idx.into());
            } else {
                unreachable!();
            }
            Some(vec![new_statement])
        }
        Rvalue::UnaryOp(u, Operand::Constant(c)) => {
            debug!("UnaryOp");
            let new_local_decl_idx = insert_new_local_decl(**c, local_decls);
            let new_statement = get_new_statement(**c, new_local_decl_idx);
            b.1 = Rvalue::UnaryOp(*u, Operand::Move(new_local_decl_idx.into()));
            Some(vec![new_statement])
        }
        Rvalue::Aggregate(box AggregateKind::Array(_), v) => {
            debug!("Aggregate");
            let mut new_statements = vec![];
            for i in 0..v.len() {
                let item = &v[i];
                if let Operand::Constant(c) = item {
                    let new_local_decl_idx = insert_new_local_decl(**c, local_decls);
                    new_statements.push(get_new_statement(**c, new_local_decl_idx));
                    let new_op = Operand::Move(new_local_decl_idx.into());
                    v.remove(i);
                    v.insert(i, new_op);
                }
            }
            Some(new_statements)
        }
        _ => {
            debug!("Other: {r:?}");
            None
        }
    }
}

pub fn insert_new_local_decl<'tcx>(c: Constant<'tcx>, local_decls: &mut LocalDecls<'tcx>) -> Local {
    let new_local_decl = LocalDecl::new(c.ty(), rustc_span::DUMMY_SP);
    local_decls.push(new_local_decl)
}

pub fn get_new_statement<'tcx>(c: Constant<'tcx>, local_decl: Local) -> Statement<'tcx> {
    Statement {
        source_info: SourceInfo::outermost(rustc_span::DUMMY_SP),
        kind: StatementKind::Assign(Box::new((
            local_decl.into(),
            Rvalue::Use(Operand::Constant(Box::new(c))),
        ))),
    }
}
