extern crate rustc_middle;

use rustc_middle::mir::{Rvalue, StatementKind, TerminatorKind};

fn visit_terminator(kind: TerminatorKind) {
    match kind {
        TerminatorKind::Goto { target } =>
        /* Nothing */
        {
            ()
        }
        TerminatorKind::SwitchInt {
            discr,
            switch_ty,
            targets,
        } => todo!(),
        TerminatorKind::Yield { .. } | TerminatorKind::Resume | TerminatorKind::GeneratorDrop =>
        /* Even after testing generators couldn't find these operations appear. Maybe they belong to intermediate stages. */
        {
            todo!()
        }
        TerminatorKind::Abort => todo!(),
        TerminatorKind::Return => todo!(),
        TerminatorKind::Unreachable =>
        /* Should not appear after some stages of MIR processing */
        {
            ()
        }
        TerminatorKind::Drop {
            place,
            target,
            unwind,
        } => todo!(),
        TerminatorKind::DropAndReplace {
            place,
            value,
            target,
            unwind,
        } =>
        /* Should not appear after some stages of MIR processing */
        {
            ()
        }
        TerminatorKind::Call {
            func,
            args,
            destination,
            cleanup,
            from_hir_call,
            fn_span,
        } => todo!(),
        TerminatorKind::Assert {
            cond,
            expected,
            msg,
            target,
            cleanup,
        } =>
        /* Have not been successful in making this appear yet */
        {
            todo!()
        }
        TerminatorKind::FalseUnwind { .. } | TerminatorKind::FalseEdge { .. } =>
        /* For borrow checker */
        {
            ()
        }
        TerminatorKind::InlineAsm {
            template,
            operands,
            options,
            line_spans,
            destination,
            cleanup,
        } => todo!(),
    };
}

fn visit_statement(kind: StatementKind) {
    match kind {
        StatementKind::Assign(_) => todo!(),
        StatementKind::FakeRead(_) =>
        /* For pattern matching. Does nothing during runtime. */
        {
            ()
        }
        StatementKind::SetDiscriminant {
            place,
            variant_index,
        } =>
        /* No idea yet. */
        {
            todo!()
        }
        StatementKind::Deinit(_) => todo!(),
        StatementKind::StorageLive(_)
        | StatementKind::StorageDead(_)
        | StatementKind::Retag(_, _) =>
        /* For borrow checker */
        {
            ()
        }
        StatementKind::AscribeUserType(_, _) =>
        /* For type checking. Does nothing during runtime. */
        {
            ()
        }
        StatementKind::Coverage(_) =>
        /* For coverage testing. */
        {
            ()
        }
        StatementKind::CopyNonOverlapping(_) => todo!(),
        StatementKind::Nop => (),
    }
}

fn visit_rvalue(rvalue: Rvalue) {
    match rvalue {
        Rvalue::Use(_) => todo!(),
        Rvalue::Repeat(_, _) => todo!(),
        Rvalue::Ref(_, _, _) => todo!(),
        Rvalue::ThreadLocalRef(_) => todo!(),
        Rvalue::AddressOf(_, _) => todo!(),
        Rvalue::Len(_) =>
        /* Couldn't make this appear. No clear idea yet. */
        {
            todo!()
        }
        Rvalue::Cast(_, _, _) => todo!(),
        Rvalue::BinaryOp(_, _) => todo!(),
        Rvalue::CheckedBinaryOp(_, _) => todo!(),
        Rvalue::NullaryOp(_, _) =>
        /* Not sure if it appears after some stages or gets computed. */
        {
            todo!()
        }
        Rvalue::UnaryOp(_, _) => todo!(),
        Rvalue::Discriminant(_) =>
        /* No idea yet. */
        {
            todo!()
        }
        Rvalue::Aggregate(_, _) => todo!(),
        Rvalue::ShallowInitBox(_, _) => todo!(),
    }
}
