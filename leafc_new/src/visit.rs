use rustc_ast::{InlineAsmOptions, InlineAsmTemplatePiece, Mutability};
use rustc_middle::{
    mir::{
        AggregateKind, AssertMessage, BasicBlock, BinOp, BorrowKind, CastKind, InlineAsmOperand,
        NullOp, Operand, Place, Rvalue, SwitchTargets, TerminatorKind, UnOp,
    },
    ty::{Const, Region, Ty},
};
use rustc_span::Span;

trait TerminatorKindMutVisitor<'tcx, T> {
    fn visit_terminator_kind(&mut self, kind: &mut TerminatorKind<'tcx>) -> T {
        self.super_visit_terminator_kind(kind)
    }

    fn visit_goto(&mut self, target: &mut BasicBlock) -> T;

    fn visit_switch_int(
        &mut self,
        discr: &mut Operand<'tcx>,
        switch_ty: Ty<'tcx>,
        targets: &mut SwitchTargets,
    ) -> T;

    fn visit_resume(&mut self) -> T;

    fn visit_abort(&mut self) -> T;

    fn visit_return(&mut self) -> T;

    fn visit_unreachable(&mut self) -> T;

    fn visit_drop(
        &mut self,
        place: &mut Place<'tcx>,
        target: &mut BasicBlock,
        unwind: &mut Option<BasicBlock>,
    ) -> T;

    fn visit_drop_and_replace(
        &mut self,
        place: &mut Place<'tcx>,
        value: &mut Operand<'tcx>,
        target: &mut BasicBlock,
        unwind: &mut Option<BasicBlock>,
    ) -> T;

    fn visit_call(
        &mut self,
        func: &mut Operand<'tcx>,
        args: &mut Vec<Operand<'tcx>>,
        destination: &mut Option<(Place<'tcx>, BasicBlock)>,
        cleanup: &mut Option<BasicBlock>,
        from_hir_call: bool,
        fn_span: Span,
    ) -> T;

    fn visit_assert(
        &mut self,

        cond: &mut Operand<'tcx>,
        expected: &mut bool,
        msg: &mut AssertMessage<'tcx>,
        target: &mut BasicBlock,
        cleanup: &mut Option<BasicBlock>,
    ) -> T;

    fn visit_yield(
        &mut self,
        value: &mut Operand<'tcx>,
        resume: &mut BasicBlock,
        resume_arg: &mut Place<'tcx>,
        drop: &mut Option<BasicBlock>,
    ) -> T;

    fn visit_generator_drop(&mut self) -> T;

    fn visit_false_edge(
        &mut self,
        real_target: &mut BasicBlock,
        imaginary_target: &mut BasicBlock,
    ) -> T;

    fn visit_false_unwind(
        &mut self,
        real_target: &mut BasicBlock,
        unwind: &mut Option<BasicBlock>,
    ) -> T;

    fn visit_inline_asm(
        &mut self,
        template: &'tcx mut [InlineAsmTemplatePiece],
        operands: &mut Vec<InlineAsmOperand<'tcx>>,
        options: &mut InlineAsmOptions,
        line_spans: &'tcx [Span],
        destination: &mut Option<BasicBlock>,
        cleanup: &mut Option<BasicBlock>,
    ) -> T;

    fn super_visit_terminator_kind(&mut self, kind: &mut TerminatorKind<'tcx>) -> T {
        match kind {
            TerminatorKind::Goto { target } => self.visit_goto(&mut target),
            TerminatorKind::SwitchInt {
                discr,
                switch_ty,
                targets,
            } => self.visit_switch_int(discr, *switch_ty, &mut targets),
            TerminatorKind::Resume => self.visit_resume(),
            TerminatorKind::Abort => self.visit_abort(),
            TerminatorKind::Return => self.visit_return(),
            TerminatorKind::Unreachable => self.visit_unreachable(),
            TerminatorKind::Drop {
                place,
                target,
                unwind,
            } => self.visit_drop(&mut place, &mut target, &mut unwind),
            TerminatorKind::DropAndReplace {
                place,
                value,
                target,
                unwind,
            } => self.visit_drop_and_replace(&mut place, &mut value, &mut target, &mut unwind),
            TerminatorKind::Call {
                func,
                args,
                destination,
                cleanup,
                from_hir_call,
                fn_span,
            } => self.visit_call(
                func,
                args,
                &mut destination,
                &mut cleanup,
                *from_hir_call,
                *fn_span,
            ),
            TerminatorKind::Assert {
                cond,
                expected,
                msg,
                target,
                cleanup,
            } => self.visit_assert(
                &mut cond,
                &mut expected,
                &mut msg,
                &mut target,
                &mut cleanup,
            ),
            TerminatorKind::Yield {
                value,
                resume,
                resume_arg,
                drop,
            } => self.visit_yield(&mut value, &mut resume, &mut resume_arg, &mut drop),
            TerminatorKind::GeneratorDrop => self.visit_generator_drop(),
            TerminatorKind::FalseEdge {
                real_target,
                imaginary_target,
            } => self.visit_false_edge(&mut real_target, &mut imaginary_target),
            TerminatorKind::FalseUnwind {
                real_target,
                unwind,
            } => self.visit_false_unwind(&mut real_target, &mut unwind),
            TerminatorKind::InlineAsm {
                template,
                operands,
                options,
                line_spans,
                destination,
                cleanup,
            } => self.visit_inline_asm(
                &mut template,
                &mut operands,
                &mut options,
                line_spans,
                &mut destination,
                &mut cleanup,
            ),
        }
    }
}

trait DefaultTerminatorKindMutVisitor<'tcx>: TerminatorKindMutVisitor<'tcx, ()> {
    fn visit_goto(&mut self, target: &mut BasicBlock) {}

    fn visit_switch_int(
        &mut self,
        discr: &mut Operand<'tcx>,
        switch_ty: Ty<'tcx>,
        targets: &mut SwitchTargets,
    ) {
    }

    fn visit_resume(&mut self) {}

    fn visit_abort(&mut self) {}

    fn visit_return(&mut self) {}

    fn visit_unreachable(&mut self) {}

    fn visit_drop(
        &mut self,
        place: &mut Place<'tcx>,
        target: &mut BasicBlock,
        unwind: &mut Option<BasicBlock>,
    ) {
    }

    fn visit_drop_and_replace(
        &mut self,
        place: &mut Place<'tcx>,
        value: &mut Operand<'tcx>,
        target: &mut BasicBlock,
        unwind: &mut Option<BasicBlock>,
    ) {
    }

    fn visit_call(
        &mut self,
        func: &mut Operand<'tcx>,
        args: &mut Vec<Operand<'tcx>>,
        destination: &mut Option<(Place<'tcx>, BasicBlock)>,
        cleanup: &mut Option<BasicBlock>,
        from_hir_call: bool,
        fn_span: Span,
    ) {
    }

    fn visit_assert(
        &mut self,

        cond: &mut Operand<'tcx>,
        expected: &mut bool,
        msg: &mut AssertMessage<'tcx>,
        target: &mut BasicBlock,
        cleanup: &mut Option<BasicBlock>,
    ) {
    }

    fn visit_yield(
        &mut self,
        value: &mut Operand<'tcx>,
        resume: &mut BasicBlock,
        resume_arg: &mut Place<'tcx>,
        drop: &mut Option<BasicBlock>,
    ) {
    }

    fn visit_generator_drop(&mut self) {}

    fn visit_false_edge(
        &mut self,
        real_target: &mut BasicBlock,
        imaginary_target: &mut BasicBlock,
    ) {
    }

    fn visit_false_unwind(
        &mut self,
        real_target: &mut BasicBlock,
        unwind: &mut Option<BasicBlock>,
    ) {
    }

    fn visit_inline_asm(
        &mut self,
        template: &'tcx mut [InlineAsmTemplatePiece],
        operands: &mut Vec<InlineAsmOperand<'tcx>>,
        options: &mut InlineAsmOptions,
        line_spans: &'tcx [Span],
        destination: &mut Option<BasicBlock>,
        cleanup: &mut Option<BasicBlock>,
    ) {
    }
}

trait RvalueMutVisitor<'tcx, T> {
    fn visit_rvalue(&mut self, rvalue: &mut Rvalue<'tcx>) -> T {
        self.super_visit_rvalue(rvalue)
    }

    fn visit_use(&mut self, operand: &mut Operand<'tcx>) -> T;

    fn visit_repeat(&mut self, operand: &mut Operand<'tcx>, count: &mut Const<'tcx>) -> T;

    fn visit_ref(
        &mut self,
        region: &mut Region,
        borrow_kind: &mut BorrowKind,
        place: &mut Place<'tcx>,
    ) -> T;

    fn visit_thread_local_ref(
        &mut self,
        /* DefId is weirdly private at the current time of development.
        def_id: &mut DefId,
        */
    ) -> T;

    fn visit_address_of(&mut self, mutability: &mut Mutability, place: &mut Place<'tcx>) -> T;

    fn visit_len(&mut self, place: &mut Place<'tcx>) -> T;

    fn visit_cast(
        &mut self,
        kind: &mut CastKind,
        operand: &mut Operand<'tcx>,
        ty: &mut Ty<'tcx>,
    ) -> T;

    fn visit_binary_op(
        &mut self,
        op: &mut BinOp,
        operands: &mut Box<(Operand<'tcx>, Operand<'tcx>)>,
    ) -> T;

    fn visit_checked_binary_op(
        &mut self,
        op: &mut BinOp,
        operands: &mut Box<(Operand<'tcx>, Operand<'tcx>)>,
    ) -> T;

    fn visit_nullary_op(&mut self, op: &mut NullOp, ty: &mut Ty<'tcx>) -> T;

    fn visit_unary_op(&mut self, op: &mut UnOp, operand: &mut Operand<'tcx>) -> T;

    fn visit_discriminant(&mut self, place: &mut Place<'tcx>) -> T;

    fn visit_aggregate(
        &mut self,
        kind: &mut Box<AggregateKind>,
        operands: &mut Vec<Operand<'tcx>>,
    ) -> T;

    fn visit_shallow_init_box(&mut self, operand: &mut Operand<'tcx>, ty: &mut Ty<'tcx>) -> T;

    fn super_visit_rvalue(&mut self, rvalue: &mut Rvalue<'tcx>) -> T {
        match rvalue {
            Rvalue::Use(operand) => self.visit_use(operand),
            Rvalue::Repeat(operand, count) => self.visit_repeat(operand, count),
            Rvalue::Ref(region, borrow_kind, place) => self.visit_ref(region, borrow_kind, place),
            Rvalue::ThreadLocalRef(_) => self.visit_thread_local_ref(),
            Rvalue::AddressOf(mutability, place) => self.visit_address_of(mutability, place),
            Rvalue::Len(place) => self.visit_len(place),
            Rvalue::Cast(kind, operand, ty) => self.visit_cast(kind, operand, ty),
            Rvalue::BinaryOp(op, operands) => self.visit_binary_op(op, operands),
            Rvalue::CheckedBinaryOp(op, operands) => self.visit_checked_binary_op(op, operands),
            Rvalue::NullaryOp(op, ty) => self.visit_nullary_op(op, ty),
            Rvalue::UnaryOp(op, operand) => self.visit_unary_op(op, operand),
            Rvalue::Discriminant(place) => self.visit_discriminant(place),
            Rvalue::Aggregate(kind, operands) => self.visit_aggregate(kind, operands),
            Rvalue::ShallowInitBox(operand, ty) => self.visit_shallow_init_box(operand, ty),
        }
    }
}

trait DefaultRvalueMutVisitor<'tcx>: RvalueMutVisitor<'tcx, ()> {
    fn visit_use(&mut self, operand: &mut Operand<'tcx>) {}

    fn visit_repeat(&mut self, operand: &mut Operand<'tcx>, count: &mut Const<'tcx>) {}

    fn visit_ref(
        &mut self,
        region: &mut Region,
        borrow_kind: &mut BorrowKind,
        place: &mut Place<'tcx>,
    ) {
    }

    fn visit_thread_local_ref(
        &mut self,
        /* DefId is weirdly private at the current time of development.
        def_id: &mut DefId,
        */
    ) {
    }

    fn visit_address_of(&mut self, mutability: &mut Mutability, place: &mut Place<'tcx>) {}

    fn visit_len(&mut self, place: &mut Place<'tcx>) {}

    fn visit_cast(&mut self, kind: &mut CastKind, operand: &mut Operand<'tcx>, ty: &mut Ty<'tcx>) {}

    fn visit_binary_op(
        &mut self,
        op: &mut BinOp,
        operands: &mut Box<(Operand<'tcx>, Operand<'tcx>)>,
    ) {
    }

    fn visit_checked_binary_op(
        &mut self,
        op: &mut BinOp,
        operands: &mut Box<(Operand<'tcx>, Operand<'tcx>)>,
    ) {
    }

    fn visit_nullary_op(&mut self, op: &mut NullOp, ty: &mut Ty<'tcx>) {}

    fn visit_unary_op(&mut self, op: &mut UnOp, operand: &mut Operand<'tcx>) {}

    fn visit_discriminant(&mut self, place: &mut Place<'tcx>) {}

    fn visit_aggregate(
        &mut self,
        kind: &mut Box<AggregateKind>,
        operands: &mut Vec<Operand<'tcx>>,
    ) {
    }

    fn visit_shallow_init_box(&mut self, operand: &mut Operand<'tcx>, ty: &mut Ty<'tcx>) {}
}
