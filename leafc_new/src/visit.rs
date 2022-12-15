use rustc_ast::{InlineAsmOptions, InlineAsmTemplatePiece};
use rustc_middle::{
    mir::{
        AssertMessage, BasicBlock, InlineAsmOperand, Operand, Place,
        SwitchTargets, TerminatorKind,
    },
    ty::Ty,
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
