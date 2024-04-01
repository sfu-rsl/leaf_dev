use super::{
    alias::{RRef, ValueRefBinaryExprBuilder},
    ConcreteValueRef, Constraint, SymValueRef, TraceManager,
};

pub(super) trait Concretizer {
    fn stamp(&mut self, value: SymValueRef, concrete_value: ConcreteValueRef);
}

pub(super) struct BasicConcretizer<EB: ValueRefBinaryExprBuilder> {
    expr_builder: RRef<EB>,
    trace_manager: RRef<TraceManager>,
}

impl<EB: ValueRefBinaryExprBuilder> BasicConcretizer<EB> {
    pub fn new(expr_builder: RRef<EB>, trace_manager: RRef<TraceManager>) -> Self {
        Self {
            expr_builder,
            trace_manager,
        }
    }
}

impl<EB: ValueRefBinaryExprBuilder> Concretizer for BasicConcretizer<EB> {
    fn stamp(&mut self, value: SymValueRef, concrete_value: ConcreteValueRef) {
        log::debug!("Stamping symbolic value {} == {}", value, concrete_value);
        let eq_expr = self
            .expr_builder
            .borrow_mut()
            .eq((value.0, concrete_value.0).into());
        let constraint = Constraint::Bool(eq_expr.into());
        self.trace_manager.borrow_mut().notify_step(
            0, /* TODO: A unique index like basic block index. */
            vec![constraint],
        );
    }
}
