use delegate::delegate;

use core::assert_matches::assert_matches;
use std::collections::HashMap;

use common::log_info;

use crate::utils::alias::RRef;

use super::{
    BinaryExprBuilder, ConcreteValue, ConcreteValueRef, SymValue, SymValueRef, SymVarId,
    SymVariable, SymbolicVar, Value, ValueRef,
};

pub(super) trait SymVariablesManager {
    fn add_variable(&mut self, var: SymVariable<ValueRef>) -> SymValueRef;

    fn iter_variables(
        &self,
    ) -> impl ExactSizeIterator<Item = (&SymVarId, &SymValueRef, &ConcreteValueRef)>;

    fn iter_concretization_constraints(
        &self,
    ) -> impl ExactSizeIterator<Item = (&SymVarId, &ValueRef)>;
}

pub(super) struct BasicSymVariablesManager<EB: BinaryExprBuilder> {
    variables: HashMap<SymVarId, (SymValueRef, ConcreteValueRef)>,
    conc_constraints: HashMap<SymVarId, EB::Expr<'static>>,
    expr_builder: RRef<EB>,
}

impl<EB: BinaryExprBuilder> BasicSymVariablesManager<EB> {
    pub(crate) fn new(expr_builder: RRef<EB>) -> Self {
        Self {
            variables: HashMap::new(),
            conc_constraints: HashMap::new(),
            expr_builder,
        }
    }

    delegate! {
        to self.variables {
            pub(crate) fn len(&self) -> usize;
        }
    }
}

impl<EB: BinaryExprBuilder> SymVariablesManager for BasicSymVariablesManager<EB> {
    fn add_variable(&mut self, var: SymVariable<ValueRef>) -> SymValueRef {
        let conc_val = var
            .conc_value
            .expect("Concrete value of symbolic variables is required.");

        assert_matches!(
            conc_val.as_ref(),
            Value::Concrete(ConcreteValue::Const(..)),
            "Only constant values are currently expected to be used as the concrete value."
        );

        let id = self.len() as u32 + 1;

        let sym_val = SymValue::Variable(SymbolicVar::new(id, var.ty)).to_value_ref();
        let conc_val = ConcreteValueRef::new(conc_val);

        self.variables
            .insert(id, (sym_val.clone(), conc_val.clone()));

        log_info!("Added a new symbolic variable: {} = {}", sym_val, conc_val);

        self.conc_constraints.insert(
            id,
            self.expr_builder
                .borrow_mut()
                .eq((sym_val.clone().into(), conc_val.into())),
        );

        sym_val.into()
    }

    fn iter_variables(
        &self,
    ) -> impl ExactSizeIterator<Item = (&SymVarId, &SymValueRef, &ConcreteValueRef)> {
        self.variables
            .iter()
            .map(|(id, (sym_val, conc_val))| (id, sym_val, conc_val))
    }

    #[inline]
    fn iter_concretization_constraints(
        &self,
    ) -> impl ExactSizeIterator<Item = (&SymVarId, &ValueRef)> {
        self.conc_constraints.iter()
    }
}
