use crate::abs::Local;

use super::{
    get_operand_value, operand::Operand, place::Place, CallStackManager, ValueRef, VariablesState,
};

pub(super) struct BasicCallStackManager<VS: VariablesState> {
    stack: Vec<CallStackFrame<VS>>,
    vars_state_factory: Box<dyn Fn() -> VS>,
    call_info: CallInfo,
}

pub(super) struct CallStackFrame<VS> {
    vars_state: VS,
}

pub(super) struct CallInfo {
    passed_args: Vec<Operand>,
    returned_val: Option<ValueRef>,
    is_external: bool,
}

impl<VS: VariablesState> BasicCallStackManager<VS> {
    pub(super) fn new(vars_state_factory: Box<dyn Fn() -> VS>) -> Self {
        Self {
            stack: vec![],
            vars_state_factory,
            call_info: CallInfo {
                passed_args: vec![],
                returned_val: None,
                is_external: true,
            },
        }
    }
}

impl<VS: VariablesState> CallStackManager for BasicCallStackManager<VS> {
    fn update_args(&mut self, args: Vec<Operand>) {
        self.call_info.passed_args = args;
    }

    fn push_stack_frame(&mut self) {
        let mut vars_state = (self.vars_state_factory)();

        // set places for the arguments in the new frame using values from the current frame
        for (i, operand) in self.call_info.passed_args.drain(..).enumerate() {
            let value = get_operand_value(top_vars_state::<VS>(&mut self.stack), operand);
            let local_index = (i + 1) as u32;
            let place = &Place::new(Local::Argument(local_index));
            vars_state.set_place(place, value);
        }

        self.stack.push(CallStackFrame { vars_state });
    }

    fn pop_stack_frame(&mut self) {
        self.call_info.returned_val = self.top().try_take_place(&Place::new(Local::ReturnValue));
        self.call_info.is_external = false;
        self.stack.pop();
    }

    fn get_return_info(&mut self) -> (Option<ValueRef>, bool) {
        let returned_val = self.call_info.returned_val.take();
        let is_external = self.call_info.is_external;
        self.call_info.is_external = true;

        (returned_val, is_external)
    }

    fn top(&mut self) -> &mut dyn VariablesState {
        top_vars_state::<VS>(&mut self.stack)
    }
}

fn top_vars_state<VS>(stack: &mut Vec<CallStackFrame<VS>>) -> &mut VS {
    &mut stack.last_mut().expect("Call stack is empty.").vars_state
}
