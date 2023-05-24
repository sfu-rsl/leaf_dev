use crate::abs::Local;

use super::{get_operand_value, operand::Operand, place::Place, MutableVariablesState, ValueRef};

pub(super) struct CallStackManager {
    stack: Vec<CallStackFrame>,
    call_info: CallInfo,
}

pub(super) struct CallStackFrame {
    vars_state: MutableVariablesState,
}

pub(super) struct CallInfo {
    passed_args: Vec<Operand>,
    returned_val: Option<ValueRef>,
    is_external: bool,
}

impl CallStackManager {
    pub(super) fn new() -> Self {
        Self {
            stack: vec![],
            call_info: CallInfo {
                passed_args: vec![],
                returned_val: None,
                is_external: true,
            },
        }
    }

    pub(super) fn update_args(&mut self, args: Vec<Operand>) {
        self.call_info.passed_args = args;
    }

    pub(super) fn push_stack_frame(&mut self) {
        let mut vars_state = MutableVariablesState::new();

        // set places for the arguments in the new frame using values from the current frame
        let passed_args: Vec<Operand> = self.call_info.passed_args.drain(..).collect();
        for (i, operand) in passed_args.iter().enumerate() {
            let value = get_operand_value(self.top(), &operand);
            let local_index = (i + 1) as u32;
            let place = &Place::new(Local::Argument(local_index));
            vars_state.set_place(place, value);
        }

        self.stack.push(CallStackFrame { vars_state });
    }

    pub(super) fn pop_stack_frame(&mut self) {
        self.call_info.returned_val = self.top().try_take_place(&Place::new(Local::ReturnValue));
        self.call_info.is_external = false;
        self.stack.pop();
    }

    pub(super) fn get_return_info(&mut self) -> (Option<ValueRef>, bool) {
        let returned_val = self.call_info.returned_val.take();
        let is_external = self.call_info.is_external;
        self.call_info.is_external = true;

        (returned_val, is_external)
    }

    pub(super) fn top(&mut self) -> &mut MutableVariablesState {
        &mut self
            .stack
            .last_mut()
            .expect("Call stack is empty.")
            .vars_state
    }
}
