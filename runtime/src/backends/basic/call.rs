use crate::{abs::Local, utils::Hierarchical};

use super::{
    get_operand_value, operand::Operand, place::Place, CallStackManager, ValueRef, VariablesState,
};

type VariablesStateFactory<VS> = Box<dyn Fn(usize) -> VS>;

pub(super) struct BasicCallStackManager<VS: VariablesState> {
    stack: Vec<CallStackFrame>,
    vars_state_factory: VariablesStateFactory<VS>,
    call_info: CallInfo,
    vars_state: Option<VS>,
}

pub(super) struct CallStackFrame {
    /* This struct previously contained the variables state.
     * Currently, there's no specific information to put here,
     * but as the call stack is a principle entity, we keep it for now.
     */
}

pub(super) struct CallInfo {
    passed_args: Vec<Operand>,
    returned_val: Option<ValueRef>,
    is_external: bool,
}

impl<VS: VariablesState> BasicCallStackManager<VS> {
    pub(super) fn new(vars_state_factory: VariablesStateFactory<VS>) -> Self {
        Self {
            stack: vec![],
            vars_state_factory,
            call_info: CallInfo {
                passed_args: vec![],
                returned_val: None,
                is_external: true,
            },
            vars_state: None,
        }
    }
}

impl<VS: VariablesState + Hierarchical<VS>> CallStackManager for BasicCallStackManager<VS> {
    fn update_args(&mut self, args: Vec<Operand>) {
        self.call_info.passed_args = args;
    }

    fn push_stack_frame(&mut self) {
        self.vars_state = Some(if let Some(mut current_vars) = self.vars_state.take() {
            let passed_args = &mut self.call_info.passed_args;
            let args = if !passed_args.is_empty() {
                passed_args
                    .drain(..)
                    .map(|operand| get_operand_value(&mut current_vars, operand))
                    .collect()
            } else {
                vec![]
            };

            let mut vars_state = (self.vars_state_factory)(current_vars.id() + 1);
            vars_state.set_parent(current_vars);
            // set places for the arguments in the new frame using values from the current frame
            for (i, value) in args.into_iter().enumerate() {
                let local_index = (i + 1) as u32;
                let place = &Place::new(Local::Argument(local_index));
                vars_state.set_place(place, value);
            }

            vars_state
        }
        // The first push when the stack is empty
        else {
            (self.vars_state_factory)(0)
        });

        self.stack.push(CallStackFrame {});
    }

    fn pop_stack_frame(&mut self) {
        self.call_info.returned_val = self.top().try_take_place(&Place::new(Local::ReturnValue));
        self.call_info.is_external = false;
        self.stack.pop().unwrap();
        self.vars_state = self.vars_state.take().unwrap().give_back_parent();
    }

    fn get_return_info(&mut self) -> (Option<ValueRef>, bool) {
        let returned_val = self.call_info.returned_val.take();
        let is_external = self.call_info.is_external;
        self.call_info.is_external = true;

        (returned_val, is_external)
    }

    fn top(&mut self) -> &mut dyn VariablesState {
        self.vars_state.as_mut().expect("Call stack is empty.")
    }
}
