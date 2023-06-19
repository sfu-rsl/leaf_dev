use crate::{abs::Local, utils::TryGiveBack};

use super::{
    get_operand_value, operand::Operand, place::Place, CallStackManager, ValueRef, VariablesState,
};

type VariablesStateFactory<VS> = Box<dyn Fn(Option<VS>) -> VS>;

pub(super) struct BasicCallStackManager<VS: VariablesState> {
    stack: Vec<CallStackFrame<VS>>,
    vars_state_factory: VariablesStateFactory<VS>,
    call_info: CallInfo,
}

pub(super) struct CallStackFrame<VS> {
    vars_state: Option<VS>,
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
        }
    }
}

impl<VS: VariablesState + TryGiveBack<VS>> CallStackManager for BasicCallStackManager<VS> {
    fn update_args(&mut self, args: Vec<Operand>) {
        self.call_info.passed_args = args;
    }

    fn push_stack_frame(&mut self) {
        let current_vars = top_vars_state::<VS>(&mut self.stack);

        let vars_state = if let Some(current_vars) = current_vars {
            let mut current_vars = current_vars.take().unwrap();

            let passed_args = &mut self.call_info.passed_args;
            let args = if !passed_args.is_empty() {
                passed_args
                    .drain(..)
                    .map(|operand| get_operand_value(&mut current_vars, operand))
                    .collect()
            } else {
                vec![]
            };

            let mut vars_state = (self.vars_state_factory)(Some(current_vars));
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
            (self.vars_state_factory)(None)
        };

        self.stack.push(CallStackFrame {
            vars_state: Some(vars_state),
        });
    }

    fn pop_stack_frame(&mut self) {
        self.call_info.returned_val = self.top().try_take_place(&Place::new(Local::ReturnValue));
        self.call_info.is_external = false;
        let popped = self.stack.pop().unwrap();
        if let Some(current) = self.stack.last_mut() {
            current.vars_state = Some(popped.vars_state.unwrap().try_give_back().unwrap());
        }
    }

    fn get_return_info(&mut self) -> (Option<ValueRef>, bool) {
        let returned_val = self.call_info.returned_val.take();
        let is_external = self.call_info.is_external;
        self.call_info.is_external = true;

        (returned_val, is_external)
    }

    fn top(&mut self) -> &mut dyn VariablesState {
        top_vars_state::<VS>(&mut self.stack)
            .expect("Call stack is empty.")
            .as_mut()
            .unwrap()
    }
}

fn top_vars_state<VS>(stack: &mut Vec<CallStackFrame<VS>>) -> Option<&mut Option<VS>> {
    if let Some(frame) = stack.last_mut() {
        Some(&mut frame.vars_state)
    } else {
        None
    }
}
