use crate::{
    abs::{Local, LocalIndex, RawPointer},
    utils::SelfHierarchical,
};

use super::{
    place::LocalWithMetadata, CallStackManager, EntranceKind, Place, ValueRef, VariablesState,
};

type VariablesStateFactory<VS> = Box<dyn Fn(usize) -> VS>;

pub(super) struct BasicCallStackManager<VS: VariablesState> {
    stack: Vec<CallStackFrame>,
    vars_state_factory: VariablesStateFactory<VS>,
    latest_call: Option<CallInfo>,
    latest_returned_val: Option<ValueRef>,
    vars_state: Option<VS>,
}

#[derive(Default)]
pub(super) struct CallStackFrame {
    // this doesn't refer to the current stack frame, but the function that is about to be / was just called
    is_callee_external: Option<bool>,
}

#[cfg(not(place_addr))]
type ArgLocal = Local;
#[cfg(place_addr)]
type ArgLocal = LocalWithMetadata;
pub(super) struct CallInfo {
    expected_func: ValueRef,
    args: Vec<(ArgLocal, ValueRef)>,
    #[cfg(place_addr)]
    return_value_addr: Option<RawPointer>,
}

impl<VS: VariablesState> BasicCallStackManager<VS> {
    pub(super) fn new(vars_state_factory: VariablesStateFactory<VS>) -> Self {
        Self {
            stack: vec![],
            vars_state_factory,
            latest_call: None,
            latest_returned_val: None,
            vars_state: None,
        }
    }
}

impl<VS: VariablesState + SelfHierarchical> BasicCallStackManager<VS> {
    fn push_new_stack_frame(&mut self, args: impl Iterator<Item = (ArgLocal, ValueRef)>) {
        self.vars_state = Some(if let Some(current_vars) = self.vars_state.take() {
            let mut vars_state = current_vars.add_layer();
            // set places for the arguments in the new frame using values from the current frame
            for (local, value) in args {
                vars_state.set_place(&Place::from(local), value);
            }

            vars_state
        } else {
            // The first push when the stack is empty
            (self.vars_state_factory)(0)
        });

        self.stack.push(CallStackFrame::default());
    }

    fn top_frame(&mut self) -> &mut CallStackFrame {
        self.stack.last_mut().expect("Call stack is empty")
    }
}

impl<VS: VariablesState + SelfHierarchical> CallStackManager for BasicCallStackManager<VS> {
    fn prepare_for_call(&mut self, func: ValueRef, args: Vec<ValueRef>) {
        self.latest_call = Some(CallInfo {
            expected_func: func,
            args: args
                .into_iter()
                .enumerate()
                .map(|(i, arg)| {
                    let local = Local::Argument((i + 1) as LocalIndex);
                    (ArgLocal::from(local), arg)
                })
                .collect(),
            #[cfg(place_addr)]
            return_value_addr: Default::default(),
        });
    }

    /// This function is called when a function is entered. `kind` tells us whether the entered function is
    /// instrumented (internal) or not.
    fn notify_enter(&mut self, kind: EntranceKind) {
        // if parent_frame doesn't exist, we can assume we're in an instrumented function
        if let Some(parent_frame) = self.stack.last_mut() {
            parent_frame.is_callee_external = Some(match kind {
                EntranceKind::ForcedInternal => false,
                EntranceKind::ByFuncId(curr) => {
                    // If the entered func's id matches what was expected in the parent, it's an internal function
                    let CallInfo { expected_func, .. } = self.latest_call.as_ref().unwrap();
                    curr.unwrap_func_id() != expected_func.unwrap_func_id()
                }
            });
        }

        let args = self
            .latest_call
            .as_ref()
            .map(|call| call.args.clone())
            .unwrap_or_default();
        self.push_new_stack_frame(args.into_iter());
    }

    fn pop_stack_frame(&mut self) {
        self.latest_returned_val = None;

        if let Some(latest_call) = self.latest_call.take() {
            // Cleaning the arguments
            latest_call.args.into_iter().for_each(|(local, _)| {
                self.top().take_place(&Place::from(local));
            });

            let ret_local = Local::ReturnValue;
            #[cfg(place_addr)]
            let ret_local = LocalWithMetadata::new(ret_local, todo!());
            self.latest_returned_val = self.top().try_take_place(&Place::from(ret_local));
        }

        self.stack.pop().unwrap();
        self.vars_state = self.vars_state.take().unwrap().drop_layer();
    }

    fn finalize_call(&mut self, result_dest: Place) {
        let is_external = self.top_frame().is_callee_external.take().unwrap_or(true);
        if is_external {
            // NOTE: The return value of an external function must be an untracked constant,
            //       because it's not possible to track it.
            todo!("handle the case when an external function is called")
        } else if let Some(returned_val) = self.latest_returned_val.take() {
            self.top().set_place(&result_dest, returned_val)
        } else {
            // The unit return type
        }
    }

    fn top(&mut self) -> &mut dyn VariablesState {
        self.vars_state.as_mut().expect("Call stack is empty")
    }

    fn set_local_address(&mut self, local: Local, addr: RawPointer) {
        match local {
            Local::ReturnValue => self.latest_call.as_mut().unwrap().return_value_addr = Some(addr),
            Local::Argument(..) => {
                let args = &mut self.latest_call.as_mut().unwrap().args;
                args.iter_mut()
                    .find(|(arg, _)| arg.as_ref().eq(&local))
                    .unwrap()
                    .0
                    .set_address(addr);
            }
            Local::Normal(_) => (),
        }
    }
}
