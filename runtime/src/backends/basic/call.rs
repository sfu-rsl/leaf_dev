use crate::{
    abs::{self, Local, LocalIndex},
    utils::SelfHierarchical,
};

use super::{
    expr::{ConcreteValue, UnevalValue},
    place::{LocalWithMetadata, PlaceMetadata},
    CallStackManager, EntranceKind, Place, ValueRef, VariablesState,
};

type VariablesStateFactory<VS> = Box<dyn Fn(usize) -> VS>;

pub(super) struct BasicCallStackManager<VS: VariablesState> {
    /// The call stack. Each frame consists of the data that is held for the
    /// current function call and is preserved through calls and returns.
    stack: Vec<CallStackFrame>,
    vars_state_factory: VariablesStateFactory<VS>,
    /// The data passed between from the call point (in the caller)
    /// to the entrance point (in the callee).
    latest_call: Option<CallInfo>,
    /// The data (return value) passed between the exit point (in the callee)
    /// to the return point (in the caller).
    latest_returned_val: Option<ValueRef>,
    vars_state: Option<VS>,
}

#[derive(Default)]
pub(super) struct CallStackFrame {
    /* this doesn't refer to the current stack frame,
     * but the function that is about to be / was just called by this function.
     */
    is_callee_external: Option<bool>,
    /// The return value forced by a call to `override_return_value`.
    /// If it is set in an internal call, it will be consumed as the returned
    /// value of the current function when popping the frame.
    /// If it is set in an external call, it will be consumed as the returned
    /// value from the external call when storing the returned value in the
    /// destination variable.
    overridden_return_val: Option<ValueRef>,
    arg_locals: Vec<ArgLocal>,
    #[cfg(place_addr)]
    return_val_metadata: Option<PlaceMetadata>,
}

#[cfg(not(place_addr))]
type ArgLocal = Local;
#[cfg(place_addr)]
type ArgLocal = LocalWithMetadata;
pub(super) struct CallInfo {
    expected_func: ValueRef,
    args: Vec<(ArgLocal, ValueRef)>,
    #[cfg(place_addr)]
    return_val_metadata: Option<PlaceMetadata>,
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
    fn push_new_stack_frame(
        &mut self,
        args: impl Iterator<Item = (ArgLocal, ValueRef)>,
        frame: CallStackFrame,
    ) {
        let mut arg_locals = Vec::new();

        self.vars_state = Some(if let Some(current_vars) = self.vars_state.take() {
            let mut vars_state = current_vars.add_layer();
            for (local, value) in args {
                vars_state.set_place(&Place::from(local.clone()), value);
                arg_locals.push(local);
            }

            vars_state
        } else {
            // The first push when the stack is empty
            (self.vars_state_factory)(0)
        });

        self.stack.push(frame);
    }

    fn top_frame(&mut self) -> &mut CallStackFrame {
        self.stack
            .last_mut()
            .expect("Call stack should not be empty")
    }

    fn finalize_external_call(&mut self, result_dest: &Place) {
        if let Some(overridden) = self.top_frame().overridden_return_val.take() {
            log::info!(concat!(
                "Consuming the overridden return value as the returned value ",
                "from the external function."
            ));
            self.top().set_place(result_dest, overridden);
            return;
        }

        if cfg!(external_call = "panic") {
            panic!("External function call detected.");
        }

        // FIXME: The configuration should be set dynamically.
        enum Strategy {
            Concretization,
            OverApproximation,
        }
        use Strategy::*;

        let strategy = if cfg!(external_call = "concretize") {
            Concretization
        } else if cfg!(external_call = "overapprox") {
            OverApproximation
        } else if cfg!(external_call = "optim_conc") {
            /* NOTE: What is optimistic here?
             * It correspond to the optimistic assumption that the callee has been a
             * pure function and no symbolic input results in no symbolic output. */
            /* FIXME: With the current implementation, references to symbolic values
             * skip this check. */
            let all_concrete = self
                .latest_call
                .take()
                .is_some_and(|c| c.args.iter().all(|v| !v.1.is_symbolic()));
            if all_concrete {
                Concretization
            } else {
                OverApproximation
            }
        } else {
            unreachable!("Invalid external call configuration.");
        };

        match strategy {
            Concretization => {
                #[cfg(abs_concrete)]
                let value = ConcreteValue::from(abs::Constant::Some).to_value_ref();
                #[cfg(not(abs_concrete))]
                let value = unimplemented!(
                    "Abstract concrete values are not supported in this configuration."
                );
                self.top().set_place(&result_dest, value)
            }
            OverApproximation => {
                todo!("#306: Over-approximated symbolic values are not supported.")
            }
        }
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
            return_val_metadata: None,
        });
    }

    /// This function is called when a function is entered. `kind` tells us whether the entered function is
    /// instrumented (internal) or not.
    fn notify_enter(&mut self, kind: EntranceKind) {
        let call_info = self.latest_call.take();

        // if parent_frame doesn't exist, we can assume we're in an instrumented function
        if let Some(parent_frame) = self.stack.last_mut() {
            parent_frame.is_callee_external = Some(match kind {
                EntranceKind::ForcedInternal => false,
                EntranceKind::ByFuncId(curr) => {
                    // If the entered func's id matches what was expected in the parent, it's an internal function
                    let expected_func = &call_info.as_ref().unwrap().expected_func;
                    curr.unwrap_func_id() != expected_func.unwrap_func_id()
                }
            });
        }

        if let Some(call_info) = call_info {
            #[cfg(place_addr)]
            let return_value_metadata = call_info.return_val_metadata;
            let arg_locals = call_info
                .args
                .iter()
                .map(|(local, _)| local.clone())
                .collect();
            self.push_new_stack_frame(
                call_info.args.into_iter(),
                CallStackFrame {
                    arg_locals,
                    #[cfg(place_addr)]
                    return_val_metadata: return_value_metadata,
                    ..Default::default()
                },
            );
        } else {
            if !self.stack.is_empty() {
                log::warn!(concat!(
                    "No call info was found for this entrance. ",
                    "This means a mixture of external and internal call has happened."
                ));
            }

            self.push_new_stack_frame(core::iter::empty(), Default::default());
        }
    }

    fn pop_stack_frame(&mut self) {
        self.latest_returned_val = None;

        let popped_frame = self.stack.pop().unwrap();

        // Cleaning the arguments
        popped_frame.arg_locals.into_iter().for_each(|local| {
            self.top().take_place(&Place::from(local));
        });

        #[cfg(not(place_addr))]
        let ret_local = Some(Local::ReturnValue);
        #[cfg(place_addr)]
        let ret_local = popped_frame
            .return_val_metadata
            // When return type is unit, metadata may be removed.
            .map(|m| LocalWithMetadata::new(Local::ReturnValue, m));
        self.latest_returned_val = ret_local
            .map(Place::from)
            .and_then(|p| self.top().try_take_place(&p));
        if let Some(overridden) = popped_frame.overridden_return_val {
            if self.latest_returned_val.is_some() {
                log::warn!(concat!(
                    "The return value is overridden while an actual value was available. ",
                    "This may not be intended."
                ))
            }
            self.latest_returned_val = Some(overridden);
        }

        self.vars_state = self.vars_state.take().unwrap().drop_layer();
    }

    fn finalize_call(&mut self, result_dest: Place) {
        let is_external = self.top_frame().is_callee_external.take().unwrap_or(true);
        if is_external {
            self.finalize_external_call(&result_dest)
        } else if let Some(returned_val) = self.latest_returned_val.take() {
            self.top().set_place(&result_dest, returned_val)
        } else {
            // The unit return type
        }
    }

    fn override_return_value(&mut self, value: ValueRef) {
        log::info!("Overriding the return value with {:?}", value);
        self.top_frame().overridden_return_val = Some(value);
    }

    fn top(&mut self) -> &mut dyn VariablesState {
        self.vars_state.as_mut().expect("Call stack is empty")
    }

    #[cfg(place_addr)]
    fn set_local_metadata(&mut self, local: &Local, metadata: super::place::PlaceMetadata) {
        use crate::abs::place::HasMetadata;

        match local {
            Local::ReturnValue => {
                self.latest_call.as_mut().unwrap().return_val_metadata = Some(metadata)
            }
            Local::Argument(..) => {
                let args = &mut self.latest_call.as_mut().unwrap().args;
                *args
                    .iter_mut()
                    .find(|(arg, _)| arg.as_ref().eq(&local))
                    .unwrap()
                    .0
                    .metadata_mut() = metadata;
            }
            _ => (),
        }
    }
}
