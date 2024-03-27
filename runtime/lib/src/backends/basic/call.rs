use crate::{
    abs::{self, Local, LocalIndex},
    backends::basic::config::ExternalCallStrategy,
    utils::SelfHierarchical,
};

use super::{
    config::CallConfig,
    expr::ConcreteValue,
    place::{LocalWithMetadata, PlaceMetadata},
    CallStackManager, Place, UntupleHelper, ValueRef, VariablesState,
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
    #[cfg(place_addr)]
    args_metadata: Vec<Option<PlaceMetadata>>,
    #[cfg(place_addr)]
    return_val_metadata: Option<PlaceMetadata>,
    /// The data (return value) passed between the exit point (in the callee)
    /// to the return point (in the caller).
    latest_returned_val: Option<ValueRef>,
    vars_state: Option<VS>,
    config: CallConfig,
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
    args: Vec<ValueRef>,
    are_args_tupled: bool,
}

impl<VS: VariablesState> BasicCallStackManager<VS> {
    pub(super) fn new(vars_state_factory: VariablesStateFactory<VS>, config: &CallConfig) -> Self {
        Self {
            stack: vec![],
            vars_state_factory,
            latest_call: None,
            #[cfg(place_addr)]
            args_metadata: vec![],
            #[cfg(place_addr)]
            return_val_metadata: None,
            latest_returned_val: None,
            vars_state: None,
            config: config.clone(),
        }
    }
}

impl<VS: VariablesState + SelfHierarchical> BasicCallStackManager<VS> {
    fn push_new_stack_frame(
        &mut self,
        args: impl Iterator<Item = (ArgLocal, ValueRef)>,
        frame: CallStackFrame,
    ) {
        self.vars_state = Some(if let Some(current_vars) = self.vars_state.take() {
            let mut vars_state = current_vars.add_layer();
            for (local, value) in args {
                vars_state.set_place(&Place::from(local.clone()), value);
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

        // FIXME: The configuration should be set dynamically.
        enum Action {
            Concretize,
            OverApproximate,
        }
        use Action::*;

        let action = match self.config.external_call {
            ExternalCallStrategy::Panic => panic!("External function call detected."),
            ExternalCallStrategy::Concretization => Concretize,
            ExternalCallStrategy::OverApproximation => OverApproximate,
            ExternalCallStrategy::OptimisticConcretization => {
                /* NOTE: What is optimistic here?
                 * It correspond to the optimistic assumption that the callee has been a
                 * pure function and no symbolic input results in no symbolic output. */
                /* FIXME: With the current implementation, references to symbolic values
                 * skip this check. */
                let all_concrete = self
                    .latest_call
                    .take()
                    .is_some_and(|c| c.args.iter().all(|v| !v.is_symbolic()));
                if all_concrete {
                    Concretize
                } else {
                    OverApproximate
                }
            }
        };
        match action {
            Concretize => {
                #[cfg(abs_concrete)]
                let value = ConcreteValue::from(abs::Constant::Some).to_value_ref();
                #[cfg(not(abs_concrete))]
                let value = unimplemented!(
                    "Abstract concrete values are not supported in this configuration."
                );
                self.top().set_place(&result_dest, value)
            }
            OverApproximate => {
                todo!("#306: Over-approximated symbolic values are not supported.")
            }
        }
    }

    fn untuple(
        tupled_value: ValueRef,
        #[cfg(place_addr)] tupled_arg_metadata: Option<&PlaceMetadata>,
        untuple_helper: &mut dyn UntupleHelper,
        isolated_vars_state: VS,
    ) -> Vec<ValueRef> {
        // Make a pseudo place for the tupled argument
        let tupled_local = Local::Argument(1);
        #[cfg(place_addr)]
        let tupled_local = {
            let metadata = untuple_helper.make_tupled_arg_pseudo_place_meta(
                /* NOTE: The address should not really matter, but let's keep it realistic. */
                tupled_arg_metadata.and_then(|m| m.address()).unwrap_or(1),
            );
            ArgLocal::from((tupled_local, metadata))
        };
        #[cfg(not(place_addr))]
        let tupled_local = ArgLocal::from(tupled_local);
        let tupled_pseudo_place = Place::from(tupled_local);

        // Write the value to the pseudo place in an isolated state, then read the fields
        let mut vars_state = isolated_vars_state;
        let num_fields = untuple_helper.num_fields(&tupled_value);
        vars_state.set_place(&tupled_pseudo_place, tupled_value);
        // Read the fields (values inside the tuple) one by one.
        (0..num_fields)
            .into_iter()
            .map(|i| untuple_helper.field_place(tupled_pseudo_place.clone(), i))
            .map(|arg_place| {
                vars_state.try_take_place(&arg_place).unwrap_or_else(|| {
                    panic!("Could not untuple the argument at field {}.", arg_place)
                })
            })
            .collect()
    }
}

impl<VS: VariablesState + SelfHierarchical> CallStackManager for BasicCallStackManager<VS> {
    fn prepare_for_call(&mut self, func: ValueRef, args: Vec<ValueRef>, are_args_tupled: bool) {
        self.latest_call = Some(CallInfo {
            expected_func: func,
            args,
            are_args_tupled,
        });
    }
    #[cfg(place_addr)]
    fn set_local_metadata(&mut self, local: &Local, metadata: super::place::PlaceMetadata) {
        match local {
            Local::ReturnValue => self.return_val_metadata = Some(metadata),
            Local::Argument(local_index) => {
                log::info!("Setting metadata for argument {:?}.", local);
                let args_metadata = &mut self.args_metadata;
                let index = *local_index as usize - 1;
                if args_metadata.len() <= index {
                    args_metadata.resize(index + 1, None);
                }
                args_metadata[index] = Some(metadata);
            }
            _ => (),
        }
    }

    fn try_untuple_argument<'a, 'b>(
        &'a mut self,
        arg_index: LocalIndex,
        untuple_helper: &dyn Fn() -> Box<dyn UntupleHelper + 'b>,
    ) {
        let Some(CallInfo {
            args,
            are_args_tupled,
            ..
        }) = self.latest_call.as_mut()
        else {
            return;
        };

        if !*are_args_tupled {
            return;
        }

        let arg_index = arg_index as usize - 1;
        log::debug!("Untupling argument at index {}.", arg_index);
        let tupled_value = args.remove(arg_index);
        let untupled_args = Self::untuple(
            tupled_value,
            #[cfg(place_addr)]
            self.args_metadata.get(arg_index).and_then(|m| m.as_ref()),
            untuple_helper().as_mut(),
            (self.vars_state_factory)(usize::MAX),
        );
        // Replace the tupled argument with separate ones.
        args.splice(arg_index..arg_index, untupled_args);
    }

    fn notify_enter(&mut self, current_func: ValueRef) {
        if let Some(CallInfo {
            expected_func,
            mut args,
            are_args_tupled: _,
        }) = self.latest_call.take()
        {
            let expected_func = &expected_func;
            let broken_stack = current_func.unwrap_func_id() != expected_func.unwrap_func_id();

            if let Some(parent_frame) = self.stack.last_mut() {
                parent_frame.is_callee_external = Some(broken_stack);
            }

            #[cfg(place_addr)]
            let arg_locals = self
                .args_metadata
                .drain(0..)
                .into_iter()
                .map(|m| m.expect("Missing argument metadata."))
                .enumerate()
                .map(|(i, metadata)| {
                    ArgLocal::new(Local::Argument((i + 1) as LocalIndex), metadata)
                })
                .collect::<Vec<_>>();
            #[cfg(not(place_addr))]
            let arg_locals = (1..=args.len())
                .map(|i| Local::Argument(i as LocalIndex))
                .collect::<Vec<_>>();

            if broken_stack {
                args.clear()
            } else {
                assert_eq!(
                    args.len(),
                    arg_locals.len(),
                    "Inconsistent number of passed arguments."
                );
            }

            let return_val_metadata = self.return_val_metadata.take();
            self.push_new_stack_frame(
                arg_locals.clone().into_iter().zip(args.into_iter()),
                CallStackFrame {
                    arg_locals,
                    #[cfg(place_addr)]
                    return_val_metadata,
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
}
