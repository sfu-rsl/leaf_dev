use crate::{
    abs::{self, Local, LocalIndex},
    utils::SelfHierarchical,
};

use super::{
    config::{CallConfig, ExternalCallStrategy},
    expr::FuncId,
    place::{LocalWithMetadata, PlaceMetadata},
    CallStackManager, ConcreteValue, ConstValue, Place, UntupleHelper, Value, ValueRef,
    VariablesState,
};

use common::{log_debug, log_warn, pri::RawPointer};

type VariablesStateFactory<VS> = Box<dyn Fn(usize) -> VS>;

use self::logging::TAG;

/* Here is what happens during function calls:
 * +---------------+---------------------------+
 * | Caller        | Callee                    |
 * +---------------+---------------------------+
 * | before_call() |                           |
 * |               | preserve_metadata()       |
 * |               | [try_untuple_argument()]  |
 * |               | enter()                   |
 * |               | [override_return_value()] |
 * |               | return()                  |
 * | after_call()  |                           |
 * +---------------+---------------------------+
 * Or equivalently (names are changed to better reflect the current position of program):
 * +--------------------+---------------------------+
 * | Caller             | Callee                    |
 * +--------------------+---------------------------+
 * | prepare_for_call() |                           |
 * |                    | set_local_metadata()      |
 * |                    | [try_untuple_argument()]  |
 * |                    | notify_enter()            |
 * |                    | [override_return_value()] |
 * |                    | pop_stack_frame()         |
 * | finalize_call()    |                           |
 * +--------------------+---------------------------+
 */

/* Functionality Specification:
 * - `latest_call` holds the information from the caller sent to the callee.
 *   It is set in `prepare_for_call` and consumed in `notify_enter()` or cleared in `finalize_external_call`.
 * - `latest_returned_val` holds the latest returned value, not necessarily the called function's
 *   return value (look at external functions).
 * - `args_metadata` and `return_val_metadata` are expected to be set before `notify_enter`
 *   and get consumed by it.
 */
/* How are external function calls detected/handled?
 * (internal = instrumented, external = not instrumented)
 * It is important to consider both scenarios where an external function is in the programs call stack.
 * 1. When the external function is called from an internal one.
 * 2. When the external function calls an internal one.
 *
 * Detection is handled by setting a function id on the caller side and comparing it with the current
 * function id on the callee side.
 * Specification:
 * - Using `latest_call.expected_func` to store the information.
 * - `is_callee_external` is stack-based and works in pessimistic mode, i.e., it is set to true if
 *   the function id is the same as the expected function id, otherwise remains unset or is set to false.
 * Scenarios:
 * - internal -> external:
 *   When returned to the caller, `is_callee_external` is unset.
 * - external -> internal:
 *   When entered the callee, the expected function id does not match and
 *   `is_callee_external` is set to false.
 *   - internal -> external -> internal
 *   When returned to the first caller, `is_callee_external` is false.
 *
 * Return Value
 * The return value is set based on the external call strategy.
 * Any existing return value is from an internal function called by the external function
 * and thus should be discarded.
 */

pub(super) struct BasicCallStackManager<VS: VariablesState> {
    /// The call stack. Each frame consists of the data that is held for the
    /// current function call and is preserved through calls and returns.
    stack: Vec<CallStackFrame>,
    vars_state_factory: VariablesStateFactory<VS>,
    /// The data passed between from the call point (in the caller)
    /// to the entrance point (in the callee).
    latest_call: Option<CallInfo>,
    args_metadata: Vec<Option<PlaceMetadata>>,
    return_val_metadata: Option<PlaceMetadata>,
    /// The data (return value) passed between the exit point (in the callee)
    /// to the return point (in the caller).
    latest_returned_val: Option<ValueRef>,
    vars_state: Option<VS>,
    config: CallConfig,
    log_span: tracing::span::EnteredSpan,
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
    return_val_metadata: Option<PlaceMetadata>,
}

type ArgLocal = LocalWithMetadata;

#[derive(Debug)]
pub(super) struct CallInfo {
    expected_func: ValueRef,
    args: Vec<ValueRef>,
    are_args_tupled: bool,
}

impl<VS: VariablesState> BasicCallStackManager<VS> {
    pub(super) fn new(vars_state_factory: VariablesStateFactory<VS>, config: &CallConfig) -> Self {
        let log_span = tracing::Span::none().entered();
        Self {
            stack: vec![],
            vars_state_factory,
            latest_call: None,
            args_metadata: vec![],
            return_val_metadata: None,
            latest_returned_val: None,
            vars_state: None,
            config: config.clone(),
            log_span,
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
        self.log_span_reset();
    }

    fn top_frame(&mut self) -> &mut CallStackFrame {
        self.stack
            .last_mut()
            .expect("Call stack should not be empty")
    }

    fn finalize_internal_call(&mut self, result_dest: &Place) {
        if let Some(returned_val) = self.latest_returned_val.take() {
            self.top().set_place(&result_dest, returned_val)
        } else {
            // The unit return type
        }
    }

    fn finalize_external_call(&mut self, result_dest: &Place) {
        // Clearing the data that is not cleared by the external function.
        let latest_call = self.latest_call.take();
        let symbolic_args = latest_call
            .as_ref()
            .map(|info| self.inspect_external_call_info(info));
        self.latest_returned_val = None;

        if let Some(overridden) = self.top_frame().overridden_return_val.take() {
            log_debug!(
                target: TAG,
                concat!(
                "Consuming the overridden return value as the returned value ",
                "from the external function."
                ),
            );
            self.top().set_place(result_dest, overridden);
            return;
        }

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
                if !symbolic_args.map(|args| !args.is_empty()).unwrap_or(false) {
                    Concretize
                } else {
                    OverApproximate
                }
            }
        };
        match action {
            Concretize => self.top().set_place(
                &result_dest,
                ConcreteValue::from(abs::Constant::Some).to_value_ref(),
            ),
            OverApproximate => {
                todo!("#306: Over-approximated symbolic values are not supported.")
            }
        }
    }

    fn untuple(
        tupled_value: ValueRef,
        tupled_arg_metadata: Option<&PlaceMetadata>,
        untuple_helper: &mut dyn UntupleHelper,
        isolated_vars_state: VS,
    ) -> Vec<ValueRef> {
        // Make a pseudo place for the tupled argument
        let tupled_local = Local::Argument(1);
        let tupled_local = {
            let metadata = untuple_helper.make_tupled_arg_pseudo_place_meta(
                /* NOTE: The address should not really matter, but let's keep it realistic. */
                tupled_arg_metadata
                    .map(|m| m.address() as RawPointer)
                    .unwrap_or(1),
            );
            ArgLocal::from((tupled_local, metadata))
        };
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

    fn inspect_external_call_info<'a>(&self, info: &'a CallInfo) -> Vec<(usize, &'a ValueRef)> {
        log_debug!(
            target: TAG,
            "External function call detected. Expected: {}",
            info.expected_func,
        );

        let symbolic_args: Vec<_> = info
            .args
            .iter()
            .enumerate()
            .filter(|(_, v)| v.is_symbolic())
            .collect();
        if !symbolic_args.is_empty() {
            log_warn!(
                target: TAG,
                "Possible loss of symbolic values in external function call",
            );
            log_debug!(
                target: TAG,
                "Symbolic arguments passed to the function: {:?}",
                symbolic_args,
            );
        }
        symbolic_args
    }
}

impl<VS: VariablesState + SelfHierarchical> CallStackManager for BasicCallStackManager<VS> {
    fn prepare_for_call(&mut self, func: ValueRef, args: Vec<ValueRef>, are_args_tupled: bool) {
        self.log_span_start_trans(logging::TransitionDirection::Call);

        // Some sanity checks
        let is_currently_clean = self.latest_call.is_none()
            && self.args_metadata.is_empty()
            && self.return_val_metadata.is_none();
        debug_assert!(
            is_currently_clean,
            concat!(
                "The call information is not consumed or cleaned correctly. ",
                "This is due to a problem in external function handling or instrumentation. ",
                "`latest_call`: {:?}, ",
                "`args_metadata`: {:?}, ",
                "`return_val_metadata`: {:?}",
            ),
            self.latest_call, self.args_metadata, self.return_val_metadata,
        );

        self.latest_call = Some(CallInfo {
            expected_func: func,
            args,
            are_args_tupled,
        });
    }

    fn set_local_metadata(&mut self, local: &Local, metadata: super::place::PlaceMetadata) {
        match local {
            Local::ReturnValue => self.return_val_metadata = Some(metadata),
            Local::Argument(local_index) => {
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
        log_debug!(target: TAG, "Untupling argument at index {}.", arg_index);
        let tupled_value = args.remove(arg_index);
        let untupled_args = Self::untuple(
            tupled_value,
            self.args_metadata.get(arg_index).and_then(|m| m.as_ref()),
            untuple_helper().as_mut(),
            (self.vars_state_factory)(usize::MAX),
        );
        // Replace the tupled argument with separate ones.
        args.splice(arg_index..arg_index, untupled_args);
    }

    fn notify_enter(&mut self, current_func: ValueRef) {
        let arg_locals = self
            .args_metadata
            .drain(..)
            .into_iter()
            .map(|m| m.expect("Missing argument metadata."))
            .enumerate()
            .map(|(i, metadata)| ArgLocal::new(Local::Argument((i + 1) as LocalIndex), metadata))
            .collect::<Vec<_>>();

        let call_stack_frame = CallStackFrame {
            arg_locals: arg_locals.clone(),
            return_val_metadata: self.return_val_metadata.take(),
            ..Default::default()
        };

        let args_if_not_broken = if let Some(call_info) = self.latest_call.take() {
            if current_func.unwrap_func_id() == call_info.expected_func.unwrap_func_id() {
                let args = call_info.args;
                assert_eq!(
                    args.len(),
                    arg_locals.len(),
                    "Inconsistent number of passed arguments."
                );

                Ok(arg_locals.into_iter().zip(args.into_iter()))
            } else {
                Err(Some(call_info))
            }
        } else {
            // NOTE: An example of this case is when an external function calls multiple internal ones.
            Err(None)
        };

        if let Some(parent_frame) = self.stack.last_mut() {
            parent_frame.is_callee_external = Some(args_if_not_broken.is_err());
        }

        match args_if_not_broken {
            Ok(args) => {
                self.push_new_stack_frame(args, call_stack_frame);
            }
            Err(call_info) => {
                if let Some(call_info) = call_info {
                    self.inspect_external_call_info(&call_info);
                } else {
                    log_debug!(
                        "External function call detected with no call information available"
                    );
                }
                self.push_new_stack_frame(core::iter::empty(), call_stack_frame);
            }
        }

        log_debug!(target: TAG, "Entered the function");
    }

    fn pop_stack_frame(&mut self) {
        self.latest_returned_val = None;

        let popped_frame = self.stack.pop().unwrap();

        self.log_span_reset();
        if self
            .stack
            .last()
            .is_some_and(|f| !f.is_callee_external.unwrap())
        {
            self.log_span_start_trans(logging::TransitionDirection::Return);
        }

        // Cleaning the arguments
        popped_frame.arg_locals.into_iter().for_each(|local| {
            self.top().take_place(&Place::from(local));
        });

        let ret_local = popped_frame
            .return_val_metadata
            // When return type is unit, metadata may be removed.
            .map(|m| LocalWithMetadata::new(Local::ReturnValue, m));
        self.latest_returned_val = ret_local
            .map(Place::from)
            .and_then(|p| self.top().try_take_place(&p));
        if let Some(overridden) = popped_frame.overridden_return_val {
            if self.latest_returned_val.is_some() {
                log_warn!(
                    target: TAG,
                    concat!(
                        "The return value is overridden while an actual value was available. ",
                        "This may not be intended."
                    )
                );
            }
            self.latest_returned_val = Some(overridden);
        }

        self.vars_state = self.vars_state.take().unwrap().drop_layer();
    }

    fn finalize_call(&mut self, result_dest: Place) {
        self.log_span_reset();

        let is_external = self.top_frame().is_callee_external.take().unwrap_or(true);
        if !is_external {
            log_debug!(target: TAG, "Finalizing an internal call.");
            self.finalize_internal_call(&result_dest)
        } else {
            log_debug!(target: TAG, "Finalizing an external call.");
            self.finalize_external_call(&result_dest)
        }
    }

    fn override_return_value(&mut self, value: ValueRef) {
        log_debug!(target: TAG, "Overriding the return value with {:?}", value);
        self.top_frame().overridden_return_val = Some(value);
    }

    fn top(&mut self) -> &mut dyn VariablesState {
        self.vars_state.as_mut().expect("Call stack is empty")
    }
}

impl Value {
    #[inline]
    pub(crate) fn unwrap_func_id(&self) -> FuncId {
        match self {
            Value::Concrete(ConcreteValue::Const(ConstValue::Func(f))) => *f,
            _ => panic!("Expected a function id, but got {:?}", self),
        }
    }
}

mod logging {
    use super::*;

    use const_format::concatcp;
    use tracing::{debug_span, Span};

    pub(super) const TAG: &str = "call_manager";
    const TAG_STACK: &str = concatcp!(TAG, "::stack");
    const SPAN_CALL: &str = "call";
    const SPAN_TRANSITION: &str = "call_trans";
    const FIELD_DEPTH: &str = "depth";
    const FIELD_TRANS_DIR: &str = "dir";
    #[derive(derive_more::Display)]
    pub(super) enum TransitionDirection {
        #[display(fmt = "call")]
        Call,
        #[display(fmt = "ret")]
        Return,
    }

    impl<VS: VariablesState> BasicCallStackManager<VS> {
        #[inline]
        pub(super) fn log_span_reset(&mut self) {
            // We avoid the hierarchical structure of log spans as they don't suit our purpose.
            self.log_span = debug_span!(
                target: TAG_STACK,
                parent: Span::none(),
                SPAN_CALL,
                { FIELD_DEPTH } = self.stack.len(),
            )
            .entered();
        }

        #[inline]
        pub(super) fn log_span_start_trans(&mut self, direction: TransitionDirection) {
            self.log_span = debug_span!(
                target: TAG_STACK,
                parent: self.log_span.id(),
                SPAN_TRANSITION,
                { FIELD_TRANS_DIR } = direction.to_string(),
            )
            .entered();
        }
    }
}
