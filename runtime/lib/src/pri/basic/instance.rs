use super::utils::{DefaultRefManager, RefManager, UnsafeSync};
use super::{BranchingInfo, OperandRef, PlaceRef};
use crate::abs::backend::{
    AssignmentHandler, BranchingHandler, OperandHandler, PlaceHandler, RuntimeBackend,
};

#[allow(unused_imports)] // Mutex is detected as unused unless runtime_access is set to safe_mt
use std::{
    cell::RefCell,
    sync::{Mutex, Once},
};

type BackendImpl = crate::backends::basic::BasicBackend;
//type BackendImpl = crate::backends::basic::logger::LoggerBackend; // for DEBUG

type PlaceImpl = <<BackendImpl as RuntimeBackend>::PlaceHandler<'static> as PlaceHandler>::Place;
pub(super) type OperandImpl =
    <<BackendImpl as RuntimeBackend>::OperandHandler<'static> as OperandHandler>::Operand;
pub(super) type FieldImpl =
    <<BackendImpl as RuntimeBackend>::AssignmentHandler<'static> as AssignmentHandler>::Field;

static INIT: Once = Once::new();
#[cfg(runtime_access = "safe_mt")]
static BACKEND: Mutex<Option<BackendImpl>> = Mutex::new(None);
#[cfg(runtime_access = "safe_brt")]
static BACKEND: UnsafeSync<RefCell<Option<BackendImpl>>> = UnsafeSync::new(RefCell::new(None));
#[cfg(runtime_access = "unsafe")]
static mut BACKEND: Option<BackendImpl> = None;

#[cfg(any(runtime_access = "safe_mt", runtime_access = "safe_brt"))]
thread_local! {
/*
 * Breaking places and operands is something that happens in our compiler, so
 * it is not possible to see references shared between threads.
*/
static PLACE_REF_MANAGER: RefCell<DefaultRefManager<PlaceImpl>> =
    RefCell::new(DefaultRefManager::new());
}
#[cfg(runtime_access = "unsafe")]
static mut PLACE_REF_MANAGER: DefaultRefManager<PlaceImpl> = DefaultRefManager::new();

#[cfg(any(runtime_access = "safe_mt", runtime_access = "safe_brt"))]
thread_local! {
static OPERAND_REF_MANAGER: RefCell<DefaultRefManager<OperandImpl>> =
    RefCell::new(DefaultRefManager::new());
}
#[cfg(runtime_access = "unsafe")]
static mut OPERAND_REF_MANAGER: DefaultRefManager<OperandImpl> = DefaultRefManager::new();

pub(super) fn init_backend() {
    INIT.call_once(|| {
        crate::init();

        let config = load_config();
        log::info!("Initializing basic backend");
        let backend =
            BackendImpl::try_from(config).expect("Failed to initialize backend through the config");
        #[cfg(runtime_access = "safe_mt")]
        {
            let mut guard = BACKEND.lock().unwrap();
            *guard = Some(backend);
        }
        #[cfg(runtime_access = "safe_brt")]
        {
            let mut binding = BACKEND.borrow_mut();
            *binding = Some(backend);
        }
        #[cfg(runtime_access = "unsafe")]
        {
            unsafe {
                BACKEND = Some(backend);
            }
        }
    });
}

fn load_config() -> ::config::Config {
    use config::{Environment, File};
    config::Config::builder()
        .add_source(
            File::with_name(&common::utils::search_current_ancestor_dirs_for(
                "leaf_config",
            ))
            .required(false),
        )
        .add_source(
            Environment::with_prefix("LEAF")
                .prefix_separator("_")
                .separator("__"),
        )
        .build()
        .expect("Failed to read configurations")
}

/* FIXME: Make these functions rely on abstract traits rather than concrete types.
 * It may require some wrapper types to make borrowing/not borrowing of inner objects
 * definite.
 */

pub(super) fn push_place_ref(
    get_place: impl FnOnce(<BackendImpl as RuntimeBackend>::PlaceHandler<'_>) -> PlaceImpl,
) -> PlaceRef {
    let place = perform_on_backend(|r| {
        let handler = r.place();
        get_place(handler)
    });
    perform_on_place_ref_manager(|rm| rm.push(place))
}
pub(super) fn mut_place_ref(
    place_ref: PlaceRef,
    mut_place: impl FnOnce(<BackendImpl as RuntimeBackend>::PlaceHandler<'_>, &mut PlaceImpl),
) {
    perform_on_place_ref_manager(|rm| {
        let place = rm.get_mut(place_ref);
        perform_on_backend(|r| {
            let handler = r.place();
            mut_place(handler, place);
        });
    });
}
pub(super) fn take_back_place_ref(reference: PlaceRef) -> PlaceImpl {
    perform_on_place_ref_manager(|rm| rm.take(reference))
}

pub(super) fn assign_to<T>(
    dest: PlaceRef,
    assign_action: impl FnOnce(<BackendImpl as RuntimeBackend>::AssignmentHandler<'_>) -> T,
) -> T {
    let dest = take_back_place_ref(dest);
    perform_on_backend(|r| assign_action(r.assign_to(dest)))
}

pub(super) fn push_operand_ref(
    get_operand: impl FnOnce(<BackendImpl as RuntimeBackend>::OperandHandler<'_>) -> OperandImpl,
) -> OperandRef {
    let operand = perform_on_backend(|r| get_operand(r.operand()));
    perform_on_operand_ref_manager(|rm| rm.push(operand))
}
pub(super) fn mut_operand_ref(
    operand_ref: OperandRef,
    mut_operand: impl FnOnce(<BackendImpl as RuntimeBackend>::OperandHandler<'_>, &mut OperandImpl),
) {
    perform_on_operand_ref_manager(|rm| {
        let operand = rm.get_mut(operand_ref);
        perform_on_backend(|r| {
            let handler = r.operand();
            mut_operand(handler, operand);
        });
    });
}
pub(super) fn take_back_operand_ref(reference: OperandRef) -> OperandImpl {
    perform_on_operand_ref_manager(|rm| rm.take(reference))
}

pub(super) fn branch<T>(
    branch_action: impl FnOnce(<BackendImpl as RuntimeBackend>::BranchingHandler<'_>) -> T,
) -> T {
    perform_on_backend(|r| {
        let handler = r.branch();
        branch_action(handler)
    })
}

pub(super) fn conditional<T>(
    info: BranchingInfo,
    conditional_action: impl FnOnce(
        <<BackendImpl as RuntimeBackend>::BranchingHandler<'_> as BranchingHandler>::ConditionalBranchingHandler,
    ) -> T,
) -> T {
    branch(|b| {
        let handler = b.conditional(take_back_operand_ref(info.discriminant), info.metadata);
        conditional_action(handler)
    })
}

pub(super) fn func_control<T>(
    func_action: impl FnOnce(<BackendImpl as RuntimeBackend>::FunctionHandler<'_>) -> T,
) -> T {
    perform_on_backend(|r| {
        let func_control = r.func_control();
        func_action(func_control)
    })
}

#[cfg(runtime_access = "safe_mt")]
fn perform_on_backend<T>(action: impl FnOnce(&mut BackendImpl) -> T) -> T {
    let mut guard = BACKEND.lock().unwrap();
    check_and_perform_on_backend(&mut guard, action)
}

#[cfg(runtime_access = "safe_brt")]
fn perform_on_backend<T>(action: impl FnOnce(&mut BackendImpl) -> T) -> T {
    let mut binding = BACKEND.borrow_mut();
    check_and_perform_on_backend(&mut binding, action)
}

#[cfg(runtime_access = "unsafe")]
fn perform_on_backend<T>(action: impl FnOnce(&mut BackendImpl) -> T) -> T {
    check_and_perform_on_backend(unsafe { &mut BACKEND }, action)
}

fn check_and_perform_on_backend<T>(
    backend: &mut Option<BackendImpl>,
    action: impl FnOnce(&mut BackendImpl) -> T,
) -> T {
    let backend = backend.as_mut().expect("Runtime is not initialized.");
    action(backend)
}

#[cfg(any(runtime_access = "safe_mt", runtime_access = "safe_brt"))]
fn perform_on_place_ref_manager<T>(
    action: impl FnOnce(&mut DefaultRefManager<PlaceImpl>) -> T,
) -> T {
    PLACE_REF_MANAGER.with_borrow_mut(action)
}

#[cfg(runtime_access = "unsafe")]
fn perform_on_place_ref_manager<T>(
    action: impl FnOnce(&mut DefaultRefManager<PlaceImpl>) -> T,
) -> T {
    action(unsafe { &mut PLACE_REF_MANAGER })
}

#[cfg(any(runtime_access = "safe_mt", runtime_access = "safe_brt"))]
fn perform_on_operand_ref_manager<T>(
    action: impl FnOnce(&mut DefaultRefManager<OperandImpl>) -> T,
) -> T {
    OPERAND_REF_MANAGER.with_borrow_mut(action)
}

#[cfg(runtime_access = "unsafe")]
fn perform_on_operand_ref_manager<T>(
    action: impl FnOnce(&mut DefaultRefManager<OperandImpl>) -> T,
) -> T {
    action(unsafe { &mut OPERAND_REF_MANAGER })
}
