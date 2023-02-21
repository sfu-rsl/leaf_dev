use super::utils::{DefaultRefManager, RefManager, UnsafeSync};
use super::{OperandRef, PlaceRef};
use crate::abs::{OperandHandler, PlaceHandler, Runtime};

use std::{
    cell::RefCell,
    sync::{Mutex, Once},
};

type RuntimeImpl = crate::backends::fake::FakeRuntime;

type PlaceImpl = <<RuntimeImpl as Runtime>::PlaceHandler as PlaceHandler>::Place;
type OperandImpl = <<RuntimeImpl as Runtime>::OperandHandler as OperandHandler>::Operand;

static INIT: Once = Once::new();
#[cfg(runtime_access = "safe_mt")]
static RUNTIME: Mutex<Option<RuntimeImpl>> = Mutex::new(None);
#[cfg(runtime_access = "safe_brt")]
static RUNTIME: UnsafeSync<RefCell<Option<RuntimeImpl>>> = UnsafeSync::new(RefCell::new(None));
#[cfg(runtime_access = "unsafe")]
static mut RUNTIME: Option<RuntimeImpl> = None;

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

pub(super) fn push_place_ref(
    get_place: impl FnOnce(<RuntimeImpl as Runtime>::PlaceHandler) -> PlaceImpl,
) -> PlaceRef {
    let place = perform_on_runtime(|r| get_place(r.place()));
    perform_on_place_ref_manager(|rm| rm.push(place))
}

pub(super) fn assign_to_place_ref(dest: PlaceRef) -> <RuntimeImpl as Runtime>::AssignmentHandler {
    let dest = take_back_place_ref(dest);
    perform_on_runtime(|r| r.assign_to(dest))
}

pub(super) fn take_back_place_ref(reference: PlaceRef) -> PlaceImpl {
    perform_on_place_ref_manager(|rm| rm.take_back(reference))
}

pub(super) fn push_operand_ref(
    get_operand: impl FnOnce(<RuntimeImpl as Runtime>::OperandHandler) -> OperandImpl,
) -> OperandRef {
    let operand = perform_on_runtime(|r| get_operand(r.operand()));
    perform_on_operand_ref_manager(|rm| rm.push(operand))
}

pub(super) fn take_back_operand_ref(reference: OperandRef) -> OperandImpl {
    perform_on_operand_ref_manager(|rm| rm.take_back(reference))
}

#[cfg(runtime_access = "safe_mt")]
fn perform_on_runtime<T>(action: impl FnOnce(&mut RuntimeImpl) -> T) -> T {
    let mut guard = RUNTIME.lock().unwrap();
    check_and_perform_on_runtime(&mut guard, action)
}

#[cfg(runtime_access = "safe_brt")]
fn perform_on_runtime<T>(action: impl FnOnce(&mut RuntimeImpl) -> T) -> T {
    let mut binding = RUNTIME.borrow_mut();
    check_and_perform_on_runtime(&mut binding, action)
}

#[cfg(runtime_access = "unsafe")]
fn perform_on_runtime<T>(action: impl FnOnce(&mut RuntimeImpl) -> T) -> T {
    check_and_perform_on_runtime(unsafe { &mut RUNTIME }, action)
}

fn check_and_perform_on_runtime<T>(
    runtime: &mut Option<RuntimeImpl>,
    action: impl FnOnce(&mut RuntimeImpl) -> T,
) -> T {
    let mut runtime = runtime.as_mut().expect("Runtime is not initialized.");
    action(&mut runtime)
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
