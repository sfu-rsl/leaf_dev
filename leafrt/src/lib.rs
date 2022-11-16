#[macro_use]
extern crate lazy_static;

use leafcommon::place::{Local, Place};
use leafcommon::rvalue::{
    BinOp, Constant, ConstantKind, Operand, OperandConstValueVec, OperandVec, Rvalue,
};
use std::borrow::Borrow;
//use z3_sys::{Z3_config, Z3_context, Z3_solver};
//use z3::{Config, Context, Solver};
use crate::expression::{FunctionCallContext, FunctionCallStack};
use leafcommon::misc::{DebugInfo, PlaceAndDebugInfo};
use leafcommon::switchtargets::SwitchTargets;
use leafcommon::ty::FloatTy::{F32, F64};
use leafcommon::ty::IntTy::{Isize, I128, I16, I32, I64, I8};
use leafcommon::ty::UintTy::{Usize, U128, U16, U32, U64, U8};
use leafcommon::ty::{IntTy, Ty, TyKind};
use paste::paste;
use std::collections::{HashMap, HashSet};
use std::ops::Deref;
use std::str::FromStr;
use std::sync::Mutex;
use std::{concat, stringify};
use z3;
use z3::ast::{Ast, Int};
use z3::SatResult;

mod expression;
mod utils;

struct Config(z3::Config);

unsafe impl std::marker::Sync for Config {}

impl Config {
    fn new() -> Config {
        Config(z3::Config::new())
    }
}

#[derive(Debug)]
struct Context(z3::Context);

unsafe impl std::marker::Sync for Context {}

impl Context {
    fn new(cfg: &Config) -> Context {
        Context(z3::Context::new(&cfg.0))
    }
}

struct Solver<'a>(z3::Solver<'a>);

unsafe impl<'a> std::marker::Sync for Solver<'a> {}

impl<'a> Solver<'a> {
    fn new(ctx: &Context) -> Solver {
        Solver(z3::Solver::new(&ctx.0))
    }
}

lazy_static! {
    //static ref CFG: Z3_config = z3_sys::Z3_mk_config();
    //static ref CTX: Z3_context = z3_sys::Z3_mk_context(CFG);
    //static ref SOLVER: Z3_solver = z3_sys::Z3_mk_solver(CTX);
    static ref CFG: Config = Config::new();
    static ref CTX: Context = Context::new(&CFG);
    static ref SOLVER: Solver<'static> = Solver::new(&CTX);

    /// Keeps track of the function call stack. When we return, we can pop the information
    /// about the function we returned from.
    static ref FUNCTION_CALL_STACK: Mutex<FunctionCallStack<'static>> = Mutex::new(FunctionCallStack::new());
}

pub fn switch_int(basic_block_idx: u32, discr: &str, switch_targets: &str) {
    let discr: Operand = discr.try_into().unwrap();
    let switch_targets: SwitchTargets = switch_targets.try_into().unwrap();
    let mut stack = FUNCTION_CALL_STACK.lock().unwrap();
    println!("[switch_int] bb{basic_block_idx:?}, discr: {discr:?} switch_targets: {switch_targets:?}");

    stack.handle_switch_int(&SOLVER, discr, switch_targets);
}

pub fn ret(basic_block_idx: u32) {
    let mut stack = FUNCTION_CALL_STACK.lock().unwrap();
    stack.handle_ret(&CTX, basic_block_idx);
}

pub fn call(
    basic_block_idx: u32,
    func_debug_info: &str,
    func: &str,
    func_return_type: &str,
    args: &str,
    const_arg_values: &str,
    dest_and_debug_info: &str,
) {
    let func_debug_info: DebugInfo = func_debug_info.try_into().unwrap();
    let func: Operand = func.try_into().unwrap();
    let func_return_type: Ty = func_return_type.try_into().unwrap();
    let args: OperandVec = args.try_into().unwrap();
    let const_arg_values: OperandConstValueVec = const_arg_values.try_into().unwrap();
    assert_eq!(const_arg_values.0.len(), args.0.len());
    let destination_and_debug_info: PlaceAndDebugInfo = dest_and_debug_info.try_into().unwrap();
    println!("[call] bb{basic_block_idx:?}, func_debug_info: {func_debug_info:?} func: {func:?} func_return_type: {func_return_type:?} args: {args:?} const_arg_values: {const_arg_values:?} dest: {destination_and_debug_info:?}");

    FUNCTION_CALL_STACK.lock().unwrap().handle_fn_call(
        &CTX,
        basic_block_idx,
        func_debug_info,
        func_return_type,
        destination_and_debug_info,
        args,
        const_arg_values,
    )
}

pub fn assign(basic_block_idx: u32, place_and_debug_info: &str, rvalue: &str) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();
    println!("[assign] bb{basic_block_idx:?}, {place_and_debug_info:?} rvalue: {rvalue:?}");
    let ty: Option<TyKind> = rvalue.clone().try_into().ok();
    handle_place(
        basic_block_idx,
        &place_and_debug_info,
        Some(&rvalue),
        ty,
        None,
    );
}

macro_rules! assign_fn {
    ($t:ty) => {
        paste! {
            pub fn [< assign_ $t >](basic_block_idx: u32, place_and_debug_info: &str, rvalue: &str, constant: $t) {
                let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
                let rvalue: Rvalue = rvalue.try_into().unwrap();
                println!(
                    concat!(
                        "[assign_",
                        stringify!($t),
                        "] bb{}, {:?} rvalue: {:?} constant: {:?}"
                    ),
                    basic_block_idx,
                    place_and_debug_info,
                    rvalue,
                    constant
                );
                handle_place(
                    basic_block_idx,
                    &place_and_debug_info,
                    Some(&rvalue),
                    Some(TyKind::[<for_ $t>]()),
                    Some(constant.to_string()),
                )
            }
        }
    };
}

assign_fn!(isize);
assign_fn!(i8);
assign_fn!(i16);
assign_fn!(i32);
assign_fn!(i64);
assign_fn!(usize);
assign_fn!(u8);
assign_fn!(u16);
assign_fn!(u32);
assign_fn!(u64);
assign_fn!(f32);
assign_fn!(f64);
assign_fn!(char);
assign_fn!(bool);

pub fn assign_str(basic_block_idx: u32, place_and_debug_info: &str, rvalue: &str, constant: &str) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_str] bb{basic_block_idx:?}, {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(
        basic_block_idx,
        &place_and_debug_info,
        Some(&rvalue),
        Some(TyKind::Str),
        Some(constant.to_string()),
    );
}

/// Common function for handling places. Pass in [rvalue] for assign statements to be able to
/// detect `Ref`s / `Copy`s / `Move`s of symbolic variables and propagate their status.
fn handle_place(
    basic_block_idx: u32,
    place_and_debug_info: &PlaceAndDebugInfo,
    rvalue: Option<&Rvalue>,
    ty: Option<TyKind>,
    serialized_const_value: Option<String>,
) {
    let mut function_call_stack = FUNCTION_CALL_STACK.lock().unwrap();
    function_call_stack.handle_assign(
        &CTX,
        basic_block_idx,
        place_and_debug_info,
        rvalue,
        ty,
        serialized_const_value,
    );
}

pub fn drop(place_and_debug_info: &str) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let variable_removed = {
        // place_map.remove(&place)
        // TODO: Fix
        false
    };

    println!("[drop] {place_and_debug_info:?} variable_removed: {variable_removed:?}");
}
