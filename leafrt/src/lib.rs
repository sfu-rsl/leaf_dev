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
use std::collections::{HashMap, HashSet};
use std::ops::Deref;
use std::str::FromStr;
use std::sync::Mutex;
use z3;
use z3::ast::{Ast, Int};
use z3::SatResult;

mod expression;

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
    println!("[switch_int] discr: {discr:?} switch_targets: {switch_targets:?}, stack: {stack:?}");

    stack.handle_switch_int(&SOLVER, discr, switch_targets);
}

pub fn ret() {
    let mut stack = FUNCTION_CALL_STACK.lock().unwrap();
    stack.handle_ret(&CTX);
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
    println!("[call] func_debug_info: {func_debug_info:?} func: {func:?} func_return_type: {func_return_type:?} args: {args:?} const_arg_values: {const_arg_values:?} dest: {destination_and_debug_info:?}");

    FUNCTION_CALL_STACK.lock().unwrap().handle_fn_call(
        &CTX,
        func_debug_info,
        func_return_type,
        destination_and_debug_info,
        args,
        const_arg_values,
    )
}

pub fn assign(place_and_debug_info: &str, rvalue: &str) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();
    println!("[assign] {place_and_debug_info:?} rvalue: {rvalue:?}");
    let ty: Option<TyKind> = rvalue.clone().try_into().ok();
    handle_place(&place_and_debug_info, Some(&rvalue), ty, None);
}

pub fn assign_isize(place_and_debug_info: &str, rvalue: &str, constant: isize) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_isize] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(
        &place_and_debug_info,
        Some(&rvalue),
        Some(TyKind::Int(Isize)),
        Some(constant.to_string()),
    )
}

pub fn assign_i8(place_and_debug_info: &str, rvalue: &str, constant: i8) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_i8] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(
        &place_and_debug_info,
        Some(&rvalue),
        Some(TyKind::Int(I8)),
        Some(constant.to_string()),
    )
}

pub fn assign_i16(place_and_debug_info: &str, rvalue: &str, constant: i16) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_i16] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(
        &place_and_debug_info,
        Some(&rvalue),
        Some(TyKind::Int(I16)),
        Some(constant.to_string()),
    )
}

pub fn assign_i32(place_and_debug_info: &str, rvalue: &str, constant: i32) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_i32] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(
        &place_and_debug_info,
        Some(&rvalue),
        Some(TyKind::Int(I32)),
        Some(constant.to_string()),
    )
}

pub fn assign_i64(place_and_debug_info: &str, rvalue: &str, constant: i64) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_i64] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(
        &place_and_debug_info,
        Some(&rvalue),
        Some(TyKind::Int(I64)),
        Some(constant.to_string()),
    )
}

pub fn assign_i128(place_and_debug_info: &str, rvalue: &str, constant: i128) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_i128] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(
        &place_and_debug_info,
        Some(&rvalue),
        Some(TyKind::Int(I128)),
        Some(constant.to_string()),
    )
}

pub fn assign_usize(place_and_debug_info: &str, rvalue: &str, constant: usize) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_usize] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(
        &place_and_debug_info,
        Some(&rvalue),
        Some(TyKind::Uint(Usize)),
        Some(constant.to_string()),
    );
}

pub fn assign_u8(place_and_debug_info: &str, rvalue: &str, constant: u8) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_u8] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(
        &place_and_debug_info,
        Some(&rvalue),
        Some(TyKind::Uint(U8)),
        Some(constant.to_string()),
    )
}

pub fn assign_u16(place_and_debug_info: &str, rvalue: &str, constant: u16) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_u16] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(
        &place_and_debug_info,
        Some(&rvalue),
        Some(TyKind::Uint(U16)),
        Some(constant.to_string()),
    )
}

pub fn assign_u32(place_and_debug_info: &str, rvalue: &str, constant: u32) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_u32] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(
        &place_and_debug_info,
        Some(&rvalue),
        Some(TyKind::Uint(U32)),
        Some(constant.to_string()),
    )
}

pub fn assign_u64(place_and_debug_info: &str, rvalue: &str, constant: u64) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_u64] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(
        &place_and_debug_info,
        Some(&rvalue),
        Some(TyKind::Uint(U64)),
        Some(constant.to_string()),
    )
}

pub fn assign_u128(place_and_debug_info: &str, rvalue: &str, constant: u128) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_u128] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(
        &place_and_debug_info,
        Some(&rvalue),
        Some(TyKind::Uint(U128)),
        Some(constant.to_string()),
    )
}

pub fn assign_f32(place_and_debug_info: &str, rvalue: &str, constant: f32) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_f32] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(
        &place_and_debug_info,
        Some(&rvalue),
        Some(TyKind::Float(F32)),
        Some(constant.to_string()),
    )
}

pub fn assign_f64(place_and_debug_info: &str, rvalue: &str, constant: f64) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_f64] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(
        &place_and_debug_info,
        Some(&rvalue),
        Some(TyKind::Float(F64)),
        Some(constant.to_string()),
    )
}

pub fn assign_char(place_and_debug_info: &str, rvalue: &str, constant: char) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_char] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(
        &place_and_debug_info,
        Some(&rvalue),
        Some(TyKind::Char),
        Some(constant.to_string()),
    )
}

pub fn assign_bool(place_and_debug_info: &str, rvalue: &str, constant: bool) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_bool] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(
        &place_and_debug_info,
        Some(&rvalue),
        Some(TyKind::Bool),
        Some(constant.to_string()),
    );
}

pub fn assign_str(place_and_debug_info: &str, rvalue: &str, constant: &str) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_str] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(
        &place_and_debug_info,
        Some(&rvalue),
        Some(TyKind::Str),
        Some(constant.to_string()),
    );
}

/// Common function for handling places. Pass in [rvalue] for assign statements to be able to
/// detect `Ref`s / `Copy`s / `Move`s of symbolic variables and propagate their status.
fn handle_place(
    place_and_debug_info: &PlaceAndDebugInfo,
    rvalue: Option<&Rvalue>,
    ty: Option<TyKind>,
    serialized_const_value: Option<String>,
) {
    let mut function_call_stack = FUNCTION_CALL_STACK.lock().unwrap();
    function_call_stack.handle_assign(
        &CTX,
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
