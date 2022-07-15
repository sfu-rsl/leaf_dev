#[macro_use]
extern crate lazy_static;

use leafcommon::place::Place;
use leafcommon::rvalue::{Constant, Operand, OperandVec, Rvalue};
//use z3_sys::{Z3_config, Z3_context, Z3_solver};
//use z3::{Config, Context, Solver};
use leafcommon::misc::DebugInfo;
use std::collections::HashSet;
use std::sync::Mutex;
use z3;

struct Config(z3::Config);

unsafe impl std::marker::Sync for Config {}

impl Config {
    fn new() -> Config {
        Config(z3::Config::new())
    }
}

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

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
enum SymbolicVariable {
    Place(Place),
    Constant(Constant),
}

lazy_static! {
    //static ref CFG: Z3_config = z3_sys::Z3_mk_config();
    //static ref CTX: Z3_context = z3_sys::Z3_mk_context(CFG);
    //static ref SOLVER: Z3_solver = z3_sys::Z3_mk_solver(CTX);
    static ref CFG: Config = Config::new();
    static ref CTX: Context = Context::new(&CFG);
    static ref SOLVER: Solver<'static> = Solver::new(&CTX);

    /// Keep track of places in memory that are known to be symbolic.
    /// TODO: Keep track of drops to remove from the set?
    static ref SYMBOLIC_VARIABLES: Mutex<HashSet<SymbolicVariable>> = Mutex::new(HashSet::new());
}

pub fn switch_int(discr: &str) {
    let discr: Operand = discr.try_into().unwrap();
    println!("[switch_int] discr: {discr:?}");
}

pub fn ret() {
    println!(
        "[ret] SYMBOLIC_VARIABLES = {:?}",
        SYMBOLIC_VARIABLES.lock().unwrap()
    );
}

pub fn call(debug_info: &str, func: &str, args: &str, destination: &str) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let func: Operand = func.try_into().unwrap();
    let args: OperandVec = args.try_into().unwrap();
    let destination: Place = destination.try_into().unwrap();
    println!("[call] func: {func:?} args: {args:?} destination: {destination:?} destination_debug_info: {debug_info:?}");
    handle_place(&debug_info, &destination)
}

pub fn assign(debug_info: &str, place: &str, rvalue: &str) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign] debug_info: {debug_info:?} place: {place:?} rvalue: {rvalue:?}");
    handle_place(&debug_info, &place)
}

pub fn assign_isize(debug_info: &str, place: &str, rvalue: &str, constant: isize) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_isize] {place:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&debug_info, &place)
}

pub fn assign_i8(debug_info: &str, place: &str, rvalue: &str, constant: i8) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_i8] debug_info: {debug_info:?} place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&debug_info, &place)
}

pub fn assign_i16(debug_info: &str, place: &str, rvalue: &str, constant: i16) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_i16] debug_info: {debug_info:?} place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&debug_info, &place)
}

pub fn assign_i32(debug_info: &str, place: &str, rvalue: &str, constant: i32) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_i32] debug_info: {debug_info:?} place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&debug_info, &place)
}

pub fn assign_i64(debug_info: &str, place: &str, rvalue: &str, constant: i64) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_i64] debug_info: {debug_info:?} place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&debug_info, &place)
}

pub fn assign_i128(debug_info: &str, place: &str, rvalue: &str, constant: i128) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_i128] debug_info: {debug_info:?} place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&debug_info, &place)
}

pub fn assign_usize(debug_info: &str, place: &str, rvalue: &str, constant: usize) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_usize] debug_info: {debug_info:?} place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&debug_info, &place)
}

pub fn assign_u8(debug_info: &str, place: &str, rvalue: &str, constant: u8) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_u8] debug_info: {debug_info:?} place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&debug_info, &place)
}

pub fn assign_u16(debug_info: &str, place: &str, rvalue: &str, constant: u16) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_u16] debug_info: {debug_info:?} place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&debug_info, &place)
}

pub fn assign_u32(debug_info: &str, place: &str, rvalue: &str, constant: u32) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_u32] debug_info: {debug_info:?} place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&debug_info, &place)
}

pub fn assign_u64(debug_info: &str, place: &str, rvalue: &str, constant: u64) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_u64] debug_info: {debug_info:?} place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&debug_info, &place)
}

pub fn assign_u128(debug_info: &str, place: &str, rvalue: &str, constant: u128) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_u128] debug_info: {debug_info:?} place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&debug_info, &place)
}

pub fn assign_f32(debug_info: &str, place: &str, rvalue: &str, constant: f32) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_f32] debug_info: {debug_info:?} place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&debug_info, &place)
}

pub fn assign_f64(debug_info: &str, place: &str, rvalue: &str, constant: f64) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_f64] debug_info: {debug_info:?} place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&debug_info, &place)
}

pub fn assign_char(debug_info: &str, place: &str, rvalue: &str, constant: char) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_char] debug_info: {debug_info:?} place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&debug_info, &place)
}

pub fn assign_bool(debug_info: &str, place: &str, rvalue: &str, constant: bool) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_bool] debug_info: {debug_info:?} place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&debug_info, &place)
}

pub fn assign_str(debug_info: &str, place: &str, rvalue: &str, constant: &str) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_str] debug_info: {debug_info:?} place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&debug_info, &place)
}

/// Common function for all assign statements
fn handle_place(debug_info: &DebugInfo, place: &Place) {
    // TODO: Add a proper mechanism for denoting symbolic variables instead of just reading the
    //  variable name.
    let mut symbolic_variables_set = SYMBOLIC_VARIABLES.lock().unwrap();
    if symbolic_variables_set.contains(&SymbolicVariable::Place(place.clone()))
        || debug_info
            .variable_name
            .to_lowercase()
            .contains("leaf_symbolic")
    {
        symbolic_variables_set.insert(SymbolicVariable::Place(place.clone()));
    }
}
