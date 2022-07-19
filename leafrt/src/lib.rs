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
    handle_place(&debug_info, &destination, None);
}

pub fn assign(debug_info: &str, place: &str, rvalue: &str) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign] debug_info: {debug_info:?} place: {place:?} rvalue: {rvalue:?}");
    handle_place(&debug_info, &place, Some(&rvalue));
}

pub fn assign_isize(debug_info: &str, place: &str, rvalue: &str, constant: isize) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_isize] {place:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&debug_info, &place, Some(&rvalue));
}

pub fn assign_i8(debug_info: &str, place: &str, rvalue: &str, constant: i8) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_i8] debug_info: {debug_info:?} place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&debug_info, &place, Some(&rvalue));
}

pub fn assign_i16(debug_info: &str, place: &str, rvalue: &str, constant: i16) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_i16] debug_info: {debug_info:?} place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&debug_info, &place, Some(&rvalue));
}

pub fn assign_i32(debug_info: &str, place: &str, rvalue: &str, constant: i32) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_i32] debug_info: {debug_info:?} place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&debug_info, &place, Some(&rvalue))
}

pub fn assign_i64(debug_info: &str, place: &str, rvalue: &str, constant: i64) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_i64] debug_info: {debug_info:?} place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&debug_info, &place, Some(&rvalue));
}

pub fn assign_i128(debug_info: &str, place: &str, rvalue: &str, constant: i128) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_i128] debug_info: {debug_info:?} place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&debug_info, &place, Some(&rvalue));
}

pub fn assign_usize(debug_info: &str, place: &str, rvalue: &str, constant: usize) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_usize] debug_info: {debug_info:?} place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&debug_info, &place, Some(&rvalue));
}

pub fn assign_u8(debug_info: &str, place: &str, rvalue: &str, constant: u8) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_u8] debug_info: {debug_info:?} place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&debug_info, &place, Some(&rvalue))
}

pub fn assign_u16(debug_info: &str, place: &str, rvalue: &str, constant: u16) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_u16] debug_info: {debug_info:?} place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&debug_info, &place, Some(&rvalue))
}

pub fn assign_u32(debug_info: &str, place: &str, rvalue: &str, constant: u32) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_u32] debug_info: {debug_info:?} place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&debug_info, &place, Some(&rvalue))
}

pub fn assign_u64(debug_info: &str, place: &str, rvalue: &str, constant: u64) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_u64] debug_info: {debug_info:?} place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&debug_info, &place, Some(&rvalue))
}

pub fn assign_u128(debug_info: &str, place: &str, rvalue: &str, constant: u128) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_u128] debug_info: {debug_info:?} place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&debug_info, &place, Some(&rvalue))
}

pub fn assign_f32(debug_info: &str, place: &str, rvalue: &str, constant: f32) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_f32] debug_info: {debug_info:?} place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&debug_info, &place, Some(&rvalue))
}

pub fn assign_f64(debug_info: &str, place: &str, rvalue: &str, constant: f64) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_f64] debug_info: {debug_info:?} place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&debug_info, &place, Some(&rvalue))
}

pub fn assign_char(debug_info: &str, place: &str, rvalue: &str, constant: char) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_char] debug_info: {debug_info:?} place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&debug_info, &place, Some(&rvalue))
}

pub fn assign_bool(debug_info: &str, place: &str, rvalue: &str, constant: bool) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_bool] debug_info: {debug_info:?} place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&debug_info, &place, Some(&rvalue));
}

pub fn assign_str(debug_info: &str, place: &str, rvalue: &str, constant: &str) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_str] debug_info: {debug_info:?} place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&debug_info, &place, Some(&rvalue));
}

/// Common function for handling places. Pass in [rvalue] for assign statements to be able to
/// detect `Ref`s / `Copy`s / `Move`s of symbolic variables and propagate their status.
fn handle_place(debug_info: &DebugInfo, place: &Place, rvalue: Option<&Rvalue>) {
    let mut symbolic_variables_set = SYMBOLIC_VARIABLES.lock().unwrap();

    if let Some(rvalue) = rvalue {
        match *rvalue {
            Rvalue::Use(ref use_operand) => match use_operand {
                Operand::Copy(cooy_place) => {
                    let old_place = SymbolicVariable::Place(cooy_place.clone());
                    if symbolic_variables_set.contains(&old_place) {
                        let new_place = SymbolicVariable::Place(place.clone());
                        symbolic_variables_set.insert(new_place);
                    }
                }
                Operand::Move(move_place) => {
                    let old_place = SymbolicVariable::Place(move_place.clone());
                    if symbolic_variables_set.remove(&old_place) {
                        let new_place = SymbolicVariable::Place(place.clone());
                        symbolic_variables_set.insert(new_place);
                    }
                }
                Operand::Constant(constant) => {
                    let old_constant = SymbolicVariable::Constant(*constant.clone());
                    if symbolic_variables_set.contains(&old_constant) {
                        let new_place = SymbolicVariable::Place(place.clone());
                        symbolic_variables_set.insert(new_place);
                    }
                }
            },
            Rvalue::Repeat(_, _) => {}
            Rvalue::Ref(_, ref ref_place) => {
                // TODO: Is ther a way to tell if a reference is expired
                let old_place = SymbolicVariable::Place(ref_place.clone());
                if symbolic_variables_set.contains(&old_place) {
                    let new_place = SymbolicVariable::Place(place.clone());
                    symbolic_variables_set.insert(new_place);
                }
            }
            Rvalue::ThreadLocalRef => {}
            Rvalue::AddressOf(_, _) => {}
            Rvalue::Len(_) => {}
            Rvalue::Cast(_, _, _) => {}
            Rvalue::BinaryOp(_, _) => {}
            Rvalue::CheckedBinaryOp(_, _) => {}
            Rvalue::NullaryOp(_, _) => {}
            Rvalue::UnaryOp(_, _) => {}
            Rvalue::Discriminant(_) => {}
            Rvalue::Aggregate(_, _) => {}
            Rvalue::ShallowInitBox(_, _) => {}
        }
    // TODO: Add a proper way for denoting symbolic variables instead of just reading the
    //  variable name.
    } else if debug_info
        .variable_name
        .as_ref()
        .map(|n| n.to_lowercase().contains("leaf_symbolic"))
        .unwrap_or(false)
    {
        symbolic_variables_set.insert(SymbolicVariable::Place(place.clone()));
    }
}

pub fn drop(debug_info: &str, place: &str) {
    let debug_info: DebugInfo = debug_info.try_into().unwrap();
    let place: Place = place.try_into().unwrap();

    let mut symbolic_variables_set = SYMBOLIC_VARIABLES.lock().unwrap();
    let symbolic_variable_removed =
        symbolic_variables_set.remove(&SymbolicVariable::Place(place.clone()));

    println!("[drop] debug_info: {debug_info:?} place: {place:?} symbolic_variable_removed: {symbolic_variable_removed:?}");
}
