#[macro_use]
extern crate lazy_static;

use leafcommon::place::{Local, Place};
use leafcommon::rvalue::{Operand, OperandVec, Rvalue};
//use z3_sys::{Z3_config, Z3_context, Z3_solver};
//use z3::{Config, Context, Solver};
use leafcommon::misc::{DebugInfo, PlaceAndDebugInfo};
use leafcommon::switchtargets::SwitchTargets;
use std::collections::{HashMap, HashSet};
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

#[derive(Debug, Clone)]
struct SymbolicVarMap {
    map: HashMap<Local, SymbolicVarInfo>,
}

impl SymbolicVarMap {
    fn new() -> Self {
        Self {
            map: HashMap::default(),
        }
    }

    /// Inserts a key-value pair into the map.
    /// If the map did not have this key present, `None` is returned.
    /// If the map did have this key present, the value is updated, and the old value is returned.
    fn insert(&mut self, place: Place) -> Option<SymbolicVarInfo> {
        self.map
            .insert(place.local.clone(), SymbolicVarInfo::default())
    }

    /// Inserts a `reference` to a `source` Place. Returns true iff insertion was successful
    fn insert_ref(&mut self, source: &Place, reference: Place) -> bool {
        self.get_mut(source)
            .map_or(false, |info| info.insert_ref(reference))
    }

    fn get(&self, place: &Place) -> Option<&SymbolicVarInfo> {
        self.map.get(&place.local)
    }

    fn get_mut(&mut self, place: &Place) -> Option<&mut SymbolicVarInfo> {
        self.map.get_mut(&place.local)
    }

    fn contains(&self, place: &Place) -> bool {
        self.map.contains_key(&place.local)
    }

    fn remove(&mut self, place: &Place) -> Option<SymbolicVarInfo> {
        self.map.remove(&place.local)
    }
}

impl Default for SymbolicVarMap {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Default)]
struct SymbolicVarInfo {
    references: HashSet<Local>,
}

impl SymbolicVarInfo {
    fn insert_ref(&mut self, reference: Place) -> bool {
        self.references.insert(reference.local)
    }
}

lazy_static! {
    //static ref CFG: Z3_config = z3_sys::Z3_mk_config();
    //static ref CTX: Z3_context = z3_sys::Z3_mk_context(CFG);
    //static ref SOLVER: Z3_solver = z3_sys::Z3_mk_solver(CTX);
    static ref CFG: Config = Config::new();
    static ref CTX: Context = Context::new(&CFG);
    static ref SOLVER: Solver<'static> = Solver::new(&CTX);

    /// Keep track of places in memory that are known to be symbolic.
    static ref SYMBOLIC_VARIABLES: Mutex<SymbolicVarMap> = Mutex::new(SymbolicVarMap::new());
}

pub fn switch_int(discr: &str, switch_targets: &str) {
    let discr: Operand = discr.try_into().unwrap();
    let switch_targets: SwitchTargets = switch_targets.try_into().unwrap();
    println!("[switch_int] discr: {discr:?} switch_targets: {switch_targets:?}");
}

pub fn ret() {
    println!(
        "[ret] SYMBOLIC_VARIABLES = {:?}",
        SYMBOLIC_VARIABLES.lock().unwrap()
    );
}

pub fn call(func_debug_info: &str, func: &str, args: &str, dest_and_debug_info: &str) {
    let func_debug_info: DebugInfo = func_debug_info.try_into().unwrap();
    let func: Operand = func.try_into().unwrap();
    let args: OperandVec = args.try_into().unwrap();
    let destination_and_debug_info: PlaceAndDebugInfo = dest_and_debug_info.try_into().unwrap();
    println!("[call] func_debug_info: {func_debug_info:?} func: {func:?} args: {args:?} dest: {destination_and_debug_info:?}");
    handle_place(&destination_and_debug_info, None);
}

pub fn assign(place_and_debug_info: &str, rvalue: &str) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign] {place_and_debug_info:?} rvalue: {rvalue:?}");
    handle_place(&place_and_debug_info, Some(&rvalue));
}

pub fn assign_isize(place_and_debug_info: &str, rvalue: &str, constant: isize) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_isize] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&place_and_debug_info, Some(&rvalue));
}

pub fn assign_i8(place_and_debug_info: &str, rvalue: &str, constant: i8) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_i8] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&place_and_debug_info, Some(&rvalue));
}

pub fn assign_i16(place_and_debug_info: &str, rvalue: &str, constant: i16) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_i16] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&place_and_debug_info, Some(&rvalue));
}

pub fn assign_i32(place_and_debug_info: &str, rvalue: &str, constant: i32) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_i32] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&place_and_debug_info, Some(&rvalue))
}

pub fn assign_i64(place_and_debug_info: &str, rvalue: &str, constant: i64) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_i64] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&place_and_debug_info, Some(&rvalue));
}

pub fn assign_i128(place_and_debug_info: &str, rvalue: &str, constant: i128) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_i128] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&place_and_debug_info, Some(&rvalue));
}

pub fn assign_usize(place_and_debug_info: &str, rvalue: &str, constant: usize) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_usize] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&place_and_debug_info, Some(&rvalue));
}

pub fn assign_u8(place_and_debug_info: &str, rvalue: &str, constant: u8) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_u8] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&place_and_debug_info, Some(&rvalue))
}

pub fn assign_u16(place_and_debug_info: &str, rvalue: &str, constant: u16) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_u16] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&place_and_debug_info, Some(&rvalue))
}

pub fn assign_u32(place_and_debug_info: &str, rvalue: &str, constant: u32) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_u32] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&place_and_debug_info, Some(&rvalue))
}

pub fn assign_u64(place_and_debug_info: &str, rvalue: &str, constant: u64) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_u64] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&place_and_debug_info, Some(&rvalue))
}

pub fn assign_u128(place_and_debug_info: &str, rvalue: &str, constant: u128) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_u128] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&place_and_debug_info, Some(&rvalue))
}

pub fn assign_f32(place_and_debug_info: &str, rvalue: &str, constant: f32) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_f32] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&place_and_debug_info, Some(&rvalue))
}

pub fn assign_f64(place_and_debug_info: &str, rvalue: &str, constant: f64) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_f64] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&place_and_debug_info, Some(&rvalue))
}

pub fn assign_char(place_and_debug_info: &str, rvalue: &str, constant: char) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_char] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&place_and_debug_info, Some(&rvalue))
}

pub fn assign_bool(place_and_debug_info: &str, rvalue: &str, constant: bool) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_bool] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&place_and_debug_info, Some(&rvalue));
}

pub fn assign_str(place_and_debug_info: &str, rvalue: &str, constant: &str) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_str] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&place_and_debug_info, Some(&rvalue));
}

/// Common function for handling places. Pass in [rvalue] for assign statements to be able to
/// detect `Ref`s / `Copy`s / `Move`s of symbolic variables and propagate their status.
fn handle_place(place_and_debug_info: &PlaceAndDebugInfo, rvalue: Option<&Rvalue>) {
    let mut symbolic_variables_set = SYMBOLIC_VARIABLES.lock().unwrap();
    let place = if let Some(ref place) = place_and_debug_info.place {
        place
    } else {
        return;
    };

    if let Some(rvalue) = rvalue {
        match *rvalue {
            Rvalue::Use(ref use_operand) => match use_operand {
                Operand::Copy(source_place) => {
                    if symbolic_variables_set.contains(&source_place) {
                        symbolic_variables_set.insert(place.clone());
                    }
                }
                Operand::Move(source_place) => {
                    if symbolic_variables_set.remove(&source_place).is_some() {
                        symbolic_variables_set.insert(place.clone());
                    }
                }
                Operand::Constant(_) => {}
            },
            Rvalue::Repeat(_, _) => {}
            Rvalue::Ref(_, ref source_place) => {
                if symbolic_variables_set.contains(&source_place) {
                    // Add references to symbolic variables to the set
                    symbolic_variables_set.insert_ref(source_place, place.clone());
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
    } else if let Some(ref debug_info) = place_and_debug_info.debug_info {
        if debug_info
            .name
            .as_ref()
            .map(|n| n.to_lowercase().contains("leaf_symbolic"))
            .unwrap_or(false)
        {
            symbolic_variables_set.insert(place.clone());
        }
    }
}

pub fn drop(place_and_debug_info: &str) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let place = if let Some(ref place) = place_and_debug_info.place {
        place
    } else {
        return;
    };

    let symbolic_variable_removed = {
        let mut symbolic_variables_set = SYMBOLIC_VARIABLES.lock().unwrap();
        symbolic_variables_set.remove(&place)
    };

    println!(
        "[drop] {place_and_debug_info:?} symbolic_variable_removed: {symbolic_variable_removed:?}"
    );
}
