#[macro_use]
extern crate lazy_static;

use leafcommon::place::{Local, Place};
use leafcommon::rvalue::{BinOp, Operand, OperandVec, Rvalue};
//use z3_sys::{Z3_config, Z3_context, Z3_solver};
//use z3::{Config, Context, Solver};
use leafcommon::misc::{DebugInfo, PlaceAndDebugInfo};
use leafcommon::switchtargets::SwitchTargets;
use leafcommon::ty::FloatTy::{F32, F64};
use leafcommon::ty::IntTy::{Isize, I128, I16, I32, I64, I8};
use leafcommon::ty::TyKind;
use leafcommon::ty::UintTy::{Usize, U128, U16, U32, U64, U8};
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

/// Contains an insertion result from an [Operand] insert. Since an Operand insert can use Move
#[derive(Debug, Eq, PartialEq)]
enum InsertResult {
    Noop,
    Inserted,
    NeedsRemoval(Local),
}

impl InsertResult {
    fn is_inserted(&self) -> bool {
        InsertResult::Noop == *self
    }
}

impl SymbolicVarMap {
    fn new() -> Self {
        Self {
            map: HashMap::default(),
        }
    }

    /// Returns true if `operand` is operating on a symbolic variable and `Copy` or `Move` is being
    /// done to a symbolic variable. True means the `new_place` was added.
    fn insert_from_operand(&mut self, new_place: &Place, operand: &Operand) -> InsertResult {
        match operand {
            Operand::Copy(source_place) => {
                // If this is a Copy of a symbolic variable, add the Copy as well to the symbolic
                // variables set
                if let Some(source) = self.get(source_place) {
                    if self.insert_if_absent(new_place.clone(), source.var_type.clone()) {
                        InsertResult::Inserted
                    } else {
                        InsertResult::Noop
                    }
                } else {
                    InsertResult::Noop
                }
            }
            Operand::Move(source_place) => {
                if let Some(source) = self.get(source_place) {
                    self.insert_if_absent(new_place.clone(), source.var_type.clone());
                    InsertResult::NeedsRemoval(source_place.local.clone())
                } else {
                    InsertResult::Noop
                }
            }
            Operand::Constant(_) => InsertResult::Noop,
        }
    }

    fn process_insert_result(&mut self, insert_result: &InsertResult) {
        if let InsertResult::NeedsRemoval(key) = *insert_result {
            self.map.remove(&key);
        }
    }

    fn insert_place_rvalue_pair(&mut self, place: &Place, rvalue: &Rvalue) -> bool {
        // TODO: Handle other Rvalue variants
        let is_rvalue_on_symbolic_variable = match rvalue {
            Rvalue::Use(ref use_operand) => {
                let insert_result = self.insert_from_operand(place, use_operand);
                self.process_insert_result(&insert_result);
                insert_result.is_inserted()
            }
            Rvalue::Ref(_, ref source_place) => {
                // Add references to symbolic variables to the set.
                self.insert_ref(source_place, place.clone())
            }
            Rvalue::BinaryOp(op, b) => {
                let (left, right) = &**b;
                let left_insert_result = self.insert_from_operand(place, left);
                let right_insert_result = self.insert_from_operand(place, right);
                let any_symbolic_variables =
                    left_insert_result.is_inserted() || right_insert_result.is_inserted();
                if any_symbolic_variables {
                    match op {
                        BinOp::Add => {}
                        BinOp::Sub => {}
                        BinOp::Mul => {}
                        BinOp::Div => {}
                        BinOp::Rem => {}
                        BinOp::BitXor => {}
                        BinOp::BitAnd => {}
                        BinOp::BitOr => {}
                        BinOp::Shl => {}
                        BinOp::Shr => {}
                        BinOp::Eq => {}
                        BinOp::Lt => {}
                        BinOp::Le => {}
                        BinOp::Ne => {}
                        BinOp::Ge => {}
                        BinOp::Gt => {}
                        BinOp::Offset => {}
                    }
                }
                self.process_insert_result(&left_insert_result);
                self.process_insert_result(&right_insert_result);
                any_symbolic_variables
            }
            _ => false,
        };

        if is_rvalue_on_symbolic_variable {
            if let Some(info) = self.get_mut(place) {
                info.insert_expression(place.clone(), rvalue.clone());
            }
        }

        return false;
    }

    /// Inserts a new symbolic variable place into the map.
    /// If the map did not have this key present, true is returned.
    /// If the map did have this key present, the value is not updated, and falsei s returned
    fn insert_if_absent(&mut self, place: Place, ty: TyKind) -> bool {
        if !self.map.contains_key(&place.local) {
            self.map.insert(place.local, SymbolicVarInfo::new(ty));
            true
        } else {
            false
        }
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

#[derive(Debug, Clone)]
struct SymbolicVarInfo {
    var_type: TyKind,
    references: HashSet<Local>,
    /// Map of `Place` to `Rvalue`s stored in those places.
    expressions: HashMap<Local, Rvalue>,
}

impl SymbolicVarInfo {
    fn new(var_type: TyKind) -> Self {
        Self {
            var_type,
            references: Default::default(),
            expressions: Default::default(),
        }
    }

    fn insert_ref(&mut self, reference: Place) -> bool {
        self.references.insert(reference.local)
    }

    fn insert_expression(&mut self, place: Place, rvalue: Rvalue) -> Option<Rvalue> {
        self.expressions.insert(place.local, rvalue)
    }
}

#[derive(Debug, Clone)]
enum FunctionCallContext {
    NoReturn,
    WithReturn {
        last_seen_type: Option<TyKind>,
        destination: PlaceAndDebugInfo,
    },
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
    /// Keeps track of the function call stack. When we return, we can pop the information
    /// about the function we returned from.
    static ref FUNCTION_CALL_CTX: Mutex<Vec<FunctionCallContext>> = Mutex::new(Vec::new());
}

pub fn switch_int(discr: &str, switch_targets: &str) {
    let discr: Operand = discr.try_into().unwrap();
    let switch_targets: SwitchTargets = switch_targets.try_into().unwrap();
    println!("[switch_int] discr: {discr:?} switch_targets: {switch_targets:?}");
}

pub fn ret() {
    let latest_ctx = { FUNCTION_CALL_CTX.lock().unwrap().pop() };
    if let Some(FunctionCallContext::WithReturn {
        last_seen_type,
        destination,
    }) = latest_ctx
    {
        handle_place_with_function_call_ctx(&destination, None, last_seen_type, true);
    }

    println!(
        "[ret] SYMBOLIC_VARIABLES = {:?}",
        SYMBOLIC_VARIABLES.lock().unwrap()
    );
}

pub fn call(func_debug_info: &str, func: &str, args: &str, dest_and_debug_info: &str) {
    let func_debug_info: DebugInfo = func_debug_info.try_into().unwrap();
    let func: Operand = func.try_into().unwrap();
    let func_return_type: TyKind = func.clone().try_into().unwrap();
    let args: OperandVec = args.try_into().unwrap();
    let destination_and_debug_info: PlaceAndDebugInfo = dest_and_debug_info.try_into().unwrap();
    println!("[call] func_debug_info: {func_debug_info:?} func: {func:?} args: {args:?} dest: {destination_and_debug_info:?}");

    // handle_place(&destination_and_debug_info, None, Some(func_return_type));
    FUNCTION_CALL_CTX
        .lock()
        .unwrap()
        .push(if destination_and_debug_info.place.is_some() {
            FunctionCallContext::WithReturn {
                last_seen_type: None,
                destination: destination_and_debug_info,
            }
        } else {
            FunctionCallContext::NoReturn
        })
}

pub fn assign(place_and_debug_info: &str, rvalue: &str) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();
    println!("[assign] {place_and_debug_info:?} rvalue: {rvalue:?}");
    let ty: Option<TyKind> = rvalue.clone().try_into().ok();
    handle_place(&place_and_debug_info, Some(&rvalue), ty);
}

pub fn assign_isize(place_and_debug_info: &str, rvalue: &str, constant: isize) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_isize] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(
        &place_and_debug_info,
        Some(&rvalue),
        Some(TyKind::Int(Isize)),
    )
}

pub fn assign_i8(place_and_debug_info: &str, rvalue: &str, constant: i8) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_i8] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&place_and_debug_info, Some(&rvalue), Some(TyKind::Int(I8)))
}

pub fn assign_i16(place_and_debug_info: &str, rvalue: &str, constant: i16) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_i16] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&place_and_debug_info, Some(&rvalue), Some(TyKind::Int(I16)))
}

pub fn assign_i32(place_and_debug_info: &str, rvalue: &str, constant: i32) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_i32] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&place_and_debug_info, Some(&rvalue), Some(TyKind::Int(I32)))
}

pub fn assign_i64(place_and_debug_info: &str, rvalue: &str, constant: i64) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_i64] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&place_and_debug_info, Some(&rvalue), Some(TyKind::Int(I64)))
}

pub fn assign_i128(place_and_debug_info: &str, rvalue: &str, constant: i128) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_i128] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(
        &place_and_debug_info,
        Some(&rvalue),
        Some(TyKind::Int(I128)),
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
    );
}

pub fn assign_u8(place_and_debug_info: &str, rvalue: &str, constant: u8) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_u8] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&place_and_debug_info, Some(&rvalue), Some(TyKind::Uint(U8)))
}

pub fn assign_u16(place_and_debug_info: &str, rvalue: &str, constant: u16) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_u16] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(
        &place_and_debug_info,
        Some(&rvalue),
        Some(TyKind::Uint(U16)),
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
    )
}

pub fn assign_char(place_and_debug_info: &str, rvalue: &str, constant: char) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_char] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&place_and_debug_info, Some(&rvalue), Some(TyKind::Char))
}

pub fn assign_bool(place_and_debug_info: &str, rvalue: &str, constant: bool) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_bool] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&place_and_debug_info, Some(&rvalue), Some(TyKind::Bool));
}

pub fn assign_str(place_and_debug_info: &str, rvalue: &str, constant: &str) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign_str] {place_and_debug_info:?} rvalue: {rvalue:?} constant: {constant:?}");
    handle_place(&place_and_debug_info, Some(&rvalue), Some(TyKind::Str));
}

fn handle_place(
    place_and_debug_info: &PlaceAndDebugInfo,
    rvalue: Option<&Rvalue>,
    ty: Option<TyKind>,
) {
    handle_place_with_function_call_ctx(place_and_debug_info, rvalue, ty, false)
}

/// Common function for handling places. Pass in [rvalue] for assign statements to be able to
/// detect `Ref`s / `Copy`s / `Move`s of symbolic variables and propagate their status.
fn handle_place_with_function_call_ctx(
    place_and_debug_info: &PlaceAndDebugInfo,
    rvalue: Option<&Rvalue>,
    ty: Option<TyKind>,
    is_return: bool,
) {
    if !is_return {
        if let Some(ref ty) = ty {
            let mut function_call_ctx = FUNCTION_CALL_CTX.lock().unwrap();
            if let Some(FunctionCallContext::WithReturn { last_seen_type, .. }) =
                function_call_ctx.last_mut()
            {
                *last_seen_type = Some(ty.clone());
            }
        }
    }

    let mut symbolic_variables_set = SYMBOLIC_VARIABLES.lock().unwrap();
    let place = if let Some(ref place) = place_and_debug_info.place {
        place
    } else {
        return;
    };

    if let Some(rvalue) = rvalue {
        symbolic_variables_set.insert_place_rvalue_pair(place, rvalue);
    // TODO: Add a proper way for denoting symbolic variables instead of just reading the
    //  variable name.
    } else if let Some(ref debug_info) = place_and_debug_info.debug_info {
        if debug_info
            .name
            .as_ref()
            .map(|n| n.to_lowercase().contains("leaf_symbolic"))
            .unwrap_or(false)
        {
            symbolic_variables_set
                .insert_if_absent(place.clone(), ty.expect("type needed for first variable"));
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
