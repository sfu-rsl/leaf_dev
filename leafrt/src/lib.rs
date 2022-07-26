#[macro_use]
extern crate lazy_static;

use leafcommon::place::{Local, Place};
use leafcommon::rvalue::{BinOp, Constant, ConstantKind, Operand, OperandVec, Rvalue};
use std::borrow::Borrow;
//use z3_sys::{Z3_config, Z3_context, Z3_solver};
//use z3::{Config, Context, Solver};
use leafcommon::misc::{DebugInfo, PlaceAndDebugInfo};
use leafcommon::switchtargets::SwitchTargets;
use leafcommon::ty::FloatTy::{F32, F64};
use leafcommon::ty::IntTy::{Isize, I128, I16, I32, I64, I8};
use leafcommon::ty::UintTy::{Usize, U128, U16, U32, U64, U8};
use leafcommon::ty::{IntTy, TyKind};
use std::collections::{HashMap, HashSet};
use std::ops::Deref;
use std::str::FromStr;
use std::sync::Mutex;
use z3;
use z3::ast::{Ast, Int};
use z3::SatResult;

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
struct PlaceMap {
    map: HashMap<Local, VarInfo>,
}

/// Contains an insertion result from an [Operand] insert. Since an Operand insert can use Move
#[derive(Debug, Eq, PartialEq)]
enum InsertResult {
    Noop(bool),
    Inserted(bool),
    NeedsRemoval(bool, Local),
}

impl InsertResult {
    fn is_inserted(&self) -> bool {
        if let InsertResult::Noop(_) = self {
            true
        } else {
            false
        }
    }

    fn is_symbolic(&self) -> bool {
        match self {
            InsertResult::Noop(s) => *s,
            InsertResult::Inserted(s) => *s,
            InsertResult::NeedsRemoval(s, _) => *s,
        }
    }
}

impl PlaceMap {
    fn new() -> Self {
        Self {
            map: HashMap::default(),
        }
    }

    /// Returns true if `operand` is operating on a variable and `Copy` or `Move` is being
    /// done to a variable. True means the `new_place` was added.
    fn insert_from_operand(
        &mut self,
        new_place: Option<&Place>,
        new_place_debug_info: Option<DebugInfo>,
        serialized_const_val: Option<String>,
        operand: &Operand,
    ) -> InsertResult {
        match operand {
            Operand::Copy(source_place) => {
                // If this is a Copy of a symbolic variable, add the Copy as well to the symbolic
                // variables set
                if let (Some(source), Some(new_place)) = (self.get(source_place), new_place) {
                    let source_symbolic = source.is_symbolic;
                    if self.insert(
                        new_place.clone(),
                        source.var_type.clone(),
                        serialized_const_val,
                        new_place_debug_info,
                        source_symbolic,
                    ) {
                        InsertResult::Inserted(source_symbolic)
                    } else {
                        InsertResult::Noop(source_symbolic)
                    }
                } else {
                    InsertResult::Noop(false)
                }
            }
            Operand::Move(source_place) => {
                if let Some(source) = self.get(source_place) {
                    let source_symbolic = source.is_symbolic;
                    // TODO: Handling Move via function argument
                    if let Some(new_place) = new_place {
                        self.insert(
                            new_place.clone(),
                            source.var_type.clone(),
                            serialized_const_val,
                            new_place_debug_info,
                            source_symbolic,
                        );
                    }
                    InsertResult::NeedsRemoval(source_symbolic, source_place.local.clone())
                } else {
                    InsertResult::Noop(false)
                }
            }
            Operand::Constant(c) => {
                if let Some(new_place) = new_place {
                    let c = &**c;
                    match &c.literal {
                        // TODO: Handle other types
                        ConstantKind::Ty(_) => InsertResult::Noop(false),
                        ConstantKind::Val(_, ty) => {
                            if self.insert(
                                new_place.clone(),
                                ty.kind().try_into().unwrap(),
                                serialized_const_val,
                                new_place_debug_info,
                                false,
                            ) {
                                InsertResult::Inserted(false)
                            } else {
                                InsertResult::Noop(false)
                            }
                        }
                    }
                } else {
                    InsertResult::Noop(false)
                }
            }
        }
    }

    fn process_insert_result(&mut self, insert_result: &InsertResult) {
        if let InsertResult::NeedsRemoval(_, key) = *insert_result {
            // FIXME: Removing this now ruins values that need evaluation later
            // self.map.remove(&key);
        }
    }

    fn insert_place_rvalue_pair(
        &mut self,
        place: &Place,
        place_debug_info: Option<DebugInfo>,
        serialized_const_value: Option<String>,
        rvalue: &Rvalue,
    ) -> bool {
        // TODO: Handle other Rvalue variants
        let is_rvalue_on_symbolic_variable = match rvalue {
            Rvalue::Use(ref use_operand) => {
                let insert_result = self.insert_from_operand(
                    Some(place),
                    place_debug_info,
                    serialized_const_value,
                    use_operand,
                );
                self.process_insert_result(&insert_result);
                insert_result.is_symbolic()
            }
            Rvalue::Ref(_, ref source_place) => {
                // Add references to symbolic variables to the set.
                self.insert_ref(source_place, place.clone());
                if let Some(info) = self.get(source_place) {
                    info.is_symbolic
                } else {
                    false
                }
            }
            Rvalue::BinaryOp(op, b) => {
                let (left, right) = &**b;
                let left_insert_result =
                    self.insert_from_operand(Some(place), place_debug_info.clone(), None, left);
                let right_insert_result =
                    self.insert_from_operand(Some(place), place_debug_info, None, right);
                let any_symbolic_variables =
                    left_insert_result.is_symbolic() || right_insert_result.is_symbolic();
                if any_symbolic_variables {
                    if let Some(info) = self.get_mut(place) {
                        info.is_symbolic = true
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
    /// If the map did have this key present, the value is not updated, and false is returned
    fn insert(
        &mut self,
        place: Place,
        ty: TyKind,
        serialized_const_value: Option<String>,
        debug_info: Option<DebugInfo>,
        is_symbolic: bool,
    ) -> bool {
        let key_contained_before = if !self.map.contains_key(&place.local) {
            true
        } else {
            false
        };
        self.map.insert(
            place.local,
            VarInfo::new(is_symbolic, ty, serialized_const_value, debug_info),
        );
        key_contained_before
    }

    /// Inserts a `reference` to a `source` Place. Returns true iff insertion was successful
    fn insert_ref(&mut self, source: &Place, reference: Place) -> bool {
        self.get_mut(source)
            .map_or(false, |info| info.insert_ref(reference))
    }

    fn get(&self, place: &Place) -> Option<&VarInfo> {
        self.map.get(&place.local)
    }

    fn get_mut(&mut self, place: &Place) -> Option<&mut VarInfo> {
        self.map.get_mut(&place.local)
    }

    fn contains(&self, place: &Place) -> bool {
        self.map.contains_key(&place.local)
    }

    fn remove(&mut self, place: &Place) -> Option<VarInfo> {
        self.map.remove(&place.local)
    }
}

impl Default for PlaceMap {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
struct VarInfo {
    is_symbolic: bool,
    var_type: TyKind,
    /// If this Place is for a constant, then this holds the serialized value of the constant
    serialized_const_value: Option<String>,
    debug_info: Option<DebugInfo>,
    references: HashSet<Local>,
    /// Map of `Place` to `Rvalue`s stored in those places.
    expressions: HashMap<Local, Rvalue>,
}

impl VarInfo {
    fn new(
        is_symbolic: bool,
        var_type: TyKind,
        serialized_const_value: Option<String>,
        debug_info: Option<DebugInfo>,
    ) -> Self {
        Self {
            is_symbolic,
            var_type,
            serialized_const_value,
            debug_info,
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

    fn get_expression(&self, place: &Place) -> Option<&Rvalue> {
        self.expressions.get(&place.local)
    }
}

#[derive(Debug, Clone)]
enum FunctionCallContext {
    NoReturn,
    WithReturn {
        last_seen_type: Option<TyKind>,
        last_const_val: Option<String>,
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
    static ref PLACE_MAP: Mutex<PlaceMap> = Mutex::new(PlaceMap::new());
    /// Keeps track of the function call stack. When we return, we can pop the information
    /// about the function we returned from.
    static ref FUNCTION_CALL_CTX: Mutex<Vec<FunctionCallContext>> = Mutex::new(Vec::new());
}

fn make_int_ast(intty: IntTy, place: &Place, info: &VarInfo) -> Int<'static> {
    let left_ast = Int::new_const(
        &CTX.0,
        info.debug_info
            .as_ref()
            .and_then(|dbg| dbg.name.clone())
            .unwrap_or_else(|| format!("_{}", place.local.get_private())),
    );

    if !info.is_symbolic {
        if let Some(ref str) = info.serialized_const_value {
            let ctx = &CTX.0;
            let f1 = match intty {
                Isize => left_ast._eq(&Int::from_i64(ctx, isize::from_str(str).unwrap() as i64)),
                I8 => left_ast._eq(&Int::from_i64(ctx, i8::from_str(str).unwrap() as i64)),
                I16 => left_ast._eq(&Int::from_i64(ctx, i16::from_str(str).unwrap() as i64)),
                I32 => left_ast._eq(&Int::from_i64(ctx, i32::from_str(str).unwrap() as i64)),
                I64 => left_ast._eq(&Int::from_i64(ctx, i64::from_str(str).unwrap() as i64)),
                I128 => left_ast._eq(&Int::from_i64(ctx, i128::from_str(str).unwrap() as i64)),
            };
            SOLVER.0.assert(&f1);
        }
    }
    left_ast
}

pub fn switch_int(discr: &str, switch_targets: &str) {
    let discr: Operand = discr.try_into().unwrap();
    let switch_targets: SwitchTargets = switch_targets.try_into().unwrap();

    let var_map = PLACE_MAP.lock().unwrap();

    fn get_place_from_operand(operand: &Operand) -> &Place {
        match operand {
            Operand::Copy(ref p) => p,
            Operand::Move(ref p) => p,
            Operand::Constant(_) => panic!("not expected"),
        }
    }

    let place = get_place_from_operand(&discr);
    let var_info = if let Some(var_info) = var_map.get(place) {
        var_info
    } else {
        return;
    };
    println!(
        "[switch_int] discr: {discr:?} switch_targets: {switch_targets:?} var_info: {var_info:?}"
    );
    if !var_info.is_symbolic {
        return;
    }
    let expression: &Rvalue = if let Some(rvalue) = var_info.get_expression(place) {
        rvalue
    } else {
        return;
    };
    match expression {
        Rvalue::BinaryOp(op, b) => {
            let (left, right) = &**b;
            let (left, right) = (get_place_from_operand(left), get_place_from_operand(right));
            if let (Some(left_info), Some(right_info)) = (var_map.get(left), var_map.get(right)) {
                assert_eq!(left_info.var_type, right_info.var_type);
                let (left_ast, right_ast) = match left_info.var_type {
                    TyKind::Int(intty) => (
                        make_int_ast(intty, left, left_info),
                        make_int_ast(intty, right, right_info),
                    ),
                    _ => return,
                };

                // SwitchTargets value of 0 means false
                for (value, basic_block_idx) in &switch_targets.switch_targets {
                    println!(
                        "Checking results for value = {}, bb_idx = {}",
                        value, basic_block_idx
                    );
                    let mut f1 = match op {
                        BinOp::Gt => left_ast.gt(&right_ast),
                        // TODO: Eq with constants is just using the value directly inside of
                        //  switch_targets
                        BinOp::Eq => left_ast._eq(&right_ast),
                        BinOp::Lt => left_ast.lt(&right_ast),
                        BinOp::Le => left_ast.le(&right_ast),
                        BinOp::Ne => left_ast._eq(&right_ast).not(),
                        BinOp::Ge => left_ast.ge(&right_ast),
                        // TODO: Include the rest of the binary operations
                        _ => return
                    };
                    if *value == 0 {
                        f1 = f1.not()
                    };
                    SOLVER.0.assert(&f1);

                    match SOLVER.0.check() {
                        SatResult::Sat => {
                            println!("SAT");
                            // Note: Must check satisfiability before you can get the model.
                            if let Some(model) = SOLVER.0.get_model() {
                                // Retrieve interpretations
                                println!("{}", model);
                            } else {
                                println!("Failed to get model!");
                            }
                        }
                        SatResult::Unsat => {
                            println!("UNSAT");
                        }
                        _ => {
                            println!("UNKNOWN");
                        }
                    }
                }
            }
        }
        _ => {}
    }
}

pub fn ret() {
    let latest_ctx = { FUNCTION_CALL_CTX.lock().unwrap().pop() };
    if let Some(FunctionCallContext::WithReturn {
        last_seen_type,
        last_const_val,
        destination,
    }) = latest_ctx
    {
        handle_place_with_function_call_ctx(
            &destination,
            None,
            last_seen_type,
            true,
            last_const_val,
        );
    }

    println!(
        "[ret] PLACE_MAP = {:?}, FUNCTION_CALL_CTX = {:?}",
        PLACE_MAP.lock().unwrap(),
        FUNCTION_CALL_CTX.lock().unwrap(),
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
                last_const_val: None,
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

fn handle_place(
    place_and_debug_info: &PlaceAndDebugInfo,
    rvalue: Option<&Rvalue>,
    ty: Option<TyKind>,
    serialized_const_value: Option<String>,
) {
    handle_place_with_function_call_ctx(
        place_and_debug_info,
        rvalue,
        ty,
        false,
        serialized_const_value,
    )
}

/// Common function for handling places. Pass in [rvalue] for assign statements to be able to
/// detect `Ref`s / `Copy`s / `Move`s of symbolic variables and propagate their status.
fn handle_place_with_function_call_ctx(
    place_and_debug_info: &PlaceAndDebugInfo,
    rvalue: Option<&Rvalue>,
    ty: Option<TyKind>,
    is_return: bool,
    serialized_const_value: Option<String>,
) {
    if !is_return {
        let mut function_call_ctx = FUNCTION_CALL_CTX.lock().unwrap();
        if let Some(FunctionCallContext::WithReturn {
            last_seen_type,
            last_const_val,
            ..
        }) = function_call_ctx.last_mut()
        {
            *last_seen_type = ty.clone();
            *last_const_val = serialized_const_value.clone();
        }
    }

    let mut place_map = PLACE_MAP.lock().unwrap();
    let place = if let Some(ref place) = place_and_debug_info.place {
        place
    } else {
        return;
    };

    if let Some(rvalue) = rvalue {
        place_map.insert_place_rvalue_pair(
            place,
            place_and_debug_info.debug_info.clone(),
            serialized_const_value,
            rvalue,
        );
    // TODO: Add a proper way for denoting symbolic variables instead of just reading the
    //  variable name.
    } else {
        let debug_info = place_and_debug_info.debug_info.clone();
        let is_symbolic = debug_info
            .as_ref()
            .and_then(|info| info.name.as_ref())
            .map(|n| n.to_lowercase().contains("leaf_symbolic"))
            .unwrap_or(false);

        place_map.insert(
            place.clone(),
            ty.expect("type needed for variable"),
            serialized_const_value,
            debug_info,
            is_symbolic,
        );
    }
}

pub fn drop(place_and_debug_info: &str) {
    let place_and_debug_info: PlaceAndDebugInfo = place_and_debug_info.try_into().unwrap();
    let place = if let Some(ref place) = place_and_debug_info.place {
        place
    } else {
        return;
    };

    let variable_removed = {
        let mut place_map = PLACE_MAP.lock().unwrap();
        place_map.remove(&place)
    };

    println!("[drop] {place_and_debug_info:?} variable_removed: {variable_removed:?}");
}
