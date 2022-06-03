use leafcommon::place::Place;
use leafcommon::rvalue::{Operand, OperandVec, Rvalue};

pub fn switch_int(discr: &str) {
    let discr: Operand = discr.try_into().unwrap();
    println!("[switch_int] discr: {discr:?}");
}

pub fn ret() {
    println!("[ret]");
}

pub fn call(func: &str, args: &str, destination: &str) {
    let func: Operand = func.try_into().unwrap();
    let args: OperandVec = args.try_into().unwrap();
    let destination: Place = destination.try_into().unwrap();
    println!("[call] func: {func:?} args: {args:?} destination: {destination:?}");
}

pub fn assign(place: &str, rvalue: &str) {
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[assign] place: {place:?} rvalue: {rvalue:?}");
}

pub fn const_isize(place: &str, rvalue: &str, constant: isize) {
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[const_isize] place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
}

pub fn const_i8(place: &str, rvalue: &str, constant: i8) {
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[const_i8] place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
}

pub fn const_i16(place: &str, rvalue: &str, constant: i16) {
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[const_i16] place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
}

pub fn const_i32(place: &str, rvalue: &str, constant: i32) {
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[const_i32] place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
}

pub fn const_i64(place: &str, rvalue: &str, constant: i64) {
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[const_i64] place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
}

pub fn const_i128(place: &str, rvalue: &str, constant: i128) {
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[const_i128] place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
}

pub fn const_usize(place: &str, rvalue: &str, constant: usize) {
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[const_usize] place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
}

pub fn const_u8(place: &str, rvalue: &str, constant: u8) {
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[const_u8] place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
}

pub fn const_u16(place: &str, rvalue: &str, constant: u16) {
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[const_u16] place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
}

pub fn const_u32(place: &str, rvalue: &str, constant: u32) {
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[const_u32] place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
}

pub fn const_u64(place: &str, rvalue: &str, constant: u64) {
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[const_u64] place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
}

pub fn const_u128(place: &str, rvalue: &str, constant: u128) {
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[const_u128] place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
}

pub fn const_f32(place: &str, rvalue: &str, constant: f32) {
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[const_f32] place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
}

pub fn const_f64(place: &str, rvalue: &str, constant: f64) {
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[const_f64] place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
}

pub fn const_char(place: &str, rvalue: &str, constant: char) {
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[const_char] place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
}

pub fn const_bool(place: &str, rvalue: &str, constant: bool) {
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[const_bool] place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
}

pub fn const_str(place: &str, rvalue: &str, constant: &str) {
    let place: Place = place.try_into().unwrap();
    let rvalue: Rvalue = rvalue.try_into().unwrap();

    println!("[const_str] place: {place:?} rvalue: {rvalue:?} constant: {constant:?}");
}
