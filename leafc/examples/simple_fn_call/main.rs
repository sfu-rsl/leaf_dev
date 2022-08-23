fn get_int() -> u32 {
    55
}

fn get_c(arg: u32) -> i32 {
    if arg > 25 {
        1
    } else {
        2
    }
}

/// FIXME: The MIR for this is
///
/// bb0: {
///     _1 = get_int() -> bb1;           // scope 0 at src/main.rs:14:29: 14:38
///     // mir::Constant
///     // + span: src/main.rs:14:29: 14:36
///     // + literal: Const { ty: fn() -> i32 {get_int}, val: Value(Scalar(<ZST>)) }
/// }
///
/// bb1: {
///     _3 = _1;                         // scope 1 at src/main.rs:16:33: 16:50
///     _2 = get_c(move _3) -> bb2;      // scope 1 at src/main.rs:16:27: 16:51
///     // mir::Constant
///     // + span: src/main.rs:16:27: 16:32
///     // + literal: Const { ty: fn(i32) -> i32 {get_c}, val: Value(Scalar(<ZST>)) }
/// }
///
/// bb2: {
///     _5 = _2;                         // scope 2 at src/main.rs:18:8: 18:23
///     _4 = Gt(move _5, const 25_i32);  // scope 2 at src/main.rs:18:8: 18:28
///     switchInt(move _4) -> [false: bb4, otherwise: bb3]; // scope 2 at src/main.rs:18:8: 18:28
/// }
///
/// However, the actual MIR is reusing _2 for _5 for some reason.
fn main() {
    let leaf_symbolic_arg = get_int();

    let c = get_c(leaf_symbolic_arg);

    if c == 1 {
        "Equals to 1"
    } else {
        "Not 1"
    };
}
