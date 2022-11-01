fn get_int() -> i32 {
    55
}

fn get_c(arg: i32) -> i32 {
    if arg > 50 {
        44
    } else {
        17
    }
}

/// Here, we have the linear diophantine equation ax + by = c, where a, b, and c are given integers,
/// and x and y are integer variables. We want to determine whether ax + by = c has a solution in x
/// and y.
fn main() {
    let a = 2;
    let b = 6;
    // FIXME: Arguments using constants are not linked to the original place for some reason.
    // let leaf_symbolic_arg = get_int();
    // let c = get_c(leaf_symbolic_arg);
    let c = get_c(55);

    // x and y treated as symbolic variables, although there's currently no code to run this
    // program multiple times with different values.
    // We have to use `get_int()` here in order to avoid optimization of the if-statement below.
    let leaf_symbolic_x = get_int();
    let leaf_symbolic_y = get_int();

    if a * leaf_symbolic_x + b * leaf_symbolic_y != c {
        "ax + by == c is not satisfied"
    } else {
        "ax + by == c is satisfied"
    };
}
