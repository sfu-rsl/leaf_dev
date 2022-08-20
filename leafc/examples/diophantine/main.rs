fn get_int() -> i32 {
    6
}

fn get_c() -> i32 {
    5
}

/// Here, we have the linear diophantine equation ax + by = c, where a, b, and c are given integers,
/// and x and y are integer variables. We want to determine whether ax + by = c has a solution in x
/// and y.
fn main() {
    let a = 2;
    let b = 6;
    let c = get_c();
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
