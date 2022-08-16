fn get_int() -> i32 {
    6
}

fn main() {
    // This is treated as a symbolic variable, although there's currently no code to run this
    // program multiple times with different values.
    let leaf_symbolic = get_int();

    if leaf_symbolic + 1 > 5 {
        "Greater than 5"
    } else {
        "Less than 5"
    };
}
