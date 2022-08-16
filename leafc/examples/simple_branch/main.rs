#![allow(unused)]

fn get_bool(return_value: bool) -> bool {
    return_value
}

fn get_int() -> i32 {
    if get_bool(false) {
        42
    } else {
        1
    }
}

fn main() -> Result<(), i32> {
    // This is treated as a symbolic variable, although there's currently no code to run this
    // program multiple times with different values.
    let leaf_symbolic = get_int();

    let y = (leaf_symbolic + 1).pow(2);

    let z = get_int();

    let x = if y < 1 { "Hi" } else { "Bye" };

    Ok(())
}
