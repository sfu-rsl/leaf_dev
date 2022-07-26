#![allow(unused)]

fn get_bool() -> bool {
    false
}

fn get_int() -> i32 {
    if get_bool() {
        42
    } else {
        1
    }
}

fn main() {
    let leaf_symbolic = get_int();
    let x = if leaf_symbolic >= 14 { "Hi" } else { "Bye" };
}
