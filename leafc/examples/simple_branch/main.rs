#![allow(unused)]

fn get_int() -> i32 {
    5
}

fn main() {
    let leaf_symbolic = get_int();

    let x = if leaf_symbolic > 0 { "Hi" } else { "Bye" };
}
