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

fn main() {
    let leaf_symbolic_arg = get_int();

    let c = get_c(leaf_symbolic_arg);

    let result = if c == 2 { "Equals to 1" } else { "Not 1" };
}
