fn get_int() -> u32 {
    55
}

fn main() {
    let leaf_symbolic_arg = get_int();
    let c = leaf_symbolic_arg;

    let result = if c*c == 25 { "49" } else { "Not 49" };
}
