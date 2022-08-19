fn get_int(arg: i32) -> i32 {
    6
}

fn get_int_2(strarg: &str) {}

fn main() {
    // This is treated as a symbolic variable, although there's currently no code to run this
    // program multiple times with different values.
    let a = 8;
    let b = 54;
    let c = 2;
    get_int_2("Hi");
    let leaf_symbolic_x = get_int(1);
    let leaf_symbolic_y = get_int(leaf_symbolic_x);

    if a == 2 {
        "abc";
    }

    if a * leaf_symbolic_x + b * leaf_symbolic_y != c {
        "ax + by == c is not satisfied"
    } else {
        "ax + by == c is satisfied"
    };
}
