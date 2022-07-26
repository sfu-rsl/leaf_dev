fn this_function_is_busy(a: &str) -> i64 {
    let x = String::from("Function string");
    if x == *a {
        5
    } else {
        0
    }
}

fn main() {
    let leaf_symbolic = String::from("Test string");
    let _compile_time_constant = 503;
    let y = leaf_symbolic;
    this_function_is_busy(&y);
}
