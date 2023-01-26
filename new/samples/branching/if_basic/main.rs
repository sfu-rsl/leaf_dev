fn main() {
    let x = get_num();

    let condition = x == 5;
    if condition {
        foo();
    }

    if !condition {
        foo();
    }

    if x % 2 == 1 {
        foo();
    }

    let y = get_float_num();
    if y == 2.0 {
        foo();
    }
}

fn get_num() -> i32 {
    10
}
fn get_float_num() -> f32 {
    10.0
}

fn foo() {}
