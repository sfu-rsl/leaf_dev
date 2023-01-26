fn main() {
    let x = get_num();

    match x % 3 {
        2 => foo(),
        1 => foo(),
        _ => foo(),
    }

    match x - 100 {
        -2 => foo(),
        1 => foo(),
        _ => foo(),
    }
}

fn get_num() -> i32 {
    10
}
fn get_float_num() -> f32 {
    10.0
}

fn foo() {}
