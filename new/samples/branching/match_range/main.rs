fn main() {
    let x = get_num();

    match x % 5 {
        1..=3 => foo(),
        4 => foo(),
        _ => foo(),
    }
}

fn get_num() -> i32 {
    10
}

fn foo() {}
