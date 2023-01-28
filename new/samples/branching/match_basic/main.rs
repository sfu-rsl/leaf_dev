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

    let y = get_char();
    match y {
        'a' => foo(),
        'x' => foo(),
        _ => foo(),
    }
}

fn get_num() -> i32 {
    10
}
fn get_char() -> char {
    'x'
}

fn foo() {}
