fn main() {
    let x = get_num();

    let condition = x == 5;
    if condition {
        foo(true);
    }

    if !condition {
        foo(false);
    }

    if x % 2 == 1 {
        foo(1);
    }

    let x = get_float_num();
    if x == 2.0 {
        foo(2.0);
    }

    let x = get_char();
    if x == 'a' {
        foo('a');
    }
}

fn get_num() -> i32 {
    10
}
fn get_float_num() -> f32 {
    10.0
}
fn get_char() -> char {
    'm'
}

fn foo<C>(x: C) {}
