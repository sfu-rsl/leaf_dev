macro_rules! do_arithmetic {
    ($expression: expr) => {
        let result = $expression;
        use_num(result);
    };
}

macro_rules! do_bitwise {
    ($expression: expr) => {
        do_arithmetic!($expression);
    };
}

macro_rules! do_cmp {
    ($expression: expr) => {
        let result = $expression;
        use_bool(result);
    };
}

fn main() {
    let a = get_num();
    let b = get_num() + 5;

    do_arithmetic!(a + b);
    do_arithmetic!(a - b);
    do_arithmetic!(a * b);
    do_arithmetic!(a / b);
    do_arithmetic!(a % b);

    do_bitwise!(a ^ b);
    do_bitwise!(a & b);
    do_bitwise!(a | b);
    do_bitwise!(a << b);
    do_bitwise!(a >> b);

    do_cmp!(a == b);
    do_cmp!(a < b);
    do_cmp!(a <= b);
    do_cmp!(a != b);
    do_cmp!(a >= b);
    do_cmp!(a > b);
}

fn get_num() -> isize {
    10
}

fn use_num(_: isize) {}

fn use_bool(_: bool) {}
