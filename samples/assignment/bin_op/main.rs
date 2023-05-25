fn main() {
    let a = get_num();
    let b = get_num() + 5;

    let x = a + b;
    let x = a - b;
    let x = a * b;
    let x = a / b;
    let x = a % b;

    let x = a ^ b;
    let x = a & b;
    let x = a | b;
    let x = a << b;
    let x = a >> b;

    let x = a == b;
    let x = a < b;
    let x = a <= b;
    let x = a != b;
    let x = a >= b;
    let x = a > b;
}

fn get_num() -> isize {
    10
}
