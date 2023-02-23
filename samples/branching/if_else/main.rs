fn main() {
    let x = get_num();

    let y = x % 3;
    if y == 1 {
        foo();
    } else {
        foo();
    }

    if y == 2 {
        foo();
    } else if y == 1 {
        foo();
    } else {
        foo();
    }
}

fn get_num() -> i32 {
    10
}

fn foo() {}
