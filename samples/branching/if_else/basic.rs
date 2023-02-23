fn main() {
    let x = get_num();
    if x == 1 {
        foo();
    } else {
        bar();
    }

    after();
}

fn get_num() -> i32 {
    10
}

fn foo() {}

fn bar() {}

fn after() {}