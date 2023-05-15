use runtime::annotations::Symbolizable;

fn main() {
    let x = read();
    if x == 15 {
        foo();
    }
}

fn read() -> i32 {
    10.mark_symbolic()
}

fn foo() {}
