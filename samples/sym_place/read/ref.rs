use leaf::annotations::Symbolizable;

fn main() {
    let array = [1, 2, 3];
    let i = 0.mark_symbolic();
    let item = &array[i];
    if *item > 2 {
        foo();
    }
}

fn foo() {}
