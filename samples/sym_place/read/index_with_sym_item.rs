use leaf::annotations::Symbolizable;

fn main() {
    let x = 10.mark_symbolic();
    let array = [1, 2, 3, x];
    let i = 0.mark_symbolic();
    if array[i] > 100 {
        foo();
    }
}

fn foo() {}
