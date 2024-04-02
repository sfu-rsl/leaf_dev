use leaf::annotations::Symbolizable;

fn main() {
    let mut array = [1, 2, 3, 4];
    let i = 1.mark_symbolic();
    array[i] *= 10;
    if array[i] == 30 {
        foo();
    }
}

fn foo() {}
