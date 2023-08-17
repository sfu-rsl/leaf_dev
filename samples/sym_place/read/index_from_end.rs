use runtime::annotations::Symbolizable;

fn main() {
    let array: [&[u8]; 2] = [&[1, 2], &[3, 4, 5]];
    let i = 1.mark_symbolic();
    match array[i] {
        [.., 2] => foo(),
        _ => bar(),
    }
}

fn foo() {}

fn bar() {}
