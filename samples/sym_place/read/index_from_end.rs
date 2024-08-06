use leaf::annotations::Symbolizable;

fn main() {
    let array: [&[u8]; 2] = [&[16, 32], &[48, 64, 80]];
    let i = 1.mark_symbolic();
    match array[i] {
        [.., 32] => foo(),
        _ => bar(),
    }
}

fn foo() {}

fn bar() {}
