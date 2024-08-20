use leaf::annotations::Symbolizable;

fn main() {
    let first: [&[u8]; 2] = [&[16, 32], &[48, 64, 80]];
    let i = 1.mark_symbolic();
    let second: [&[u8]; 2] = [&[96, 112], first[i]];
    let j = 1.mark_symbolic();
    match second[j] {
        [.., 32] => foo(),
        _ => bar(),
    }
}

fn foo() {}

fn bar() {}
