use runtime::annotations::Symbolizable;

fn main() {
    let array: [&[u8]; 2] = [&[1, 2], &[3]];
    let i = 0.mark_symbolic();
    let j = 0.mark_symbolic();
    if array[i][j] == 3 {
        foo();
    }
}

fn foo() {}
