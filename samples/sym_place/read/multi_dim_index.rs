use runtime::annotations::Symbolizable;

fn main() {
    let array = [[1, 2], [3, 4]];
    let i = 1.mark_symbolic();
    let j = 1.mark_symbolic();
    if array[i][j] == 3 {
        foo();
    }
}

fn foo() {}
