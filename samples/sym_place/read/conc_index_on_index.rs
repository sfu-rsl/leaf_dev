use leaf::annotations::Symbolizable;

fn main() {
    let array = [[1, 2], [3, 4]];
    let i = 1.mark_symbolic();
    if array[i][1] == 2 {
        foo();
    }
}

fn foo() {}
