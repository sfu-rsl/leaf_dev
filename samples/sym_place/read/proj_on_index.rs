use runtime::annotations::Symbolizable;

fn main() {
    let array = [Point { x: 1, y: 2 }, Point { x: 3, y: 4 }];
    let i = 0.mark_symbolic();
    if array[i].x == 3 {
        foo();
    }
}

fn foo() {}

struct Point {
    x: i32,
    y: i32,
}
