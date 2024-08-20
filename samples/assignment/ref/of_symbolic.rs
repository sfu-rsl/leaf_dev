use leaf::annotations::Symbolizable;

fn main() {
    let array = [
        Point { x: 1, y: 2 },
        Point {
            x: 3,
            y: 4.mark_symbolic(),
        },
    ];
    let i = get_index();
    let item = &array[i];
    let y = &item.y;
    if *y == 5 {
        foo();
    }
}

fn foo() {}

#[inline(never)]
fn get_index() -> usize {
    1
}

struct Point {
    x: i32,
    y: i32,
}
