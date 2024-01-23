use leaf::annotations::Symbolizable;

fn main() {
    let x = 10.mark_symbolic();
    branch_on(for_fn(x), 1);
    branch_on(for_fn_mut(x), 2);
    branch_on(for_fn_once(x), 3);
}

fn for_fn(x: i32) -> i32 {
    let f = |y: i32| -> i32 { x + y };
    f(10)
}

fn for_fn_mut(mut x: i32) -> i32 {
    let mut f = |y: i32| {
        x += y;
    };
    f(10);
    x
}

fn for_fn_once(x: i32) -> i32 {
    struct Consumable(i32);
    let mut x = Consumable(x);
    let f = move |y: i32| -> Consumable {
        x.0 += y;
        x
    };
    f(10).0
}

fn branch_on(x: i32, y: i32) {
    if x + y == 100 {
        foo();
    }
}

fn foo() {}
