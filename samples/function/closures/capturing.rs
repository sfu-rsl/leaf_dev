fn main() {
    for_fn();
    for_fn_mut();
    for_fn_once();
}

fn for_fn() {
    let y = 20;
    let f = |x: i32| -> i32 { x + y };
    f(10);
}

fn for_fn_mut() {
    let mut y = 20;
    let mut f = |x: i32| -> i32 {
        y += 1;
        x + y
    };
    f(10);
}

fn for_fn_once() {
    struct Consumable(i32);
    let mut y = Consumable(20);
    let f = move |x: i32| -> Consumable {
        y.0 += x;
        y
    };
    f(10);
}
