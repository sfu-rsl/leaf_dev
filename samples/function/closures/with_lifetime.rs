fn main() {
    on_parameters();
    on_captured_variables();
}

fn on_parameters() {
    let f = |x: &i32, y: &i32| -> i32 { x + y };
    f(&10, &20);
}

fn on_captured_variables() {
    let y = &20;
    let f = |x: i32| -> i32 { x + y };
    f(10);
}
