fn main() {
    let a = get_bar();
    match a {
        Bar::First => {}
        Bar::Second => {}
    }
}

enum Bar {
    First,
    Second,
}

fn get_bar() -> Bar {
    Bar::First
}
