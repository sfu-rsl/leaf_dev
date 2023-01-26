fn main() {
    let a = get_bar();
    match a {
        Bar::First => foo(),
        Bar::Second => foo(),
    }
}

enum Bar {
    First,
    Second,
}

fn get_bar() -> Bar {
    Bar::First
}

fn foo() {}
