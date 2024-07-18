fn main() {
    let a = get_bar();
    match a {
        Bar::First => foo(),
        Bar::Second(c) => foo(),
    }

    let b = get_another_bar();
    match b {
        AnotherBar::Third => foo(),
        AnotherBar::Fourth { x, y } => foo(),
    }
}

enum Bar {
    First,
    Second(char),
}

fn get_bar() -> Bar {
    Bar::First
}

enum AnotherBar {
    Third,
    Fourth { x: Bar, y: i64 },
}

fn get_another_bar() -> AnotherBar {
    AnotherBar::Fourth {
        x: Bar::First,
        y: 15,
    }
}

fn foo() {}
