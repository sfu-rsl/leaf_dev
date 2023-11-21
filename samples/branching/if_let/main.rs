use runtime::annotations::Symbolizable;

fn main() {
    let x = 10_u8.mark_symbolic();
    let y = check(x + 1);
    if let NSOption::Some(z) = y {
        if z > 9 {
            foo("Large");
        } else {
            foo("Good");
        }
    }
}

fn foo<T>(x: T) {}

fn check(x: u8) -> NSOption<u8> {
    if x > 5 {
        NSOption::Some(x + 1)
    } else {
        NSOption::None
    }
}

#[repr(u32)]
enum NSOption<T> {
    Some(T),
    None,
}
