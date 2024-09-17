fn main() {
    let c_bit = get_num();
    let c = unsafe { core::char::from_u32_unchecked(c_bit) };
    let bar = get_bar(c);
    match bar {
        Bar::A => foo(1),
        Bar::B(..) => foo(2),
        Bar::C => foo(3),
    }

    let discr = core::mem::discriminant(&bar);
    if discr == core::mem::discriminant(&Bar::A) {
        foo(4);
    }
}

enum Bar {
    A,
    B(Option<(u8, char)>),
    C,
}

fn get_bar(c: char) -> Bar {
    Bar::B(Some((5, c)))
}

fn get_num() -> u32 {
    use leaf::annotations::Symbolizable;
    ('x' as u32).mark_symbolic()
}

#[inline(never)]
fn foo<T>(x: T) {
    let _ = core::hint::black_box(x);
}
