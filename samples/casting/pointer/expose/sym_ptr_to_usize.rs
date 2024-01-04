use runtime::annotations::Symbolizable;

fn main() {
    let x = 24_u32.mark_symbolic();
    let y = &x as *const u32;
    let z = y as usize;
    foo(z);
}

#[inline(never)]
fn foo<T>(x: T) {}
