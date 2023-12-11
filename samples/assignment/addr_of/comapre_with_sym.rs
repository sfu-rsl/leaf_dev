use runtime::annotations::Symbolizable;

fn main() {
    let a = 20;
    let y = core::ptr::addr_of!(a);
    let z = y as usize;
    let x = 10.mark_symbolic();
    if z < x {
        foo(z);
    }
}

#[inline(never)]
fn foo<T>(x: T) {}
