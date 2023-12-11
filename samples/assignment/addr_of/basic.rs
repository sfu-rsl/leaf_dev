fn main() {
    let mut a = 10;
    let x: *const i32 = core::ptr::addr_of!(a);
    foo(x);
    let y: *mut i32 = core::ptr::addr_of_mut!(a);
    foo(y);
}

#[inline(never)]
fn foo<T>(x: T) {}
