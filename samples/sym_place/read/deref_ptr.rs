use leaf::annotations::Symbolizable;

fn main() {
    let array = [1, 2, 3, 4];
    let item_addr = &array as *const i32;
    let i = 1.mark_symbolic();
    let addr = unsafe { item_addr.offset(i) };
    let addr = (item_addr as usize + i as usize * std::mem::size_of::<i32>()) as *const i32;
    let item = unsafe { *addr };
    if item == 3 {
        foo();
    }
}

fn foo() {}
