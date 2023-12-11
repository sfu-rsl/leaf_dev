use runtime::annotations::Symbolizable;

fn main() {
    let array = [1, 2, 3, 4];
    let x = 0.mark_symbolic();
    let item_addr = core::ptr::addr_of!(array[x]);
    let item_addr_usize = item_addr as usize;
    if item_addr_usize % 3 == 0 {
        foo();
    }
}

fn foo() {}
