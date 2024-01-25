use leaf::annotations::Symbolizable;

fn main() {
    let a = 1_usize.mark_symbolic();
    let b = a as *const u32;
    let c = unsafe { *b };
}
