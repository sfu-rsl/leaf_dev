use runtime::annotations::Symbolizable;

fn main() {
    let mut x: i32 = 10.mark_symbolic();
    let y = -x;
}
