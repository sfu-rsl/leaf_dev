use runtime::annotations::Symbolizable;

fn main() {
    let mut x = 20_u32.mark_symbolic();
    let y = 21 - x;
}
