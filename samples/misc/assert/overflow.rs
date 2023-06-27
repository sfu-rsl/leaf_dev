use runtime::annotations::Symbolizable;

fn main() {
    let mut x: u64 = 10.mark_symbolic();
    let y = x + 1;
}
