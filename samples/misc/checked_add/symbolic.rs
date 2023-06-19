use runtime::annotations::Symbolizable;

fn main() {
    let mut x = 20.mark_symbolic();
    let y = x + 10;
}
