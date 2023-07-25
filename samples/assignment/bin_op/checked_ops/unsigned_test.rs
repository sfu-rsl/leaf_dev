use runtime::annotations::Symbolizable;

fn main() {
    let mut x = 0_u64.mark_symbolic();
    let y = x + 5 == 3;

    let mut z = 0;
    if y {
        z = 10;
    } else {
        z = 20;
    }
}
