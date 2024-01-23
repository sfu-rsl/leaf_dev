use leaf::annotations::Symbolizable;

fn main() {
    let ar = [1, 2];
    let z = ar[0.mark_symbolic()] as u32;
}
