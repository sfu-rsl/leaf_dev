use runtime::annotations::Symbolizable;

fn main() {
    let x = 0_u8.mark_symbolic();

    if (x + 5) + 10 < 100 {
        do_something();
    } else {
        do_something_else();
    }
}

fn do_something() {}

fn do_something_else() {}
