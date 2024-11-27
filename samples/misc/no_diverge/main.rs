use leaf::annotations::*;

fn main() {
    let x = 10.mark_symbolic();

    push_tag(tags::NO_DIVERGE);
    // Should not solve for x >= 12
    if x < 12 {
        core::hint::black_box(foo());
    }
    pop_tag();

    // Should solve for 12 > x > 10
    if x > 10 {
        core::hint::black_box(foo());
    }
}

fn foo() {}
