use leaf::annotations::Symbolizable;

fn main() {
    let a1 = 0b1u8.mark_symbolic() << 7u8; // == 2^15
    let a2 = 0b10u8.mark_symbolic() << 7u64; // == 0
    let a3 = 0b100u16.mark_symbolic() << 15u8; // == 0
    let a4 = 0b10u32.mark_symbolic() << 24u8; // == 33554432

    if a1 > 10 {
        do_stuff();
    }
    if a2 > 10 {
        do_stuff();
    }
    if a3 > 10 {
        do_stuff();
    }
    if a4 > 10 {
        do_stuff();
    }

    let b1 = 0b10001u32.mark_symbolic() >> 3u8; // == 2
    let b2 = (-1i32).mark_symbolic() >> 5u8; // == -1
    let b3 = (-1i32).mark_symbolic() >> 5i8; // == -1
    let b4 = 0b_0010_1111_u32.mark_symbolic() >> 5i8; // == 1

    if b1 > 10 {
        do_stuff();
    }
    if b2 > 10 {
        do_stuff();
    }
    if b3 > 10 {
        do_stuff();
    }
    if b4 > 10 {
        do_stuff();
    }

    let c1 = 0b10001u32 >> 3u8.mark_symbolic(); // == 2
    let c2 = (-1i32) >> 5u8.mark_symbolic(); // == -1
    let c3 = (-1i32) >> 5i8.mark_symbolic(); // == -1
    let c4 = 0b_0010_1111_u32 >> 5i8.mark_symbolic(); // == 1

    if c1 > 10 {
        do_stuff();
    }
    if c2 > 10 {
        do_stuff();
    }
    if c3 > 10 {
        do_stuff();
    }
    if c4 > 10 {
        do_stuff();
    }
}

fn do_stuff() {
    // no-op
}
