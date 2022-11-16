fn main() {
    let leaf_symbolic_arg = get_char();

    if get_a_char() == leaf_symbolic_arg {
        panic!("Ah!");
    }

    match leaf_symbolic_arg {
        'x' => panic!("Ah!"),
        'z' => panic!("Oh!"),
        _ => ()
    };
}

fn get_the_ascii_one(a: char, b: char) -> char {
    if a.is_ascii() {
        a
    } else if b.is_ascii() {
        b
    } else {
        '\0'
    }
}

fn get_char() -> char {
    't'
}

fn get_a_char() -> char {
    'y'
}
