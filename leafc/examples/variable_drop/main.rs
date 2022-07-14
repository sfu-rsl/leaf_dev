static TREAT_ME_AS_SYMBOLIC: i32 = 4411;

fn main() {
    let x = TREAT_ME_AS_SYMBOLIC;
    let compile_time_constant = 503;
    let y = x;
    let z = x;
}
