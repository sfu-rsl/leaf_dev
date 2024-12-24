fn main() {
    let input = read_input();
    basic_pro_conc::run_input(&input);
}

fn read_input() -> Vec<u8> {
    #[cfg(leafc)]
    use leaf::annotations::*;
    use std::io::Read;

    let mut buffer = Vec::new();
    let count = std::io::stdin().read_to_end(&mut buffer).unwrap();

    #[cfg(leafc)]
    push_tag(tags::NO_DIVERGE);

    for i in 0..count {
        let byte = buffer[i];
        #[cfg(leafc)]
        let byte = byte.mark_symbolic();
        buffer[i] = byte;
    }

    #[cfg(leafc)]
    pop_tag();

    buffer
}
