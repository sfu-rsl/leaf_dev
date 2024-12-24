pub fn run_input(data: &[u8]) {
    if data.len() < 8 {
        return;
    }

    if data[0] == b'a' {
        if data[1] == b'b' {
            if data[2] == b'c' {
                if data[3] == b'd' {
                    if u32::from_le_bytes([data[4], data[5], data[6], data[7]]) == 0xdeadbeef {
                        panic!("BOOM");
                    }
                }
            }
        }
    }
}
