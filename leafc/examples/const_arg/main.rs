fn main() {
    test_str("Hi");
    test_num(2);
    test_array([1, 2, 3, 4]);
    test_byte_str(b"Hi");
    panic!("x");
}

fn test_str(text: &str) -> usize {
    text.len()
}

fn test_num(num: i32) -> i32 {
    num + 2
}

fn test_array(nums: [i32; 4]) -> usize {
    nums.len()
}

fn test_byte_str(bytes: &[u8]) -> usize {
    bytes.len()
}
