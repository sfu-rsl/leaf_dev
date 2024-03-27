fn main() {
    let v = [1, 2, 3, 4, 5];
    let _ = v
        .iter()
        .filter_map(|&x| if x % 2 == 0 { Some(x) } else { None })
        .collect::<Vec<_>>();
}
