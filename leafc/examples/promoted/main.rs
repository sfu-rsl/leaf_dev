fn main() {
    const T0: i32 = 1234;
    const T1: u64 = 1234;
    const T2: bool = true;
    const T3: &str = "T3";
    const T0R : &i32 = &T0;
    const T1R: &u64 = &T1;
    const T2R: &bool = &T2;
    const T3R: &&str = &T3;

    println!("{} Testing {} print {} string {} slices. {} {}", 5, *T0R, T0R, T1R, T2R, T3R);
}