fn main() {
    let mut a = get_num();
    increase_num(&mut a);

    let mut arr = [10, 20, 30];
    increase_array(&mut arr);

    let mut loc = Location { x: 10, y: 20 };
    increase_field(&mut loc);

    let mut x = 10;
    let mut y = 20;
    swap(&mut x, &mut y);

    let mut x = &loc.x;
    let mut y = &loc.y;
    swap_targets(&mut x, &mut y);
}

fn get_num() -> i32 {
    10
}

fn increase_num(x: &mut i32) {
    *x += 5;
}

fn increase_array(arr: &mut [i32; 3]) {
    let mut i = 0;
    while i < 3 {
        increase_num(&mut arr[i]);
        i += 1;
    }
}

fn increase_field(loc: &mut Location) {
    increase_num(&mut loc.x);
    increase_num(&mut loc.y);
}

struct Location {
    x: i32,
    y: i32,
}

fn swap(x: &mut i32, y: &mut i32) {
    let tmp = *x;
    *x = *y;
    *y = tmp;
}

fn swap_targets<'a>(x: &mut &'a i32, y: &mut &'a i32) {
    let tmp = *x;
    *x = *y;
    *y = tmp;
}
