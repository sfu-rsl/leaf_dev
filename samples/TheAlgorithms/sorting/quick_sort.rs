/* Adapted from: https://github.com/TheAlgorithms/Rust */

use runtime::annotations::Symbolizable;

fn main() {
    all_symbolic();
}

#[no_mangle]
fn foo() {}

#[no_mangle]
pub fn have_same_elements(a: &[i32], b: &[i32]) -> bool {
    #[no_mangle]
    fn contains(arr: &[i32], val: i32) -> bool {
        let mut i = 0;
        while i < arr.len() {
            if arr[i] == val {
                return true;
            }
            i += 1;
        }

        false
    }

    if a.len() != b.len() {
        return false;
    }

    let mut i = 0;
    while i < a.len() {
        if !contains(b, a[i]) {
            return false;
        }
        i += 1;
    }

    true
}

#[no_mangle]
pub fn is_sorted(arr: &[i32]) -> bool {
    let mut i = 1;
    while i < arr.len() {
        if arr[i] < arr[i - 1] {
            return false;
        }
        i += 1;
    }

    true
}

#[no_mangle]
pub fn clone(arr: &[i32; 4]) -> [i32; 4] {
    let mut res = [0; 4];
    let mut i = 0;
    while i < arr.len() {
        res[i] = arr[i];
        i += 1;
    }

    res
}

#[no_mangle]
fn all_symbolic() {
    let mut res = [
        3.mark_symbolic(),
        2.mark_symbolic(),
        1.mark_symbolic(),
        4.mark_symbolic(),
    ];
    let cloned = clone(&res);
    quick_sort(&mut res);
    if !is_sorted(&res) || !have_same_elements(&res, &cloned) {
        foo();
    }
}

#[no_mangle]
pub fn quick_sort(arr: &mut [i32]) {
    if arr.len() > 1 {
        _quick_sort(arr, 0, (arr.len() - 1) as isize);
    }
}

#[no_mangle]
fn _quick_sort(arr: &mut [i32], lo: isize, hi: isize) {
    if lo < hi {
        let p = partition(arr, lo, hi);
        _quick_sort(arr, lo, p - 1);
        _quick_sort(arr, p + 1, hi);
    }
}

#[no_mangle]
pub fn partition(arr: &mut [i32], lo: isize, hi: isize) -> isize {
    let pivot = hi as usize;
    let mut i = lo - 1;
    let mut j = hi;

    loop {
        i += 1;
        while arr[i as usize] < arr[pivot] {
            i += 1;
        }
        j -= 1;
        while j >= 0 && arr[j as usize] > arr[pivot] {
            j -= 1;
        }
        if i >= j {
            break;
        } else {
            swap(arr, i as usize, j as usize);
        }
    }
    swap(arr, i as usize, pivot);
    i
}

#[no_mangle]
pub fn swap(arr: &mut [i32], i: usize, j: usize) {
    let tmp = arr[i];
    arr[i] = arr[j];
    arr[j] = tmp;
}
