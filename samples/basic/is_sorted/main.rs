use runtime::annotations::Symbolizable;

fn main() {
    let arr = [
        3.mark_symbolic(),
        2.mark_symbolic(),
        1.mark_symbolic(),
        4.mark_symbolic(),
    ];

    if !is_sorted(&arr) {
        fail();
    }

    if !is_sorted_no_short_circuit(&arr) {
        fail();
    }
}

fn fail() {}

#[no_mangle]
fn is_sorted(arr: &[u8]) -> bool {
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
fn is_sorted_no_short_circuit(arr: &[u8]) -> bool {
    let mut result = true;
    let mut i = 1;
    while i < arr.len() {
        result = result & (arr[i] >= arr[i - 1]);
        i += 1;
    }

    result
}
