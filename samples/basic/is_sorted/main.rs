use runtime::annotations::Symbolizable;

fn main() {
    let arr = get_symbolic_array();

    if !regular::is_sorted(&arr) {
        fail();
    }

    if !nsc::is_sorted(&arr) {
        fail();
    }
}

fn fail() {}

fn get_symbolic_array() -> [u8; 4] {
    [
        3.mark_symbolic(),
        2.mark_symbolic(),
        1.mark_symbolic(),
        4.mark_symbolic(),
    ]
}

mod regular {
    pub fn is_sorted(arr: &[u8]) -> bool {
        let mut i = 1;
        while i < arr.len() {
            if arr[i] < arr[i - 1] {
                return false;
            }
            i += 1;
        }

        true
    }
}

// Non-short-circuiting
mod nsc {
    pub fn is_sorted(arr: &[u8]) -> bool {
        let mut result = true;
        let mut i = 1;
        while i < arr.len() {
            result = result & (arr[i] >= arr[i - 1]);
            i += 1;
        }

        result
    }
}
