/* TODO: Make this example more appealing once more binary operations are supported.
 * Ideally in a way that can be directly solved by the solver.
 */

use runtime::annotations::Symbolizable;

fn main() {
    let x = (-4).mark_symbolic();
    if neg_degree(x) == 5 {
        foo();
    }
}

fn neg_degree(num: i32) -> u32 {
    if num == 0 { 0 } else { 1 + neg_degree(num + 1) }
}

fn foo() {}
