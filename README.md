# leaf

Concolic execution for Rust using MIR (mid-level IR). Project is in very early stages.

As a brief overview, leafc is used to compile a Rust program. It compiles the program normally, but
also modifies the code and injects various function calls from leafrt (leaf runtime).

## Setup

Install Rust development components:

``` 
rustup component add rust-src rustc-dev llvm-tools-preview 
```

## Running an example

Here's a guide on how to use leafc on one of the examples. We are interested in the `diophantine`
example.  In this example, we have the linear diophantine equation $ax + by = c$, where $a$, $b$,
and $c$ are given integers, and $x$ and $y$ are integer variables. We want to determine whether
$ax + by = c$ has a solution in $x$ and $y$. It can be proved that $ax + by = c$ has solution(s)
in the integers if and only if $\gcd(a,b)$ divides $c$.

The default values for the program are:

```rust 
fn main() {
    let a = 2;
    let b = 6;
    let c = 100;
    // x and y treated as symbolic variables, although there's currently no code to run this
    // program multiple times with different values.
    // We have to use `get_int()` here in order to avoid optimization of the if-statement below.
    let leaf_symbolic_x = get_int();
    let leaf_symbolic_y = get_int();

    if a * leaf_symbolic_x + b * leaf_symbolic_y != c {
        "ax + by == c is not satisfied"
    } else {
        "ax + by == c is satisfied"
    };
}
```

In the root directory of the project, run the following to compile an example program using leafc:

``` 
RUST_LOG=debug cargo run leafc/examples/diophantine/main.rs 
```

After the example is compiled, `main` will be the output binary. We can run this, and it produces a
bunch of output from the injected leafrt code. We highligt the relevant output for the if-statement:

```
Reaching the false branch is possible
model: main_9 -> 2
a -> 2
leaf_symbolic_x -> 41
main_13 -> 6
b -> 6
leaf_symbolic_y -> 3
main_17 -> 100
c -> 100
get_c_0 -> 100
main_6 -> false
main_7 -> 100
main_16 -> 100
main_11 -> 82
main_10 -> 41
main_12 -> 18
main_14 -> 3
main_8 -> 82
main_15 -> 18
```

We can see that the calls to the SMT solver has found that $2x+6y=100$ can be satisfied (i.e. we can
reach the `false` branch) using $x = 41$, $y=3$. Sure enough, $2x+6y = 2\cdot41 + 6\cdot3 = 82 + 18 = 100$.

If we modify it so that we have $c = 5$ (note that $\gcd(2, 6) = 2$ does not divide $5$), and then
recompile the example, then the binary will output `UNSAT`, which means the equation $2x+6y=5$
cannot be satisfied (i.e. we cannot reach the `false` branch).
