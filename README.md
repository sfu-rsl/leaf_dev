# leaf

Concolic execution for Rust using MIR (mid-level IR). The project is in very early stages.

As a brief overview, `leafc` is used instead of `rustc` to compile a Rust program. It compiles the program normally, but
also modifies the code and injects various function calls to the `runtime` via the pri (program runtime interface).

## Setup

### Prerequisites:
- Install rust https://www.rust-lang.org/tools/install & python3
- For Linux:
  - install cmake, clang, and distutils: `apt install cmake clang python3-distutils`
- For Windows: 
  - install cmake: https://cmake.org/download/ (download .msi installer)
  - install clang:
    - A. (1) install Visual Studio Build Tools https://visualstudio.microsoft.com/downloads/#build-tools-for-visual-studio-2022 (2) open Visual Studio Installer (3) under Visual Studio Build Tools press modify (4) under workloads, and Desktop development with C++, select "C++ Clang tools for Windows", then install it
    - B. if A doesn't work, try installing llvm via choco: https://community.chocolatey.org/packages/llvm (make sure to `cargo clean` after switching installations)

### Run Simple Example:
- `git clone git@github.com:sfu-rsl/leaf.git`
- `cd <your-path>/leaf/` 
- `cargo build` (z3-sys may elapse up to 10 mins as it compiles)
- `cd ./samples/function/sym_arg/`
- `cargo run ./main.rs` 
  - or `cargo run -- -O ./main.rs` to compile `main.rs` in release mode
  - Note that any options after `--` will be passed directly to `rustc`
- After the sample is compiled, an output binary `main` will be generated. We can run this as follows:
  - `.\main.exe` or `./main`
- The output from leaf should be as follows (at the time of writing):
```rust
Took step: 0 with constraints [Not(Symbolic(Expression(Binary { operator: Lt, first: SymValueGuard(Symbolic(Variable(SymbolicVar { id: 0, ty: Int { size: 32, is_signed: true } }))), second: Concrete(Const(Int { bit_rep: 5, size: 32, is_signed: true })), is_flipped: false })))]
Found a solution:
0 = Concrete(
    Const(
        Int {
            bit_rep: 4,
            size: 32,
            is_signed: true,
        },
    ),
)
```

## More Examples

### Debug Info:
- To see debug logging from the leaf compiler, set the environment variable `RUST_LOG=debug`
  - linux: `export RUST_LOG=debug`
  - powershell: `$Env:RUST_LOG="debug"`
- `cd ./samples/function/sym_arg/`
- `cargo run ./main.rs`
- The output from cargo should appear appended as follows (at the time of writing):
```
[2023-05-29T06:41:42Z INFO  compiler::pass] Running leaf pass on body at ./samples/function/sym_arg/main.rs:3:1: 6:2 (#0)
[2023-05-29T06:41:42Z WARN  compiler::mir_transform::call_addition] Referencing a function with substitution variables (generic).
[2023-05-29T06:41:42Z DEBUG compiler::pass] Visiting Rvalue: _1
...
[2023-05-29T06:41:42Z INFO  compiler::pass] Running leaf pass on body at ./samples/function/sym_arg/main.rs:18:1: 18:12 (#0)
[2023-05-29T06:41:42Z DEBUG compiler::mir_transform::modification] Updating jump target from bb4294967040 to bb1
[2023-05-29T06:41:42Z DEBUG compiler::mir_transform::modification] Updating jump target from bb4294967040 to bb2
```

### Emit MIR:
- `cd ./samples/function/sym_arg/`
- `cargo run -- --emit=mir ./main.rs`
- open main.mir

### Example Reasoning:
- Let's look at the example located at `./samples/function/sym_arg/main.rs`
```rust
use runtime::annotations::Symbolizable;

fn main() {
    let x = 10.mark_symbolic();
    calc(x, 5);
}

fn calc(x: i32, y: i32) {
    if x < y { // line 9
        foo();
    } else {
        bar();
    }
}

fn foo() {}

fn bar() {}
```
- 10 is the only symbolic value in the program, as converted with `mark_symbolic()`. The rest are concrete
- The solution found above of `0 = Concrete(Const(Int { bit_rep: 4, ... } ))` reflects that the 0th symbolic variable takes the value `4`, which satisfies the negation of the condition `!(x < 5)` (line 9), since `10 < 5 => false` is evaluated by the program. `x = 4` satisfies `x < 5` and would take the program down a new execution path.

## Rustfmt

To any project developers, please make sure to copy all hooks from `.github/hooks/` to `.git/hooks/` via the following command, assuming you're in leaf's root directory:

```sh
cp .github/hooks/pre-commit .git/hooks/pre-commit
```

Also note that `.git/` is not cloned with the rest of the repo.