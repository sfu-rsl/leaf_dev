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

- Configuring z3
- For Linux:
  - `apt install build-essential python3 python3-dev git`
  - `git clone https://github.com/Z3Prover/z3.git`
  - `cd z3`
  - `python3 scripts/mk_make.py --ml`
  - `cd build`
  - `make`
  - `make install`
- For Windows:
  - 1. Install z3:
    - Visit the Z3 GitHub repository's releases page: https://github.com/Z3Prover/z3/releases
    - Scroll down to the "Assets" section and find the latest release version. Look for the file with the extension .zip for the Windows platform.
    - Click on the .zip file to download it.
    - Extract the contents of the downloaded .zip file to a directory of your choice, such as C:\z3.
    - Add the Z3 directory to both the system AND the User's PATH environment variable by running a  command similar to the following: `setx PATH "%PATH%;C:\z3\bin` (you may need to change the address of the folder)
    - This will allow you to access the Z3 solver and its libraries from any command prompt window.
    - Verify the installation by running the following command: `z3 --version`
  - 2. Configure newer z3-rust bindings
    - Set the `Z3_SYS_Z3_HEADER` environment variable to `<path-to-your-z3>\include\z3.h`
    - For the time being, you must also replace `z3 = { version = "0.11.2" }` in `leaf/runtime/Cargo.toml` with `z3 = { git = "https://github.com/prove-rs/z3.rs.git" }` in order to use the newest version that supports the use of the `Z3_SYS_Z3_HEADER` environment variable.


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