# leaf

Concolic execution for Rust using MIR (mid-level IR). The project is in very early stages.

As a brief overview, `leafc` is used instead of `rustc` to compile a Rust program. It compiles the program normally, but
also modifies the code and injects various function calls to the `runtime` via the pri (program runtime interface).

## Table of Contents
- [How to run?](#how-to-run)
  - [Setup](#setup)
    - [Setup for local machines](#setup-for-local-machines)
    - [Setup for VS Code Dev Container](#setup-for-vs-code-dev-container)
  - [Run a simple example](#run-a-simple-example)
  - [Emit MIR](#emit-mir)
- [Miscellaneous](#miscellaneous)
  - [Example reasoning](#example-reasoning)
  - [`rushfmt` Git Hook](#rushfmt-git-hook)
  - [Project Wiki](#project-wiki)

## How to run?

### Setup

You can choose to setup and run the `leaf` project on your local machine or VS Code Dev Container.

#### Setup for local machines

Follow one the below guides based on your platform.

<details>
<summary><b>Linux</b></summary>

1. Install Rust.
```sh
$ curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

2. Install dependencies.
```sh
$ sudo apt install clang z3
```

3. Make sure the project can be built successfully.
```sh
$ cd leaf
$ cargo build
```
</details>

<details>
<summary><b>Windows</b></summary>

1. Follow the specific Rust [installation methods](https://www.rust-lang.org/tools/install) for Windows.

2. Install [cmake](https://cmake.org/download/) (download .msi installer).

3. Install `clang`:
  - **Method 1**: 
    - Install [Visual Studio Build Tools](https://visualstudio.microsoft.com/downloads/#build-tools-for-visual-studio-2022).
    - Open Visual Studio Installer.
    - Under Visual Studio Build Tools select modify.
    - Under workloads, and Desktop development with C++, select "C++ Clang tools for Windows", then install it.
  - **Method 2**: if Method 1 doesn't work, try installing llvm via [choco](https://community.chocolatey.org/packages/llvm) (make sure to `cargo clean` after switching installations).

4. Install `z3`:
  - Visit the Z3 GitHub repository's [releases page](https://github.com/Z3Prover/z3/releases).
  - Scroll down to the "Assets" section and find the latest release version. Look for the file with the extension `.zip` for the Windows platform.
  - Click on the `.zip` file to download it.
  - Extract the contents of the downloaded `.zip` file to a directory of your choice, such as `C:\z3`.
  - Add the Z3 directory to both the system AND the User's PATH environment variable by running a  command similar to the following: `setx PATH "%PATH%;C:\z3\bin` (you may need to change the address of the folder).
  - This will allow you to access the Z3 solver and its libraries from any command prompt window.
  - Verify the installation by running the following command: `z3 --version`.

5. Configure newer `z3-rust` bindings:
  - Set the `Z3_SYS_Z3_HEADER` environment variable to `<path-to-your-z3>\include\z3.h`.
  - For the time being, you must also replace `z3 = { version = "0.11.2" }` in `leaf\runtime\Cargo.toml` with `z3 = { git = "https://github.com/prove-rs/z3.rs.git" }` in order to use the newest version that supports the use of the `Z3_SYS_Z3_HEADER` environment variable.

6. Make sure the project can be built successfully.
```sh
$ cd leaf
$ cargo build
```

</details>

#### Setup for VS Code Dev Container

Regardless of which platform you used, the setup for VS Code Dev Container should only be as follows.

<details>
<summary><b>Instructions</b></summary>

1. Install [VS Code](https://code.visualstudio.com/download) and [Dev Container](https://code.visualstudio.com/docs/devcontainers/containers) extension.

2. Duplicate `leaf/.vscode/devcontainer.json.example` to `leaf/.vscode/devcontainer.json` and properly set the path for `CMAKE_BIN_URL` as instructed (e.g. `https://github.com/Kitware/CMake/releases/download/v3.27.4/cmake-3.27.4-linux-x86_64.tar.gz`).

3. Reopen the workspace in Dev Container.

4. Some extensions in `leaf/.vscode/extensions.json` are required in Dev Container so it is recommended to install them all.

5. Open a terminal inside the container and make sure the project can be built successfully.
```sh
$ cd leaf
$ cargo build
```

</details>

### Run a simple example

1. All runnable examples for the `leaf` compiler can be found under `leaf/samples/` folder.

2. Choose an example you want to run.
```sh
$ cd leaf/samples/branching/if_basic
$ cargo run -- main.rs                  # or `cargo run -- -O main.rs` to compile `main.rs` in release mode
$ LEAF_LOG=info ./main                  # you can also try LEAF_LOG=debug to see more output messages
```

3. Observe the output from `leaf`, which should be similar to the below one.

<details>
<summary><b>Sample output</b></summary>

```
[2023-09-04T23:30:13Z INFO  runtime::trace] Took step: 0 with constraints [(&(true, !=(<Var1: i32>, 5i32)))]
[2023-09-04T23:30:13Z INFO  runtime::outgen] Found a solution:
    {
        "1": 5i32,
    }
    
[2023-09-04T23:30:13Z INFO  runtime::trace] Took step: 0 with constraints [(!(==(<Var1: i32>, 5i32)))]
[2023-09-04T23:30:13Z INFO  runtime::trace] Unsatisfiable or unknown result.
[2023-09-04T23:30:13Z INFO  runtime::outgen] Found a solution:
    {
        "1": 5i32,
    }
    
[2023-09-04T23:30:13Z INFO  runtime::trace] Took step: 0 with constraints [(==(<Var1: i32>, 10i32))]
[2023-09-04T23:30:13Z INFO  runtime::outgen] Found a solution:
    {
        "1": 0i32,
    }
    
[2023-09-04T23:30:13Z INFO  runtime::trace] Took step: 0 with constraints [(&(true, !=(%(<Var1: i32>, 2i32), 1i32)))]
[2023-09-04T23:30:13Z INFO  runtime::trace] Unsatisfiable or unknown result.
[2023-09-04T23:30:13Z INFO  runtime::outgen] Found a solution:
    {
        "1": 1i32,
    }
``` 

</details>

### Emit MIR

1. Run the below commands.
```sh
$ cd ./samples/branching/if_basic/
$ cargo run -- --emit=mir main.rs
```

2. Observe the output in `main.mir` file.

## Miscellaneous

### Example reasoning
- Let's look at the example located at `leaf/samples/function/sym_arg/main.rs`.
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
- 10 is the only symbolic value in the program, as converted with `mark_symbolic()`. The rest are concrete.
- The solution found above of `0 = Concrete(Const(Int { bit_rep: 4, ... } ))` reflects that the 0th symbolic variable takes the value `4`, which satisfies the negation of the condition `!(x < 5)` (line 9), since `10 < 5 => false` is evaluated by the program. `x = 4` satisfies `x < 5` and would take the program down a new execution path.

### `rushfmt` Git Hook

To any project developers, please make sure to copy all hooks from `.github/hooks/` to `.git/hooks/` via the following command, assuming you're in leaf's root directory:

```sh
cp .github/hooks/pre-commit .git/hooks/pre-commit
```

With `rushfmt` Git Hook, if you try to commit code that is not well-formatted, it will fail.

Also note that `.git/` is not cloned with the rest of the repo.

### Project Wiki

You can find more information (e.g. tips and tricks) under [project wiki](https://github.com/sfu-rsl/leaf/wiki) pages.
