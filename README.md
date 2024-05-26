# leaf

Concolic execution for Rust using MIR (mid-level IR). The project is under development toward the first version.

As a brief overview, `leafc` is a wrapper around the Rust compiler and is usable anywhere in place of `rustc`. It compiles and instruments the program at the MIR level with function calls to the runtime library (shim)

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

## Development Setup

You can set up and run the `leaf` project on your native environment or in a VS Code Dev Container.

### Native Environment

Follow one of the guides below based on your platform.

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

### VS Code Dev Container

Regardless of which platform you used, the setup for VS Code Dev Container should only be as follows.

<details>
<summary><b>Instructions</b></summary>

1. Install [VS Code](https://code.visualstudio.com/download) and [Dev Container](https://code.visualstudio.com/docs/devcontainers/containers) extension.

2. Duplicate `leaf/.devcontainer/devcontainer.json.example` to `leaf/.devcontainer/devcontainer.json` and properly set the path for `CMAKE_BIN_URL` as instructed (e.g. `https://github.com/Kitware/CMake/releases/download/v3.27.4/cmake-3.27.4-linux-x86_64.tar.gz`).

3. Reopen the workspace in Dev Container.

4. Some extensions in `leaf/.vscode/extensions.json` are required in Dev Container so it is recommended to install them all.

5. Open a terminal inside the container and make sure the project can be built successfully.
```sh
$ cd leaf
$ cargo build
```

</details>

## Compiling and Running
`leaf`'s compiler (`leafc`) is a wrapper around `rustc` and should be usable in place of it. This includes the compilation of single Rust source code to a binary executable, or `cargo` crates.
By default, an executable named `leafc` will be generated under `target/<profile>`. Note that at the moment this executable is not standalone and is dependent on dependencies in the folders nearby and an implementation of the runtime library named `leafrt[-flavor].so`. Passing the arguments you regularly pass to `rustc` to `leafc` should give you an instrumented version of the output.

An alternative approach is to run the compiler using `cargo run`. In the root directory of the project, or in the compiler's project directory execute `cargo run -- <your arguments>` to run `leafc`.

### An Example
1. Choose a single-file program you want to run. A set of sample programs can be found under the `samples` directory in the project's root. Let's pick `if_basic` for this example.
1. Compile it using `cargo run` (or `target/<profile>/leafc`)
    ```sh
    $ cd samples/branching/if_basic
    $ cargo run -- main.rs                  # or `cargo run -- -O main.rs` to compile `main.rs` in release mode
    ```
1. Enable logging by setting `LEAF_LOG` environment variable and then run the compiled program.
    ```sh
    $ LEAF_LOG=info ./main                  # you can also try LEAF_LOG=debug to see more output messages
    ```
1. Observe the output from the backend, which should be similar to the below one.
    <details>
    <summary><b>Sample output</b></summary>

    ```log
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

### Cargo
In most cases, we have Rust projects rather than a single file and we build them using `cargo`. To make `cargo` use `leafc` instead of `rustc` you can set `RUSTC` environment variable ([more information]([url](https://doc.rust-lang.org/cargo/reference/environment-variables.html))). 
A few notes:
- It might get complicated if you use `cargo run` for `leafc` as the target project and leaf may get overridden by each other. So a reasonable approach is to provide a path to `leafc` for `RUSTC`.
- Currently, `leafc` is built with the latest nightly version of Rust and might require the latest nightly version of the standard library as well. So make sure that when compiling your target project `cargo` is using a compatible nightly toolchain.
- You might get errors regarding missing shared libraries. In this case, make sure that a compatible nightly version of the standard library is in the search path of the environment you're running the commands in (`LD_LIBRARY_PATH`).
- The compiler links the runtime shim library statically to the target program, so make sure that the `rlib` file is discoverable by it. On failures, please enable bugs and check which paths are searched by it.

#### Example
1. Choose a project you want to run. A set of sample projects can be found under the `samples/crates` directory in the project's root. Let's pick `sym_basic_bin` for this example.
1. Set `RUSTC` environment variable to override the compiler `cargo` uses during the build.
    ```sh
    $ export RUSTC=<path_to_leaf>/target/<profile>/leafc
    ```
1. Build the project with `cargo build`
    ```sh
    $ cd samples/crates/sym_basic_bin
    $ cargo build
    ```
1. Run it using `cargo run`.
   ```sh
   $ LEAF_LOG=info cargo run
   ```
1. Observe the output from the backend, which should be similar to the below one.
    <details>
    <summary><b>Sample output</b></summary>

    ```log
    [2024-02-14T23:00:00Z INFO  leafrt::trace] Took step: 0 with constraints [(<(<Var1: i32>, 5i32))]
    [2024-02-14T23:00:00Z INFO  leafrt::outgen] Found a solution:
        {
            "1": 8i32,
        }
    ``` 
    </details>
### Emit MIR
It can be handy to check the instrumented MIR of a program. By passing `--emit=mir` you can get such output.

1. Run the below commands.
```sh
$ cd ./samples/branching/if_basic/
$ cargo run -- --emit=mir main.rs
```

2. Observe the output in `main.mir` file.

## Miscellaneous

### Logging
We use [`env_logger`](https://docs.rs/env_logger/latest/env_logger/) with environment variables `LEAFC_LOG` and `LEAF_LOG` respectively for the compiler and the runtime library. Setting these variables to `info` will make the loggings emitted at the standard level. Some logs are tagged particularly to be easily configurable independently. You can refer to the example `tasks.json` under `.vscode` directory to find them.

### Example reasoning
- Let's look at the example located at `leaf/samples/function/sym_arg/main.rs`.
```rust
use leaf::annotations::Symbolizable;

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

### `rustfmt` Git Hook

To any project developers, please make sure to copy all hooks from `.github/hooks/` to `.git/hooks/` via the following command, assuming you're in `leaf`'s root directory:

```sh
cp .github/hooks/pre-commit .git/hooks/pre-commit
```

With `rustfmt` Git Hook, if you try to commit code that is not well-formatted, the commit command will fail with the formatting message. Make sure you stage those changes and commit again.

Also note that `.git/` is not cloned with the rest of the repo.

### Project Wiki

You can find more information (e.g. tips and tricks) under [project wiki](https://github.com/sfu-rsl/leaf/wiki) pages.
