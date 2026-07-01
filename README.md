# Leaf

## Instructions
1. Clone the repository.
1. Leaf requires an environment with rust, python, and libclang (required by some dependencies) available. The easiest way is to use the `Dockerfile` in the root folder.
1. Build the docker image and run a container with the source folder mounted to it.
    ```console
    $> docker build -t leaf_env . && \
        docker run \
            --rm -it \
            --user "$(id -u)":"$(id -g)" \
            --mount type=bind,src=$PWD,dst=/workspaces/leaf \
            -w /workspaces/leaf \
            leaf_env \
            bash
    ```
    You can optionally provide the following volumes to maintain cargo's cache across the temporary containers.
    ```
    --volume cargo_cache:/usr/local/cargo \
    --volume rustup_cache:/usr/local/rustup \
    ```
    From now on, we assume we work in the environment.

1. Install `leafc`
    ```console
    $> cargo install --path ./compiler
    ```
1. Build and install the analyses shipped using the script `install_analysis`.
    ```console
    $> ./install_analyses
    ```
    It will build the analyses and puts them in separate folders next to `leafc` in `/usr/local/cargo/bin`.
1. Instrument and compile target programs using `leafc`, e.g.,
    ```console
    $> leafc samples/hello_world.rs -o ./hello_world-leaf
    ```
1. The program by default is linked with the no-op analysis and running it behaves as normal. To run an analysis along with the execution use `LD_LIBRARY_PATH`. For example, we can use the symbolic execution analysis. Other options are the control flow tracer (`cf_tracer`), and the `ManuallyDrop` sanitizer (`md_san`). Enable logging for the analysis using `LEAF_LOG` environment variable.
    ```console
    $> export LD_LIBRARY_PATH=/usr/local/cargo/bin/runtime_symex_li
    $> export LEAF_LOG="info"
    ```
1. Run the instrumented program.
    ```
    hello_world-leaf
    ```
1. An output similar to the following is expected from the execution.
    ```log
    2024-12-10 00:40:55  INFO leafrt Initializing runtime library
    2024-12-10 00:40:55  INFO leafrt::pri::basic::instance Initializing basic backend
    2024-12-10 00:40:55  INFO leafrt::backends::basic::outgen Setting up binary output writing to directory: output
    2024-12-10 00:40:55  INFO leafrt::pri::basic::instance Basic backend initialized
    2024-12-10 00:40:55  INFO leafrt::backends::basic::sym_vars Added a new symbolic variable: <Var1: u8> = 10u8
    2024-12-10 00:40:55  INFO leafrt::trace::log Notified about constraint {!(<(<Var1: u8>, 5u8))} at step Def(0:5)[2]
    2024-12-10 00:40:55  INFO leafrt::outgen Found a solution:
    {
        "1": 0u8,
    }
    ```

This was a demonstration of the basic workflow to perform instrumentation and analysis using MIR.

`leafc` can be used in place of `rustc`. To instrument crates using `cargo`, you can set `RUSTC` environment variable before running the cargo command.