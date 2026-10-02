# Experiment Scripts

This folder contains the scripts used for running the experiments in the paper.
They mainly set up the environment variables and the flags for `cargo` to build and the target crates appropriately with `leafc` as the compiler.

`*_matrix` scripts receive the options to build a single target, while `*_targets` scripts use the former to perform the task for all targets. Targets and the options sent to them are defined in `*_targets.toml` files which can be given as input to the scripts.

The configurations used for the experiments are available under `*_configs` directories.

Targets are Rust crates, which are well-known public repositories. For anonymity reasons we could not use submodules for our forks in this repository. Instead, we embed them here. We provide the revisions and patches on top, if any, for transparency.

## Crates

### bat
At `v0.26.1` with no patch commits.
### bitflags
At `2.11.0` with no patch commits.
### crossterm
At `0.29` with no patch commits.
### flate2
At `1.1.9` with 1 patch commits: [flate2.patch](./patches/flate2.patch)
### hashbrown
At `v0.17.0` with 1 patch commits: [hashbrown.patch](./patches/hashbrown.patch)
### rust-url
At `v2.5.8` with 1 patch commits: [rust-url.patch](./patches/rust-url.patch)
### rustls
At `v/0.23.40` with no patch commits.
### wasmer
At `v7.1.0` with 4 patch commits: [wasmer.patch](./patches/wasmer.patch)

## `rustc-perf`

### For Leaf
At `bb75ff607703cd0a779697f64445a6177910f0b2` with a patch commit: [rustc-perf.patch](./patches/rustc-perf.patch)
## For Miri
At `bb75ff607703cd0a779697f64445a6177910f0b2` with a patch commit: [rustc-perf-miri.patch](./patches/rustc-perf-miri.patch)