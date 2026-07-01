# Experiment Scripts

This folder contains the scripts used for running the experiments in the paper.
They mainly set up the environment variables and the flags for `cargo` to build and the target crates appropriately with `leafc` as the compiler.

`*_matrix` scripts receive the options to build a single target, while `*_targets` scripts use the former to perform the task for all targets. Targets and the options sent to them are defined in `*_targets.toml` files which can be given as input to the scripts.

The configurations used for the experiments are available under `*_configs` directories.

Targets are Rust crates, which are well-known public repositories. For anonymity reasons we could not include our forks in this repository, however, the original repositories are listed in `.gitmodules`. 