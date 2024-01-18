#![feature(rustc_private)]
#![feature(let_chains)]
#![feature(extend_one)]
#![feature(box_patterns)]
#![feature(extract_if)]
#![deny(rustc::internal)]
#![feature(iter_order_by)]
#![feature(macro_metavar_expr)]
#![feature(box_into_inner)]
#![feature(assert_matches)]

mod mir_transform;
mod passes;
mod pri_utils;
mod utils;
mod visit;

extern crate rustc_abi;
extern crate rustc_apfloat;
extern crate rustc_ast;
extern crate rustc_codegen_ssa;
extern crate rustc_const_eval;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_index;
extern crate rustc_interface;
extern crate rustc_metadata;
extern crate rustc_middle;
extern crate rustc_mir_build;
extern crate rustc_mir_transform;
extern crate rustc_query_system;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;
extern crate rustc_type_ir;
extern crate thin_vec;

use std::path::PathBuf;

use constants::*;
use rustc_driver::RunCompiler;

use crate::utils::chain;

pub fn run_compiler(args: impl Iterator<Item = String>, input_path: Option<PathBuf>) -> i32 {
    let args = driver_args::set_up_args(args, input_path);
    log::info!("Running compiler with args: {:?}", args);

    rustc_driver::install_ice_hook(URL_BUG_REPORT, |_| ());

    use passes::*;
    let run_pass = |mut pass: Callbacks| -> i32 {
        rustc_driver::catch_with_exit_code(|| RunCompiler::new(&args, pass.as_mut()).run())
    };

    #[cfg(nctfe)]
    let nctfe_pass = {
        let ctfe_block_ids = {
            let mut pass = chain!(<PrerequisitePass>, <CtfeScanner>,);
            run_pass(pass.to_callbacks());
            pass.second.into_result()
        };
        NctfeFunctionAdder::new(ctfe_block_ids.len())
    };
    #[cfg(not(nctfe))]
    let nctfe_pass = NullPass;

    let pass = chain!(
        <PrerequisitePass>,
        <TypeExporter>,
        nctfe_pass,
        <Instrumentor>,
    )
    .into_logged();
    run_pass(pass.to_callbacks())
}

pub mod constants {
    pub(super) const CRATE_RUNTIME: &str = "leafrtsh";

    pub(super) const URL_BUG_REPORT: &str = "https://github.com/sfu-rsl/leaf/issues/new";

    pub(super) const LEAF_AUG_MOD_NAME: &str = "__leaf_augmentation";

    pub const LOG_PASS_OBJECTS_TAG: &str = super::passes::logger::OBJECTS_TAG;
}

mod driver_args {
    use super::*;

    use std::env;
    use std::path::{Path, PathBuf};

    const CMD_RUSTC: &str = "rustc";
    const CMD_RUSTUP: &str = "rustup";

    const DIR_DEPS: &str = "deps";

    const ENV_RUSTUP_HOME: &str = "RUSTUP_HOME";
    const ENV_SYSROOT: &str = "RUST_SYSROOT";
    const ENV_TOOLCHAIN: &str = "RUSTUP_TOOLCHAIN";

    const FILE_RUNTIME_LIB: &str = "libleafrtsh.rlib";

    const OPT_EXTERN: &str = "--extern";
    const OPT_PRINT_SYSROOT: &str = "--print=sysroot";
    const OPT_SYSROOT: &str = "--sysroot";
    const OPT_SEARCH_PATH: &str = "-L";
    const OPT_UNSTABLE: &str = "-Zunstable-options";

    const SUFFIX_OVERRIDE: &str = "(override)";

    macro_rules! read_var {
        ($name:expr) => {{ env::var($name).ok() }};
    }

    trait ArgsExt {
        fn set_if_absent(&mut self, key: &str, get_value: impl FnOnce() -> String);

        fn add_pair(&mut self, key: &str, value: String);
    }

    impl<T: AsMut<Vec<String>>> ArgsExt for T {
        fn set_if_absent(&mut self, key: &str, get_value: impl FnOnce() -> String) {
            if !self.as_mut().iter().any(|arg| arg.starts_with(key)) {
                self.add_pair(key, get_value());
            }
        }

        fn add_pair(&mut self, key: &str, value: String) {
            self.as_mut().push(key.to_owned());
            self.as_mut().push(value);
        }
    }

    pub(super) fn set_up_args(
        given_args: impl Iterator<Item = String>,
        input_path: Option<PathBuf>,
    ) -> Vec<String> {
        // Although the driver throws out the first argument, we set the correct value for it.
        let given_args = std::iter::once(
            env::current_exe()
                .unwrap_or_default()
                .to_string_lossy()
                .into_owned(),
        )
        .chain(given_args);
        let mut args = given_args.collect::<Vec<_>>();

        args.set_if_absent(OPT_SYSROOT, find_sysroot);

        args.push(OPT_UNSTABLE.to_owned());

        // Add runtime library as a direct dependency.
        args.add_pair(
            OPT_EXTERN,
            format!("force:{}={}", CRATE_RUNTIME, find_runtime_lib_path()),
        );

        // Add runtime project's dependencies into the search path.
        args.add_pair(OPT_SEARCH_PATH, find_deps_path());

        if let Some(input_path) = input_path {
            args.push(input_path.to_string_lossy().into_owned());
        }

        args
    }

    fn find_sysroot() -> String {
        let try_rustc = || {
            use std::process::Command;
            // Find a nightly toolchain if available.
            // NOTE: It is possible to prioritize the overridden toolchain even if not nightly.
            let toolchain_arg = Command::new(CMD_RUSTUP)
                .args(&["toolchain", "list"])
                .output()
                .ok()
                .filter(|out| out.status.success())
                .and_then(|out| {
                    let lines = std::str::from_utf8(&out.stdout)
                        .ok()?
                        .lines()
                        .filter(|l| l.starts_with("nightly"))
                        .map(str::to_owned)
                        .collect::<Vec<_>>();
                    Some(lines)
                })
                .and_then(|toolchains| {
                    toolchains
                        .iter()
                        .find_map(|t| t.rfind(SUFFIX_OVERRIDE).map(|i| t[..i].to_owned()))
                        .or(toolchains.first().cloned())
                })
                .map(|t| format!("+{}", t.trim()))
                .unwrap_or_else(|| {
                    log::warn!("Unable to find a nightly toolchain. Using the default one.");
                    Default::default()
                });

            Command::new(CMD_RUSTC)
                .arg(toolchain_arg)
                .arg(OPT_PRINT_SYSROOT)
                .output()
                .ok()
                .filter(|out| {
                    if out.status.success() {
                        true
                    } else {
                        log::debug!("Rustc print sysroot was not successful: {:?}", out);
                        false
                    }
                })
                .map(|out| std::str::from_utf8(&out.stdout).unwrap().trim().to_owned())
        };

        let try_toolchain_env = || {
            read_var!(ENV_RUSTUP_HOME)
                .zip(read_var!(ENV_TOOLCHAIN))
                .map(|(home, toolchain)| format!("{home}/toolchains/{toolchain}"))
        };

        let try_sysroot_env = || read_var!(ENV_SYSROOT);

        // NOTE: Check the priority of these variables in the original compiler.
        try_rustc()
            .or_else(try_toolchain_env)
            .or_else(try_sysroot_env)
            .expect("Unable to find sysroot.")
    }

    fn find_runtime_lib_path() -> String {
        // FIXME: Do not depend on the project's structure and adjacency of runtime.

        fn try_dir(path: &Path) -> Option<PathBuf> {
            log::debug!("Trying dir for runtime lib: {:?}", path);
            try_join(path, FILE_RUNTIME_LIB)
        }

        let try_cwd = || try_cwd(try_dir);

        let try_exe_path = || try_exe_ancestors(try_dir);

        try_cwd()
            .or_else(try_exe_path)
            .map(|path| path.to_string_lossy().to_string())
            .expect("Unable to find runtime lib file.")
    }

    fn find_deps_path() -> String {
        // FIXME: Don't depend on the project structure and adjacency of runtime.

        fn try_dir(path: &Path) -> Option<PathBuf> {
            log::debug!("Trying dir for deps dir: {:?}", path);
            try_join(path, DIR_DEPS)
        }

        let try_cwd = || try_cwd(try_dir);
        let try_exe_path = || try_exe_ancestors(try_dir);

        try_cwd()
            .or_else(try_exe_path)
            .map(|path| path.to_string_lossy().to_string())
            .expect("Unable to find runtime lib file.")
    }

    fn try_cwd(f: impl Fn(&Path) -> Option<PathBuf>) -> Option<PathBuf> {
        env::current_dir().ok().and_then(|p| f(p.as_path()))
    }

    fn try_exe_ancestors(f: impl Fn(&Path) -> Option<PathBuf>) -> Option<PathBuf> {
        env::current_exe()
            .ok()
            .and_then(|p| p.ancestors().skip(1).find_map(f))
    }

    fn try_join(path: impl AsRef<Path>, child: impl AsRef<Path>) -> Option<PathBuf> {
        let path = path.as_ref().join(child);
        if path.exists() {
            Some(path.to_owned())
        } else {
            None
        }
    }
}
