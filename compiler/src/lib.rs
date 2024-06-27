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
#![feature(core_intrinsics)]
#![feature(const_option)]

mod config;
mod mir_transform;
mod passes;
mod pri_utils;
mod utils;
mod visit;

extern crate rustc_abi;
extern crate rustc_apfloat;
extern crate rustc_ast;
extern crate rustc_attr;
extern crate rustc_codegen_ssa;
extern crate rustc_const_eval;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_hir;
extern crate rustc_index;
extern crate rustc_infer;
extern crate rustc_interface;
extern crate rustc_metadata;
extern crate rustc_middle;
extern crate rustc_mir_build;
extern crate rustc_mir_transform;
extern crate rustc_monomorphize;
extern crate rustc_query_system;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;
extern crate rustc_trait_selection;
extern crate rustc_type_ir;
extern crate thin_vec;

use common::{log_debug, log_info, log_warn};
use std::path::PathBuf;

use config::CompilerConfig;
use constants::*;
use rustc_driver::RunCompiler;

use crate::utils::chain;

pub fn set_up_compiler() {
    use rustc_session::{config::ErrorOutputType, EarlyDiagCtxt};

    rustc_driver::init_rustc_env_logger(&EarlyDiagCtxt::new(ErrorOutputType::default()));
    rustc_driver::install_ice_hook(URL_BUG_REPORT, |_| ());
}

pub fn run_compiler(args: impl Iterator<Item = String>, input_path: Option<PathBuf>) -> i32 {
    let config = config::load_config();

    let args = driver_args::set_up_args(args, input_path, &config);
    log_info!("Running compiler with args: {:?}", args);

    use passes::*;
    let run_pass = |pass: &mut Callbacks| -> i32 {
        rustc_driver::catch_with_exit_code(|| RunCompiler::new(&args, pass).run())
    };

    let prerequisites_pass = RuntimeAdder::new(
        config.runtime_shim.crate_name.clone(),
        config.runtime_shim.as_external,
    );

    #[cfg(nctfe)]
    let nctfe_pass = {
        let ctfe_block_ids = {
            let pass = chain!(prerequisites_pass.clone(), <CtfeScanner>,);
            let mut callbacks = pass.to_callbacks();
            run_pass(&mut callbacks);
            let pass = callbacks.into_pass();
            pass.second.into_result()
        };
        NctfeFunctionAdder::new(ctfe_block_ids.len())
    };
    #[cfg(not(nctfe))]
    let nctfe_pass = NoOpPass;

    let should_instrument = should_instrument(&config, &args);
    if !should_instrument {
        log_info!("Instrumentation will be skipped.");
    }
    let instrumentor_pass = Instrumentor::new(should_instrument);

    let pass = chain!(
        prerequisites_pass,
        <TypeExporter>,
        nctfe_pass,
        instrumentor_pass,
    )
    .into_logged();
    run_pass(&mut pass.to_callbacks())
}

fn should_instrument(config: &CompilerConfig, args: &[String]) -> bool {
    let crate_name = driver_args::find_crate_name(args);

    if crate_name.is_some_and(|name| name == "build_script_build") {
        return false;
    }

    true
}

pub mod constants {
    use const_format::concatcp;

    // The instrumented code is going to call the shim.
    pub(super) const CRATE_RUNTIME: &str = "leafrtsh";

    pub(crate) const CONFIG_ENV_PREFIX: &str = "LEAFC";

    pub(super) const URL_BUG_REPORT: &str = "https://github.com/sfu-rsl/leaf/issues/new";

    pub(super) const LEAF_AUG_MOD_NAME: &str = "__leaf_augmentation";

    pub const LOG_ENV: &str = concatcp!(CONFIG_ENV_PREFIX, "_LOG");
    pub const LOG_WRITE_STYLE_ENV: &str = concatcp!(CONFIG_ENV_PREFIX, "_LOG_STYLE");

    pub const LOG_PASS_OBJECTS_TAG: &str = super::passes::logger::OBJECTS_TAG;
    pub const LOG_PRI_DISCOVERY_TAG: &str = super::pri_utils::TAG_DISCOVERY;
}

mod driver_args {
    use super::*;

    use std::path::{Path, PathBuf};
    use std::{env, iter};

    const CMD_RUSTC: &str = "rustc";
    const CMD_RUSTUP: &str = "rustup";

    const CODEGEN_LINK_ARG: &str = "link-arg";

    const DIR_DEPS: &str = "deps";

    const ENV_RUSTUP_HOME: &str = "RUSTUP_HOME";
    const ENV_SYSROOT: &str = "RUST_SYSROOT";
    const ENV_TOOLCHAIN: &str = "RUSTUP_TOOLCHAIN";

    const FILE_RUNTIME_SHIM_LIB: &str = "libleafrtsh.rlib";

    const FILE_RUNTIME_DYLIB_DEFAULT: &str = FILE_RUNTIME_DYLIB_BASIC_LATE_INIT;
    #[allow(dead_code)]
    const FILE_RUNTIME_DYLIB_BASIC: &str = "libleafrt_basic.so";
    #[allow(dead_code)]
    const FILE_RUNTIME_DYLIB_BASIC_LATE_INIT: &str = "libleafrt_basic_li.so";
    const FILE_RUNTIME_DYLIB_NOOP: &str = "libleafrt_noop.so";
    #[allow(dead_code)]
    const FILE_RUNTIME_DYLIB: &str = "libleafrt.so";

    const DIR_RUNTIME_DYLIB_DEFAULT: &str = DIR_RUNTIME_DYLIB_BASIC_LATE_INIT;
    #[allow(dead_code)]
    const DIR_RUNTIME_DYLIB_BASIC: &str = "runtime_basic";
    #[allow(dead_code)]
    const DIR_RUNTIME_DYLIB_BASIC_LATE_INIT: &str = "runtime_basic_li";
    #[allow(dead_code)]
    const DIR_RUNTIME_DYLIB_NOOP: &str = "runtime_noop";

    const LIB_RUNTIME: &str = "leafrt";

    const OPT_EXTERN: &str = "--extern";
    const OPT_CODEGEN: &str = "-C";
    const OPT_CRATE_NAME: &str = "--crate-name";
    const OPT_LINK_NATIVE: &str = "-l";
    const OPT_PRINT_SYSROOT: &str = "--print=sysroot";
    const OPT_SYSROOT: &str = "--sysroot";
    const OPT_SEARCH_PATH: &str = "-L";
    const OPT_UNSTABLE: &str = "-Zunstable-options";

    const PATH_SHIM_LIB_LOCATION: &str = env!("SHIM_LIB_LOCATION"); // Set by the build script.

    const SEARCH_KIND_TRANS_DEP: &str = "dependency";
    const SEARCH_KIND_NATIVE: &str = "native";

    const SUFFIX_OVERRIDE: &str = "(override)";

    const MAX_RETRY: usize = 5;

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
        config: &crate::config::CompilerConfig,
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

        if config.set_sysroot {
            args.set_if_absent(OPT_SYSROOT, find_sysroot);
        }

        args.push(OPT_UNSTABLE.to_owned());

        if config.runtime_shim.as_external {
            // Add the runtime shim library as a direct external dependency.
            let shim_lib_path = find_shim_lib_path();
            args.add_pair(OPT_EXTERN, format!("{}={}", CRATE_RUNTIME, shim_lib_path));
            // Add the runtime shim library dependencies into the search path.
            args.add_pair(
                OPT_SEARCH_PATH,
                format!(
                    "{SEARCH_KIND_TRANS_DEP}={}",
                    find_shim_lib_deps_path(&shim_lib_path)
                ),
            );
        }

        set_up_runtime_dylib(&mut args);

        if let Some(input_path) = input_path {
            args.push(input_path.to_string_lossy().into_owned());
        }

        args
    }

    fn set_up_runtime_dylib(args: &mut Vec<String>) {
        // FIXME: Add better support for setting the runtime flavor.
        // NOTE: If the compiled target is either a build script or a proc-macro crate type, we should use the noop runtime library.
        let args_str = args.join(" ");
        let use_noop_runtime = args_str.contains(&"--crate-name build_script_build".to_string())
            || args_str.contains(&"feature=\\\"proc-macro\\\"".to_string())
            || args_str.contains(&"--crate-type proc-macro ".to_string());

        ensure_runtime_dylib_exists(use_noop_runtime);
        let runtime_dylib_dir = find_runtime_dylib_dir(use_noop_runtime);
        // Add the runtime dynamic library as a dynamic dependency.
        /* NOTE: As long as the shim is getting compiled along with the program,
         * adding it explicitly should not be necessary (is expected to be
         * realized by the compiler). */
        args.add_pair(OPT_LINK_NATIVE, format!("dylib={}", LIB_RUNTIME));
        /* Add the RPATH header to the binary,
         * so there will be a default path to look for the library and including
         * it in `LD_LIBRARY_PATH` won't be necessary. */
        args.add_pair(
            OPT_CODEGEN,
            format!("{CODEGEN_LINK_ARG}=-Wl,-rpath={}", runtime_dylib_dir),
        );
        // Also include it in the search path for Rust.
        args.add_pair(
            OPT_SEARCH_PATH,
            format!("{SEARCH_KIND_NATIVE}={}", runtime_dylib_dir),
        );
    }

    pub(super) fn find_crate_name(args: &[String]) -> Option<String> {
        let index = args.iter().rposition(|arg| arg == OPT_CRATE_NAME)? + 1;
        args.get(index).cloned()
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
                    log_warn!("Unable to find a nightly toolchain. Using the default one.");
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
                        log_debug!("Rustc print sysroot was not successful: {:?}", out);
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

    fn find_shim_lib_path() -> String {
        find_dependency_path(
            FILE_RUNTIME_SHIM_LIB,
            iter::once(Path::new(PATH_SHIM_LIB_LOCATION)),
        )
    }

    fn find_shim_lib_deps_path(lib_file_path: &str) -> String {
        // Select the `deps` folder next to the lib file.
        find_dependency_path(
            DIR_DEPS,
            iter::once(Path::new(lib_file_path).parent().unwrap()),
        )
    }

    fn ensure_runtime_dylib_exists(use_noop_runtime: bool) {
        ensure_runtime_dylib_dir_exist(use_noop_runtime);
        let runtime_dylib_dir = PathBuf::from(find_runtime_dylib_dir(use_noop_runtime));

        let sym_dylib_path = runtime_dylib_dir.join(FILE_RUNTIME_DYLIB);
        if sym_dylib_path.exists() {
            return;
        }

        let physical_dylib_path = if use_noop_runtime {
            find_dependency_path(FILE_RUNTIME_DYLIB_NOOP, iter::empty())
        } else {
            find_dependency_path(FILE_RUNTIME_DYLIB_DEFAULT, iter::empty())
        };

        // NOTE: Parallel execution of the compiler may cause race conditions.
        // FIXME: Come up with a better solution.
        retry(MAX_RETRY, std::time::Duration::from_secs(1), || {
            if sym_dylib_path.exists() {
                Ok(())
            } else {
                #[cfg(unix)]
                let result = std::os::unix::fs::symlink(&physical_dylib_path, &sym_dylib_path);
                #[cfg(windows)]
                let result =
                    std::os::windows::fs::symlink_file(&physical_dylib_path, &sym_dylib_path);
                result
            }
        })
        .expect("Could not create a symlink to the fallback runtime dylib.");
    }

    fn ensure_runtime_dylib_dir_exist(use_noop_runtime: bool) {
        let runtime_dylib_folder = get_runtime_dylib_folder(use_noop_runtime);
        // FIXME: Come up with a better solution.
        retry(MAX_RETRY, std::time::Duration::from_secs(1), || {
            if try_find_dependency_path(runtime_dylib_folder, iter::empty()).is_none() {
                let runtime_dylib_dir = env::current_exe()
                    .unwrap()
                    .parent()
                    .unwrap()
                    .join(runtime_dylib_folder);
                std::fs::create_dir(&runtime_dylib_dir)
            } else {
                Ok(())
            }
        })
        .expect("Could not create a symlink to the fallback runtime dylib.");
    }

    fn find_runtime_dylib_dir(use_noop_runtime: bool) -> String {
        find_dependency_path(get_runtime_dylib_folder(use_noop_runtime), iter::empty())
    }

    fn get_runtime_dylib_folder(use_noop_runtime: bool) -> &'static str {
        if use_noop_runtime {
            DIR_RUNTIME_DYLIB_NOOP
        } else {
            DIR_RUNTIME_DYLIB_DEFAULT
        }
    }

    fn find_dependency_path<'a>(
        name: &'static str,
        priority_dirs: impl Iterator<Item = &'a Path>,
    ) -> String {
        try_find_dependency_path(name, priority_dirs)
            .unwrap_or_else(|| panic!("Unable to find the dependency with name: {}", name))
    }

    fn try_find_dependency_path<'a>(
        name: &str,
        mut priority_dirs: impl Iterator<Item = &'a Path>,
    ) -> Option<String> {
        let try_dir = |path: &Path| {
            log_debug!("Trying dir in search of `{}`: {:?}", name, path);
            common::utils::try_join_path(path, name)
        };

        let try_priority_dirs = || priority_dirs.find_map(try_dir);
        let try_cwd = || env::current_dir().ok().and_then(|p| try_dir(&p));
        let try_exe_path = || {
            env::current_exe()
                .ok()
                .and_then(|p| p.ancestors().skip(1).find_map(try_dir))
        };

        None.or_else(try_priority_dirs)
            .or_else(try_cwd)
            .or_else(try_exe_path)
            .map(|path| path.to_string_lossy().to_string())
    }

    fn retry<T, E>(
        times: usize,
        sleep_dur: std::time::Duration,
        mut f: impl FnMut() -> Result<T, E>,
    ) -> Result<T, E> {
        let mut result = f();
        for _ in 0..times {
            if result.is_ok() {
                break;
            } else {
                std::thread::sleep(sleep_dur);
            }
            result = f();
        }
        result
    }
}
