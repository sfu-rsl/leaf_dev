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
#![feature(trait_upcasting)]
#![feature(if_let_guard)]

mod config;
mod mir_transform;
mod passes;
mod pri_utils;
mod toolchain_build;
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

use rustc_driver::RunCompiler;

use common::{log_debug, log_info, log_warn};

use std::env;
use std::path::PathBuf;

use constants::*;

fn main() {
    init_logging();

    set_up_compiler();

    std::process::exit(run_compiler(
        // The first argument is the executable file name.
        env::args().collect::<Vec<_>>().into_iter().skip(1),
        None,
    ));
}

fn init_logging() {
    use env;
    use tracing_subscriber::{fmt, layer::SubscriberExt, util::SubscriberInitExt, EnvFilter};
    let env = env::var(constants::LOG_ENV).unwrap_or("warn".to_string());

    let off_tags = [LOG_PASS_OBJECTS_TAG, LOG_PRI_DISCOVERY_TAG, LOG_BB_JUMP_TAG]
        .map(|t| format!("{}=off", t))
        .join(",");
    let custom_filter = EnvFilter::builder().parse_lossy(format!("{},{}", off_tags, &env));

    // Create a formatting layer with optional write style based on environment variable
    let fmt_layer = fmt::layer()
        .with_writer(std::io::stderr)
        .with_ansi(env::var(constants::LOG_WRITE_STYLE_ENV).map_or(true, |val| val != "never"));

    // Create a subscriber
    tracing_subscriber::registry()
        .with(custom_filter)
        .with(fmt_layer)
        .init();
}

pub fn set_up_compiler() {
    use rustc_session::{config::ErrorOutputType, EarlyDiagCtxt};

    rustc_driver::init_rustc_env_logger(&EarlyDiagCtxt::new(ErrorOutputType::default()));
    rustc_driver::install_ice_hook(URL_BUG_REPORT, |_| ());
}

pub fn run_compiler(args: impl Iterator<Item = String>, input_path: Option<PathBuf>) -> i32 {
    let config = config::load_config();

    let args = driver_args::set_up_args(args, input_path, &config);
    log_info!("Running compiler with args: {:?}", args);

    let mut callbacks = driver_callbacks::set_up_callbacks(
        config,
        &driver_args::ArgsExt::parse_crate_options(&args),
    );

    rustc_driver::catch_with_exit_code(|| RunCompiler::new(&args, callbacks.as_mut()).run())
}

/// Returns `true` if the crate is ineffective for symbolic execution of the target.
/// Examples include build scripts and procedural macro crates.
fn is_ineffective_crate(opts: &driver_args::CrateOptions) -> bool {
    if opts
        .crate_name
        .as_ref()
        .is_some_and(|name| name == CRATE_NAME_BUILD_SCRIPT)
    {
        return true;
    }

    if opts
        .crate_types
        .as_ref()
        .is_some_and(|types| types.len() == 1 && types[0] == driver_args::CrateType::ProcMacro)
    {
        return true;
    }

    false
}

mod driver_callbacks {
    use common::{log_debug, log_info, log_warn};

    use super::{
        config::{LeafCompilerConfig, RuntimeShimLocation},
        passes::*,
        *,
    };
    use crate::utils::chain;

    pub(super) fn set_up_callbacks(
        config: LeafCompilerConfig,
        crate_options: &driver_args::CrateOptions,
    ) -> Box<Callbacks> {
        let is_ineffective_crate = super::is_ineffective_crate(crate_options);
        if (!config.codegen_all_mir || config.building_core) && is_ineffective_crate {
            log_info!("Leafc will work as the normal Rust compiler.");
            Box::new(NoOpPass.to_callbacks())
        } else {
            let mut passes = if config.codegen_all_mir && is_ineffective_crate {
                log_info!("Setting up passes as for a primary package in codegen all mode.");
                build_minimal_passes_in_codegen_all_mode()
            } else if config.codegen_all_mir
                && is_dependency_crate(crate_options.crate_name.as_ref())
            {
                log_info!("Setting up passes as for a dependency in codegen all mode.");
                build_dep_passes_in_codegen_all_mode()
            } else {
                log_info!("Setting up passes as for a primary package.");
                let mut passes = build_primary_passes(&config);
                passes.add_config_callback(Box::new(shim_dep::config_shim_dep));
                passes
            };
            passes.set_leaf_config(config);

            passes.add_config_callback(Box::new(move |rustc_config, leafc_config| {
                let cfg_name = leafc_config.marker_cfg_name.clone();
                if cfg_name.is_empty() {
                    return;
                }
                log_info!("Adding marker cfg to the crate config: `{cfg_name}`.");
                rustc_config
                    .crate_check_cfg
                    .push(format!("cfg({})", cfg_name));
                rustc_config.crate_cfg.push(cfg_name.clone());
            }));
            passes.add_config_callback(Box::new(move |rustc_config, _| {
                /* Forcing inlining to happen as some compiler helper functions in the PRI use
                 * generic functions from the core library which may cause infinite loops. */
                rustc_config.opts.unstable_opts.inline_mir = Some(true);
            }));
            passes.add_config_callback(Box::new(codegen_all::config_codegen_all));
            passes
        }
    }

    fn build_primary_passes(config: &LeafCompilerConfig) -> Box<Callbacks> {
        let prerequisites_pass = RuntimeExternCrateAdder::new(
            matches!(
                config.runtime_shim.location,
                RuntimeShimLocation::External { .. }
            ),
            match config.runtime_shim.location {
                RuntimeShimLocation::CoreLib => None,
                RuntimeShimLocation::External { ref crate_name, .. } => Some(crate_name.clone()),
            },
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

        let passes = chain!(
            prerequisites_pass,
            <LeafToolAdder>,
            <TypeExporter>,
            nctfe_pass,
            Instrumentor::new(true, None /* FIXME */),
        );

        if config.codegen_all_mir {
            Box::new(
                chain!(force_codegen_all_pass(), passes,)
                    .into_logged()
                    .to_callbacks(),
            )
        } else {
            Box::new(passes.into_logged().to_callbacks())
        }
    }

    fn build_dep_passes_in_codegen_all_mode() -> Box<Callbacks> {
        /* In this mode, we only internalize the items in the compiled objects,
         * and do the instrumentation with the final crate. */
        Box::new(
            chain!(
                force_codegen_all_pass(),
                <MonoItemInternalizer>,
            )
            .to_callbacks(),
        )
    }

    fn build_minimal_passes_in_codegen_all_mode() -> Box<Callbacks> {
        /* If we are using the version of the core library with internalized symbols,
         * then we have to force codegen for all items so the internalized symbols
         * will be found. */
        Box::new(chain!(force_codegen_all_pass(),).to_callbacks())
    }

    const SHOULD_CODEGEN_FLAGS: u8 = OverrideFlags::SHOULD_CODEGEN.bits();

    fn force_codegen_all_pass() -> OverrideFlagsForcePass<SHOULD_CODEGEN_FLAGS> {
        /* We must enable overriding should_codegen to force the codegen for all items.
         * Currently, overriding it equals to forcing the codegen for all items. */
        OverrideFlagsForcePass::<SHOULD_CODEGEN_FLAGS>::default()
    }

    fn is_dependency_crate(crate_name: Option<&String>) -> bool {
        let from_cargo = rustc_session::utils::was_invoked_from_cargo();
        let is_primary = env::var("CARGO_PRIMARY_PACKAGE").is_ok();

        log_debug!(
            "Checking if crate `{}` is a dependency. From cargo: {}, Primary Package: {}",
            crate_name.map(|s| s.as_str()).unwrap_or("UNKNOWN"),
            from_cargo,
            is_primary,
        );

        from_cargo && !is_primary
    }

    mod shim_dep {
        use rustc_session::{
            config::{ExternEntry, ExternLocation, Externs},
            search_paths::{PathKind, SearchPath, SearchPathFile},
            utils::CanonicalizedPath,
        };

        use std::{collections::BTreeMap, iter, path::Path};

        use super::*;
        use crate::utils::file::*;

        const DIR_DEPS: &str = "deps";
        pub(super) const FILE_RUNTIME_SHIM_LIB: &str = "libleafrtsh.rlib";
        const PATH_SHIM_LIB_LOCATION: &str = env!("SHIM_LIB_LOCATION"); // Set by the build script.

        pub(super) fn config_shim_dep(
            rustc_config: &mut rustc_interface::Config,
            leafc_config: &mut LeafCompilerConfig,
        ) {
            match &leafc_config.runtime_shim.location {
                RuntimeShimLocation::CoreLib => {
                    log_info!("Expecting the runtime shim as a part of core library")
                    // Nothing to do.
                }
                RuntimeShimLocation::External {
                    // The crate name will be taken care of in the dedicated pass.
                    crate_name: _,
                    search_path,
                } => {
                    log_info!("Adding the runtime shim as an external dependency");
                    add_shim_as_external(rustc_config, search_path)
                }
            }
        }

        fn add_shim_as_external(
            rustc_config: &mut rustc_interface::Config,
            search_path: &config::RuntimeShimExternalLocation,
        ) {
            use crate::config::RuntimeShimExternalLocation::*;
            let location = match search_path {
                CrateSearchPaths => {
                    log_debug!("Expecting the runtime shim in the search paths of the crate.");
                    ExternLocation::FoundInLibrarySearchDirectories
                }
                _ => {
                    let rlib_path = match search_path {
                        Compiler => find_shim_lib_path(),
                        Exact(path) => PathBuf::from(path),
                        _ => unreachable!(),
                    };
                    let search_path = search_path_for_transitive_deps(&rlib_path);

                    log_debug!(
                        "Runtime shim path: {}, with {} transitive deps at: {}",
                        rlib_path.display(),
                        search_path.files.len(),
                        search_path.dir.display()
                    );

                    rustc_config.opts.search_paths.push(search_path);
                    ExternLocation::ExactPaths(
                        core::iter::once(CanonicalizedPath::new(&rlib_path)).collect(),
                    )
                }
            };

            let extern_entry = ExternEntry {
                add_prelude: true,
                force: true,
                is_private_dep: false,
                nounused_dep: false,
                location,
            };
            rustc_config.opts.externs = Externs::new(
                // Let the user force the location if they want to.
                iter::once((CRATE_RUNTIME.to_owned(), extern_entry))
                    .chain(
                        rustc_config
                            .opts
                            .externs
                            .iter()
                            .map(|(k, v)| (k.clone(), v.clone())),
                    )
                    .collect::<BTreeMap<_, _>>(),
            );
        }

        fn find_shim_lib_path() -> PathBuf {
            find_dependency_path(
                FILE_RUNTIME_SHIM_LIB,
                iter::once(Path::new(PATH_SHIM_LIB_LOCATION)),
            )
        }

        fn search_path_for_transitive_deps(lib_file_path: &Path) -> SearchPath {
            // The `deps` folder next to the library file
            let deps_path = lib_file_path.parent().unwrap().join(DIR_DEPS);
            // Source: rustc_session::search_paths::SearchPath::new
            let files = std::fs::read_dir(&deps_path)
                .map(|files| {
                    files
                        .filter_map(|e| {
                            e.ok().and_then(|e| {
                                e.file_name().to_str().map(|s| SearchPathFile {
                                    path: e.path(),
                                    file_name_str: s.to_string(),
                                })
                            })
                        })
                        .collect::<Vec<_>>()
                })
                .unwrap_or_default();
            SearchPath {
                kind: PathKind::Dependency,
                dir: deps_path,
                files,
            }
        }
    }

    mod codegen_all {
        use std::{iter, path::Path};

        use common::log_error;
        use toolchain_build::{self, is_sysroot_compatible};
        use utils::file::try_find_dependency_path;

        use super::*;

        pub(super) fn config_codegen_all(
            rustc_config: &mut rustc_interface::Config,
            leafc_config: &mut LeafCompilerConfig,
        ) {
            if !leafc_config.codegen_all_mir {
                return;
            }

            rustc_config.opts.unstable_opts.always_encode_mir = true;
            rustc_config
                .opts
                .cli_forced_codegen_units
                .replace(1)
                .inspect(|old| {
                    if *old != 1 {
                        log_warn!(
                            concat!(
                                "Forcing codegen units to 1 because of compilation mode. ",
                                "The requested value was: {:?}",
                            ),
                            old,
                        );
                    }
                });

            let is_building_core = leafc_config.building_core
                || rustc_config
                    .crate_cfg
                    .iter()
                    .any(|cfg| cfg == CONFIG_CORE_BUILD);

            if !is_building_core {
                check_sysroot(rustc_config, leafc_config);
            }
        }

        fn check_sysroot(
            rustc_config: &mut rustc_interface::Config,
            leafc_config: &LeafCompilerConfig,
        ) {
            let current_sysroot = rustc_session::filesearch::materialize_sysroot(
                rustc_config.opts.maybe_sysroot.clone(),
            );
            if is_sysroot_compatible(&current_sysroot) {
                return;
            }

            if !leafc_config.override_sysroot {
                log_warn!(concat!(
                    "Codegen all MIR is enabled, while the sysroot does not seem to be built for leaf. ",
                    "It is necessary to use a sysroot with MIR for all libraries included.",
                    "Unless you are doing special build scenarios, this may lead to unexpected errors.",
                ));
                return;
            }

            log_debug!("The current sysroot is probably not compatible for codegen all MIR.");

            let sysroot = try_find_dependency_path(DIR_TOOLCHAIN, iter::empty())
                .filter(|p| is_sysroot_compatible(&p))
                .unwrap_or_else(|| {
                    build_toolchain(&current_sysroot, rustc_config.output_dir.as_deref())
                });
            log_info!(
                "Overriding the sysroot with the one found at: {}",
                sysroot.display()
            );
            rustc_config.opts.maybe_sysroot.replace(sysroot);
        }

        fn build_toolchain(current_sysroot: &Path, out_dir: Option<&Path>) -> PathBuf {
            log_info!(
                "Building a compatible toolchain based on the current sysroot: {}",
                current_sysroot.display()
            );

            let result: PathBuf = toolchain_build::build_toolchain(current_sysroot, out_dir)
                .unwrap_or_else(|e| {
                    log_error!("Failed to build the toolchain: {}", e);
                    std::process::exit(1);
                });
            assert!(is_sysroot_compatible(&result));
            result
        }
    }
}

pub mod constants {
    use const_format::concatcp;

    pub(super) const CRATE_NAME_BUILD_SCRIPT: &str = "build_script_build";

    // The instrumented code is going to call the shim.
    pub(super) const CRATE_RUNTIME: &str = "leafrtsh";

    pub(crate) const CONFIG_ENV_PREFIX: &str = "LEAFC";

    pub(super) const CONFIG_CORE_BUILD: &str = "core_build";

    pub(super) const URL_BUG_REPORT: &str = "https://github.com/sfu-rsl/leaf/issues/new";

    pub(super) const LEAF_AUG_MOD_NAME: &str = "__leaf_augmentation";

    pub const LOG_ENV: &str = concatcp!(CONFIG_ENV_PREFIX, "_LOG");
    pub const LOG_WRITE_STYLE_ENV: &str = concatcp!(CONFIG_ENV_PREFIX, "_LOG_STYLE");

    pub const LOG_PASS_OBJECTS_TAG: &str = super::passes::logger::TAG_OBJECTS;
    pub const LOG_PRI_DISCOVERY_TAG: &str = super::pri_utils::TAG_DISCOVERY;
    pub const LOG_BB_JUMP_TAG: &str = super::mir_transform::TAG_BB_JUMP;

    pub const TOOL_LEAF: &str = "leaf_attr";

    pub const DIR_TOOLCHAIN: &str = "toolchain";

    pub const ENV_RUSTUP_TOOLCHAIN: &str = "RUSTUP_TOOLCHAIN";
}

mod driver_args {
    pub(super) use rustc_session::config::CrateType;

    use super::{utils::file::*, *};

    use std::path::{Path, PathBuf};
    use std::{env, fs, iter};

    const CMD_RUSTC: &str = "rustc";
    const CMD_RUSTUP: &str = "rustup";

    const CODEGEN_LINK_ARG: &str = "link-arg";

    const ENV_RUSTUP_HOME: &str = "RUSTUP_HOME";
    const ENV_SYSROOT: &str = "RUST_SYSROOT";

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

    const OPT_CODEGEN: &str = "-C";
    const OPT_CRATE_NAME: &str = "--crate-name";
    const OPT_CRATE_TYPE: &str = "--crate-type";
    const OPT_LINK_NATIVE: &str = "-l";
    const OPT_PRINT_SYSROOT: &str = "--print=sysroot";
    const OPT_SYSROOT: &str = "--sysroot";
    const OPT_SEARCH_PATH: &str = "-L";
    const OPT_UNSTABLE: &str = "-Zunstable-options";

    const SEARCH_KIND_NATIVE: &str = "native";

    const SUFFIX_OVERRIDE: &str = "(override)";

    const MAX_RETRY: usize = 5;

    macro_rules! read_var {
        ($name:expr) => {{ env::var($name).ok() }};
    }

    // FIXME: #467
    pub(super) struct CrateOptions {
        pub crate_name: Option<String>,
        pub crate_types: Option<Vec<CrateType>>,
    }

    pub(super) trait ArgsExt {
        fn set_if_absent(&mut self, key: &str, get_value: impl FnOnce() -> String);

        fn add_pair(&mut self, key: &str, value: String);

        fn parse_crate_options(&self) -> CrateOptions;
    }

    impl<T: AsRef<Vec<String>> + AsMut<Vec<String>>> ArgsExt for T {
        fn set_if_absent(&mut self, key: &str, get_value: impl FnOnce() -> String) {
            if !self.as_mut().iter().any(|arg| arg.starts_with(key)) {
                self.add_pair(key, get_value());
            }
        }

        fn add_pair(&mut self, key: &str, value: String) {
            self.as_mut().push(key.to_owned());
            self.as_mut().push(value);
        }

        fn parse_crate_options(&self) -> CrateOptions {
            let find_crate_name = || -> Option<String> {
                let index = self
                    .as_ref()
                    .iter()
                    .rposition(|arg| arg == OPT_CRATE_NAME)?
                    + 1;
                self.as_ref().get(index).cloned()
            };

            let find_crate_types = || -> Option<Vec<CrateType>> {
                let index = self.as_ref().iter().position(|arg| arg == OPT_CRATE_TYPE)? + 1;
                let types = rustc_session::config::parse_crate_types_from_list(vec![
                    self.as_ref().get(index).cloned()?,
                ]);
                types.ok()
            };

            CrateOptions {
                crate_name: find_crate_name(),
                crate_types: find_crate_types(),
            }
        }
    }

    pub(super) fn set_up_args(
        given_args: impl Iterator<Item = String>,
        input_path: Option<PathBuf>,
        config: &crate::config::LeafCompilerConfig,
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

        let crate_options = args.parse_crate_options();

        /* Add linking to the runtime dynamic library for all commands.
         * As some crates might be built with the version of the core library
         * with embedded runtime shim, we add it anyway. It won't be effective
         * if there is no linking required.
         * Related to #462. */
        set_up_runtime_dylib(&mut args, &crate_options);

        if is_ineffective_crate(&crate_options) {
            return args;
        }

        args.push(OPT_UNSTABLE.to_owned());

        if let Some(input_path) = input_path {
            args.push(input_path.to_string_lossy().into_owned());
        }

        args
    }

    fn set_up_runtime_dylib(args: &mut Vec<String>, opts: &CrateOptions) {
        let use_noop_runtime = is_ineffective_crate(opts);

        ensure_runtime_dylib_exists(use_noop_runtime);
        let runtime_dylib_dir = find_runtime_dylib_dir(use_noop_runtime)
            .to_string_lossy()
            .to_string();
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
                .zip(read_var!(ENV_RUSTUP_TOOLCHAIN))
                .map(|(home, toolchain)| format!("{home}/toolchains/{toolchain}"))
        };

        let try_sysroot_env = || read_var!(ENV_SYSROOT);

        // NOTE: Check the priority of these variables in the original compiler.
        try_rustc()
            .or_else(try_toolchain_env)
            .or_else(try_sysroot_env)
            .expect("Unable to find sysroot.")
    }

    fn ensure_runtime_dylib_exists(use_noop_runtime: bool) {
        ensure_runtime_dylib_dir_exist(use_noop_runtime);
        let runtime_dylib_dir = PathBuf::from(find_runtime_dylib_dir(use_noop_runtime));

        fn sym_link_exists(sym_path: &Path) -> bool {
            fs::symlink_metadata(sym_path).is_ok()
        }

        let sym_dylib_path = runtime_dylib_dir.join(FILE_RUNTIME_DYLIB);
        if sym_link_exists(&sym_dylib_path) && sym_dylib_path.exists() {
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
            if sym_link_exists(&sym_dylib_path) {
                if sym_dylib_path.exists() {
                    return Ok(());
                } else {
                    // Invalid symbolic link.
                    fs::remove_file(&sym_dylib_path)?;
                }
            }

            #[cfg(unix)]
            let result = std::os::unix::fs::symlink(&physical_dylib_path, &sym_dylib_path);
            #[cfg(windows)]
            let result = std::os::windows::fs::symlink_file(&physical_dylib_path, &sym_dylib_path);
            result
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

    fn find_runtime_dylib_dir(use_noop_runtime: bool) -> PathBuf {
        find_dependency_path(get_runtime_dylib_folder(use_noop_runtime), iter::empty())
    }

    fn get_runtime_dylib_folder(use_noop_runtime: bool) -> &'static str {
        if use_noop_runtime {
            DIR_RUNTIME_DYLIB_NOOP
        } else {
            DIR_RUNTIME_DYLIB_DEFAULT
        }
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
