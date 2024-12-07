use serde::Deserialize;

use crate::CONFIG_ENV_PREFIX;
use common::log_info;

#[derive(Debug, Default, Clone, Deserialize)]
pub(crate) struct LeafCompilerConfig {
    #[serde(default)]
    pub runtime_shim: RuntimeShimConfig,
    #[serde(default)]
    pub building_core: bool,
    #[serde(default)]
    pub override_sysroot: bool,
    #[serde(default = "default_codegen_all_mir")]
    pub codegen_all_mir: bool,
    #[serde(default = "default_marker_cfg_name")]
    pub marker_cfg_name: String,
}

fn default_codegen_all_mir() -> bool {
    true
}

fn default_marker_cfg_name() -> String {
    "leafc".to_string()
}

impl LeafCompilerConfig {
    const F_RUNTIME_SHIM: &'static str = "runtime_shim";
}

#[derive(Debug, Default, Clone, Deserialize)]
pub(crate) struct RuntimeShimConfig {
    pub location: RuntimeShimLocation,
}

impl RuntimeShimConfig {
    const F_LOCATION: &'static str = "location";
}

#[derive(Debug, Clone, Deserialize)]
pub(crate) enum RuntimeShimLocation {
    #[serde(alias = "core")]
    CoreLib,
    External {
        #[serde(default = "default_runtime_shim_crate_name")]
        crate_name: String,
        search_path: RuntimeShimExternalLocation,
    },
}

impl RuntimeShimLocation {
    const V_EXTERNAL: &'static str = "external";

    const F_CRATE_NAME: &'static str = "crate_name";
    const F_SEARCH_PATH: &'static str = "search_path";
}

#[derive(Debug, Default, Clone, Deserialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum RuntimeShimExternalLocation {
    /// Treated as a normal dependency,
    /// i.e., the crate is expected to be in the sysroot or other provided search paths.
    #[default]
    #[serde(
        alias = "sysroot",
        alias = "deps",
        alias = "default",
        alias = "search_paths"
    )]
    CrateSearchPaths,
    Compiler,
    #[serde(alias = "exact")]
    Exact(String),
}

impl RuntimeShimExternalLocation {
    const V_SYSROOT: &'static str = "sysroot";
}

impl Default for RuntimeShimLocation {
    fn default() -> Self {
        RuntimeShimLocation::External {
            crate_name: default_runtime_shim_crate_name(),
            search_path: RuntimeShimExternalLocation::CrateSearchPaths,
        }
    }
}

fn default_runtime_shim_crate_name() -> String {
    "leaf".to_string()
}

const CONFIG_FILENAME: &str = "leafc_config";

pub(super) fn load_config() -> LeafCompilerConfig {
    common::config::load_config(CONFIG_FILENAME, CONFIG_ENV_PREFIX, |b| {
        Ok(b)
            .and_then(|b| {
                b.set_default(
                    format!(
                        "{}.{}.{}.{}",
                        LeafCompilerConfig::F_RUNTIME_SHIM,
                        RuntimeShimConfig::F_LOCATION,
                        RuntimeShimLocation::V_EXTERNAL,
                        RuntimeShimLocation::F_CRATE_NAME,
                    ),
                    default_runtime_shim_crate_name(),
                )
            })
            .and_then(|b| {
                b.set_default(
                    format!(
                        "{}.{}.{}.{}",
                        LeafCompilerConfig::F_RUNTIME_SHIM,
                        RuntimeShimConfig::F_LOCATION,
                        RuntimeShimLocation::V_EXTERNAL,
                        RuntimeShimLocation::F_SEARCH_PATH,
                    ),
                    RuntimeShimExternalLocation::V_SYSROOT,
                )
            })
    })
    .and_then(|c| c.try_deserialize())
    .inspect(|c| log_info!("Loaded configurations: {:?}", c))
    .expect("Failed to read configurations")
}
