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
    pub set_sysroot: bool,
    #[serde(default)]
    pub codegen_all_mir: bool,
}

impl LeafCompilerConfig {
    const F_RUNTIME_SHIM: &'static str = "runtime_shim";
}

#[derive(Debug, Default, Clone, Deserialize)]
pub(crate) struct RuntimeShimConfig {
    #[serde(default = "default_runtime_shim_crate_name")]
    pub crate_name: String,
    #[serde(default = "default_runtime_shim_as_external")]
    pub as_external: bool,
}

impl RuntimeShimConfig {
    const F_CRATE_NAME: &'static str = "crate_name";
    const F_AS_EXTERNAL: &'static str = "as_external";
}

fn default_runtime_shim_crate_name() -> String {
    "leaf".to_string()
}

fn default_runtime_shim_as_external() -> bool {
    true
}

const CONFIG_FILENAME: &str = "leafc_config";

pub(super) fn load_config() -> LeafCompilerConfig {
    common::config::load_config(CONFIG_FILENAME, CONFIG_ENV_PREFIX, |b| {
        Ok(b)
            .and_then(|b| {
                b.set_default(
                    format!(
                        "{}.{}",
                        LeafCompilerConfig::F_RUNTIME_SHIM,
                        RuntimeShimConfig::F_CRATE_NAME,
                    ),
                    default_runtime_shim_crate_name(),
                )
            })
            .and_then(|b| {
                b.set_default(
                    format!(
                        "{}.{}",
                        LeafCompilerConfig::F_RUNTIME_SHIM,
                        RuntimeShimConfig::F_AS_EXTERNAL,
                    ),
                    default_runtime_shim_as_external(),
                )
            })
    })
    .and_then(|c| c.try_deserialize())
    .inspect(|c| log_info!("Loaded configurations: {:?}", c))
    .expect("Failed to read configurations")
}
