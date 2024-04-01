use config::{builder::DefaultState, Config, ConfigBuilder};
use serde::Deserialize;

#[derive(Debug, Default, Clone, Deserialize)]
pub(crate) struct CompilerConfig {
    #[serde(default)]
    pub runtime_shim: RuntimeShimConfig,
    #[serde(default)]
    pub building_core: bool,
    #[serde(default)]
    pub set_sysroot: bool,
}

impl CompilerConfig {
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

pub(super) fn load_config() -> CompilerConfig {
    use config::{Environment, File};

    default_builder()
        .add_source(
            File::with_name(&common::utils::search_current_ancestor_dirs_for(
                CONFIG_FILENAME,
            ))
            .required(false),
        )
        .add_source(
            Environment::with_prefix(super::constants::CONFIG_ENV_PREFIX)
                .prefix_separator("_")
                .separator("__"),
        )
        .build()
        .inspect(|c| log::debug!("Loaded raw configurations: {:?}", c))
        .and_then(|c| c.try_deserialize())
        .inspect(|c| log::debug!("Loaded configurations: {:?}", c))
        .expect("Failed to read configurations")
}

fn default_builder() -> ConfigBuilder<DefaultState> {
    Ok(Config::builder())
        .and_then(|b| {
            b.set_default(
                format!(
                    "{}.{}",
                    CompilerConfig::F_RUNTIME_SHIM,
                    RuntimeShimConfig::F_CRATE_NAME,
                ),
                default_runtime_shim_crate_name(),
            )
        })
        .and_then(|b| {
            b.set_default(
                format!(
                    "{}.{}",
                    CompilerConfig::F_RUNTIME_SHIM,
                    RuntimeShimConfig::F_AS_EXTERNAL,
                ),
                default_runtime_shim_as_external(),
            )
        })
        .unwrap()
}
