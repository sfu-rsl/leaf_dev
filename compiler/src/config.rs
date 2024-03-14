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

#[derive(Debug, Default, Clone, Deserialize)]
pub(crate) struct RuntimeShimConfig {
    #[serde(default = "default_runtime_shim_crate_name")]
    pub crate_name: String,
    #[serde(default = "default_runtime_shim_as_external")]
    pub as_external: bool,
}

fn default_runtime_shim_crate_name() -> String {
    "leaf".to_string()
}

fn default_runtime_shim_as_external() -> bool {
    true
}
