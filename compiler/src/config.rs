use serde::Deserialize;

#[derive(Debug, Default, Clone, Deserialize)]
pub(crate) struct CompilerConfig {
    #[serde(default)]
    pub runtime_shim: RuntimeShimConfig,
}

#[derive(Debug, Default, Clone, Deserialize)]
pub(crate) struct RuntimeShimConfig {
    #[serde(default = "default_runtime_shim_crate_name")]
    pub crate_name: String,
    #[serde(default)]
    pub as_external: bool,
}

fn default_runtime_shim_crate_name() -> String {
    "leaf".to_string()
}
