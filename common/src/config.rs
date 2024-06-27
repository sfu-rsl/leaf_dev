use std::string::ToString;

use config::{builder::DefaultState, Config, ConfigBuilder, ConfigError, Environment, File};

use crate::log_debug;

pub fn load_config(
    file_name: &str,
    env_prefix: &str,
    config_builder: impl FnOnce(
        ConfigBuilder<DefaultState>,
    ) -> Result<ConfigBuilder<DefaultState>, ConfigError>,
) -> Result<Config, ConfigError> {
    config_builder(Config::builder())
        .expect("Failed to obtain configuration builder")
        .add_source(
            File::with_name(
                &crate::utils::search_current_ancestor_dirs_for(file_name)
                    .map(|p| p.to_string_lossy().to_string())
                    .unwrap_or_else(|| file_name.to_string()),
            )
            .required(false),
        )
        .add_source(
            Environment::with_prefix(env_prefix)
                .prefix_separator("_")
                .separator("__"),
        )
        .build()
        .inspect(|c| log_debug!("Loaded raw configurations: {:?}", c))
}
