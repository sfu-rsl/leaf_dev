use serde::Deserialize;

impl TryFrom<::config::Config> for super::BasicBackend {
    type Error = ::config::ConfigError;

    fn try_from(value: ::config::Config) -> Result<Self, Self::Error> {
        let config = value.try_deserialize()?;
        Ok(Self::new(config))
    }
}

#[derive(Debug, Default, Clone, Deserialize)]
pub(crate) struct BasicBackendConfig {
    #[serde(default)]
    pub call: CallConfig,
}

#[derive(Debug, Default, Clone, Deserialize)]
pub(crate) struct CallConfig {
    #[serde(default)]
    pub external_call: ExternalCallStrategy,
}

/* NOTE: Aliases don't work at the moment. */
#[derive(Debug, Default, Clone, Deserialize, serde::Serialize)]
pub(crate) enum ExternalCallStrategy {
    #[serde(alias = "panic")]
    Panic,
    #[default]
    #[serde(alias = "conc", alias = "concretize", alias = "underapprox")]
    Concretization,
    #[serde(alias = "overapprox", alias = "overapproximate")]
    OverApproximation,
    #[serde(alias = "opt_conc")]
    OptimisticConcretization,
}
