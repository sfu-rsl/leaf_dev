use common::log_debug;
use serde::Deserialize;

impl TryFrom<::config::Config> for super::BasicBackend {
    type Error = ::config::ConfigError;

    fn try_from(value: ::config::Config) -> Result<Self, Self::Error> {
        let config = value.try_deserialize()?;
        log_debug!("Loaded configurations: {:?}", config);
        Ok(Self::new(config))
    }
}

#[derive(Debug, Default, Clone, Deserialize)]
pub(crate) struct BasicBackendConfig {
    #[serde(default)]
    pub call: CallConfig,

    #[serde(default)]
    pub sym_place: SymbolicPlaceConfig,

    #[serde(default)]
    pub outputs: Vec<OutputConfig>,
}

#[derive(Debug, Default, Clone, Deserialize)]
pub(crate) struct CallConfig {
    #[serde(default)]
    pub external_call: ExternalCallStrategy,
}

/* NOTE: Aliases don't work at the moment. */
#[derive(Debug, Default, Clone, Deserialize, serde::Serialize)]
#[serde(rename_all = "snake_case")]
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

#[derive(Debug, Default, Clone, Deserialize)]
pub(crate) struct SymbolicPlaceConfig {
    #[serde(default)]
    pub read: SymbolicPlaceStrategy,
    #[serde(default)]
    pub write: SymbolicPlaceStrategy,
}

#[derive(Debug, Default, Clone, Copy, Deserialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum SymbolicPlaceStrategy {
    /// Builds an expression with multiple possible values.
    #[serde(alias = "proj", alias = "expr")]
    ProjExpression,
    /// Uses the concrete value of place for memory reading/writing.
    #[serde(alias = "conc", alias = "concretize", alias = "underapprox")]
    Concretization,
    /// Uses the concrete value of place for memory reading/writing then adds
    /// a constraint for the symbolic place to be equal to its concrete value.
    /// For example, if the symbolic place is generated because of a symbolic
    /// index `i` which currently has the value of `2`, then the slice is
    /// accessed at index `2` and the constraint of `i == 2` is added.
    #[default]
    #[serde(alias = "stamp")]
    Stamping,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "type")]
#[serde(rename_all = "snake_case")]
pub(crate) enum OutputConfig {
    File(FileOutputConfig),
}

#[derive(Debug, Default, Clone, Deserialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum OutputFileFormat {
    #[default]
    Json,
    /// # Remarks
    /// Requires all symbolic values to be byte (u8).
    Binary,
}

#[derive(Debug, Default, Clone, Deserialize)]
pub(crate) struct FileOutputConfig {
    /// The folder to write file outputs to.
    pub directory: std::path::PathBuf,
    /// The format to write the file outputs in.
    pub format: OutputFileFormat,
}
