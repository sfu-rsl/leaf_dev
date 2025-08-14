use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use common::{log_debug, log_warn};
use futures::{Stream, StreamExt, TryStream, TryStreamExt};

use crate::{Trace, potentials::SwitchTrace, utils::GenericError};

mod trace;

#[derive(Clone)]
pub(crate) struct ExecutionConfig {
    program_path: Arc<Path>,
    env: Arc<[(String, String)]>,
    args: Arc<[String]>,
    silent: bool,
    workdir: Arc<Path>,
}

impl ExecutionConfig {
    pub(crate) fn new(
        program_path: impl AsRef<Path>,
        env: impl IntoIterator<Item = (String, String)>,
        args: impl IntoIterator<Item = String>,
        silent: bool,
        workdir: impl AsRef<Path>,
    ) -> Self {
        ExecutionConfig {
            program_path: program_path.as_ref().to_owned().into_boxed_path().into(),
            env: env.into_iter().collect::<Vec<_>>().into(),
            args: args.into_iter().collect::<Vec<_>>().into(),
            silent,
            workdir: workdir.as_ref().to_owned().into_boxed_path().into(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct Input {
    pub path: PathBuf,
}

#[derive(Clone)]
struct Executor {
    exe_config: ExecutionConfig,
    config_toml: Arc<str>,
}

impl Executor {
    const NAME_FULL_TRACE: &str = "full_trace";
    const NAME_SYM_TRACE: &str = "sym_trace";
    const NAME_PRECONDITIONS: &str = "preconditions";

    pub(crate) fn new(exe_config: ExecutionConfig) -> Self {
        let config_toml = orchestrator::utils::config_for_execute_for_trace(
            exe_config.workdir.as_ref(),
            Self::NAME_FULL_TRACE,
            Self::NAME_SYM_TRACE,
            Self::NAME_PRECONDITIONS,
        )
        .into();
        Executor {
            exe_config,
            config_toml,
        }
    }
}

impl Executor {
    fn execute_inputs(
        self,
        inputs: impl Stream<Item = Input>,
    ) -> impl TryStream<Ok = Trace, Error = GenericError> {
        inputs
            .map(move |i| (self.clone(), i))
            .then(async move |(this, input)| this.exec(input).await)
    }

    #[tracing::instrument(level = "info", skip(self, input), fields(input = %input.path.display()))]
    async fn exec(&self, input: Input) -> Result<Trace, GenericError> {
        log_debug!("Executing");

        use orchestrator::utils::*;

        // FIXME: Send it through pipe to the executor
        let input_buf = tokio::fs::read(&input.path)
            .await
            .map_err(GenericError::from)?;

        let exe_result = execute_once(
            ExecutionParams::new(
                &self.exe_config.program_path,
                self.exe_config.env.iter().cloned(),
                Some(&input.path),
                output_silence_as_path(self.exe_config.silent),
                output_silence_as_path(self.exe_config.silent),
                self.exe_config.args.iter().cloned(),
            ),
            self.config_toml.as_ref(),
        )
        .await
        .map_err(GenericError::from)?;

        exe_result
            .status
            .exit_ok()
            .or_else(|e| {
                if self.exe_config.silent {
                    Ok(())
                } else {
                    Err(e)
                }
            })
            .map_err(GenericError::from)?;

        let switch_trace = (self).read_switch_trace().await;
        Ok(Trace {
            switches: switch_trace,
            input_buf: input_buf.into(),
        })
    }

    #[tracing::instrument(level = "debug", skip(self))]
    async fn read_switch_trace(&self) -> SwitchTrace {
        let artifact_path = |name| self.exe_config.workdir.join(name).with_extension("jsonl");

        self::trace::read_switch_trace(
            &artifact_path(Self::NAME_FULL_TRACE),
            &artifact_path(Self::NAME_SYM_TRACE),
            &artifact_path(Self::NAME_PRECONDITIONS),
        )
        .await
    }
}

pub(crate) fn traces(
    config: ExecutionConfig,
    inputs: impl Stream<Item = Input> + Send,
) -> impl Stream<Item = Trace> + Send {
    let executor = Executor::new(config);
    executor
        .execute_inputs(inputs)
        .into_stream()
        .filter_map(async |res| match res {
            Ok(trace) => Some(trace),
            Err(e) => {
                log_warn!("Error executing input: {e}");
                None
            }
        })
}
