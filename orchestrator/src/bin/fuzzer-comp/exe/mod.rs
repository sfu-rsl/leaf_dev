use std::{
    path::{Path, PathBuf},
    sync::Arc,
    time::Duration,
};

use common::{log_debug, log_info, log_warn};
use futures::{Stream, StreamExt, TryStream, TryStreamExt};
use tokio::task::JoinHandle;

use crate::{Trace, potentials::SwitchTrace, utils::GenericError};

mod trace;

#[derive(Clone)]
pub(crate) struct ExecutionConfig {
    program_path: Arc<Path>,
    env: Arc<[(String, String)]>,
    args: Arc<[String]>,
    silent: bool,
    timeout: Option<Duration>,
    workdir: Arc<Path>,
}

impl ExecutionConfig {
    pub(crate) fn new(
        program_path: impl AsRef<Path>,
        env: impl IntoIterator<Item = (String, String)>,
        args: impl IntoIterator<Item = String>,
        silent: bool,
        timeout: Option<Duration>,
        workdir: impl AsRef<Path>,
    ) -> Self {
        ExecutionConfig {
            program_path: program_path.as_ref().to_owned().into_boxed_path().into(),
            env: env.into_iter().collect::<Vec<_>>().into(),
            args: args.into_iter().collect::<Vec<_>>().into(),
            silent,
            timeout,
            workdir: workdir.as_ref().to_owned().into_boxed_path().into(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct Input {
    pub path: PathBuf,
}

trait Executor {
    fn execute_inputs(
        self,
        inputs: impl Stream<Item = Input> + Send + 'static,
    ) -> (
        Option<JoinHandle<()>>,
        impl TryStream<Ok = Trace, Error = GenericError>,
    );
}

mod sequential {
    use tracing::Instrument;

    use super::*;

    #[derive(Clone)]
    pub(super) struct SeqExecutor {
        exe_config: ExecutionConfig,
        config_toml: Arc<str>,
        log_span: tracing::Span,
    }

    impl SeqExecutor {
        const NAME_FULL_TRACE: &str = "full_trace";
        const NAME_SYM_TRACE: &str = "sym_trace";
        const NAME_PRECONDITIONS: &str = "preconditions";

        pub(crate) fn new(exe_config: ExecutionConfig, log_span: tracing::Span) -> Self {
            let config_toml = orchestrator::utils::config_for_execute_for_trace(
                exe_config.workdir.as_ref(),
                Self::NAME_FULL_TRACE,
                false,
                Self::NAME_SYM_TRACE,
                true,
                Self::NAME_PRECONDITIONS,
                true,
            )
            .into();
            SeqExecutor {
                exe_config,
                config_toml,
                log_span,
            }
        }
    }

    impl Executor for SeqExecutor {
        fn execute_inputs(
            self,
            inputs: impl Stream<Item = Input>,
        ) -> (
            Option<JoinHandle<()>>,
            impl TryStream<Ok = Trace, Error = GenericError>,
        ) {
            (
                None,
                inputs
                    .map(move |i| (self.clone(), i))
                    .then(async move |(this, input)| {
                        let log_span = this.log_span.clone();
                        this.exec(input).instrument(log_span).await
                    }),
            )
        }
    }

    impl SeqExecutor {
        #[tracing::instrument(level = "info", skip(self, input), fields(input = %input.path.display()))]
        async fn exec(&self, input: Input) -> Result<Trace, GenericError> {
            log_debug!("Executing");

            use orchestrator::utils::*;

            // FIXME: Send it through pipe to the executor
            let input_buf = tokio::fs::read(&input.path)
                .await
                .map_err(GenericError::from)?;

            let exe_result = {
                let execution = execute_once(
                    ExecutionParams::new(
                        &self.exe_config.program_path,
                        self.exe_config.env.iter().cloned(),
                        Some(&input.path),
                        output_silence_as_path(self.exe_config.silent),
                        output_silence_as_path(self.exe_config.silent),
                        self.exe_config.args.iter().cloned(),
                    ),
                    self.config_toml.as_ref(),
                );
                if let Some(timeout) = self.exe_config.timeout {
                    tokio::time::timeout(timeout, execution).await
                } else {
                    Ok(execution.await)
                }
            };

            match exe_result {
                Err(..) => {
                    log_info!("Execution timed out");
                }
                Ok(exe_result) => {
                    exe_result
                        .map_err(GenericError::from)?
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
                }
            }

            let switch_trace = (self).read_switch_trace().await;
            Ok(Trace {
                switches: switch_trace,
                input_buf: input_buf.into(),
            })
        }

        #[tracing::instrument(level = "debug", skip(self))]
        async fn read_switch_trace(&self) -> SwitchTrace {
            let artifact_path = |name| self.exe_config.workdir.join(name);

            self::trace::read_switch_trace(
                &artifact_path(Self::NAME_FULL_TRACE),
                &artifact_path(Self::NAME_SYM_TRACE),
                &artifact_path(Self::NAME_PRECONDITIONS),
            )
            .await
        }
    }
}

mod fifo {}

mod pool {
    use tokio::{sync::mpsc, task::JoinHandle};
    use tokio_stream::wrappers::ReceiverStream;

    use super::*;

    pub(super) struct ExecutorPool<E, const CAP: usize = 1> {
        executors: Vec<E>,
    }

    impl<E, const CAP: usize> ExecutorPool<E, CAP> {
        pub fn new(executors: Vec<E>) -> Self {
            Self { executors }
        }
    }

    impl<E: Executor, const CAP: usize> Executor for ExecutorPool<E, CAP> {
        fn execute_inputs(
            self,
            inputs: impl Stream<Item = Input> + Send + 'static,
        ) -> (
            Option<JoinHandle<()>>,
            impl TryStream<Ok = Trace, Error = GenericError>,
        ) {
            let (executor_txs, trace_streams): (Vec<_>, Vec<_>) = self
                .executors
                .into_iter()
                .map(|executor| {
                    let (tx, rx) = mpsc::channel(CAP);
                    (
                        tx,
                        Box::pin(
                            executor
                                .execute_inputs(ReceiverStream::new(rx))
                                .1
                                .into_stream(),
                        ),
                    )
                })
                .unzip();

            let process_handle = tokio::spawn(async move {
                tokio::pin!(inputs);
                tokio::pin!(executor_txs);
                while let Some(input) = inputs.next().await {
                    let (ready_tx, _, _) = futures::future::select_all(
                        executor_txs.iter().map(|tx| Box::pin(tx.reserve())),
                    )
                    .await;
                    ready_tx.expect("Executor is not functioning").send(input);
                }
                log_info!("All inputs consumed");
            });

            (
                Some(process_handle),
                futures::stream::select_all(trace_streams),
            )
        }
    }
}

use pool::ExecutorPool;
use sequential::SeqExecutor;

pub(crate) fn traces(
    config: ExecutionConfig,
    parallel_execution_job_count: usize,
    inputs: impl Stream<Item = Input> + Send + 'static,
    inputs_pb: indicatif::ProgressBar,
) -> (JoinHandle<()>, impl Stream<Item = Trace> + Send) {
    let execution_span = tracing::info_span!("Execution");

    let executors = (0..parallel_execution_job_count)
        .map(|i| {
            let mut config = config.clone();
            config.workdir = config.workdir.join(format!("exe_{i}")).into();
            SeqExecutor::new(config, execution_span.clone())
        })
        .collect();
    let executor = ExecutorPool::<SeqExecutor>::new(executors);

    let (process_handle, traces) =
        executor.execute_inputs(inputs.inspect(move |_| inputs_pb.inc(1)));

    (
        process_handle.unwrap(),
        traces.into_stream().filter_map(async move |res| match res {
            Ok(trace) => Some(trace),
            Err(e) => {
                log_warn!("Error executing input: {e}");
                None
            }
        }),
    )
}
