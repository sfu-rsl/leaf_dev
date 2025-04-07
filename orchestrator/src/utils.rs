use std::{
    collections::HashMap,
    fs::OpenOptions,
    io,
    path::Path,
    process::{self, Command, Stdio},
    time::{Duration, Instant},
};

use derive_more::{Deref, From};

#[derive(Debug, Deref, From)]
pub struct ExecutionOutput {
    #[deref]
    pub output: process::Output,
    pub elapsed_time: Duration,
}

pub struct ExecutionParams<'a> {
    pub program: &'a Path,
    pub env: HashMap<String, String>,
    pub stdin: Stdio,
    pub stdout: Stdio,
    pub stderr: Stdio,
    pub args: Vec<String>,
}

impl ExecutionParams<'_> {
    pub fn new<'a>(
        program: &'a impl AsRef<Path>,
        extra_envs: impl Iterator<Item = (String, String)>,
        stdin_path: Option<impl AsRef<Path>>,
        stdout_path: Option<impl AsRef<Path>>,
        stderr_path: Option<impl AsRef<Path>>,
        args: impl Iterator<Item = String>,
    ) -> ExecutionParams<'a> {
        ExecutionParams {
            program: program.as_ref(),
            env: extra_envs.collect(),
            stdin: stdio_from_path(stdin_path),
            stdout: stdio_from_path(stdout_path),
            stderr: stdio_from_path(stderr_path),
            args: args.collect(),
        }
    }
}

#[tracing::instrument(level = "debug", skip(params))]
pub fn execute_once_for_div_inputs(
    params: ExecutionParams,
    diverging_inputs_dir: &Path,
    diverging_input_prefix: &str,
) -> Result<ExecutionOutput, io::Error> {
    execute_once(
        params,
        format!(
            r#"
        [[outputs]]
        type = "file"
        directory = "{dir}"
        format = "binary"
        prefix = "{diverging_input_prefix}"
        extension = ".bin"
        "#,
            dir = diverging_inputs_dir.display(),
        ),
    )
}

#[tracing::instrument(level = "debug", skip(params))]
pub fn execute_once_for_trace(
    params: ExecutionParams,
    traces_dir: &Path,
    full_trace_filename: &str,
    sym_trace_filename: &str,
) -> Result<ExecutionOutput, io::Error> {
    execute_once(
        params,
        format!(
            r#"
        [exe_trace.decisions_dump]
        type = "file"
        directory = "{dir}"
        prefix = "{full_trace_filename}"
        format = "json"

        [exe_trace.constraints_dump]
        type = "file"
        directory = "{dir}"
        prefix = "{sym_trace_filename}"
        format = "json"
        "#,
            dir = traces_dir.display(),
        ),
    )
}

fn execute_once(
    ExecutionParams {
        program,
        env,
        stdin,
        stdout,
        stderr,
        args,
    }: ExecutionParams,
    config_toml: String,
) -> Result<ExecutionOutput, io::Error> {
    const ENV_PREFIX: &str = "LEAF_";
    use common::config::{CONFIG_STR, CONFIG_STR_FORMAT};
    let (result, elapsed) = measure_time(|| {
        Command::new(program)
            .envs(env)
            .env(format!("{ENV_PREFIX}{CONFIG_STR}"), config_toml)
            .env(format!("{ENV_PREFIX}{CONFIG_STR_FORMAT}"), "toml")
            .stdin(stdin)
            .stdout(stdout)
            .stderr(stderr)
            .args(args)
            .output()
    });
    Ok(ExecutionOutput {
        output: result?,
        elapsed_time: elapsed,
    })
}

pub fn measure_time<T>(f: impl FnOnce() -> T) -> (T, Duration) {
    let now = Instant::now();
    let result = f();
    (result, now.elapsed())
}

const PATH_INHERIT: &str = "-";

pub fn output_silence_as_path(silent: bool) -> Option<impl AsRef<Path>> {
    if silent {
        None
    } else {
        Some(PATH_INHERIT.to_owned())
    }
}

pub fn stdio_from_path(path: Option<impl AsRef<Path>>) -> Stdio {
    if let Some(path) = path {
        if path.as_ref().to_string_lossy() == PATH_INHERIT {
            Stdio::inherit()
        } else {
            Stdio::from(
                OpenOptions::new()
                    .read(true)
                    .open(path.as_ref())
                    .unwrap_or_else(|e| {
                        panic!("Failed to open file `{}`: {}", path.as_ref().display(), e)
                    }),
            )
        }
    } else {
        Stdio::null()
    }
}
