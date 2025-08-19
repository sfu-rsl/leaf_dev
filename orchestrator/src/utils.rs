use core::f64;
use std::{
    collections::HashMap,
    fs::OpenOptions,
    io,
    path::Path,
    process::{self, Stdio},
    time::{Duration, Instant},
};

use common::{conc_loop::GeneratedInputRecord, log_debug};
use derive_more::{Deref, From};

use crate::args::OutputFormat;

pub fn run_blocking<Fut>(f: Fut) -> Fut::Output
where
    Fut: futures::Future,
{
    tokio::runtime::Builder::new_current_thread()
        .enable_io()
        .build()
        .unwrap()
        .block_on(f)
}

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
    blocking_execute_once(
        params,
        format!(
            r#"
            [[exe_trace.inspectors]]
            type = "diverging_input"
            check_optimistic = true
            
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
pub async fn execute_once_for_trace<'a>(
    params: ExecutionParams<'a>,
    traces_dir: &Path,
    full_trace_filename: &str,
    sym_trace_filename: &str,
    preconditions_filename: &str,
) -> Result<ExecutionOutput, io::Error> {
    execute_once(
        params,
        config_for_execute_for_trace(
            traces_dir,
            full_trace_filename,
            true,
            sym_trace_filename,
            true,
            preconditions_filename,
            true,
        ),
    )
    .await
}

pub fn config_for_execute_for_trace(
    traces_dir: &Path,
    full_trace_filename: &str,
    is_full_trace_jsonl: bool,
    sym_trace_filename: &str,
    is_sym_trace_jsonl: bool,
    preconditions_filename: &str,
    is_preconditions_jsonl: bool,
) -> String {
    let format_name = |is_jsonl: bool| {
        if is_jsonl { "jsonl" } else { "bins" }
    };

    format!(
        r#"
            [exe_trace.control_flow_dump]
            type = "file"
            directory = "{dir}"
            prefix = "{full_trace_filename}"
            format = "{full_trace_format}"
            extension = ""

            [exe_trace.constraints_dump]
            type = "file"
            directory = "{dir}"
            prefix = "{sym_trace_filename}"
            format = "{sym_trace_format}"
            extension = ""

            [exe_trace.preconditions_dump]
            type = "file"
            directory = "{dir}"
            prefix = "{preconditions_filename}"
            format = "{preconditions_format}"
            extension = ""

            [exe_trace]
            dump_interval = 5
            inspectors = []
            "#,
        dir = traces_dir.display(),
        full_trace_format = format_name(is_full_trace_jsonl),
        sym_trace_format = format_name(is_sym_trace_jsonl),
        preconditions_format = format_name(is_preconditions_jsonl),
    )
}

fn blocking_execute_once(
    params: ExecutionParams,
    config_toml: String,
) -> Result<ExecutionOutput, io::Error> {
    run_blocking(execute_once(params, config_toml))
}

pub async fn execute_once<'a>(
    ExecutionParams {
        program,
        env,
        stdin,
        stdout,
        stderr,
        args,
    }: ExecutionParams<'a>,
    config_toml: impl AsRef<str>,
) -> Result<ExecutionOutput, io::Error> {
    const ENV_PREFIX: &str = "LEAF_";
    use common::config::{CONFIG_STR, CONFIG_STR_FORMAT};
    let (result, elapsed) = measure_time_async(async || {
        let mut cmd = tokio::process::Command::new(program);
        cmd.envs(env)
            .env(format!("{ENV_PREFIX}{CONFIG_STR}"), config_toml.as_ref())
            .env(format!("{ENV_PREFIX}{CONFIG_STR_FORMAT}"), "toml")
            .stdin(stdin)
            .stdout(stdout)
            .stderr(stderr)
            .args(args);
        log_debug!("Command for execution: {:?}", cmd);
        cmd.output().await
    })
    .await;
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

pub async fn measure_time_async<T, Fut: futures::Future<Output = T>>(
    f: impl FnOnce() -> Fut,
) -> (T, Duration) {
    let now = Instant::now();
    let result = f().await;
    (result, now.elapsed())
}

const PATH_INHERIT: &str = "-";

pub fn output_silence_as_path(silent: bool) -> Option<impl AsRef<Path>> {
    if silent { None } else { Some(PATH_INHERIT) }
}

pub fn is_inherit(path: impl AsRef<Path>) -> bool {
    path.as_ref().to_string_lossy() == PATH_INHERIT
}

#[tracing::instrument(level = "debug", skip_all, fields(path = path.as_ref().and_then(|p| p.as_ref().to_str())))]
pub fn stdio_from_path(path: Option<impl AsRef<Path>>) -> Stdio {
    if let Some(path) = path {
        if is_inherit(&path) {
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

pub fn report_diverging_input(path: &Path, score: Option<f64>, output_format: &OutputFormat) {
    match output_format {
        OutputFormat::Csv => println!("{}, {}", path.display(), score.unwrap_or(f64::NAN)),
        OutputFormat::JsonLines => {
            println!(
                "{}",
                serde_json::ser::to_string(&GeneratedInputRecord {
                    path: path.to_owned(),
                    score
                })
                .unwrap()
            )
        }
    }
}
