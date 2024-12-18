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

#[tracing::instrument(level = "debug", skip(env))]
pub fn execute_once_for_div_inputs(
    program: &Path,
    env: &HashMap<String, String>,
    stdin: Stdio,
    stdout: Stdio,
    stderr: Stdio,
    diverging_inputs_dir: &Path,
    diverging_input_prefix: &str,
    args: Vec<String>,
) -> Result<ExecutionOutput, io::Error> {
    const ENV_PREFIX: &str = "LEAF_";
    use common::config::{CONFIG_STR, CONFIG_STR_FORMAT};
    let config_toml = format!(
        r#"
        [[outputs]]
        type = "file"
        directory = "{dir}"
        format = "binary"
        prefix = "{diverging_input_prefix}"
        extension = ".bin"
        "#,
        dir = diverging_inputs_dir.display(),
    );

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

pub fn stdio_from_path(path: Option<&Path>) -> Stdio {
    if let Some(path) = path {
        if path.to_string_lossy() == "-" {
            Stdio::inherit()
        } else {
            Stdio::from(
                OpenOptions::new()
                    .read(true)
                    .open(path)
                    .unwrap_or_else(|e| panic!("Failed to open file `{}`: {}", path.display(), e)),
            )
        }
    } else {
        Stdio::null()
    }
}
