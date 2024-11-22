#![feature(exit_status_error)]

extern crate orchestrator;
use orchestrator::*;

use clap::Parser;
use glob::glob;

use std::{
    fs,
    path::PathBuf,
    process::{ExitCode, Stdio},
};

use utils::stdio_from_path;

fn parse_env_pair(s: &str) -> Result<(String, String), String> {
    let mut split = s.splitn(2, '=');
    split
        .next()
        .zip(split.next())
        .map(|(k, v)| (k.to_string(), v.to_string()))
        .ok_or_else(|| "Expected format: KEY=VALUE".to_owned())
}

#[derive(Parser, Debug)]
struct Args {
    /// Leaf-instrumented program to run
    #[arg(short, long)]
    program: PathBuf,
    /// Path to the file to be given to the program on stdin
    #[arg(long)]
    stdin: Option<PathBuf>,
    /// Path to the directory to write diverging inputs to
    #[arg(short, long)]
    outdir: PathBuf,
    #[arg(short, long, action)]
    silent: bool,
    /// Environment variables to pass to the program
    #[arg(long, value_parser=parse_env_pair, number_of_values=1)]
    env: Vec<(String, String)>,
    /// Argument to pass to the program
    #[arg(last = true)]
    args: Vec<String>,
}

fn main() -> ExitCode {
    let args = Args::parse();

    const FILE_PREFIX: &str = "diverging_";

    let grab = || {
        glob(
            args.outdir
                .join(format!("{}*", FILE_PREFIX))
                .to_str()
                .unwrap(),
        )
        .unwrap()
    };

    let get_std_output = || {
        if args.silent {
            Stdio::null()
        } else {
            Stdio::inherit()
        }
    };

    grab().for_each(|entry| {
        fs::remove_file(entry.unwrap()).expect("Failed to remove diverging input");
    });

    let result = utils::execute_once_for_div_inputs(
        &args.program,
        &std::env::vars().chain(args.env).collect(),
        stdio_from_path(args.stdin.as_deref()),
        get_std_output(),
        get_std_output(),
        &args.outdir,
        FILE_PREFIX,
        args.args,
    )
    .expect("Failed to execute the program");

    grab().for_each(|entry| {
        println!("{}", entry.unwrap().display());
    });

    if !args.silent && !result.status.success() {
        eprintln!("Program exited with status: {}", result.status);
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}
