#![feature(exit_status_error)]
#![feature(impl_trait_in_assoc_type)]
#![feature(result_flattening)]
#![feature(iterator_try_collect)]
#![feature(buf_read_has_data_left)]

mod exe;
mod inputs;
mod output;
mod potentials;
mod solve;
mod trace;
mod utils;

use std::path::PathBuf;

use clap::Parser;
use derive_more::derive::Deref;
use futures::StreamExt;

use common::{log_debug, log_info};

use orchestrator::args::CommonArgs;

use self::{
    exe::Input,
    output::{HasBaseBytes, HasByteAnswers},
    potentials::{SwitchTrace, Trace},
    solve::{HasByteInput, IntoQuery, SolveQuery},
    trace::{TraceConstraint, TraceSwitchStep},
};

#[derive(Parser, Debug, Deref)]
struct Args {
    #[command(flatten)]
    #[deref]
    common: CommonArgs,

    #[command(flatten)]
    input_corpus: InputCorpusArgs,

    /// In offline mode, the orchestrator will not watch for new inputs
    /// and will finish after processing the existing ones.
    #[arg(long, action)]
    offline: bool,

    /// The working directory for the orchestrator.
    /// Various use cases include execution of the program and storing its traces at this location.
    /// Default to a temporary directory.
    #[arg(long)]
    workdir: Option<PathBuf>,

    /// The solver process to be used for solving queries.
    #[arg(long, default_value = "leafsolver")]
    solver: PathBuf,

    #[arg(long, default_value = "4")]
    j_exe: usize,

    /// The (tty) file to which the progress bars will be rendered.
    #[arg(long)]
    pb_target: Option<PathBuf>,
}

#[derive(Parser, Debug)]
struct InputCorpusArgs {
    /// Path to the directory containing the inputs.
    #[arg(long = "input_dir", short = 'i')]
    dir: PathBuf,
    /// Files to be excluded when grabbing inputs.
    #[arg(long, alias = "exclude")]
    exclude_patterns: Vec<String>,
    /// Files to be included when grabbing inputs.
    #[arg(long, alias = "include")]
    include_patterns: Vec<String>,
}

#[tokio::main]
async fn main() {
    orchestrator::logging::init_logging();

    let args = process_args();

    let (_, [inputs_pb, potentials_pb]) = pb::init_progress_bars(args.pb_target.as_ref());

    // 1. Grab, preprocess, and prioritize inputs. (N)
    let (input_process_handle, inputs) =
        inputs::prioritized_inputs(&args.input_corpus, args.offline, inputs_pb.clone())
            .await
            .expect("Failed to process inputs from the corpus directory");

    // 2. Execute and obtain traces for each input. (N)
    let (input_consumption_handle, traces) = exe::traces(
        exe::ExecutionConfig::new(
            &args.program,
            args.env.iter().cloned(),
            args.args.iter().cloned(),
            args.silent,
            args.timeout
                .map(|s| core::time::Duration::from_secs(s.get())),
            args.workdir.as_ref().unwrap(),
        ),
        args.j_exe,
        inputs.inspect(move |_| inputs_pb.inc(1)),
    );

    // 3. Derive potentials from each trace and prioritize. (N * M)
    let (potential_process_handle, potentials) =
        potentials::prioritized_potential(traces, potentials_pb.clone());

    // 4. Solve the potentials to obtain outputs (new inputs). (N * M)
    let outputs = solve::realize_potentials(
        &args.solver,
        args.workdir.as_ref().unwrap(),
        potentials.inspect(move |_| potentials_pb.inc(1)),
    );

    // 5. Dump outputs to the output directory.
    let output_dump_handle = output::dump_outputs(&args.outdir, outputs);

    await_all_handles(
        [
            input_process_handle,
            input_consumption_handle,
            potential_process_handle,
            output_dump_handle,
        ],
        args.offline,
    )
    .await;
}

fn process_args() -> Args {
    let mut args = Args::parse();
    args.workdir.get_or_insert(
        std::env::temp_dir()
            .join("leaf")
            .join(env!("CARGO_BIN_NAME")),
    );
    args
}

async fn await_all_handles(
    handles: impl IntoIterator<Item = tokio::task::JoinHandle<()>>,
    offline: bool,
) {
    use futures::future;

    let user_signal = async {
        if !offline {
            log_info!("Press Ctrl+C to stop the orchestrator");
            tokio::signal::ctrl_c()
                .await
                .expect("Failed to listen for Ctrl+C signal");
            log_debug!("Ctrl+C received");
        } else {
            core::future::pending::<()>().await;
        }
    };
    tokio::pin!(user_signal);

    let mut abort_handles = Vec::new();
    let join_all = future::try_join_all(
        handles
            .into_iter()
            .inspect(|h| abort_handles.push(h.abort_handle())),
    );

    let task_results = match future::select(user_signal, join_all).await {
        future::Either::Left((_user, join_all)) => {
            assert!(!offline, "User signal should not complete in offline mode");

            log_info!("Cancelling background tasks...");
            abort_handles.into_iter().for_each(|handle| {
                handle.abort();
            });

            join_all.await
        }
        future::Either::Right((task_results, _user)) => task_results,
    };

    if task_results.is_ok() {
        if !offline {
            panic!("Normal completion of background tasks is unexpected in online mode");
        } else {
            log_info!("Orchestrator finished successfully");
            return;
        }
    }

    let finish_reason = task_results.unwrap_err();
    if let Ok(err) = finish_reason.try_into_panic() {
        std::panic::resume_unwind(err);
    }

    log_info!("Orchestrator stopped successfully");
}

mod pb {
    use std::path::PathBuf;

    #[cfg(unix)]
    use indicatif::ProgressDrawTarget;
    use indicatif::{MultiProgress, ProgressBar, ProgressStyle};

    pub(super) fn init_progress_bars(
        target: Option<&PathBuf>,
    ) -> (MultiProgress, [ProgressBar; 2]) {
        let pb = MultiProgress::new();
        if let Some(path) = target {
            let target = std::fs::OpenOptions::new()
                .read(true)
                .write(true)
                .create(true)
                .open(path)
                .expect("Failed to open progress bar file");
            #[cfg(unix)]
            pb.set_draw_target(ProgressDrawTarget::term(
                console::Term::read_write_pair(
                    target.try_clone().expect("Target file should be cloneable"),
                    target,
                ),
                20,
            ));
        } else {
            pb.set_draw_target(indicatif::ProgressDrawTarget::hidden());
        }

        const PB_STYLE: &str = "{wide_bar} {pos}/{len}";
        let inputs_pb = ProgressBar::new(0)
            .with_style(ProgressStyle::with_template(&format!("Inputs {}", PB_STYLE)).unwrap());
        let potentials_pb = ProgressBar::new(0)
            .with_style(ProgressStyle::with_template(&format!("Potentials {}", PB_STYLE)).unwrap());
        pb.add(inputs_pb.clone());
        pb.add(potentials_pb.clone());

        (pb, [inputs_pb, potentials_pb])
    }
}
