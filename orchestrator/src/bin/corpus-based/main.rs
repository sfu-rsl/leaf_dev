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

    #[arg(long, action)]
    offline: bool,

    #[arg(long)]
    workdir: Option<PathBuf>,

    #[arg(long, default_value = "leafsolver")]
    solver: PathBuf,
}

#[derive(Parser, Debug)]
struct InputCorpusArgs {
    /// Path to the directory containing the inputs.
    #[arg(long, short = 'i')]
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

    let (input_process_handle, inputs) =
        inputs::prioritized_inputs(&args.input_corpus, args.offline)
            .await
            .expect("Failed to process inputs from corpus directory");

    let traces = exe::traces(
        exe::ExecutionConfig::new(
            &args.program,
            args.env.iter().cloned(),
            args.args.iter().cloned(),
            args.silent,
            args.timeout
                .map(|s| core::time::Duration::from_secs(s.get())),
            args.workdir.as_ref().unwrap(),
        ),
        inputs,
    );

    let (potential_process_handle, potentials) = potentials::prioritized_potential(traces);

    let outputs =
        solve::realize_potentials(&args.solver, args.workdir.as_ref().unwrap(), potentials);
    let output_dump_handle = output::dump_outputs(&args.outdir, outputs);

    if !args.offline {
        log_info!("The orchestrator is watching for new inputs in the corpus directory...");
    }

    await_all_handles(
        [
            input_process_handle,
            potential_process_handle,
            output_dump_handle,
        ],
        args.offline,
    )
    .await;
}

fn process_args() -> Args {
    let mut args = Args::parse();
    args.workdir
        .get_or_insert(std::env::temp_dir().join("leaf").join("corpus-based"));
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
