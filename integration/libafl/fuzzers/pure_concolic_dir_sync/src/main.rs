mod utils;

use core::num::NonZero;
use std::{
    borrow::Cow,
    io::Write,
    path::{Path, PathBuf},
    process::{Child, Command, ExitStatus, Stdio},
    time::Duration,
};

use clap::Parser;

use libafl::{
    corpus::ondisk::OnDiskMetadataFormat,
    executors::command::CommandConfigurator,
    prelude::{command::CommandExecutorBuilder, *},
};
use libafl_bolts::{AsSlice, Named, current_nanos, nonzero, rands::StdRand, tuples::tuple_list};

use ::common::log_debug;
use serde::{Deserialize, Serialize};

const NAME_ORCHESTRATOR: &str = "leafo_onetime";

const DIR_MUTATOR_WORK: &str = "mutator";

#[derive(Parser, Debug)]
struct Args {
    /// Original program to test with the generated inputs
    #[arg(short, long)]
    program: PathBuf,
    /// Argument to pass to the program
    #[arg(long = "program-arg", alias = "parg", alias = "p-arg", alias = "pa")]
    program_args: Vec<String>,
    #[arg(long)]
    workdir: Option<PathBuf>,
    /// The directory to store the artifacts, e.g., crashes
    #[arg(long, default_value = "./artifacts")]
    artifacts_dir: PathBuf,
    /// The directory to store the corpus.
    /// Defaults to `workdir/corpus`.
    #[arg(long)]
    corpus_dir: Option<PathBuf>,
    /// The directory where the new inputs will be placed in.
    #[arg(long)]
    new_inputs_dir: PathBuf,
    /// The interval in seconds to check for new inputs.
    #[arg(long, default_value = "10")]
    interval: u64,
    /// The seed used for random generation.
    /// Set it to make the process deterministic.
    #[arg(long)]
    rand_seed: Option<u64>,
    /// Directories to load initial inputs from
    #[arg(long = "initial-input-dir")]
    initial_input_dirs: Vec<PathBuf>,
    #[command(flatten)]
    initial_rand_input: InitialRandInputArgs,
}

#[derive(Parser, Debug)]
struct InitialRandInputArgs {
    #[arg(long = "initial_rand_input_min_size", default_value_t = nonzero!(1))]
    min_size: NonZero<usize>,
    #[arg(long = "initial_rand_input_max_size", default_value_t = nonzero!(32))]
    max_size: NonZero<usize>,
    #[arg(long = "initial_rand_input_num", default_value_t = nonzero!(8))]
    num: NonZero<usize>,
}

pub fn main() {
    utils::init_logging();

    let args = process_args();

    let observer = ();
    // For pure concolic execution, all distinct inputs are interesting
    let mut feedback = feedback_and_fast!(ConstFeedback::True,);
    let mut objective = feedback_or!(feedback_and_fast!(CrashFeedback::new(), feedback.clone(),),);

    let mut state = StdState::new(
        StdRand::with_seed(args.rand_seed.unwrap()),
        InMemoryOnDiskCorpus::with_meta_format_and_prefix(
            args.corpus_dir.unwrap(),
            Some(OnDiskMetadataFormat::JsonPretty),
            None,
            false,
        )
        .unwrap(),
        OnDiskCorpus::new(args.artifacts_dir.join("crashes")).unwrap(),
        &mut feedback,
        &mut objective,
    )
    .unwrap();

    let scheduler = QueueScheduler::new();

    let mut fuzzer =
        StdFuzzer::with_bloom_input_filter(scheduler, feedback, objective, 1 << 20, 0.01);

    let mut executor = CommandExecutorBuilder::default()
        .program(args.program)
        .args(args.program_args)
        .build(observer)
        .expect("Failed to build the command executor");

    let monitor = make_monitor();
    let mut manager = SimpleEventManager::new(monitor);

    let load_initial_inputs = || {
        state
            .load_initial_inputs(
                &mut fuzzer,
                &mut executor,
                &mut manager,
                &args.initial_input_dirs,
            )
            .and_then(|_| {
                if state.corpus().is_empty() {
                    let params = args.initial_rand_input;
                    let mut generator =
                        RandBytesGenerator::with_min_size(params.min_size, params.max_size);
                    state.generate_initial_inputs(
                        &mut fuzzer,
                        &mut executor,
                        &mut generator,
                        &mut manager,
                        params.num.into(),
                    )
                } else {
                    Ok(())
                }
            })
            .expect("Failed to generate the initial corpus");
    };
    load_initial_inputs();

    std::fs::create_dir_all(&args.new_inputs_dir).unwrap();
    let mut stages = tuple_list!(stages::SyncFromDiskStage::with_from_file(
        vec![args.new_inputs_dir],
        Duration::from_secs(args.interval),
    ));

    fuzzer
        .fuzz_loop(&mut stages, &mut executor, &mut state, &mut manager)
        .unwrap_or_else(|e| match e {
            Error::Empty(..) => {
                println!("Finished covering all instrumented parts");
            }
            _ => {
                println!("Concolic loop stopped: {}", e);
            }
        });
}

fn process_args() -> Args {
    let mut args = Args::parse();
    args.workdir
        .get_or_insert(std::env::temp_dir().join("leaf").join("pure_concolic"));
    args.corpus_dir
        .get_or_insert(args.workdir.as_ref().unwrap().join("corpus"));
    args.rand_seed.get_or_insert_with(current_nanos);
    args
}

fn make_monitor() -> impl Monitor {
    #[cfg(feature = "tui")]
    {
        use libafl::monitors::tui::TuiMonitor;
        TuiMonitor::builder()
            .title("Leaf's Pure Concolic Fuzzer")
            .enhanced_graphics(false)
            .build()
    }
    #[cfg(not(feature = "tui"))]
    {
        use libafl::monitors::SimpleMonitor;

        #[cfg(not(feature = "tui"))]
        SimpleMonitor::with_user_monitor(|s| println!("{s}"))
    }
}
