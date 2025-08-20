// FIXME: The implementation can be optimized in several ways, such as piping the input, avoiding serialization, etc.

use std::{collections::HashMap, path::Path, sync::Arc};

use common::utils::serde::JsonLinesFormatter;
use serde::{Deserialize, Serialize};
use tokio::io::AsyncWriteExt;

use crate::{solve::QueryResult, utils::GenericError};

use super::{Solver, TraceConstraint};

#[derive(Clone)]
pub(crate) struct Adapter {
    solver_bin_path: Arc<Path>,
    input_file_path: Arc<Path>,
    output_file_path: Arc<Path>,
}

impl Adapter {
    pub(crate) fn new(solver_bin_path: &Path, workdir: impl AsRef<Path>) -> Self {
        std::fs::create_dir_all(&workdir).expect("Failed to create workdir for solver");

        const FILENAME_INPUT: &'static str = "constraints.jsonl";
        const FILENAME_OUTPUT: &'static str = "result.json";
        Self {
            solver_bin_path: solver_bin_path.into(),
            input_file_path: workdir.as_ref().join(FILENAME_INPUT).into(),
            output_file_path: workdir.as_ref().join(FILENAME_OUTPUT).into(),
        }
    }
}

impl Solver for Adapter {
    type Constraint = TraceConstraint;

    async fn solve<Q>(&mut self, query: Q) -> crate::solve::QueryResult
    where
        Q: crate::solve::SolveQuery<Constraint = Self::Constraint>,
    {
        write_input(&self.input_file_path, query.into_constraints())
            .await
            .expect("Failed to write input for solver process");

        execute_solver(
            &self.solver_bin_path,
            &self.input_file_path,
            &self.output_file_path,
        )
        .await
        .expect("Failed to execute solver process");

        read_result(&self.output_file_path)
            .await
            .expect("Failed to read result from solver")
    }
}

async fn write_input(
    path: &Path,
    constraints: impl Iterator<Item = TraceConstraint>,
) -> Result<(), std::io::Error> {
    let mut file = tokio::fs::File::create(path).await?;

    let mut buffer = Vec::with_capacity(1024);
    for constraint in constraints {
        #[derive(Serialize)]
        struct Wrapper {
            constraint: TraceConstraint,
        }
        let mut serializer =
            serde_json::Serializer::with_formatter(&mut buffer, JsonLinesFormatter::default());
        Wrapper { constraint }.serialize(&mut serializer).unwrap();
        file.write_all(&buffer).await?;
        buffer.clear();
    }

    Ok(())
}

async fn execute_solver(
    bin_path: &Path,
    constraints_path: &Path,
    result_path: &Path,
) -> Result<(), GenericError> {
    const ARG_INPUT: &str = "--input";
    const ARG_OUTPUT: &str = "--output";
    const ARG_FORMAT: &str = "--format";

    use std::process::Stdio;
    tokio::process::Command::new(bin_path)
        .arg(ARG_INPUT)
        .arg(constraints_path)
        .arg(ARG_OUTPUT)
        .arg(result_path)
        .arg(ARG_FORMAT)
        .arg("bytes")
        .stdout(Stdio::null())
        .stderr(Stdio::inherit())
        .stdin(Stdio::null())
        .spawn()
        .map_err(GenericError::from)?
        .wait()
        .await
        .map_err(GenericError::from)?
        .exit_ok()
        .map_err(GenericError::from)
}

async fn read_result(path: &Path) -> Result<QueryResult, GenericError> {
    let buf = tokio::fs::read(path).await?;

    #[derive(Deserialize)]
    #[serde(rename_all = "lowercase")]
    enum Result {
        Sat,
        Unsat,
        Unknown,
    }

    #[derive(Deserialize)]
    struct SolverOutput {
        result: Result,
        model: Option<HashMap<u32, u8>>,
    }

    let output: SolverOutput = serde_json::from_slice(&buf).map_err(GenericError::from)?;
    let result = match output.result {
        Result::Sat => QueryResult::Satisfied(
            output
                .model
                .unwrap()
                .into_iter()
                .map(|(k, v)| (k as usize - 1, v))
                .collect(),
        ),
        Result::Unsat | Result::Unknown => QueryResult::Unsatisfied,
    };

    Ok(result)
}
