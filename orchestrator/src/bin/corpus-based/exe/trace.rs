use std::{fs::OpenOptions, path::Path};

use common::{
    types::trace::{Constraint, ConstraintKind, ExeTraceRecord},
    z3::serdes::SmtLibExpr,
};

use crate::{SwitchStep, SwitchTrace};

type Expression = SmtLibExpr;

trait TraceReader {
    fn read_trace(&mut self) -> SwitchTrace;
}

pub(crate) async fn read_switch_trace(
    full_trace_path: &Path,
    sym_trace_path: &Path,
    preconditions_path: &Path,
) -> SwitchTrace {
    // FIXME: Change to async
    json_sync::JsonListTraceReader {
        full_trace_reader: OpenOptions::new()
            .read(true)
            .open(full_trace_path)
            .expect("Failed to open full trace file for reading"),
        sym_trace_reader: OpenOptions::new()
            .read(true)
            .open(sym_trace_path)
            .expect("Failed to open sym trace file for reading"),
        preconditions_reader: OpenOptions::new()
            .read(true)
            .open(preconditions_path)
            .map(Some)
            .or_else(|e| {
                if e.kind() == std::io::ErrorKind::NotFound {
                    Ok(None)
                } else {
                    Err(e)
                }
            })
            .expect("Failed to open sym trace file for reading"),
    }
    .read_trace()
}

mod json_sync {
    use std::io::Read;

    use serde_json::Value;

    use common::{directed::RawCaseValue, pri::BasicBlockLocation, types::trace::BranchRecord};

    use super::*;

    pub(super) struct JsonListTraceReader<R> {
        pub full_trace_reader: R,
        pub sym_trace_reader: R,
        pub preconditions_reader: Option<R>,
    }

    impl<R> JsonListTraceReader<R> {
        fn read_full_trace(
            &mut self,
        ) -> Vec<(usize, (BasicBlockLocation, ConstraintKind<RawCaseValue>))>
        where
            R: Read,
        {
            let records = serde_json::Deserializer::from_reader(&mut self.full_trace_reader)
                .into_iter::<Value>();

            const FORMAT_MSG: &str =
                "The trace was not in the expected format: { value: { .. }, index: # }*";
            records
                .filter_map(Result::ok)
                .map(|mut rec| {
                    let rec = rec.as_object_mut().expect(FORMAT_MSG);
                    let index = rec
                        .remove("index")
                        .and_then(|v| v.as_u64())
                        .expect(FORMAT_MSG) as usize;
                    let step = rec
                        .remove("value")
                        .and_then(|v| {
                            serde_json::from_value::<ExeTraceRecord<RawCaseValue>>(v).ok()
                        })
                        .expect(FORMAT_MSG);
                    // We don't process the cases in the full trace, so plain json value is fine.
                    (index, step)
                })
                .filter_map(|(index, record)| match record {
                    ExeTraceRecord::Branch(BranchRecord {
                        location,
                        decision: kind,
                    }) => Some((index, (location, kind))),
                    ExeTraceRecord::Call { .. } | ExeTraceRecord::Return { .. } => None,
                })
                .collect()
        }

        fn read_sym_trace(
            &mut self,
        ) -> Vec<(
            usize,
            (BasicBlockLocation, Constraint<Expression, Expression>),
        )>
        where
            R: Read,
        {
            let records = serde_json::Deserializer::from_reader(&mut self.sym_trace_reader)
                .into_iter::<Value>();
            const FORMAT_MSG: &str = "The trace was not in the expected format: { step: { value: <location>, index: # }, constraint: { discr: ttt, kind: ttt} }*";
            records
                .filter_map(Result::ok)
                .map(|mut rec| {
                    let rec = rec.as_object_mut().expect(FORMAT_MSG);
                    let step = rec
                        .get_mut("step")
                        .and_then(|v| v.as_object_mut())
                        .expect(FORMAT_MSG);
                    let location = step.remove("value").expect(FORMAT_MSG);
                    let location =
                        serde_json::from_value::<BasicBlockLocation>(location).expect(FORMAT_MSG);
                    let index = step
                        .get("index")
                        .and_then(|v| v.as_u64())
                        .expect(FORMAT_MSG) as usize;
                    let constraint = rec.remove("constraint").expect(FORMAT_MSG);
                    let constraint = serde_json::from_value(constraint).expect(FORMAT_MSG);
                    (index, (location, constraint))
                })
                .collect()
        }

        fn read_precondition_trace(&mut self) -> Vec<(usize, (BasicBlockLocation, Vec<usize>))>
        where
            R: Read,
        {
            let Some(precondition_reader) = &mut self.preconditions_reader else {
                return Default::default();
            };

            let records =
                serde_json::Deserializer::from_reader(precondition_reader).into_iter::<Value>();
            const FORMAT_MSG: &str = "The trace was not in the expected format: { step: { value: <location>, index: # }, preconditions: [ ## ] }*";
            records
                .filter_map(Result::ok)
                .map(|mut rec| {
                    let rec = rec.as_object_mut().expect(FORMAT_MSG);
                    let step = rec
                        .get_mut("step")
                        .and_then(|v| v.as_object_mut())
                        .expect(FORMAT_MSG);
                    let location = step.remove("value").expect(FORMAT_MSG);
                    let location =
                        serde_json::from_value::<BasicBlockLocation>(location).expect(FORMAT_MSG);
                    let index = step
                        .get("index")
                        .and_then(|v| v.as_u64())
                        .expect(FORMAT_MSG) as usize;
                    let preconditions = rec.remove("preconditions").expect(FORMAT_MSG);
                    let preconditions = serde_json::from_value(preconditions).expect(FORMAT_MSG);
                    (index, (location, preconditions))
                })
                .collect()
        }
    }

    impl<R: Read> TraceReader for JsonListTraceReader<R> {
        fn read_trace(&mut self) -> SwitchTrace {
            let full_trace = self.read_full_trace();
            let step_indices = full_trace
                .iter()
                .map(|(index, _)| *index)
                .collect::<Vec<_>>();
            let sym_discr_trace = self.read_sym_trace().into_iter();
            let preconditions_trace = self.read_precondition_trace().into_iter();

            let with_decision = itertools::merge_join_by(
                full_trace.into_iter(),
                sym_discr_trace,
                |(f_index, _), (s_index, _)| f_index.cmp(s_index),
            )
            .map(|either| {
                let (f, s) = either.left_and_right();
                let f = f.unwrap();
                (f.0, (f.1, s.map(|(_, (_, d))| d)))
            });

            let with_preconditions = itertools::merge_join_by(
                with_decision,
                preconditions_trace,
                |(f_index, _), (p_index, _)| f_index.cmp(p_index),
            )
            .map(|either| {
                let (f, p) = either.left_and_right();
                let f = f.unwrap();
                (
                    f.0,
                    (f.1.0, f.1.1, p.map(|(_, (_, p))| p).unwrap_or_default()),
                )
            });

            with_preconditions
                .enumerate()
                .map(
                    |(result_index, (index, ((loc, taken), decision, preconditions)))| SwitchStep {
                        trace_index: index,
                        location: loc,
                        decision: taken,
                        discr: decision.map(|c| c.discr),
                        implied_by_offset: preconditions
                            .into_iter()
                            .map(|abs_step_index| {
                                result_index - step_indices.binary_search(&abs_step_index).unwrap()
                            })
                            .collect(),
                    },
                )
                .collect()
        }
    }
}

impl core::fmt::Display for SwitchStep {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "@{}: {}", self.location, Constraint {
            discr: self
                .discr
                .as_ref()
                .map(|d| d.to_string())
                .unwrap_or("<Conc>".to_owned()),
            kind: self.decision.as_ref(),
        })
    }
}
