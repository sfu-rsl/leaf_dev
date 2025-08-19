use core::borrow::Borrow;

use derive_more as dm;

use common::{
    directed::RawCaseValue,
    log_debug, log_warn,
    types::{InstanceKindId, trace::BranchRecord},
};

use crate::{
    abs::{
        BasicBlockLocation, ConstraintKind, FuncDef,
        backend::{CallTraceRecorder, DecisionTraceRecorder, PhasedCallTraceRecorder},
    },
    utils::{HasIndex, Indexed, RefView, alias::RRef, serdes::TypeSerializer},
};

use super::backend;
use backend::{ConstValue, ExeTraceRecorder, ExeTraceStorage, config::OutputConfig};

type ExeTraceRecord = crate::abs::ExeTraceRecord<ConstValue>;

#[derive(Debug, dm::Deref)]
pub(crate) struct Record {
    #[deref]
    record: Indexed<crate::abs::ExeTraceRecord<ConstValue>>,
    pub(super) depth: usize,
}

impl HasIndex for Record {
    fn index(&self) -> usize {
        self.record.index
    }
}

impl Borrow<crate::abs::ExeTraceRecord<ConstValue>> for Record {
    fn borrow(&self) -> &crate::abs::ExeTraceRecord<ConstValue> {
        &self.record.value
    }
}

type SerializerObj = Box<dyn TypeSerializer<SerializedRecord>>;

pub(crate) struct BasicExeTraceRecorder {
    counter: usize,
    records: RRef<Vec<Record>>,
    stack: Vec<BasicBlockLocation<FuncDef>>,
    last_ret_point: Option<BasicBlockLocation<FuncDef>>,
    serializer: Option<SerializerObj>,
}

impl BasicExeTraceRecorder {
    fn new(config: Option<&OutputConfig>) -> Self {
        let serializer = create_serializer(config);

        Self {
            serializer,
            counter: 0,
            records: Default::default(),
            stack: Default::default(),
            last_ret_point: Default::default(),
        }
    }
}

fn create_serializer(
    config: Option<&OutputConfig>,
) -> Option<Box<dyn TypeSerializer<SerializedRecord>>> {
    use crate::utils::file::FileFormat;
    config
        .and_then(|c| match c {
            OutputConfig::File(file) => Some(file),
        })
        .and_then(|cfg| match cfg.format {
            FileFormat::JsonLines | FileFormat::BinaryStream => {
                let file = cfg
                    .open_or_create_single("exe_trace", None, true)
                    .unwrap_or_else(|e| panic!("Could not create file for trace recording: {e}"));
                let serializer: SerializerObj = match cfg.format {
                    FileFormat::JsonLines => Box::new(serde_json::Serializer::with_formatter(
                        file,
                        common::utils::serde::JsonLinesFormatter::default(),
                    )),
                    FileFormat::BinaryStream => {
                        Box::new(crate::utils::serdes::BincodeAdapter(file))
                    }
                    _ => unreachable!(),
                };
                Some(serializer)
            }
            FileFormat::Text => {
                unimplemented!("Format is not supported for this dumper: {:?}", cfg.format);
            }
            FileFormat::Binary | FileFormat::Json => None,
        })
}

pub(crate) fn create_trace_recorder(config: Option<&OutputConfig>) -> BasicExeTraceRecorder
where
    BasicExeTraceRecorder: ExeTraceRecorder,
{
    BasicExeTraceRecorder::new(config)
}

impl CallTraceRecorder for BasicExeTraceRecorder {
    fn notify_call(
        &mut self,
        call_site: BasicBlockLocation<FuncDef>,
        entered_func: FuncDef,
        broken: bool,
    ) {
        self.notify_step(ExeTraceRecord::Call {
            from: call_site.into(),
            to: entered_func.body_id,
            broken,
        });
    }

    fn notify_return(
        &mut self,
        ret_point: BasicBlockLocation<FuncDef>,
        caller_func: FuncDef,
        broken: bool,
    ) {
        self.notify_step(ExeTraceRecord::Return {
            from: ret_point.into(),
            to: caller_func.body_id,
            broken,
        });
    }
}

impl PhasedCallTraceRecorder for BasicExeTraceRecorder {
    #[tracing::instrument(level = "debug", skip(self))]
    fn start_call(&mut self, call_site: BasicBlockLocation<FuncDef>) {
        let last_ret_point = self.last_ret_point.take();
        self.handle_maybe_unfinished_return(last_ret_point);

        *self
            .stack
            .last_mut()
            .inspect(|l| debug_assert_eq!(&l.body, &call_site.body))
            .expect("Inconsistent stack info") = call_site;
    }

    #[tracing::instrument(level = "debug", skip(self))]
    fn finish_call(&mut self, entered_func: FuncDef, broken: bool) {
        let call_site = self.stack.last().copied();
        self.stack.push(BasicBlockLocation {
            body: entered_func,
            index: 0,
        });
        let Some(call_site) = call_site else {
            if !broken {
                panic!(
                    "Last call site is expected when not broken, current: {}",
                    entered_func
                )
            } else {
                return;
            }
        };

        self.notify_call(call_site, entered_func, broken);
    }

    #[tracing::instrument(level = "debug", skip(self))]
    fn start_return(&mut self, ret_point: BasicBlockLocation<FuncDef>) {
        self.stack.pop().expect("Inconsistent stack info");
        let unfinished = self.last_ret_point.replace(ret_point);
        self.handle_maybe_unfinished_return(unfinished);
    }

    #[tracing::instrument(level = "debug", skip(self))]
    fn finish_return(&mut self, broken: bool) -> BasicBlockLocation<FuncDef> {
        let call_site = self.stack.last().copied().expect("Inconsistent stack info");
        let Some(ret_point) = self.last_ret_point.take() else {
            if !broken {
                panic!(
                    "Last return point is expected when not broken, current: {}",
                    call_site
                )
            } else {
                return call_site;
            }
        };

        self.notify_return(ret_point, call_site.body, broken);
        call_site
    }
}

impl DecisionTraceRecorder for BasicExeTraceRecorder {
    type Case = ConstValue;

    fn notify_decision(
        &mut self,
        node_location: BasicBlockLocation,
        kind: &ConstraintKind<Self::Case>,
    ) -> usize {
        self.notify_step(ExeTraceRecord::Branch(BranchRecord {
            location: self.ensure_in_current_body(node_location),
            decision: kind.clone(),
        }))
    }
}

impl ExeTraceStorage for BasicExeTraceRecorder {
    type Record = Record;

    fn records(&self) -> RefView<Vec<Self::Record>> {
        self.records.clone().into()
    }
}

impl BasicExeTraceRecorder {
    #[tracing::instrument(level = "debug", skip(self), fields(index = self.counter + 1))]
    fn notify_step(&mut self, record: ExeTraceRecord) -> usize {
        let index = {
            let index = self.counter + 1;
            self.counter = index;
            index
        };
        self.records.borrow_mut().push(Record {
            record: Indexed {
                value: record,
                index,
            },
            depth: self.stack.len(),
        });
        self.append_last_to_file();
        index
    }

    fn append_last_to_file(&mut self) {
        let Some(serializer) = self.serializer.as_mut() else {
            return;
        };
        let _ = serializer
            .serialize(&SerializedRecord::from(
                self.records.as_ref().borrow().last().unwrap(),
            ))
            .inspect_err(|e| log_debug!("Failed to dump trace: {}", e));
    }

    #[inline]
    fn handle_maybe_unfinished_return(
        &mut self,
        last_ret_point: Option<BasicBlockLocation<FuncDef>>,
    ) {
        // NOTE: This happens when an external function calls internal ones.
        if let Some(unfinished) = last_ret_point {
            core::hint::cold_path();
            self.notify_return(
                unfinished,
                common::types::FuncDef {
                    body_id: InstanceKindId::INVALID,
                    static_addr: core::ptr::null(),
                    as_dyn_method: None,
                }
                .into(),
                true,
            );
        }
    }

    fn ensure_in_current_body(&self, mut location: BasicBlockLocation) -> BasicBlockLocation {
        let current = if let Some(current) = self.stack.last() {
            current.body.body_id
        } else {
            core::hint::cold_path();
            return location;
        };

        if cfg!(debug_assertions) {
            if location.body != current {
                log_warn!(
                    "Unexpected location in trace: {:?}, expected: {:?}",
                    location,
                    current
                );
            }
        }

        location.body = current;
        location
    }
}

#[derive(serde::Serialize, bincode::Encode)]
struct SerializedRecord(Indexed<crate::abs::ExeTraceRecord<RawCaseValue>>);

impl From<&Record> for SerializedRecord {
    #[inline]
    fn from(value: &Record) -> Self {
        use crate::abs::ExeTraceRecord::*;
        let record = match &value.record.value {
            Call { from, to, broken } => Call {
                from: *from,
                to: *to,
                broken: *broken,
            },
            Return { from, to, broken } => Return {
                from: *from,
                to: *to,
                broken: *broken,
            },
            Branch(branch) => Branch(to_raw_case(branch)),
        };
        Self(Indexed {
            value: record,
            index: value.record.index,
        })
    }
}

fn to_raw_case(branch: &BranchRecord<ConstValue>) -> BranchRecord<RawCaseValue> {
    BranchRecord {
        location: branch.location,
        decision: branch
            .decision
            .as_ref()
            .map(|c| c.try_to_bit_rep().unwrap()),
    }
}
