use core::borrow::Borrow;

use serde::Serialize;

use crate::{
    abs::Constraint,
    trace::{StepInspector, StreamDumperStepInspector},
    utils::{HasIndex, Indexed, alias::RRef, file::FileFormat, serdes::TypeSerializer},
};

use super::{CurrentSolverCase, CurrentSolverValue, OutputConfig, Step, backend};
use backend::{Precondition, implication::PreconditionQuery};

use self::serdes::create_serializer_hrtb;

pub(super) fn create_solver_constraints_dumper<'ctx, S, V, C>(
    config: &OutputConfig,
) -> impl StepInspector<S, V, C>
where
    S: Borrow<Step> + HasIndex,
    V: Borrow<CurrentSolverValue<'ctx>>,
    C: Borrow<CurrentSolverCase<'ctx>>,
{
    let serializer = match config {
        OutputConfig::File(cfg) => {
            create_serializer_hrtb![for<'a> crate::trace::StreamDumperRecord<'a, _, _, _>](
                cfg,
                "sym_decisions",
            )
            .expect("Problem in creating file for dumping symbolic constraints")
        }
    };
    let mut dumper_inspector = StreamDumperStepInspector::new(serializer);

    let inspector = move |step: &S, constraint: Constraint<&V, &C>| {
        let step: Indexed<Step> = (step.borrow().clone(), step.index()).into();
        let constraint =
            constraint.map(|v| v.borrow().serializable(), |c| c.borrow().serializable());
        dumper_inspector.inspect(&step, constraint.as_ref());
    };
    inspector
}

pub(super) fn create_preconditions_dumper<'ctx, S, V, C>(
    config: &OutputConfig,
) -> impl StepInspector<S, V, C>
where
    S: Borrow<Step> + HasIndex,
    V: Borrow<Precondition>,
{
    #[derive(Serialize, bincode::Encode)]
    struct Record<'a, S> {
        step: &'a S,
        preconditions: &'a Precondition,
    }

    let mut serializer = match config {
        OutputConfig::File(cfg) => {
            create_serializer_hrtb![for<'a> Record<'a, _>](cfg, "discr_preconditions")
                .expect("Problem in creating file for dumping preconditions")
        }
    };

    let inspector = move |step: &S, constraint: Constraint<&V, &C>| {
        let step: Indexed<Step> = (step.borrow().clone(), step.index()).into();
        let preconditions = constraint.discr.borrow();
        if !preconditions.is_some() {
            return;
        }

        let record = Record {
            step: &step,
            preconditions,
        };
        serializer
            .serialize(&record)
            .unwrap_or_else(|e| panic!("Could not dump step: {e}"));
    };
    inspector
}

pub(super) fn create_step_index_in_memory_dumper<'ctx, S, V, C>(
    indices: RRef<Vec<usize>>,
) -> impl StepInspector<S, V, C>
where
    S: HasIndex,
{
    let inspector = move |step: &S, _: Constraint<&V, &C>| {
        indices.borrow_mut().push(step.index());
    };
    inspector
}

mod serdes {
    use crate::trace::StreamDumperRecord;

    use super::TypeSerializer;

    // Helps with generating a serializer for types with higher-rank trait bounds.
    macro_rules! create_serializer_hrtb {
        (for<$($l:lifetime),*> $t:ty) => {
            |cfg: &crate::utils::file::FileGenConfig,
             default_file_name: &str|
             -> Result<Box<dyn for<$($l),*> TypeSerializer<$t>>, std::io::Error> {
                assert!(
                    cfg.format.is_streamable(),
                    "Only streamable formats are expected for symbolic constraints dumping"
                );
                Ok(match cfg.format {
                    FileFormat::JsonLines | FileFormat::BinaryStream => {
                        let file = cfg.open_or_create_single(default_file_name, None, true)?;
                        match cfg.format {
                            FileFormat::JsonLines => {
                                Box::new(serde_json::Serializer::with_formatter(
                                    file,
                                    common::utils::serde::JsonLinesFormatter::default(),
                                ))
                            }
                            FileFormat::BinaryStream => {
                                Box::new(crate::utils::serdes::BincodeAdapter(file))
                            }
                            _ => unreachable!(),
                        }
                    }
                    FileFormat::Text => {
                        unimplemented!("Format is not supported for this dumper: {:?}", cfg.format);
                    }
                    FileFormat::Binary | FileFormat::Json => unreachable!(),
                })
            }
        };
    }
    pub(super) use create_serializer_hrtb;

    // This is for mitigating the issue with the trait resolver.
    #[allow(coherence_leak_check)]
    impl<'b, S, V, C> TypeSerializer<StreamDumperRecord<'b, S, V, C>>
        for Box<dyn for<'c> TypeSerializer<StreamDumperRecord<'c, S, V, C>>>
    {
        fn serialize(
            &mut self,
            value: &StreamDumperRecord<'b, S, V, C>,
        ) -> Result<(), Box<dyn core::error::Error>> {
            self.as_mut().serialize(value)
        }
    }
}
