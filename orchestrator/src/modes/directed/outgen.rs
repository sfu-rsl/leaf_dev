use std::{collections::HashMap, path::Path};

use common::{
    answers::{
        AnswersWriter, BinaryFileAnswerError, BinaryFileMultiAnswersWriter, SwitchableAnswersWriter,
    },
    log_warn,
    pri::BasicBlockLocation,
    z3::{AstNode, BVNode, BVSort},
};

pub(crate) struct NextInputGenerator {
    answers_writer: SwitchableAnswersWriter<BinaryFileMultiAnswersWriter>,
}

impl NextInputGenerator {
    pub fn new(out_dir: &Path, target: BasicBlockLocation, program_input: &[u8]) -> Self {
        Self {
            answers_writer: SwitchableAnswersWriter::new(BinaryFileMultiAnswersWriter::new(
                out_dir.to_path_buf(),
                Some(format!(
                    "directed_{}_{}_{}_",
                    target.body.0, target.body.1, target.index
                )),
                "bin".to_owned(),
                Some(program_input),
            )),
        }
    }

    #[tracing::instrument(level= "debug", skip_all, fields(len = answers.len()))]
    pub fn dump_as_next_input(&mut self, answers: &HashMap<u32, AstNode>) {
        let result = self.answers_writer.write(
            answers
                .iter()
                .map(|(id, answer)| (*id as usize - 1, try_ast_to_byte(answer))),
        );

        let Ok(result) = result else {
            // Disabled
            return;
        };

        match result {
            Ok(path) => println!("{}", path.display()),
            Err(err) => match err {
                BinaryFileAnswerError::Incomplete => {
                    panic!("Unexpected missing answer")
                }
                BinaryFileAnswerError::NonByte(_) => {
                    log_warn!("Some symbolic variables were not byte, disabling output dumping");
                    self.answers_writer.switch(false);
                }
                BinaryFileAnswerError::Io(error) => {
                    panic!("Failed to dump the next input {error}")
                }
            },
        };
    }
}

fn try_ast_to_byte(ast: &AstNode) -> Option<u8> {
    match ast {
        AstNode::BitVector(BVNode(bv, BVSort { is_signed: false })) => (bv.get_size() == u8::BITS)
            .then(|| bv.as_u64())
            .flatten()
            .map(|x| x as u8),
        _ => None,
    }
}
