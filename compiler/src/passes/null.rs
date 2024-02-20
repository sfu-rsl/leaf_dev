use super::CompilationPass;

#[derive(Debug, Default)]
pub(crate) struct NullPass;

impl CompilationPass for NullPass {}
