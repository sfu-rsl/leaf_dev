use super::CompilationPass;

#[derive(Debug, Default)]
pub(crate) struct NoOpPass;

impl CompilationPass for NoOpPass {}
