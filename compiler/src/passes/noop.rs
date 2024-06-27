use super::{CompilationPass, OverrideFlags};

#[derive(Debug, Default)]
pub(crate) struct NoOpPass;

impl CompilationPass for NoOpPass {
    fn override_flags() -> OverrideFlags {
        OverrideFlags::empty()
    }
}

#[derive(Debug, Default)]
pub(crate) struct OverrideFlagsForcePass<const FLAGS: u8>();

impl<const F: u8> OverrideFlagsForcePass<F> {
    const FLAGS: OverrideFlags = OverrideFlags::from_bits(F).unwrap();
}

impl<const F: u8> CompilationPass for OverrideFlagsForcePass<F> {
    fn override_flags() -> OverrideFlags {
        Self::FLAGS
    }
}
