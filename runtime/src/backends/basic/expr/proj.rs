use crate::abs::{expr::proj::Projector, FieldIndex, VariantIndex};

use super::super::alias::SymValueRefProjector;
use super::{ConcreteValueRef, ProjExpr, ProjKind, SymIndexPair, SymValueRef, SymbolicIndex};

pub(crate) type DefaultSymProjector = core::CoreProjector;

pub(crate) fn new_sym_projector() -> DefaultSymProjector {
    DefaultSymProjector::default()
}

impl SymValueRefProjector for DefaultSymProjector {}

mod core {
    use super::*;

    #[derive(Default)]
    pub(crate) struct CoreProjector;

    impl Projector for CoreProjector {
        type HostRef<'a> = SymValueRef;
        type HIRefPair<'a> = SymIndexPair;
        type Proj<'a> = ProjExpr;

        fn deref<'a>(&mut self, host: Self::HostRef<'a>) -> Self::Proj<'a> {
            ProjExpr::SymHost {
                host,
                kind: ProjKind::Deref,
            }
        }

        fn field<'a>(&mut self, host: Self::HostRef<'a>, field: FieldIndex) -> Self::Proj<'a> {
            ProjExpr::SymHost {
                host,
                kind: ProjKind::Field(field),
            }
        }

        fn index<'a>(&mut self, host_index: Self::HIRefPair<'a>, from_end: bool) -> Self::Proj<'a> {
            match host_index {
                SymIndexPair::SymHost { host, index } => ProjExpr::SymHost {
                    host,
                    kind: ProjKind::Index { index, from_end },
                },
                SymIndexPair::SymIndex { index, host } if !host.is_symbolic() => {
                    ProjExpr::SymIndex(SymbolicIndex {
                        host: ConcreteValueRef::new(host),
                        index,
                        from_end,
                    })
                }
                /* This case is not expected, however is structurally possible. */
                SymIndexPair::SymIndex { index, host } => ProjExpr::SymHost {
                    host: SymValueRef::new(host),
                    kind: ProjKind::Index {
                        index: index.into(),
                        from_end,
                    },
                },
            }
        }

        fn subslice<'a>(
            &mut self,
            host: Self::HostRef<'a>,
            from: u64,
            to: u64,
            from_end: bool,
        ) -> Self::Proj<'a> {
            ProjExpr::SymHost {
                host,
                kind: ProjKind::Subslice { from, to, from_end },
            }
        }

        fn downcast<'a>(
            &mut self,
            host: Self::HostRef<'a>,
            to_variant: VariantIndex,
        ) -> Self::Proj<'a> {
            ProjExpr::SymHost {
                host,
                kind: ProjKind::Downcast(to_variant),
            }
        }
    }
}
