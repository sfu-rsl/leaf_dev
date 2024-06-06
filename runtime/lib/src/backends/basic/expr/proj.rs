use crate::abs::expr::proj::{macros::impl_general_proj_through_singulars, Projector};

use super::super::alias::SymValueRefProjector;
use super::{prelude::*, SliceIndex, SymIndexPair};

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
        type FieldAccessor = FieldAccessKind;
        type HIRefPair<'a> = SymIndexPair;
        type DowncastTarget = DowncastKind;
        type Proj<'a> = ProjExpr;

        impl_general_proj_through_singulars!();

        fn deref<'a>(&mut self, host: Self::HostRef<'a>) -> Self::Proj<'a> {
            // Peel off the reference.
            if let SymValue::Expression(Expr::Ref(proj)) = host.as_ref() {
                proj.as_ref().clone()
            } else {
                ProjExpr::SymHost(SymHostProj {
                    host,
                    kind: ProjKind::Deref,
                })
            }
        }

        fn field<'a>(
            &mut self,
            host: Self::HostRef<'a>,
            field: Self::FieldAccessor,
        ) -> Self::Proj<'a> {
            ProjExpr::SymHost(SymHostProj {
                host,
                kind: ProjKind::Field(field),
            })
        }

        fn index<'a>(&mut self, host_index: Self::HIRefPair<'a>, from_end: bool) -> Self::Proj<'a> {
            match host_index {
                SymIndexPair::SymHost { host, index } => ProjExpr::SymHost(SymHostProj {
                    host,
                    kind: ProjKind::Index(SliceIndex { index, from_end }),
                }),
                SymIndexPair::SymIndex { index, host } if !host.is_symbolic() => {
                    ProjExpr::SymIndex(ConcreteHostProj {
                        host: ConcreteValueRef::new(host),
                        index: SliceIndex { index, from_end },
                    })
                }
                /* This case is not expected, however is structurally possible. */
                SymIndexPair::SymIndex { index, host } => ProjExpr::SymHost(SymHostProj {
                    host: SymValueRef::new(host),
                    kind: ProjKind::Index(SliceIndex {
                        index: index.into(),
                        from_end,
                    }),
                }),
            }
        }

        fn subslice<'a>(
            &mut self,
            host: Self::HostRef<'a>,
            from: u64,
            to: u64,
            from_end: bool,
        ) -> Self::Proj<'a> {
            ProjExpr::SymHost(SymHostProj {
                host,
                kind: ProjKind::Subslice { from, to, from_end },
            })
        }

        fn downcast<'a>(
            &mut self,
            host: Self::HostRef<'a>,
            target: Self::DowncastTarget,
        ) -> Self::Proj<'a> {
            ProjExpr::SymHost(SymHostProj {
                host,
                kind: ProjKind::Downcast(target),
            })
        }
    }
}
