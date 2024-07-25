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
        type Metadata<'a> = ProjMetadata;
        type FieldAccessor = FieldAccessKind;
        type HIRefPair<'a> = SymIndexPair;
        type DowncastTarget = DowncastKind;
        type Proj<'a> = ProjExpr;

        impl_general_proj_through_singulars!();

        fn deref<'a>(
            &mut self,
            host: Self::HostRef<'a>,
            metadata: Self::Metadata<'a>,
        ) -> Self::Proj<'a> {
            // Peel off the reference.
            if let SymValue::Expression(Expr::Ref(proj)) = host.as_ref() {
                proj.as_ref().clone()
            } else {
                ProjExpr::SymHost(SymHostProj {
                    host,
                    kind: ProjKind::Deref,
                    metadata,
                })
            }
        }

        fn field<'a>(
            &mut self,
            host: Self::HostRef<'a>,
            field: Self::FieldAccessor,
            metadata: Self::Metadata<'a>,
        ) -> Self::Proj<'a> {
            ProjExpr::SymHost(SymHostProj {
                host,
                kind: ProjKind::Field(field),
                metadata,
            })
        }

        fn index<'a>(
            &mut self,
            host_index: Self::HIRefPair<'a>,
            from_end: bool,
            metadata: Self::Metadata<'a>,
        ) -> Self::Proj<'a> {
            match host_index {
                SymIndexPair::SymHost { host, index } => ProjExpr::SymHost(SymHostProj {
                    host,
                    kind: ProjKind::Index(SliceIndex { index, from_end }),
                    metadata,
                }),
                SymIndexPair::SymIndex { index, host } if !host.is_symbolic() => {
                    ProjExpr::SymIndex(ConcreteHostProj {
                        host: ConcreteValueRef::new(host),
                        index: SliceIndex { index, from_end },
                        metadata,
                    })
                }
                /* This case is not expected, however is structurally possible. */
                SymIndexPair::SymIndex { index, host } => ProjExpr::SymHost(SymHostProj {
                    host: SymValueRef::new(host),
                    kind: ProjKind::Index(SliceIndex {
                        index: index.into(),
                        from_end,
                    }),
                    metadata,
                }),
            }
        }

        fn subslice<'a>(
            &mut self,
            host: Self::HostRef<'a>,
            from: u64,
            to: u64,
            from_end: bool,
            metadata: Self::Metadata<'a>,
        ) -> Self::Proj<'a> {
            ProjExpr::SymHost(SymHostProj {
                host,
                kind: ProjKind::Subslice { from, to, from_end },
                metadata,
            })
        }

        fn downcast<'a>(
            &mut self,
            host: Self::HostRef<'a>,
            target: Self::DowncastTarget,
            metadata: Self::Metadata<'a>,
        ) -> Self::Proj<'a> {
            ProjExpr::SymHost(SymHostProj {
                host,
                kind: ProjKind::Downcast(target),
                metadata,
            })
        }
    }
}
