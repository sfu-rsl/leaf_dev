use std::ops::DerefMut;

use super::super::super::{
    expr::{prelude::*, SymIndexPair},
    FullPlace,
};
use super::{super::proj::*, mutation::MutPlaceValue, RRef, ResolvedProjection, SymbolicProjector};

pub(super) fn apply_projs<'a, 'b, SP: SymbolicProjector>(
    place_resolver: &impl MutRefResolver,
    sym_projector: RRef<SP>,
    host: &'a ValueRef,
    mut projs: impl Iterator<Item = ResolvedProjection>,
) -> ValueRef {
    let mut current = host.clone();

    if current.is_symbolic() {
        return apply_projs_sym(sym_projector, &SymValueRef::new(current), projs).0;
    }

    while let Some(proj) = projs.next() {
        current = apply_proj_con(
            place_resolver,
            sym_projector.clone(),
            ConcreteValueRef::new(current),
            &proj,
        );

        if current.is_symbolic() {
            return apply_projs_sym(sym_projector, &SymValueRef::new(current), projs).0;
        }
    }

    current
}

fn apply_proj_con<SP: SymbolicProjector>(
    place_resolver: &impl MutRefResolver,
    projector: RRef<SP>,
    host: ConcreteValueRef,
    proj: &ResolvedProjection,
) -> ValueRef {
    let mut projector = ConcreteProjector {
        get_place: |p: &'_ FullPlace| -> ValueRef { place_resolver.get_full_place(p).unwrap() },
        handle_sym_index: |host: ConcreteValueRef, index: SymValueRef, c: bool| -> ValueRef {
            projector
                .as_ref()
                .borrow_mut()
                .index(
                    SymIndexPair::SymIndex {
                        index,
                        host: host.into(),
                    }
                    .into(),
                    c,
                )
                .into()
                .to_value_ref()
                .into()
        },
    };

    apply_proj::<_, _, _, Result<ValueRef, ConcreteValueRef>>(host, proj, &mut projector)
        .unwrap_result(proj)
}

pub(super) fn apply_projs_mut<'a, 'b, 'h, SP: SymbolicProjector>(
    sym_projector: RRef<SP>,
    host: MutPlaceValue<'h>,
    projs: impl Iterator<Item = ResolvedProjection>,
) -> MutPlaceValue<'h> {
    projs.fold(host, |current, proj| {
        /* FIXME: Based on the current implementation, it is not possible to
         * get a concrete value after a symbolic projection or a projection
         * on a symbolic value. So you may want to optimize it.
         */
        match current {
            MutPlaceValue::Normal(current) => {
                if !current.is_symbolic() {
                    apply_proj_con_mut(
                        sym_projector.clone(),
                        ConcreteValueMutRef::new(current),
                        &proj,
                    )
                } else {
                    MutPlaceValue::SymProj(apply_proj_sym(
                        sym_projector.as_ref().borrow_mut().deref_mut(),
                        SymValueRef::new(current.clone()),
                        &proj,
                    ))
                }
            }
            MutPlaceValue::SymProj(current) => MutPlaceValue::SymProj(apply_proj_sym(
                sym_projector.as_ref().borrow_mut().deref_mut(),
                current.to_value_ref(),
                &proj,
            )),
        }
    })
}

fn apply_proj_con_mut<'h, SP: SymbolicProjector>(
    sym_projector: RRef<SP>,
    host: ConcreteValueMutRef<'h>,
    proj: &ResolvedProjection,
) -> MutPlaceValue<'h> {
    let mut projector = super::MutProjector::new(
        |host: ConcreteValueMutRef<'_>, index: SymValueRef, from_end: bool| {
            sym_projector
                .as_ref()
                .borrow_mut()
                .index(
                    SymIndexPair::SymIndex {
                        index,
                        host: host.0.clone(),
                    }
                    .into(),
                    from_end,
                )
                .into()
        },
    );
    apply_proj::<_, _, _, Result<MutPlaceValue, &mut ConcreteValue>>(host, proj, &mut projector)
        .unwrap_result(proj)
}
