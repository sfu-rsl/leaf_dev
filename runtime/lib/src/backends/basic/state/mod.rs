use super::{alias::SymValueRefProjector as SymbolicProjector, Projection, ValueRef};

mod local_based;
mod pointer_based;
pub(super) mod proj;

type RRef<T> = std::rc::Rc<std::cell::RefCell<T>>;

/// A projection that its possible index is resolved.
type ResolvedProjection = Projection<ValueRef>;

pub(super) use local_based::PlaceRef;
pub(super) use local_based::StackedLocalIndexVariablesState;
pub(super) use pointer_based::RawPointerVariableState;

pub(crate) enum PlaceError<L = crate::abs::Local> {
    LocalNotFound(L),
    StateNotFound(local_based::StateId),
}

impl<L: std::fmt::Display> std::fmt::Debug for PlaceError<L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PlaceError::LocalNotFound(local) => write!(
                f,
                "Local {} not found. It may be uninitialized, moved, or invalid.",
                local
            ),
            PlaceError::StateNotFound(state_id) => write!(f, "State {} not found.", state_id),
        }
    }
}
