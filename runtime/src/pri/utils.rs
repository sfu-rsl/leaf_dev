use std::ops::{Deref, DerefMut};

use super::Ref;

pub(super) struct UnsafeSync<T> {
    value: T,
}

unsafe impl<T> Sync for UnsafeSync<T> {}

impl<T> UnsafeSync<T> {
    pub const fn new(value: T) -> Self {
        Self { value }
    }
}

impl<T> Deref for UnsafeSync<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> DerefMut for UnsafeSync<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

pub(super) trait RefManager {
    type Ref;
    type Value;

    fn push(&mut self, value: Self::Value) -> Self::Ref;

    fn take_back(&mut self, reference: Self::Ref) -> Self::Value;
}

pub(super) struct DefaultRefManager<V> {
    counter: Ref,
    refs: Vec<(Ref, V)>,
}

impl<V> RefManager for DefaultRefManager<V> {
    type Ref = Ref;
    type Value = V;

    fn push(&mut self, value: V) -> Ref {
        self.counter += 1;
        self.refs.push((self.counter, value));
        self.counter
    }

    fn take_back(&mut self, reference: Ref) -> V {
        let index = self
            .refs
            .iter()
            .position(|(r, _)| r.eq(&reference))
            .unwrap();
        self.refs.swap_remove(index).1
    }
}
