pub(crate) mod logging;
pub(crate) mod meta;

use std::ops::Deref;

pub(crate) struct UnsafeSync<T>(T);

unsafe impl<T> Sync for UnsafeSync<T> {}

impl<T> UnsafeSync<T> {
    pub fn new(obj: T) -> Self {
        Self(obj)
    }
}

impl<T> Deref for UnsafeSync<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// A trait for any hierarchical structure that mat take a parent.
pub(crate) trait Hierarchical<T> {
    fn set_parent(&mut self, parent: T);

    fn give_back_parent(&mut self) -> Option<T>;
}
