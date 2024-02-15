pub(crate) mod alias;
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

/// A trait for any hierarchical structure that may take a parent.
pub(crate) trait Hierarchical<T> {
    fn set_parent(&mut self, parent: T);

    fn give_back_parent(&mut self) -> Option<T>;
}

/// A trait for any hierarchical structure that may take a parent.
pub(crate) trait SelfHierarchical {
    fn add_layer(self) -> Self;

    fn drop_layer(self) -> Option<Self>
    where
        Self: Sized;
}

#[inline(always)]
pub(crate) fn type_id_of<T: ?Sized + 'static>() -> u128 {
    core::intrinsics::type_id::<T>()
}