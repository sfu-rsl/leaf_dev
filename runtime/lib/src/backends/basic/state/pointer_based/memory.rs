use std::{
    collections::{
        btree_map::{Cursor, CursorMut, Entry},
        BTreeMap,
    },
    fmt::{self, Display},
    ops::Bound,
};

use crate::abs::RawPointer;

use super::MemoryObject;

#[derive(Debug, Default)]
pub(super) struct Memory(BTreeMap<RawPointer, MemoryObject>);

impl Memory {
    pub(crate) fn before_or_at(&self, addr: &RawPointer) -> Cursor<'_, u64, MemoryObject> {
        log::debug!("Checking memory before or at address: {}", addr);
        self.0.upper_bound(Bound::Included(addr))
    }

    pub(crate) fn after_or_at(&self, addr: &RawPointer) -> Cursor<'_, u64, MemoryObject> {
        log::debug!("Checking memory after or at address: {}", addr);
        self.0.lower_bound(Bound::Included(addr))
    }

    pub(crate) fn after_or_at_mut(
        &mut self,
        addr: &RawPointer,
    ) -> CursorMut<'_, u64, MemoryObject> {
        log::debug!("Checking memory after or at address: {}", addr);
        self.0.lower_bound_mut(Bound::Included(addr))
    }

    pub(crate) fn entry_at(&mut self, addr: RawPointer) -> Entry<'_, u64, MemoryObject> {
        log::debug!("Getting memory entry at address: {}", addr);
        self.0.entry(addr)
    }

    pub(crate) fn remove_at(&mut self, addr: &RawPointer) {
        log::debug!("Erasing memory at address: {}", addr);
        self.0.remove(addr);
    }
}

impl Display for Memory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{{")?;
        for (addr, (value, type_id)) in self.0.iter() {
            writeln!(f, "{} ->\t{} ({:?})", addr, value, type_id)?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}
