use std::{
    collections::{
        btree_map::{Cursor, CursorMut, Entry},
        BTreeMap,
    },
    fmt::{self, Display},
    ops::Bound,
};

use crate::abs::RawPointer;
use common::{log_debug, log_info, log_warn};

use super::MemoryObject;

#[derive(Debug, Default)]
pub(super) struct Memory(BTreeMap<RawPointer, MemoryObject>);

impl Memory {
    /// # Remarks
    /// The `prev` node of the returned cursor is the last entry with an address
    /// less than or equal to `addr`.
    pub(crate) fn before_or_at(&self, addr: &RawPointer) -> Cursor<'_, u64, MemoryObject> {
        log_debug!("Checking memory before or at address: {}", addr);
        self.0.upper_bound(Bound::Included(addr))
    }

    /// # Remarks
    /// The `next` node of the returned cursor is greater than or equal to `addr`.
    pub(crate) fn after_or_at(&self, addr: &RawPointer) -> Cursor<'_, u64, MemoryObject> {
        log_debug!("Checking memory after or at address: {}", addr);
        self.0.lower_bound(Bound::Included(addr))
    }

    pub(crate) fn after_or_at_mut(
        &mut self,
        addr: &RawPointer,
    ) -> CursorMut<'_, u64, MemoryObject> {
        log_debug!("Checking memory after or at address: {}", addr);
        self.0.lower_bound_mut(Bound::Included(addr))
    }

    pub(crate) fn entry_at(&mut self, addr: RawPointer) -> Entry<'_, u64, MemoryObject> {
        log_debug!("Getting memory entry at address: {}", addr);
        self.0.entry(addr)
    }

    pub(crate) fn remove_at(&mut self, addr: &RawPointer) {
        log_debug!("Erasing memory at address: {}", addr);
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
