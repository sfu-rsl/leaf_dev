use std::{
    collections::{
        btree_map::{Cursor, CursorMut, Entry},
        BTreeMap,
    },
    fmt::{self, Display},
    ops::Bound,
};

use crate::abs::RawPointer;
use common::{log_debug, types::RawAddress};

use super::MemoryObject;

pub(super) type Address = RawAddress;
#[derive(Debug, Default)]
pub(super) struct Memory(BTreeMap<Address, MemoryObject>);

impl Memory {
    /// # Remarks
    /// The `prev` node of the returned cursor is the last entry with an address
    /// less than or equal to `addr`.
    #[tracing::instrument(level = "debug", skip(self))]
    pub(crate) fn before_or_at(&self, addr: &Address) -> Cursor<'_, Address, MemoryObject> {
        self.0.upper_bound(Bound::Included(addr))
    }

    /// # Remarks
    /// The `next` node of the returned cursor is greater than or equal to `addr`.
    #[tracing::instrument(level = "debug", skip(self))]
    pub(crate) fn after_or_at(&self, addr: &Address) -> Cursor<'_, Address, MemoryObject> {
        self.0.lower_bound(Bound::Included(addr))
    }

    #[tracing::instrument(level = "debug", skip(self))]
    pub(crate) fn after_or_at_mut(
        &mut self,
        addr: &Address,
    ) -> CursorMut<'_, Address, MemoryObject> {
        self.0.lower_bound_mut(Bound::Included(addr))
    }

    #[tracing::instrument(level = "debug", skip(self))]
    pub(crate) fn entry_at(&mut self, addr: Address) -> Entry<'_, Address, MemoryObject> {
        self.0.entry(addr)
    }

    #[tracing::instrument(level = "debug", skip(self))]
    pub(crate) fn remove_at(&mut self, addr: &Address) {
        self.0.remove(addr);
    }
}

impl Display for Memory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{{")?;
        for (addr, (value, type_id)) in self.0.iter() {
            writeln!(f, "{:p} ->\t{} ({:?})", addr, value, type_id)?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}
