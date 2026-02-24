use core::assert_matches::debug_assert_matches;

use super::{InsertionLocation, StorageMarker, ctxt_reqs::ForInsertion, prelude::*};

impl<'tcx, C> StorageMarker for RuntimeCallAdder<C>
where
    Self: MirCallAdder<'tcx> + BlockInserter<'tcx>,
    C: ForInsertion<'tcx>,
{
    fn mark_live(&mut self, place: PlaceRef) {
        if !self.context.config().storage_lifetime_filter.live {
            return;
        }

        debug_assert!(
            match self.context.insertion_loc() {
                InsertionLocation::Before(b) if b == rustc_middle::mir::Location::START.block =>
                    true,
                InsertionLocation::After(..) => true,
                _ => false,
            },
            concat!(
                "Marking storage as live before it takes place is not expected, ",
                "except for the first block (for always live ones)."
            )
        );
        let block = self.make_bb_for_call(
            sym::mark_storage_live,
            vec![operand::move_for_local(place.into())],
        );
        self.insert_blocks([block]);
    }

    fn mark_dead(&mut self, place: PlaceRef) {
        if !self.context.config().storage_lifetime_filter.dead {
            return;
        }

        debug_assert_matches!(
            self.context.insertion_loc(),
            InsertionLocation::Before(..),
            "Marking storage as dead after it takes place is not expected."
        );
        let block = self.make_bb_for_call(
            sym::mark_storage_dead,
            vec![operand::move_for_local(place.into())],
        );
        self.insert_blocks([block]);
    }
}

mod utils {
    pub(super) use super::super::utils::operand;
}
use utils::*;
