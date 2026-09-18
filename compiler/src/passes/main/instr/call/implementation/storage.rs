use core::debug_assert_matches;

use rustc_middle::mir::Place;

use super::{
    InsertionLocation, PlaceReferencer, StorageMarker, ctxt_reqs::ForStorageMarking, prelude::*,
};

impl<'tcx, C> StorageMarker<'tcx> for RuntimeCallAdder<C>
where
    Self: MirCallAdder<'tcx> + BlockInserter<'tcx>,
    C: ForStorageMarking<'tcx>,
{
    fn mark_live(&mut self, place: &Place<'tcx>) {
        if !self
            .context
            .config()
            .storage_lifetime_filter
            .live
            .is_enabled()
        {
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

        let place = self.reference_place(place);
        let block = self.make_bb_for_call(
            sym::mark_storage_live,
            vec![operand::move_for_local(place.into())],
        );
        self.insert_blocks([block]);
    }

    fn mark_dead(&mut self, place: &Place<'tcx>) {
        if !self
            .context
            .config()
            .storage_lifetime_filter
            .dead
            .is_enabled()
        {
            return;
        }

        debug_assert_matches!(
            self.context.insertion_loc(),
            InsertionLocation::Before(..),
            "Marking storage as dead after it takes place is not expected."
        );

        let place = self.reference_place(place);
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
