use crate::abs::{self, Local, PlaceHandler, PlaceProjectionHandler};

/* FIXME: We should use a more efficient representation. As place is designed in
 * in this way to be moved and not borrowed, in places where there is a borrow,
 * cloning is mandatory which is expensive.
 * An alternative is the structure used in the compiler itself, which stores a
 * list of projections instead of recursive structure.
 */
#[derive(Debug, Clone)]
pub(crate) enum Place {
    Local(abs::Local),
    Projection {
        kind: ProjectionKind,
        // We use box to break infinite recursion.
        on: Box<Place>,
    },
}

#[derive(Debug, Clone)]
pub(crate) enum ProjectionKind {
    Deref,
    Field(u32),
    Index(Box<Place>),
    ConstantIndex {
        offset: u64,
        min_length: u64,
        from_end: bool,
    },
    Subslice {
        from: u64,
        to: u64,
        from_end: bool,
    },
    Downcast(u32),
    OpaqueCast,
}

pub(crate) struct DefaultPlaceHandler;

pub(crate) struct DefaultPlaceProjectionHandler {
    place: Place,
}

impl PlaceHandler for DefaultPlaceHandler {
    type Place = Place;
    type ProjectionHandler = DefaultPlaceProjectionHandler;

    fn of_local(self, local: Local) -> Self::Place {
        Place::Local(local)
    }

    fn project_on(self, place: Self::Place) -> Self::ProjectionHandler {
        DefaultPlaceProjectionHandler { place }
    }
}

impl PlaceProjectionHandler for DefaultPlaceProjectionHandler {
    type Place = Place;

    fn deref(self) -> Self::Place {
        self.create(ProjectionKind::Deref)
    }

    fn for_field(self, field: u32) -> Self::Place {
        self.create(ProjectionKind::Field(field))
    }

    fn at_index(self, index: Self::Place) -> Self::Place {
        self.create(ProjectionKind::Index(Box::new(index)))
    }

    fn at_constant_index(self, offset: u64, min_length: u64, from_end: bool) -> Self::Place {
        self.create(ProjectionKind::ConstantIndex {
            offset,
            min_length,
            from_end,
        })
    }

    fn subslice(self, from: u64, to: u64, from_end: bool) -> Self::Place {
        self.create(ProjectionKind::Subslice { from, to, from_end })
    }

    fn downcast(self, variant_index: u32) -> Self::Place {
        self.create(ProjectionKind::Downcast(variant_index))
    }

    fn opaque_cast(self) -> Self::Place {
        self.create(ProjectionKind::OpaqueCast)
    }
}

impl DefaultPlaceProjectionHandler {
    fn create(self, kind: ProjectionKind) -> Place {
        Place::Projection {
            kind,
            on: Box::new(self.place),
        }
    }
}
