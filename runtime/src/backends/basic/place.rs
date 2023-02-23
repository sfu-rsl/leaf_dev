use crate::abs::{self, Local, PlaceHandler, PlaceProjectionHandler};

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
    type ProjectionHandler<'a> = DefaultPlaceProjectionHandler;

    fn of_local(&mut self, local: Local) -> Self::Place {
        Place::Local(local)
    }

    fn project_on<'a>(&'a mut self, place: Self::Place) -> Self::ProjectionHandler<'a> {
        DefaultPlaceProjectionHandler { place }
    }
}

impl PlaceProjectionHandler for DefaultPlaceProjectionHandler {
    type Place = Place;

    fn deref(&mut self) -> Self::Place {
        self.create(ProjectionKind::Deref)
    }

    fn for_field(&mut self, field: u32) -> Self::Place {
        self.create(ProjectionKind::Field(field))
    }

    fn at_index(&mut self, index: Self::Place) -> Self::Place {
        self.create(ProjectionKind::Index(Box::new(index)))
    }

    fn at_constant_index(&mut self, offset: u64, min_length: u64, from_end: bool) -> Self::Place {
        self.create(ProjectionKind::ConstantIndex {
            offset,
            min_length,
            from_end,
        })
    }

    fn subslice(&mut self, from: u64, to: u64, from_end: bool) -> Self::Place {
        self.create(ProjectionKind::Subslice { from, to, from_end })
    }

    fn downcast(&mut self, variant_index: u32) -> Self::Place {
        self.create(ProjectionKind::Downcast(variant_index))
    }

    fn opaque_cast(&mut self) -> Self::Place {
        self.create(ProjectionKind::OpaqueCast)
    }
}

impl DefaultPlaceProjectionHandler {
    fn create(&mut self, kind: ProjectionKind) -> Place {
        Place::Projection {
            kind,
            on: Box::new(self.place.clone()),
        }
    }
}
