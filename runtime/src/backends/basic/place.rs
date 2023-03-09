use crate::abs::{self, Local, PlaceHandler, PlaceProjectionHandler, FieldIndex};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Place {
    pub local: Local,
    pub projections: Vec<Projection>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum Projection {
    Deref,
    Field(FieldIndex),
    Index(Place),
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

impl Place {
    pub fn new(local: Local) -> Self {
        Self {
            local,
            /* As most of the places are just locals, we try not to allocate at start. */
            projections: Vec::with_capacity(0),
        }
    }

    pub fn local(&self) -> Local {
        self.local
    }

    pub fn has_projection(&self) -> bool {
        !self.projections.is_empty()
    }

    pub fn with_projection(self, projection: Projection) -> Self {
        let mut projections = self.projections;
        projections.push(projection);
        Self {
            local: self.local,
            projections,
        }
    }
}

impl Projection {
    pub fn is_field(&self) -> bool {
        matches!(self, Projection::Field(_))
    }
}

pub(crate) struct DefaultPlaceHandler;

pub(crate) struct DefaultPlaceProjectionHandler {
    place: Place,
}

impl PlaceHandler for DefaultPlaceHandler {
    type Place = Place;
    type ProjectionHandler = DefaultPlaceProjectionHandler;

    fn of_local(self, local: Local) -> Self::Place {
        Place::new(local)
    }

    fn project_on(self, place: Self::Place) -> Self::ProjectionHandler {
        DefaultPlaceProjectionHandler { place }
    }
}

impl PlaceProjectionHandler for DefaultPlaceProjectionHandler {
    type Place = Place;

    fn deref(self) -> Self::Place {
        self.create(Projection::Deref)
    }

    fn for_field(self, field: FieldIndex) -> Self::Place {
        self.create(Projection::Field(field))
    }

    fn at_index(self, index: Self::Place) -> Self::Place {
        self.create(Projection::Index(index))
    }

    fn at_constant_index(self, offset: u64, min_length: u64, from_end: bool) -> Self::Place {
        self.create(Projection::ConstantIndex {
            offset,
            min_length,
            from_end,
        })
    }

    fn subslice(self, from: u64, to: u64, from_end: bool) -> Self::Place {
        self.create(Projection::Subslice { from, to, from_end })
    }

    fn downcast(self, variant_index: u32) -> Self::Place {
        self.create(Projection::Downcast(variant_index))
    }

    fn opaque_cast(self) -> Self::Place {
        self.create(Projection::OpaqueCast)
    }
}

impl DefaultPlaceProjectionHandler {
    fn create(self, proj: Projection) -> Place {
        self.place.with_projection(proj)
    }
}
