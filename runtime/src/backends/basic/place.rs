use crate::abs::{
    backend::{PlaceHandler, PlaceProjectionHandler},
    FieldIndex, Local,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Place {
    pub local: Local,
    pub projections: Vec<Projection>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum Projection {
    Field(FieldIndex),
    Deref,
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

#[derive(Debug, Clone)]
pub(crate) struct FullPlace<I = usize> {
    place: Place,
    state_id: I,
}

impl<I> FullPlace<I> {
    pub(crate) fn new(place: Place, state_id: I) -> Self {
        Self { place, state_id }
    }

    pub(crate) fn state_id(&self) -> &I {
        &self.state_id
    }
}

/* NOTE: We use [`AsRef`] instead of [`Deref`] to prevent accidentally uses of local place. */
impl<I> AsRef<Place> for FullPlace<I> {
    fn as_ref(&self) -> &Place {
        &self.place
    }
}

pub(crate) struct DefaultPlaceHandler {}

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

pub(crate) struct DefaultPlaceProjectionHandler {
    place: Place,
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
