use std::fmt::Display;

use crate::abs::{
    backend::{
        implementation::{DefaultPlaceHandler, DefaultPlaceProjectionHandler},
        PlaceHandler, PlaceProjectionHandler,
    },
    Local, Place, RawPointer,
};

pub(super) type Projection = crate::abs::Projection<Local>;

#[derive(Debug, Clone, derive_more::Deref)]
pub(crate) struct PlaceWithAddress {
    #[deref]
    pub place: Place,
    pub addresses: Vec<RawPointer>,
}

impl From<Place> for PlaceWithAddress {
    fn from(value: Place) -> Self {
        Self {
            place: value,
            addresses: vec![NONE_ADDRESS],
        }
    }
}

impl From<Local> for PlaceWithAddress {
    fn from(value: Local) -> Self {
        Self::from(Place::from(value))
    }
}

impl TryFrom<PlaceWithAddress> for Local {
    type Error = PlaceWithAddress;

    fn try_from(place: PlaceWithAddress) -> Result<Self, Self::Error> {
        if !place.place.has_projection() {
            Ok(*place.local())
        } else {
            Err(place)
        }
    }
}

impl PlaceWithAddress {
    pub(crate) fn address(&self) -> Option<RawPointer> {
        debug_assert_eq!(self.addresses.len(), self.projections().len() + 1);
        self.addresses
            .last()
            .cloned()
            .filter(|addr| *addr != NONE_ADDRESS)
    }
}

#[derive(Debug, Clone)]
pub(crate) struct FullPlace<P, I = usize> {
    place: P,
    state_id: I,
}

impl<P, I> FullPlace<P, I> {
    pub(crate) fn new(place: P, state_id: I) -> Self {
        Self { place, state_id }
    }

    pub(crate) fn state_id(&self) -> &I {
        &self.state_id
    }
}

/* NOTE: We use [`AsRef`] instead of [`Deref`] to prevent accidental uses of local place. */
impl<P, I> AsRef<P> for FullPlace<P, I> {
    fn as_ref(&self) -> &P {
        &self.place
    }
}

#[derive(Default)]
pub(crate) struct BasicPlaceHandler;

impl PlaceHandler for BasicPlaceHandler {
    type Place = PlaceWithAddress;

    type ProjectionHandler = BasicProjectionHandler;

    type MetadataHandler<'a> = BasicPlaceMetadataHandler<'a>;

    fn of_local(self, local: Local) -> Self::Place {
        PlaceWithAddress::from(DefaultPlaceHandler::default().of_local(local))
    }

    fn project_on(self, place: Self::Place) -> Self::ProjectionHandler {
        BasicProjectionHandler(place)
    }

    fn metadata(self, place: &mut Self::Place) -> Self::MetadataHandler<'_> {
        BasicPlaceMetadataHandler(place)
    }
}

pub(crate) struct BasicProjectionHandler(PlaceWithAddress);

const NONE_ADDRESS: RawPointer = 0;

impl PlaceProjectionHandler for BasicProjectionHandler {
    type Place = PlaceWithAddress;

    type Local = Local;

    fn by(mut self, projection: crate::abs::Projection<Self::Local>) -> Self::Place {
        self.0.addresses.push(NONE_ADDRESS);
        PlaceWithAddress {
            place: DefaultPlaceProjectionHandler::new(self.0.place).by(projection),
            addresses: self.0.addresses,
        }
    }
}

pub(crate) struct BasicPlaceMetadataHandler<'a>(&'a mut PlaceWithAddress);

impl BasicPlaceMetadataHandler<'_> {
    pub(crate) fn set_address(&mut self, address: RawPointer) {
        *self.0.addresses.last_mut().unwrap() = address;
    }
}

impl Display for PlaceWithAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}@{}",
            self.place,
            self.address()
                .map(|addr| format!("{:x}", addr))
                .unwrap_or_else(|| "-".to_owned())
        )
    }
}
