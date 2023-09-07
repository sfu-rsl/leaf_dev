use std::{borrow::Borrow, fmt::Display};

use crate::abs::{
    backend::{
        implementation::{DefaultPlaceHandler, DefaultPlaceProjectionHandler},
        PlaceHandler, PlaceProjectionHandler,
    },
    Local, Place, RawPointer,
};

#[derive(Debug, Clone)]
pub(crate) struct LocalWithAddress(pub(crate) Local, RawPointer);

impl LocalWithAddress {
    pub(crate) fn address(&self) -> Option<RawPointer> {
        if_not_none(&self.1)
    }
}

/* NOTE: Why not the following alternative structure?
   struct PlaceWithAddress {
       pub place: Place,
       pub addresses: Vec<RawPointer>,
   }

   While this structure is more intuitive and more compatible with the original
   `Place` structure, it causes problems with index projection where the index
   place should be backed by an address as well.
*/

#[derive(Debug, Clone, derive_more::Deref)]
pub(crate) struct PlaceWithAddress {
    #[deref]
    pub place: Place<LocalWithAddress>,
    proj_addresses: Vec<RawPointer>,
}

impl From<Local> for LocalWithAddress {
    fn from(value: Local) -> Self {
        Self(value, NONE_ADDRESS)
    }
}

impl From<Place<LocalWithAddress>> for PlaceWithAddress {
    fn from(value: Place<LocalWithAddress>) -> Self {
        Self {
            place: value,
            proj_addresses: Vec::with_capacity(0),
        }
    }
}

impl From<Local> for PlaceWithAddress {
    fn from(value: Local) -> Self {
        Self::from(Place::from(LocalWithAddress(value, NONE_ADDRESS)))
    }
}

impl AsRef<Local> for LocalWithAddress {
    fn as_ref(&self) -> &Local {
        &self.0
    }
}

impl TryFrom<PlaceWithAddress> for LocalWithAddress {
    type Error = PlaceWithAddress;

    fn try_from(place: PlaceWithAddress) -> Result<Self, Self::Error> {
        if !place.place.has_projection() {
            Ok(place.local().clone())
        } else {
            Err(place)
        }
    }
}

impl PlaceWithAddress {
    pub(crate) fn address(&self) -> Option<RawPointer> {
        if self.has_projection() {
            debug_assert_eq!(self.proj_addresses.len(), self.projections().len());
            if_not_none(self.proj_addresses.last().unwrap())
        } else {
            self.local().address()
        }
    }

    pub(crate) fn proj_addresses(&self) -> impl Iterator<Item = Option<RawPointer>> + '_ {
        self.proj_addresses.iter().map(if_not_none)
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

    type ProjectionHandler<'a> = BasicProjectionHandler<'a>;

    type MetadataHandler<'a> = BasicPlaceMetadataHandler<'a>;

    fn of_local(self, local: Local) -> Self::Place {
        PlaceWithAddress::from(DefaultPlaceHandler::default().of_local(local))
    }

    fn project_on<'a>(self, place: &'a mut Self::Place) -> Self::ProjectionHandler<'a> {
        BasicProjectionHandler(place)
    }

    fn metadata(self, place: &mut Self::Place) -> Self::MetadataHandler<'_> {
        BasicPlaceMetadataHandler(place)
    }
}

pub(crate) struct BasicProjectionHandler<'a>(&'a mut PlaceWithAddress);

const NONE_ADDRESS: RawPointer = 0;

fn if_not_none(addr: &RawPointer) -> Option<RawPointer> {
    let addr = *addr;
    if addr != NONE_ADDRESS {
        Some(addr)
    } else {
        None
    }
}

impl PlaceProjectionHandler for BasicProjectionHandler<'_> {
    type Local = LocalWithAddress;

    fn by(self, projection: crate::abs::Projection<Self::Local>) {
        self.0.proj_addresses.push(NONE_ADDRESS);
        DefaultPlaceProjectionHandler::new(&mut self.0.place).by(projection);
    }
}

pub(crate) struct BasicPlaceMetadataHandler<'a>(&'a mut PlaceWithAddress);

impl BasicPlaceMetadataHandler<'_> {
    pub(crate) fn set_address(&mut self, address: RawPointer) {
        *self.0.proj_addresses.last_mut().unwrap() = address;
    }
}

impl Display for LocalWithAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}@{:x}", self.0, self.1)
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
