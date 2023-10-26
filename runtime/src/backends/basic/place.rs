use std::{
    fmt::Display,
    ops::{Deref, DerefMut},
};

use crate::abs::{
    backend::{
        implementation::{DefaultPlaceHandler, DefaultPlaceProjectionHandler},
        PlaceHandler, PlaceProjectionHandler,
    },
    place::HasMetadata,
    Local, RawPointer, TypeId, TypeSize, ValueType,
};

use super::Projection;

const NONE_ADDRESS: RawPointer = 0;

#[derive(Debug, Clone)]
pub(crate) struct PlaceMetadata {
    address: RawPointer,
    type_id: Option<TypeId>,
    // FIXME: Temporary until merged with type system
    ty: Option<ValueType>,
    size: Option<TypeSize>,
}

impl Default for PlaceMetadata {
    fn default() -> Self {
        Self {
            address: NONE_ADDRESS,
            type_id: None,
            ty: None,
            size: None,
        }
    }
}

impl PlaceMetadata {
    #[inline]
    pub(crate) fn address(&self) -> Option<RawPointer> {
        if self.address != NONE_ADDRESS {
            Some(self.address)
        } else {
            None
        }
    }

    #[inline]
    pub(crate) fn set_address(&mut self, address: RawPointer) {
        self.address = address;
    }

    #[inline]
    pub(crate) fn type_id(&self) -> Option<TypeId> {
        self.type_id
    }

    #[inline]
    pub(crate) fn unwrap_type_id(&self) -> TypeId {
        self.type_id.expect("Type id is not available.")
    }

    #[inline]
    pub(crate) fn set_type_id(&mut self, type_id: TypeId) {
        self.type_id = Some(type_id);
    }

    #[inline]
    pub(crate) fn ty(&self) -> Option<&ValueType> {
        self.ty.as_ref()
    }

    #[inline]
    pub(crate) fn size(&self) -> Option<TypeSize> {
        self.size.as_ref().copied()
    }
}

pub(crate) type LocalWithMetadata = crate::abs::place::LocalWithMetadata<PlaceMetadata>;

pub(crate) type PlaceWithMetadata = crate::abs::place::PlaceWithMetadata<
    LocalWithMetadata,
    Projection<LocalWithMetadata>,
    PlaceMetadata,
>;

impl PlaceWithMetadata {
    pub(crate) fn address(&self) -> Option<RawPointer> {
        self.metadata().address()
    }

    pub(crate) fn proj_addresses(&self) -> impl Iterator<Item = Option<RawPointer>> + '_ {
        self.projs_metadata().map(|m| m.address())
    }
}

impl From<Local> for PlaceWithMetadata {
    fn from(value: Local) -> Self {
        Self::from(LocalWithMetadata::from(value))
    }
}

impl AsMut<PlaceWithMetadata> for PlaceWithMetadata {
    fn as_mut(&mut self) -> &mut PlaceWithMetadata {
        self
    }
}

impl TryFrom<PlaceWithMetadata> for LocalWithMetadata {
    type Error = PlaceWithMetadata;

    fn try_from(place: PlaceWithMetadata) -> Result<Self, Self::Error> {
        if !place.has_projection() {
            Ok(place.local().clone())
        } else {
            Err(place)
        }
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
    type Place = PlaceWithMetadata;

    type ProjectionHandler<'a> = BasicProjectionHandler<'a>;

    type MetadataHandler<'a> = BasicPlaceMetadataHandler<'a>;

    fn of_local(self, local: Local) -> Self::Place {
        PlaceWithMetadata::from(DefaultPlaceHandler::default().of_local(local))
    }

    fn project_on<'a>(self, place: &'a mut Self::Place) -> Self::ProjectionHandler<'a> {
        BasicProjectionHandler(place)
    }

    fn metadata(self, place: &mut Self::Place) -> Self::MetadataHandler<'_> {
        BasicPlaceMetadataHandler(place)
    }
}

pub(crate) struct BasicProjectionHandler<'a>(&'a mut PlaceWithMetadata);

impl PlaceProjectionHandler for BasicProjectionHandler<'_> {
    type Local = LocalWithMetadata;

    fn by(self, projection: crate::abs::Projection<Self::Local>) {
        self.0.push_metadata(PlaceMetadata::default());
        DefaultPlaceProjectionHandler::new(&mut self.0.deref_mut()).by(projection);
    }
}

pub(crate) struct BasicPlaceMetadataHandler<'a>(&'a mut PlaceWithMetadata);

impl BasicPlaceMetadataHandler<'_> {
    pub(crate) fn set_address(&mut self, address: RawPointer) {
        if self.0.has_projection() {
            let last = &mut self.0.projs_metadata_mut().last().unwrap();
            debug_assert!(last.address().is_none());
            last.set_address(address);
        } else {
            self.0.local_mut().set_address(address);
        }
    }

    pub(crate) fn set_type_id(&mut self, type_id: TypeId) {
        if self.0.has_projection() {
            let last = &mut self.0.projs_metadata_mut().last().unwrap();
            debug_assert!(last.type_id().is_none());
            last.set_type_id(type_id);
        } else {
            self.0.local_mut().set_type_id(type_id);
        }
    }

    pub(crate) fn set_primitive_type(&mut self, ty: ValueType) {
        self.0.metadata_mut().ty = Some(ty);
    }

    pub(crate) fn set_size(self, byte_size: crate::abs::TypeSize) {
        self.0.metadata_mut().size = Some(byte_size);
    }
}

impl Display for LocalWithMetadata {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}@{}",
            AsRef::<Local>::as_ref(self),
            self.address()
                .map(|a| format!("{:x}", a))
                .unwrap_or("-".to_owned())
        )
    }
}

impl Display for PlaceWithMetadata {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}@{}",
            self.deref(),
            self.address()
                .map(|addr| format!("{:x}", addr))
                .unwrap_or_else(|| "-".to_owned())
        )
    }
}
