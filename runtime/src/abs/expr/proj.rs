use crate::abs::FieldIndex;

/// A trait for projections of host object at a place.
pub(crate) trait Projector {
    /// Type of the reference to the host object.
    type HostRef<'a>;
    /// Type of the reference to the host object and the index object.
    /// Used for index projection.
    type HIRefPair<'a>;
    /// Type of the result projection value.
    type Proj<'a> = Self::HostRef<'a>;

    fn deref<'a>(&mut self, host: Self::HostRef<'a>) -> Self::Proj<'a>;

    fn field<'a>(&mut self, host: Self::HostRef<'a>, field: FieldIndex) -> Self::Proj<'a>;

    fn index<'a>(&mut self, host_index: Self::HIRefPair<'a>, from_end: bool) -> Self::Proj<'a>;

    fn subslice<'a>(
        &mut self,
        host: Self::HostRef<'a>,
        from: u64,
        to: u64,
        from_end: bool,
    ) -> Self::Proj<'a>;
}
