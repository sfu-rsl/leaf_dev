use crate::abs::{FieldIndex, VariantIndex};

#[derive(Debug, Clone)]
pub(crate) enum ProjectionOn<H, HI> {
    Deref(H),
    Field(H, FieldIndex),
    Index(HI, bool),
    Subslice(H, u64, u64, bool),
    Downcast(H, VariantIndex),
}
use ProjectionOn::*;

impl<H, HI> ProjectionOn<H, HI> {
    #[inline]
    pub(crate) fn map<HNew, HINew>(
        self,
        map_host: impl FnOnce(H) -> HNew,
        map_host_index: impl FnOnce(HI) -> HINew,
    ) -> ProjectionOn<HNew, HINew> {
        match self {
            Deref(host) => Deref(map_host(host)),
            Field(host, field) => Field(map_host(host), field),
            Index(host_index, from_end) => Index(map_host_index(host_index), from_end),
            Subslice(host, from, to, from_end) => Subslice(map_host(host), from, to, from_end),
            Downcast(host, to_variant) => Downcast(map_host(host), to_variant),
        }
    }

    #[inline(always)]
    pub(crate) fn into<HInto, HIInto>(self) -> ProjectionOn<HInto, HIInto>
    where
        H: Into<HInto>,
        HI: Into<HIInto>,
    {
        self.map(Into::into, Into::into)
    }
}

impl<H, I> ProjectionOn<H, (H, I)> {
    #[inline(always)]
    pub(crate) fn host_into<HInto>(self) -> ProjectionOn<HInto, (HInto, I)>
    where
        H: Into<HInto>,
    {
        self.map_host(Into::into)
    }

    #[inline(always)]
    pub(crate) fn index_into<IInto>(self) -> ProjectionOn<H, (H, IInto)>
    where
        I: Into<IInto>,
    {
        self.map(|h| h, |(h, i)| (h, i.into()))
    }
}

impl<H, I> ProjectionOn<H, (H, I)> {
    #[inline]
    pub(crate) fn host(&self) -> &H {
        match self {
            Deref(host) => host,
            Field(host, _) => host,
            Index((host, _), _) => host,
            Subslice(host, _, _, _) => host,
            Downcast(host, _) => host,
        }
    }

    #[inline(always)]
    pub(crate) fn map_host<T>(self, f: impl FnOnce(H) -> T) -> ProjectionOn<T, (T, I)> {
        match self {
            Deref(host) => Deref(f(host)),
            Field(host, field) => Field(f(host), field),
            Index((host, index), from_end) => Index((f(host), index), from_end),
            Subslice(host, from, to, from_end) => Subslice(f(host), from, to, from_end),
            Downcast(host, to_variant) => Downcast(f(host), to_variant),
        }
    }

    pub(crate) fn replace_host(&mut self, host: H) -> H {
        use std::mem::replace;
        match self {
            Deref(old_host) => replace(old_host, host),
            Field(old_host, _) => replace(old_host, host),
            Index((old_host, _), _) => replace(old_host, host),
            Subslice(old_host, _, _, _) => replace(old_host, host),
            Downcast(old_host, _) => replace(old_host, host),
        }
    }

    pub(crate) fn replace_host_with<T>(
        self,
        f: impl FnOnce(&H) -> T,
    ) -> (H, ProjectionOn<T, (T, I)>) {
        match self {
            Deref(old_host) => {
                let new_host = f(&old_host);
                (old_host, Deref(new_host))
            }
            Field(old_host, field) => {
                let new_host = f(&old_host);
                (old_host, Field(new_host, field))
            }
            Index((old_host, index), from_end) => {
                let new_host = f(&old_host);
                (old_host, Index((new_host, index), from_end))
            }
            Subslice(old_host, from, to, from_end) => {
                let new_host = f(&old_host);
                (old_host, Subslice(new_host, from, to, from_end))
            }
            Downcast(old_host, to_variant) => {
                let new_host = f(&old_host);
                (old_host, Downcast(new_host, to_variant))
            }
        }
    }

    #[inline]
    pub(crate) fn destruct(self) -> (H, ProjectionOn<(), ((), I)>) {
        self.replace_host_with(|_| ())
    }

    pub(crate) fn clone_with_host<T>(&self, host: T) -> ProjectionOn<T, (T, I)>
    where
        I: Clone,
    {
        match self {
            Deref(_) => Deref(host),
            Field(_, field) => Field(host, *field),
            Index((_, index), from_end) => Index((host, index.clone()), *from_end),
            Subslice(_, from, to, from_end) => Subslice(host, *from, *to, *from_end),
            Downcast(_, to_variant) => Downcast(host, *to_variant),
        }
    }
}

/// A trait for projections of host object at a place.
pub(crate) trait Projector {
    /// Type of the reference to the host object.
    type HostRef<'a>;
    /// Type of the reference to the host object and the index object.
    /// Used for index projection.
    type HIRefPair<'a>;
    /// Type of the result projection value.
    type Proj<'a> = Self::HostRef<'a>;

    fn project<'a>(
        &mut self,
        proj_on: ProjectionOn<Self::HostRef<'a>, Self::HIRefPair<'a>>,
    ) -> Self::Proj<'a>;

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

    fn downcast<'a>(&mut self, host: Self::HostRef<'a>, to_variant: VariantIndex)
    -> Self::Proj<'a>;
}

pub(crate) mod macros {
    macro_rules! impl_general_proj_through_singulars {
        () => {
            fn project<'a>(
                &mut self,
                pair: crate::abs::expr::proj::ProjectionOn<Self::HostRef<'a>, Self::HIRefPair<'a>>,
            ) -> Self::Proj<'a> {
                use crate::abs::expr::proj::ProjectionOn::*;
                match pair {
                    Deref(host) => self.deref(host),
                    Field(host, field) => self.field(host, field),
                    Index(host_index, from_end) => self.index(host_index, from_end),
                    Subslice(host, from, to, from_end) => self.subslice(host, from, to, from_end),
                    Downcast(host, to_variant) => self.downcast(host, to_variant),
                }
            }
        };
    }

    macro_rules! impl_singular_projs_through_general {
        () => {
            fn deref<'a>(&mut self, host: Self::HostRef<'a>) -> Self::Proj<'a> {
                self.project(ProjectionOn::Deref(host))
            }

            fn field<'a>(
                &mut self,
                host: Self::HostRef<'a>,
                field: crate::abs::FieldIndex,
            ) -> Self::Proj<'a> {
                self.project(ProjectionOn::Field(host, field))
            }

            fn index<'a>(
                &mut self,
                host_index: Self::HIRefPair<'a>,
                from_end: bool,
            ) -> Self::Proj<'a> {
                self.project(ProjectionOn::Index(host_index, from_end))
            }

            fn subslice<'a>(
                &mut self,
                host: Self::HostRef<'a>,
                from: u64,
                to: u64,
                from_end: bool,
            ) -> Self::Proj<'a> {
                self.project(ProjectionOn::Subslice(host, from, to, from_end))
            }

            fn downcast<'a>(
                &mut self,
                host: Self::HostRef<'a>,
                to_variant: crate::abs::VariantIndex,
            ) -> Self::Proj<'a> {
                self.project(ProjectionOn::Downcast(host, to_variant))
            }
        };
    }

    pub(crate) use {impl_general_proj_through_singulars, impl_singular_projs_through_general};
}
