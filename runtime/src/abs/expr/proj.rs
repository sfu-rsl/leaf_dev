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
        f: impl FnOnce(H) -> HNew,
        f_index: impl FnOnce(HI) -> HINew,
    ) -> ProjectionOn<HNew, HINew> {
        match self {
            Deref(host) => Deref(f(host)),
            Field(host, field) => Field(f(host), field),
            Index(host_index, from_end) => Index(f_index(host_index), from_end),
            Subslice(host, from, to, from_end) => Subslice(f(host), from, to, from_end),
            Downcast(host, to_variant) => Downcast(f(host), to_variant),
        }
    }
}

impl<H, I> ProjectionOn<H, (H, I)> {
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
    pub(crate) fn destruct(self) -> (H, ProjectionOn<(), ((), I)>) {
        match self {
            Deref(old_host) => (old_host, Deref(())),
            Field(old_host, field) => (old_host, Field((), field)),
            Index((old_host, index), from_end) => (old_host, Index(((), index), from_end)),
            Subslice(old_host, from, to, from_end) => (old_host, Subslice((), from, to, from_end)),
            Downcast(old_host, to_variant) => (old_host, Downcast((), to_variant)),
        }
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
