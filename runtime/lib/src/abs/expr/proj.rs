use crate::abs::FieldIndex;

#[derive(Debug, Clone)]
pub(crate) enum ProjectionOn<H, HI, DC> {
    Deref(H),
    Field(H, FieldIndex),
    Index(HI, bool),
    Subslice(H, u64, u64, bool),
    Downcast(H, DC),
}
use ProjectionOn::*;

impl<H, HI, DC> ProjectionOn<H, HI, DC> {
    #[inline]
    pub(crate) fn map<HNew, HINew, DCNew>(
        self,
        f: impl FnOnce(H) -> HNew,
        f_index: impl FnOnce(HI) -> HINew,
        f_dc: impl FnOnce(DC) -> DCNew,
    ) -> ProjectionOn<HNew, HINew, DCNew> {
        match self {
            Deref(host) => Deref(f(host)),
            Field(host, field) => Field(f(host), field),
            Index(host_index, from_end) => Index(f_index(host_index), from_end),
            Subslice(host, from, to, from_end) => Subslice(f(host), from, to, from_end),
            Downcast(host, target) => Downcast(f(host), f_dc(target)),
        }
    }

    #[inline]
    pub(crate) fn map_into<HNew, HINew, DCNew>(self) -> ProjectionOn<HNew, HINew, DCNew>
    where
        H: Into<HNew>,
        HI: Into<HINew>,
        DC: Into<DCNew>,
    {
        self.map(Into::into, Into::into, Into::into)
    }
}

impl<H, I, DC> ProjectionOn<H, (H, I), DC> {
    #[inline]
    pub(crate) fn destruct(self) -> (H, ProjectionOn<(), ((), I), DC>) {
        match self {
            Deref(old_host) => (old_host, Deref(())),
            Field(old_host, field) => (old_host, Field((), field)),
            Index((old_host, index), from_end) => (old_host, Index(((), index), from_end)),
            Subslice(old_host, from, to, from_end) => (old_host, Subslice((), from, to, from_end)),
            Downcast(old_host, target) => (old_host, Downcast((), target)),
        }
    }

    pub(crate) fn clone_with_host<T>(&self, host: T) -> ProjectionOn<T, (T, I), DC>
    where
        I: Clone,
        DC: Clone,
    {
        match self {
            Deref(_) => Deref(host),
            Field(_, field) => Field(host, *field),
            Index((_, index), from_end) => Index((host, index.clone()), *from_end),
            Subslice(_, from, to, from_end) => Subslice(host, *from, *to, *from_end),
            Downcast(_, target) => Downcast(host, target.clone()),
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
    /// Type of the downcast target.
    /// Used for downcast projection.
    type DowncastTarget;

    /// Type of the result projection value.
    type Proj<'a> = Self::HostRef<'a>;

    fn project<'a>(
        &mut self,
        proj_on: ProjectionOn<Self::HostRef<'a>, Self::HIRefPair<'a>, Self::DowncastTarget>,
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

    fn downcast<'a>(
        &mut self,
        host: Self::HostRef<'a>,
        target: Self::DowncastTarget,
    ) -> Self::Proj<'a>;
}

pub(crate) mod macros {
    macro_rules! impl_general_proj_through_singulars {
        () => {
            fn project<'a>(
                &mut self,
                pair: crate::abs::expr::proj::ProjectionOn<
                    Self::HostRef<'a>,
                    Self::HIRefPair<'a>,
                    Self::DowncastTarget,
                >,
            ) -> Self::Proj<'a> {
                use crate::abs::expr::proj::ProjectionOn::*;
                match pair {
                    Deref(host) => self.deref(host),
                    Field(host, field) => self.field(host, field),
                    Index(host_index, from_end) => self.index(host_index, from_end),
                    Subslice(host, from, to, from_end) => self.subslice(host, from, to, from_end),
                    Downcast(host, target) => self.downcast(host, target),
                }
            }
        };
    }

    macro_rules! impl_singular_proj_through_general {
        (deref) => {
            fn deref<'a>(&mut self, host: Self::HostRef<'a>) -> Self::Proj<'a> {
                self.project(ProjectionOn::Deref(host))
            }
        };
        (field) => {
            fn field<'a>(
                &mut self,
                host: Self::HostRef<'a>,
                field: crate::abs::FieldIndex,
            ) -> Self::Proj<'a> {
                self.project(ProjectionOn::Field(host, field))
            }
        };
        (index) => {
            fn index<'a>(
                &mut self,
                host_index: Self::HIRefPair<'a>,
                from_end: bool,
            ) -> Self::Proj<'a> {
                self.project(ProjectionOn::Index(host_index, from_end))
            }
        };
        (subslice) => {
            fn subslice<'a>(
                &mut self,
                host: Self::HostRef<'a>,
                from: u64,
                to: u64,
                from_end: bool,
            ) -> Self::Proj<'a> {
                self.project(ProjectionOn::Subslice(host, from, to, from_end))
            }
        };
        (downcast) => {
            fn downcast<'a>(
                &mut self,
                host: Self::HostRef<'a>,
                target: Self::DowncastTarget,
            ) -> Self::Proj<'a> {
                self.project(ProjectionOn::Downcast(host, target))
            }
        };
    }

    macro_rules! impl_singular_projs_through_general {
        () => {
            impl_singular_projs_through_general!(deref, field, index, subslice, downcast);
        };
        ($head:ident $(,$tail:ident)+) => {
            impl_singular_proj_through_general!($head);
            impl_singular_projs_through_general!($($tail),*);
        };
        ($head:ident) => {
            impl_singular_proj_through_general!($head);
        };
    }

    pub(crate) use {
        impl_general_proj_through_singulars, impl_singular_proj_through_general,
        impl_singular_projs_through_general,
    };
}
