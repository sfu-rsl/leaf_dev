use crate::pri::TypeId;

#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
pub trait Transmute<T>: From<T> + Into<T> {}
#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
impl<T> Transmute<T> for T {}

#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
#[repr(transparent)]
#[derive(Debug)]
pub struct U128Pack<T = u128>(
    [u8; core::mem::size_of::<u128>()],
    core::marker::PhantomData<T>,
);
#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
impl Transmute<u128> for U128Pack {}
#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
impl Transmute<TypeId> for U128Pack<TypeId> {}

#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
impl<T> From<U128Pack<T>> for u128 {
    #[inline(always)]
    fn from(value: U128Pack<T>) -> Self {
        unsafe { core::mem::transmute(value.0) }
    }
}
#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
impl<T> From<u128> for U128Pack<T> {
    #[inline(always)]
    fn from(value: u128) -> Self {
        Self(
            unsafe { core::mem::transmute(value) },
            core::marker::PhantomData,
        )
    }
}
#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
impl<T> From<U128Pack<T>> for TypeId {
    #[inline(always)]
    fn from(value: U128Pack<T>) -> Self {
        unsafe { TypeId::new_unchecked(Into::<u128>::into(value)) }
    }
}
#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
impl<T> From<TypeId> for U128Pack<T> {
    #[inline(always)]
    fn from(value: TypeId) -> Self {
        Into::<u128>::into(value).into()
    }
}

#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
#[repr(transparent)]
#[derive(Debug)]
pub struct CharPack([u8; core::mem::size_of::<char>()]);
#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
impl From<CharPack> for char {
    #[inline(always)]
    fn from(value: CharPack) -> Self {
        unsafe { core::mem::transmute(value.0) }
    }
}
#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
impl From<char> for CharPack {
    #[inline(always)]
    fn from(value: char) -> Self {
        Self(unsafe { core::mem::transmute(value) })
    }
}
#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
impl Transmute<char> for CharPack {}

#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
#[repr(C)]
#[derive(Debug)]
pub struct SlicePack<T> {
    #[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
    pub ptr: *const T,
    #[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
    pub len: usize,
}
#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
impl<T, U> From<&[T]> for SlicePack<U>
where
    U: Transmute<T>,
{
    #[inline(always)]
    fn from(value: &[T]) -> Self {
        Self {
            ptr: value.as_ptr() as *const U,
            len: value.len(),
        }
    }
}
#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
impl<T, U> From<SlicePack<U>> for &[T]
where
    U: Transmute<T>,
{
    #[inline(always)]
    fn from(value: SlicePack<U>) -> Self {
        unsafe { core::slice::from_raw_parts(value.ptr as *const T, value.len) }
    }
}

#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
#[repr(C)]
#[derive(Debug)]
pub struct ConstStrPack {
    #[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
    pub ptr: *const u8,
    #[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
    pub len: usize,
}
#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
impl From<ConstStrPack> for &'static str {
    #[inline(always)]
    fn from(value: ConstStrPack) -> Self {
        unsafe { core::str::from_utf8_unchecked(core::slice::from_raw_parts(value.ptr, value.len)) }
    }
}
#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
impl From<&'static str> for ConstStrPack {
    #[inline(always)]
    fn from(value: &'static str) -> Self {
        Self {
            ptr: value.as_ptr(),
            len: value.len(),
        }
    }
}

#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
pub type ConstByteStrPack = ConstStrPack;
#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
impl From<ConstByteStrPack> for &'static [u8] {
    #[inline(always)]
    fn from(value: ConstStrPack) -> Self {
        unsafe { core::slice::from_raw_parts(value.ptr, value.len) }
    }
}
#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
impl From<&'static [u8]> for ConstByteStrPack {
    #[inline(always)]
    fn from(value: &'static [u8]) -> Self {
        Self {
            ptr: value.as_ptr(),
            len: value.len(),
        }
    }
}
