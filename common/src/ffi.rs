pub trait Transmute<T>: From<T> + Into<T> {}
impl<T> Transmute<T> for T {}

#[repr(transparent)]
pub struct U128Pack<T = u128>(
    [u8; core::mem::size_of::<u128>()],
    core::marker::PhantomData<T>,
);
impl Transmute<u128> for U128Pack {}

impl<T> From<U128Pack<T>> for u128 {
    #[inline(always)]
    fn from(value: U128Pack<T>) -> Self {
        unsafe { core::mem::transmute(value.0) }
    }
}
impl<T> From<u128> for U128Pack<T> {
    #[inline(always)]
    fn from(value: u128) -> Self {
        Self(
            unsafe { core::mem::transmute(value) },
            core::marker::PhantomData,
        )
    }
}

#[repr(transparent)]
pub struct CharPack([u8; core::mem::size_of::<char>()]);
impl From<CharPack> for char {
    #[inline(always)]
    fn from(value: CharPack) -> Self {
        unsafe { core::mem::transmute(value.0) }
    }
}
impl From<char> for CharPack {
    #[inline(always)]
    fn from(value: char) -> Self {
        Self(unsafe { core::mem::transmute(value) })
    }
}
impl Transmute<char> for CharPack {}

#[repr(C)]
pub struct SlicePack<T> {
    pub ptr: *const T,
    pub len: usize,
}
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
impl<T, U> From<SlicePack<U>> for &[T]
where
    U: Transmute<T>,
{
    #[inline(always)]
    fn from(value: SlicePack<U>) -> Self {
        unsafe { core::slice::from_raw_parts(value.ptr as *const T, value.len) }
    }
}

#[repr(C)]
pub struct ConstStrPack {
    pub ptr: *const u8,
    pub len: usize,
}
impl From<ConstStrPack> for &'static str {
    #[inline(always)]
    fn from(value: ConstStrPack) -> Self {
        unsafe { core::str::from_utf8_unchecked(core::slice::from_raw_parts(value.ptr, value.len)) }
    }
}
impl From<&'static str> for ConstStrPack {
    #[inline(always)]
    fn from(value: &'static str) -> Self {
        Self {
            ptr: value.as_ptr(),
            len: value.len(),
        }
    }
}

pub type ConstByteStrPack = ConstStrPack;
impl From<ConstByteStrPack> for &'static [u8] {
    #[inline(always)]
    fn from(value: ConstStrPack) -> Self {
        unsafe { core::slice::from_raw_parts(value.ptr, value.len) }
    }
}
impl From<&'static [u8]> for ConstByteStrPack {
    #[inline(always)]
    fn from(value: &'static [u8]) -> Self {
        Self {
            ptr: value.as_ptr(),
            len: value.len(),
        }
    }
}
