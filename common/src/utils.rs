#[macro_export]
macro_rules! identity {
    ($($input:tt)+) => {
        $($input)+
    };
}
pub use identity;

#[inline(always)]
pub fn type_id_of<T: ?Sized + 'static>() -> u128 {
    core::intrinsics::type_id::<T>()
}
