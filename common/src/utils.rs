#[macro_export]
macro_rules! identity {
    ($(input:tt)+) => {
        $(input)+
    };
}
pub use identity;

