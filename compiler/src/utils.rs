#[derive(Default)]
pub(crate) struct Chain<A, B> {
    pub first: A,
    pub second: B,
}

/* NOTE: The trailing comma is mandatory for this macro.
 * Also note that a kind of expression is path, so it is important to
 * distinguish them in the pattern.
 */
macro_rules! chain {
    (<$head:path>, $($tail:tt)*) => {
        crate::utils::chain!((<$head>::default()), $($tail)*)
    };
    ($single:expr,) => {
        $single
    };
    ($head:expr, $($tail:tt)+) => {
        crate::utils::Chain {
            first: $head,
            second: crate::utils::chain!($($tail)+)
        }
    };
}
pub(crate) use chain;
