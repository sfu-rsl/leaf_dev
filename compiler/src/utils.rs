use rustc_span::def_id::{CrateNum, DefId, DefIndex};

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

pub(crate) fn compute_def_id(def: DefId) -> u64 {
    return ((def.krate.as_u32() as u64) << 32) + def.index.as_u32() as u64;
}

pub(crate) fn revert_def_id(def_id: u64) -> DefId {
    let krate_id = (def_id >> 32) as u32;
    let index_id = def_id as u32;
    DefId {
        krate: CrateNum::from_u32(krate_id),
        index: DefIndex::from_u32(index_id),
    }
}
