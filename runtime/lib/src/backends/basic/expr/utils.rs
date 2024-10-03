use super::IntType;

impl IntType {
    #[inline]
    pub(crate) fn bit_mask(&self) -> u128 {
        u128::MAX >> (u128::BITS - self.bit_size as u32)
    }

    #[inline]
    pub(crate) fn all_one(&self) -> u128 {
        self.bit_mask()
    }

    #[inline]
    pub(crate) fn masked(&self, bit_rep: u128) -> u128 {
        bit_rep & self.bit_mask()
    }

    #[inline]
    pub(crate) fn signed_masked(&self, bit_rep: u128) -> i128 {
        (bit_rep as i128) << (128 - self.bit_size) >> (128 - self.bit_size)
    }
}
