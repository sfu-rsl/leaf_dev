use crate::abs;

impl From<common::pri::BinaryOp> for abs::BinaryOp {
    #[inline(always)]
    fn from(value: common::pri::BinaryOp) -> Self {
        unsafe { core::mem::transmute(value) }
    }
}

impl From<common::pri::UnaryOp> for abs::UnaryOp {
    #[inline(always)]
    fn from(value: common::pri::UnaryOp) -> Self {
        unsafe { core::mem::transmute(value) }
    }
}

impl From<common::pri::BranchingInfo> for super::BranchingInfo {
    #[inline(always)]
    fn from(value: common::pri::BranchingInfo) -> Self {
        Self::new(
            value.node_location,
            value.discriminant,
            value.discr_bit_size,
            value.discr_is_signed,
        )
    }
}
impl From<super::BranchingInfo> for common::pri::BranchingInfo {
    #[inline(always)]
    fn from(value: super::BranchingInfo) -> Self {
        Self {
            node_location: value.metadata.node_location,
            discriminant: value.discriminant,
            discr_bit_size: value.metadata.discr_as_int.bit_size,
            discr_is_signed: value.metadata.discr_as_int.is_signed,
        }
    }
}
