pub type Ref = u32;
pub type PlaceRef = Ref;
pub type OperandRef = Ref;

pub mod encoding {
    use super::*;

    const INLINED_FLAG: Ref = 1 << (Ref::BITS - 1);

    #[inline(always)]
    const fn is_inlined(ref_value: Ref) -> bool {
        ref_value & INLINED_FLAG != 0
    }

    pub mod place {
        use super::*;

        // It should be embeddable in operand ref.
        const BITS: Ref = Ref::BITS - 3;

        const INLINED_FLAG: Ref = 1 << (BITS - 1);

        /* Places will be encoded as:
         * - 0x..x for regular references (INLINED_FLAG not set)
         * - 1x..x for inlined references (INLINED_FLAG set):
         *   - 11..x for locals (INLINED_FLAG | PLACE_LOCAL_FLAG set):
         *     - 110..0 for the return value
         *       (PLACE_LOCAL_FLAG set, PLACE_ARG_FLAG clear, local index == 0)
         *     - 111..x for arguments
         *       (PLACE_LOCAL_FLAG set, PLACE_ARG_FLAG set, local index != 0)
         *     - 110..x for other locals
         *       (PLACE_LOCAL_FLAG set, PLACE_ARG_FLAG clear, local index != 0)
         *   - 1011..1 for some (INLINED_FLAG | PLACE_LOCAL_FLAG clear, all one)
         */

        const MAX_PLACE_REF: Ref = (1 << (BITS - 2)) - 1;
        const PLACE_LOCAL_FLAG: Ref = 1 << (BITS - 2);
        const PLACE_SOME: Ref = (INLINED_FLAG | MAX_PLACE_REF) & !PLACE_LOCAL_FLAG;
        const PLACE_ARG_FLAG: Ref = 1 << (BITS - 3);
        const PLACE_LOCAL_MASK: Ref = PLACE_ARG_FLAG - 1;

        #[inline(always)]
        const fn is_inlined(ref_value: Ref) -> bool {
            ref_value & INLINED_FLAG != 0
        }

        #[inline(always)]
        const fn is_local(ref_value: Ref) -> bool {
            ref_value & PLACE_LOCAL_FLAG != 0
        }

        #[inline(always)]
        const fn is_arg(ref_value: Ref) -> bool {
            ref_value & PLACE_ARG_FLAG != 0
        }

        #[inline(always)]
        const fn is_some(ref_value: Ref) -> bool {
            ref_value == PLACE_SOME
        }

        // WARNING: Do not call any function in encoder functions, as they are used in the instrumentation.
        #[inline(always)]
        pub const fn encode_return_value() -> PlaceRef {
            const { INLINED_FLAG | PLACE_LOCAL_FLAG | 0 }
        }

        #[inline(always)]
        pub const fn encode_argument(arg_index: u32) -> PlaceRef {
            INLINED_FLAG | PLACE_LOCAL_FLAG | PLACE_ARG_FLAG | arg_index
        }

        #[inline(always)]
        pub const fn encode_local(local_index: u32) -> PlaceRef {
            (INLINED_FLAG | PLACE_LOCAL_FLAG | local_index) & !PLACE_ARG_FLAG
        }

        #[inline(always)]
        pub const fn encode_some() -> PlaceRef {
            PLACE_SOME
        }

        pub trait PlaceRefInlinedDecoder<P> {
            fn return_value() -> P;

            fn argument(arg_index: u32) -> P;

            fn local(local_index: u32) -> P;

            fn some() -> P;
        }

        #[inline]
        pub fn decode_ref<P, D: PlaceRefInlinedDecoder<P>>(ref_value: PlaceRef) -> Option<P> {
            if !is_inlined(ref_value) {
                None
            } else if is_local(ref_value) {
                let local = ref_value & PLACE_LOCAL_MASK;
                if local == 0 {
                    Some(D::return_value())
                } else if is_arg(ref_value) {
                    Some(D::argument(local))
                } else {
                    Some(D::local(local))
                }
            } else if is_some(ref_value) {
                Some(D::some())
            } else {
                None
            }
        }
    }

    pub mod operand {
        use super::*;

        /* Operands will be encoded as:
         * - 0x..x for regular references (INLINED_FLAG not set)
         * - 1x..x for inlined references (INLINED_FLAG set):
         *   - 10x..x for place references (CONST_FLAG clear):
         *     - 100r..r for copy (MOVE_FLAG clear)
         *     - 101r..r for move (MOVE_FLAG set)
         *   - 11x..x for constants (INLINED_FLAG | OPERAND_CONST_FLAG set):
         *     - 1100..x for ZST constants
         *       (OPERAND_CONST_FLAG set, const type == 00)
         *     - 1101..b for boolean constants
         *       (OPERAND_CONST_FLAG set, const type == 01)
         *     - 1110..x for some constants
         *       (OPERAND_CONST_FLAG set, const type == 10)
         *   - 111..1 for some (all one)
         */

        const MAX_OPERAND_REF: Ref = (1 << (Ref::BITS - 2)) - 1;
        const OPERAND_CONST_FLAG: Ref = 1 << (Ref::BITS - 2);
        const OPERAND_SOME: Ref = INLINED_FLAG | MAX_OPERAND_REF;

        const OPERAND_CONST_TYPE_MASK: Ref = 0b11 << (Ref::BITS - 4);
        const OPERAND_CONST_TYPE_ZST: Ref = 0b00 << (Ref::BITS - 4);
        const OPERAND_CONST_TYPE_BOOL: Ref = 0b01 << (Ref::BITS - 4);
        const OPERAND_CONST_TYPE_SOME: Ref = 0b10 << (Ref::BITS - 4);

        const OPERAND_CONST_SOME: Ref = INLINED_FLAG | OPERAND_CONST_FLAG | OPERAND_CONST_TYPE_SOME;

        const OPERAND_MOVE_FLAG: Ref = 1 << (Ref::BITS - 3);
        const OPERAND_PLACE_MASK: Ref = (1 << (Ref::BITS - 3)) - 1;

        #[inline(always)]
        const fn is_place(ref_value: Ref) -> bool {
            ref_value & OPERAND_CONST_FLAG == 0
        }

        #[inline(always)]
        const fn is_move(ref_value: Ref) -> bool {
            ref_value & OPERAND_MOVE_FLAG != 0
        }

        #[inline(always)]
        const fn is_const(ref_value: Ref) -> bool {
            ref_value & OPERAND_CONST_FLAG != 0 && ref_value != OPERAND_SOME
        }

        #[inline(always)]
        const fn is_zst(ref_value: Ref) -> bool {
            ref_value & OPERAND_CONST_TYPE_MASK == OPERAND_CONST_TYPE_ZST
        }

        #[inline(always)]
        const fn is_bool(ref_value: Ref) -> bool {
            ref_value & OPERAND_CONST_TYPE_MASK == OPERAND_CONST_TYPE_BOOL
        }

        #[inline(always)]
        const fn is_const_some(ref_value: Ref) -> bool {
            ref_value & OPERAND_CONST_TYPE_MASK == OPERAND_CONST_TYPE_SOME
        }

        #[inline(always)]
        const fn is_some(ref_value: Ref) -> bool {
            ref_value == OPERAND_SOME
        }

        // WARNING: Do not call any function in encoder functions, as they are used in the instrumentation.
        #[inline(always)]
        pub const fn encode_place_copy(place_ref: PlaceRef) -> OperandRef {
            (place_ref | INLINED_FLAG) & !(OPERAND_MOVE_FLAG | OPERAND_CONST_FLAG)
        }

        #[inline(always)]
        pub const fn encode_place_move(place_ref: PlaceRef) -> OperandRef {
            (place_ref | INLINED_FLAG) | OPERAND_MOVE_FLAG & !(OPERAND_CONST_FLAG)
        }

        #[inline(always)]
        pub const fn encode_const_zst() -> OperandRef {
            const { INLINED_FLAG | OPERAND_CONST_FLAG | OPERAND_CONST_TYPE_ZST }
        }

        #[inline(always)]
        pub const fn encode_const_bool(value: bool) -> OperandRef {
            INLINED_FLAG | OPERAND_CONST_FLAG | OPERAND_CONST_TYPE_BOOL | (value as Ref)
        }

        #[inline(always)]
        pub const fn encode_const_some() -> OperandRef {
            OPERAND_CONST_SOME
        }

        #[inline(always)]
        pub const fn encode_some() -> OperandRef {
            OPERAND_SOME
        }

        pub trait OperandRefInlinedDecoder<O> {
            fn const_zst() -> O;

            fn const_bool(value: bool) -> O;

            fn const_some() -> O;

            fn copy_of(place_ref: PlaceRef) -> O;

            fn move_of(place_ref: PlaceRef) -> O;

            fn some() -> O;
        }

        #[inline]
        pub fn decode_ref<O, D: OperandRefInlinedDecoder<O>>(ref_value: OperandRef) -> Option<O> {
            if !is_inlined(ref_value) {
                None
            } else if is_place(ref_value) {
                let place_ref = ref_value & OPERAND_PLACE_MASK;
                if is_move(ref_value) {
                    Some(D::move_of(place_ref))
                } else {
                    Some(D::copy_of(place_ref))
                }
            } else if is_const(ref_value) {
                if is_zst(ref_value) {
                    Some(D::const_zst())
                } else if is_bool(ref_value) {
                    Some(D::const_bool((ref_value & 1) != 0))
                } else if is_const_some(ref_value) {
                    Some(D::const_some())
                } else {
                    None // invalid
                }
            } else if is_some(ref_value) {
                Some(D::some())
            } else {
                None // unreachable
            }
        }
    }
}
