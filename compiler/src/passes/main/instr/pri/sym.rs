#![allow(non_upper_case_globals)]

use derive_more as dm;

#[derive(Clone, Copy, dm::Deref, Debug, dm::Display, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct LeafSymbol(&'static str);

use const_format::concatcp;

use LeafSymbol as LS;

pub const CORE_LIB_CRATE: LS = LS("core");
pub const RUNTIME_LIB_CRATE: LS = LS(crate::constants::CRATE_RUNTIME_SHIM);

pub const MODULE_MARKER_DIAG_NAME: LS = LS("leaf_module_marker");

macro_rules! in_lib {
    ($name: ident) => {
        concatcp!(RUNTIME_LIB_CRATE.0, "::", stringify!($name))
    };
}
macro_rules! symbols_in_lib {
        ($($name: ident),* $(,)?) => {
            $(pub(crate) const $name: LS = LS(in_lib!($name));)*
        };
    }

symbols_in_lib! {
    pri,
}

macro_rules! in_pri {
    ($name: ident) => {
        concatcp!(pri.0, "::", stringify!($name))
    };
}
macro_rules! symbols_in_pri {
        ($($name: ident),* $(,)?) => {
            #[allow(dead_code)]
            $(pub(crate) const $name: LS = LS(in_pri!($name));)*
        };
    }

symbols_in_pri! {
    MODULE_MARKER,
    compiler_helpers,
}

macro_rules! bracket {
        ($($name: ident),*$(,)?) => {
            [
                $($name),*
            ]
        };
    }

mod mains {
    #![allow(non_upper_case_globals)]

    use super::*;

    common::pri::pass_func_names_to!(symbols_in_pri, all_comma_separated);

    pub(crate) const ALL_MAINS: [LeafSymbol; 141] =
        common::pri::pass_func_names_to!(bracket, all_comma_separated);

    pub(crate) mod intrinsics {
        #![allow(non_upper_case_globals)]
        use super::*;

        #[derive(
            Clone, Copy, dm::Deref, Debug, dm::Display, Hash, PartialEq, Eq, PartialOrd, Ord,
        )]
        #[repr(transparent)]
        pub struct LeafIntrinsicSymbol(LeafSymbol);

        macro_rules! symbols_for_intrinsics {
                ($($name: ident),* $(,)?) => {
                    $(
                        #[allow(non_upper_case_globals)]
                        pub(crate) const $name: LeafIntrinsicSymbol = LeafIntrinsicSymbol(super::$name);
                    )*
                };
            }

        symbols_for_intrinsics! {
            intrinsic_assign_identity,
            intrinsic_assign_rotate_left,
            intrinsic_assign_rotate_right,
            intrinsic_assign_saturating_add,
            intrinsic_assign_saturating_sub,
            intrinsic_assign_disjoint_bitor,
            intrinsic_assign_exact_div,
            intrinsic_assign_carryless_mul,
            intrinsic_assign_bitreverse,
            intrinsic_assign_cttz_nonzero,
            intrinsic_assign_cttz,
            intrinsic_assign_ctpop,
            intrinsic_assign_ctlz_nonzero,
            intrinsic_assign_ctlz,
            intrinsic_assign_bswap,
            intrinsic_assign_funnel_shl,
            intrinsic_assign_funnel_shr,
            intrinsic_assign_select_unpredictable,
            intrinsic_assign_carrying_mul_add,

            intrinsic_atomic_load,
            intrinsic_atomic_store,
            intrinsic_atomic_xchg,
            intrinsic_atomic_cxchg,
            intrinsic_atomic_binary_op,
            intrinsic_atomic_fence,

            intrinsic_memory_load,
            intrinsic_memory_store,
            intrinsic_memory_copy,
            intrinsic_memory_set,
            intrinsic_memory_swap,
            intrinsic_assign_raw_eq,
            intrinsic_assign_compare_bytes,
        }

        pub(crate) mod atomic {
            use super::*;

            #[derive(
                Clone, Copy, dm::Deref, Debug, dm::Display, Hash, PartialEq, Eq, PartialOrd, Ord,
            )]
            #[repr(transparent)]
            pub struct LeafAtomicIntrinsicSymbol(LeafIntrinsicSymbol);

            macro_rules! symbols_for_atomic_intrinsics {
                    ($($name: ident),* $(,)?) => {
                        $(
                            #[allow(non_upper_case_globals)]
                            pub(crate) const $name: LeafAtomicIntrinsicSymbol = LeafAtomicIntrinsicSymbol(super::$name);
                        )*
                    };
                }

            symbols_for_atomic_intrinsics! {
                intrinsic_atomic_load,
                intrinsic_atomic_store,
                intrinsic_atomic_xchg,
                intrinsic_atomic_cxchg,
                intrinsic_atomic_binary_op,
                intrinsic_atomic_fence,
            }
        }

        pub(crate) mod memory {
            use super::*;

            #[derive(
                Clone, Copy, dm::Deref, Debug, dm::Display, Hash, PartialEq, Eq, PartialOrd, Ord,
            )]
            #[repr(transparent)]
            pub struct LeafMemoryIntrinsicSymbol(LeafIntrinsicSymbol);

            macro_rules! symbols_for_mem_intrinsics {
                    ($($name: ident),* $(,)?) => {
                        $(
                            #[allow(non_upper_case_globals)]
                            pub(crate) const $name: LeafMemoryIntrinsicSymbol = LeafMemoryIntrinsicSymbol(super::$name);
                        )*
                    };
                }

            symbols_for_mem_intrinsics! {
                intrinsic_memory_load,
                intrinsic_memory_store,
                intrinsic_memory_copy,
                intrinsic_memory_set,
                intrinsic_memory_swap,
                intrinsic_assign_raw_eq,
                intrinsic_assign_compare_bytes,
            }
        }
    }
}
pub(crate) use mains::*;

pub(crate) mod helpers {
    use const_format::concatcp;

    use super::{LS, compiler_helpers};

    macro_rules! make_pass_compiler_helpers_to_macro {
            (funcs: [$($fname:ident),+,], others: [$($o_name:ident),+,]) => {
                macro_rules! pass_compiler_helpers_to {
                    ($$macro:ident, just_funcs) => {
                        $$macro! {
                            $($fname),+
                        }
                    };
                    ($$macro:ident) => {
                        $$macro! {
                            $($fname),+,
                            $($o_name),+
                        }
                    };
                }
            };
        }

    make_pass_compiler_helpers_to_macro! {
        funcs: [
            f32_to_bits,
            f64_to_bits,

            place_with_address_typed,
            type_id_of,
            size_of,

            assertion_info,

            before_call_control,
            before_call_control_precise,
            before_call_control_precise_maybe_virtual,
            before_drop_control,
            before_drop_control_precise,
            before_drop_control_precise_maybe_virtual,
            enter_func,
            enter_func_precise,
            enter_func_precise_dyn_comp,

            const_binary_op_of,
            const_unary_op_of,

            const_atomic_ord_of,
            const_atomic_binary_op_of,

            const_primitive_type_of,

            special_func_placeholder,

            ref_place_return_value_encoded,
            ref_place_argument_encoded,
            ref_place_local_encoded,
            ref_place_some_encoded,

            ref_operand_copy_encoded,
            ref_operand_move_encoded,
            ref_operand_const_zst_encoded,
            ref_operand_const_bool_encoded,
            ref_operand_const_some_encoded,
            ref_operand_some_encoded,
        ],
        others: [
            CH_MODULE_MARKER,

            PLACE_REF_TYPE_HOLDER,
            OPERAND_REF_TYPE_HOLDER,
        ]
    }

    pub(crate) use pass_compiler_helpers_to;

    macro_rules! in_compiler_helpers {
        ($name: ident) => {
            concatcp!(compiler_helpers.0, "::", stringify!($name))
        };
    }
    macro_rules! symbols_in_compiler_helpers {
            ($($name: ident),* $(,)?) => {
                $(
                    #[allow(non_upper_case_globals)]
                    pub(crate) const $name: LS = LS(in_compiler_helpers!($name));
                )*
            };
        }

    pass_compiler_helpers_to!(symbols_in_compiler_helpers);

    pub(crate) const ALL_HELPERS: [LS; 34] = pass_compiler_helpers_to!(bracket);
}
pub(crate) use helpers::pass_compiler_helpers_to;

impl TryFrom<String> for LeafSymbol {
    type Error = String;

    fn try_from(s: String) -> Result<Self, Self::Error> {
        ALL_MAINS
            .iter()
            .chain(helpers::ALL_HELPERS.iter())
            .find(|sym| sym.0 == s)
            .copied()
            .ok_or(s)
    }
}
