use super::{ffi, utils};

pub use super::types::{
    Alignment, BasicBlockIndex, FieldIndex, FuncId, LocalIndex, RawPointer, TypeId, TypeSize,
    VariantIndex,
};
pub type Ref = u64;
pub type PlaceRef = Ref;
pub type OperandRef = Ref;

#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
#[repr(transparent)]
#[derive(Clone, Copy, Debug)]
pub struct BinaryOp(pub u8);
#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
#[repr(transparent)]
#[derive(Clone, Copy, Debug)]
pub struct UnaryOp(pub u8);

macro_rules! op_const {
    ($($name:ident = $value:expr;)*) => {
        $(
            #[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
            pub const $name: Self = Self($value);
        )*
    };
}

#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
impl BinaryOp {
    const OVERFLOW: u8 = 0b01 << (core::mem::size_of::<u8>() * 8 - 2);
    const UNCHECKED: u8 = 0b10 << (core::mem::size_of::<u8>() * 8 - 2);

    op_const! {
        ADD = 1;
        ADD_UNCHECKED = BinaryOp::ADD.0 | BinaryOp::UNCHECKED;
        ADD_WITH_OVERFLOW = BinaryOp::ADD.0 | BinaryOp::OVERFLOW;
        SUB = 2;
        SUB_UNCHECKED = BinaryOp::SUB.0 | BinaryOp::UNCHECKED;
        SUB_WITH_OVERFLOW = BinaryOp::SUB.0 | BinaryOp::OVERFLOW;
        MUL = 3;
        MUL_UNCHECKED = BinaryOp::MUL.0 | BinaryOp::UNCHECKED;
        MUL_WITH_OVERFLOW = BinaryOp::MUL.0 | BinaryOp::OVERFLOW;
        DIV = 4;
        REM = 5;
        BIT_XOR = 6;
        BIT_AND = 7;
        BIT_OR = 8;
        SHL = 9;
        SHL_UNCHECKED = BinaryOp::SHL.0 | BinaryOp::UNCHECKED;
        SHR = 10;
        SHR_UNCHECKED = BinaryOp::SHR.0 | BinaryOp::UNCHECKED;
        EQ = 11;
        LT = 12;
        LE = 13;
        NE = 14;
        GE = 15;
        GT = 16;
        CMP = 17;
        OFFSET = 18;
    }

    #[cfg_attr(core_build, rustc_const_stable(feature = "rust1", since = "1.0.0"))]
    #[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
    pub const fn from_raw(raw: u8) -> Self {
        if raw == BinaryOp::ADD.as_u8() {
            BinaryOp::ADD
        } else if raw == BinaryOp::ADD_UNCHECKED.as_u8() {
            BinaryOp::ADD_UNCHECKED
        } else if raw == BinaryOp::ADD_WITH_OVERFLOW.as_u8() {
            BinaryOp::ADD_WITH_OVERFLOW
        } else if raw == BinaryOp::SUB.as_u8() {
            BinaryOp::SUB
        } else if raw == BinaryOp::SUB_UNCHECKED.as_u8() {
            BinaryOp::SUB_UNCHECKED
        } else if raw == BinaryOp::SUB_WITH_OVERFLOW.as_u8() {
            BinaryOp::SUB_WITH_OVERFLOW
        } else if raw == BinaryOp::MUL.as_u8() {
            BinaryOp::MUL
        } else if raw == BinaryOp::MUL_UNCHECKED.as_u8() {
            BinaryOp::MUL_UNCHECKED
        } else if raw == BinaryOp::MUL_WITH_OVERFLOW.as_u8() {
            BinaryOp::MUL_WITH_OVERFLOW
        } else if raw == BinaryOp::DIV.as_u8() {
            BinaryOp::DIV
        } else if raw == BinaryOp::REM.as_u8() {
            BinaryOp::REM
        } else if raw == BinaryOp::BIT_XOR.as_u8() {
            BinaryOp::BIT_XOR
        } else if raw == BinaryOp::BIT_AND.as_u8() {
            BinaryOp::BIT_AND
        } else if raw == BinaryOp::BIT_OR.as_u8() {
            BinaryOp::BIT_OR
        } else if raw == BinaryOp::SHL.as_u8() {
            BinaryOp::SHL
        } else if raw == BinaryOp::SHL_UNCHECKED.as_u8() {
            BinaryOp::SHL_UNCHECKED
        } else if raw == BinaryOp::SHR.as_u8() {
            BinaryOp::SHR
        } else if raw == BinaryOp::SHR_UNCHECKED.as_u8() {
            BinaryOp::SHR_UNCHECKED
        } else if raw == BinaryOp::EQ.as_u8() {
            BinaryOp::EQ
        } else if raw == BinaryOp::LT.as_u8() {
            BinaryOp::LT
        } else if raw == BinaryOp::LE.as_u8() {
            BinaryOp::LE
        } else if raw == BinaryOp::NE.as_u8() {
            BinaryOp::NE
        } else if raw == BinaryOp::GE.as_u8() {
            BinaryOp::GE
        } else if raw == BinaryOp::GT.as_u8() {
            BinaryOp::GT
        } else if raw == BinaryOp::CMP.as_u8() {
            BinaryOp::CMP
        } else if raw == BinaryOp::OFFSET.as_u8() {
            BinaryOp::OFFSET
        } else {
            unreachable!()
        }
    }

    #[cfg_attr(core_build, rustc_const_stable(feature = "rust1", since = "1.0.0"))]
    #[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
    pub const fn as_u8(self) -> u8 {
        self.0
    }
}

#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
impl UnaryOp {
    op_const! {
        NOT = 31;
        NEG = 32;
        PTR_METADATA = 33;
    }

    #[cfg_attr(core_build, rustc_const_stable(feature = "rust1", since = "1.0.0"))]
    #[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
    pub const fn from_raw(raw: u8) -> Self {
        if raw == UnaryOp::NOT.as_u8() {
            UnaryOp::NOT
        } else if raw == UnaryOp::NEG.as_u8() {
            UnaryOp::NEG
        } else if raw == UnaryOp::PTR_METADATA.as_u8() {
            UnaryOp::PTR_METADATA
        } else {
            unreachable!()
        }
    }

    #[cfg_attr(core_build, rustc_const_stable(feature = "rust1", since = "1.0.0"))]
    #[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
    pub const fn as_u8(self) -> u8 {
        self.0
    }
}

static DEFAULT_BRANCHING_INFO: BranchingInfo = BranchingInfo {
    node_location: 0,
    discriminant: 0,
    discr_bit_size: 0,
    discr_is_signed: false,
};

#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct BranchingInfo {
    pub node_location: BasicBlockIndex,
    pub discriminant: OperandRef,
    pub discr_bit_size: u64,
    pub discr_is_signed: bool,
}

#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
impl Default for BranchingInfo {
    #[inline(always)]
    fn default() -> Self {
        DEFAULT_BRANCHING_INFO
    }
}

/// The definition of the interface between the program and the runtime library.
///
/// This trait provides a compile-time guarantee that the list of functions
/// is kept consistent between the runtime library, its exported C ABI, and the
/// shim. User is required to implement this trait wherever the list of functions
/// is used.
#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
pub trait ProgramRuntimeInterface {
    type U128;
    type Char;
    type ConstStr;
    type ConstByteStr;
    type Slice<'a, T: 'a>;
    type BranchingInfo;
    type TypeId;
    type BinaryOp;
    type UnaryOp;

    list_func_decls! { modifier: utils::identity, (from Self) }
}

/// A marker trait to make sure that the FFI/ABI is the same between
/// the runtime library and the shim.
#[cfg_attr(core_build, stable(feature = "rust1", since = "1.0.0"))]
pub trait FfiPri:
    // This is currently not supported by the compiler.
    // for<'a, T>
    ProgramRuntimeInterface<
        U128 = ffi::U128Pack,
        Char = ffi::CharPack,
        ConstStr = ffi::ConstStrPack,
        ConstByteStr = ffi::ConstByteStrPack,
        // Slice<'a, T> = ffi::SlicePack<T>,
        BranchingInfo = BranchingInfo,
        TypeId = ffi::U128Pack<TypeId>,
        BinaryOp = BinaryOp,
        UnaryOp = UnaryOp,
    >
{
}

pub mod macros {
    #[cfg_attr(not(core_build), macro_export)]
    macro_rules! self_slice_of { ($t:ty) => { Self::Slice<'_, $t> }; }

    #[cfg_attr(not(core_build), macro_export)]
    macro_rules! slice_pack_of { ($t:ty) => { common::ffi::SlicePack<$t> }; }

    /* NOTE: What are these macros for?
     * The list of PRI functions should get repeated in multiple places:
     * - in the runtime library, the actual PRI calling the backend.
     * - in the runtime library, the exported C ABI.
     * - in the runtime shim, the external functions.
     * - in the runtime shim, the exported Rust ABI.
     * - in delegations
     * But there are subtle differences between them concerning parameter types,
     * the modifiers of the functions, and their body.
     * We should be able to repeat the list easily and clean as much as possible.
     * Another motivation is to keep this list in one place to be easy to maintain.
     *
     * Why the macros are complicated and ugly?
     * - First, for the modifications, we let the user pass a `modifier` macro
     *   which receives the function and can change it to any form.
     * - Second, macro expansion works from outside to inside, so passing the
     *   function correctly to the modifier needs some tricks.
     * - Third, hygiene rules are not complete thus sometimes we need to add
     *   parentheses and braces to fix that.
     * - Fourth, to simplify some rules we use token trees which introduces
     *   braces in some places.
     */

    /* How to add/modify functions to the PRI?
     * - Add or modify the function's signature in the list below.
     * - Implement the actual functionality in `runtime::pri`.
     * - Ideally, all other use cases of these functions should be updated automatically.
     */

    #[cfg_attr(not(core_build), macro_export)]
    macro_rules! pass_func_decls_to {
        ($macro:ident) => {
            $macro! {
              { fn init_runtime_lib() }
              { fn shutdown_runtime_lib() }

              { fn ref_place_return_value() -> PlaceRef }
              { fn ref_place_argument(local_index: LocalIndex) -> PlaceRef }
              { fn ref_place_local(local_index: LocalIndex) -> PlaceRef }

              { fn ref_place_deref(place: PlaceRef) }
              { fn ref_place_field(place: PlaceRef, field: FieldIndex /*, type */) }
              { fn ref_place_index(place: PlaceRef, index_place: PlaceRef) }
              { fn ref_place_constant_index(place: PlaceRef, offset: u64, min_length: u64, from_end: bool) }
              { fn ref_place_subslice(place: PlaceRef, from: u64, to: u64, from_end: bool) }
              { fn ref_place_downcast(place: PlaceRef, variant_index: u32 /*, type */) }
              { fn ref_place_opaque_cast(place: PlaceRef /*, type */) }
              { fn ref_place_subtype(place: PlaceRef /*, type */) }

              { fn set_place_address(place: PlaceRef, raw_ptr: RawPointer) }
              #[allow(unused_parens)]
              { fn set_place_type_id(place: PlaceRef, type_id: ($type_id_ty)) }
              { fn set_place_type_bool(place: PlaceRef) }
              { fn set_place_type_char(place: PlaceRef) }
              { fn set_place_type_int(place: PlaceRef, bit_size: u64, is_signed: bool) }
              { fn set_place_type_float(place: PlaceRef, e_bits: u64, s_bits: u64) }
              { fn set_place_size(place: PlaceRef, byte_size: TypeSize) }

              { fn ref_operand_copy(place: PlaceRef) -> OperandRef }
              { fn ref_operand_move(place: PlaceRef) -> OperandRef }

              { fn ref_operand_const_bool(value: bool) -> OperandRef }
              #[allow(unused_parens)]
              { fn ref_operand_const_int(bit_rep: ($u128_ty), bit_size: u64, is_signed: bool) -> OperandRef }
              #[allow(unused_parens)]
              { fn ref_operand_const_float(bit_rep: ($u128_ty), e_bits: u64, s_bits: u64) -> OperandRef }
              #[allow(unused_parens)]
              { fn ref_operand_const_char(value: ($char_ty)) -> OperandRef }
              { fn ref_operand_const_func(id: FuncId) -> OperandRef }
              #[allow(unused_parens)]
              { fn ref_operand_const_str(value: ($str_ty)) -> OperandRef }
              #[allow(unused_parens)]
              { fn ref_operand_const_byte_str(value: ($byte_str_ty)) -> OperandRef }
              { fn ref_operand_const_zst() -> OperandRef }
              #[cfg(abs_concrete)]
              { fn ref_operand_const_some() -> OperandRef }

              { fn new_sym_value_bool() -> OperandRef }
              { fn new_sym_value_char() -> OperandRef }
              { fn new_sym_value_int(bit_size: u64, is_signed: bool) -> OperandRef }
              { fn new_sym_value_float(e_bits: u64, s_bits: u64) -> OperandRef }

              { fn assign_use(dest: PlaceRef, operand: OperandRef) }
              { fn assign_repeat(dest: PlaceRef, operand: OperandRef, count: usize) }
              { fn assign_ref(dest: PlaceRef, place: PlaceRef, is_mutable: bool) }
              { fn assign_thread_local_ref(dest: PlaceRef) }
              { fn assign_address_of(dest: PlaceRef, place: PlaceRef, is_mutable: bool) }
              { fn assign_len(dest: PlaceRef, place: PlaceRef) }

              { fn assign_cast_char(dest: PlaceRef, operand: OperandRef) }
              { fn assign_cast_integer(dest: PlaceRef, operand: OperandRef, bit_size: u64, is_signed: bool) }
              { fn assign_cast_float(dest: PlaceRef, operand: OperandRef, e_bits: u64, s_bits: u64) }
              { fn assign_cast_expose_prov(dest: PlaceRef, operand: OperandRef) }
              #[allow(unused_parens)]
              { fn assign_cast_with_exposed_prov(dest: PlaceRef, operand: OperandRef, dst_type_id: ($type_id_ty)) }
              #[allow(unused_parens)]
              { fn assign_cast_to_another_ptr(dest: PlaceRef, operand: OperandRef, dst_type_id: ($type_id_ty)) }

              { fn assign_cast_unsize(dest: PlaceRef, operand: OperandRef) }
              { fn assign_cast_sized_dyn(dest: PlaceRef, operand: OperandRef) }
              #[allow(unused_parens)]
              { fn assign_cast_transmute(dest: PlaceRef, operand: OperandRef, dst_type_id: ($type_id_ty)) }

              #[allow(unused_parens)]
              { fn assign_binary_op(
                  dest: PlaceRef,
                  operator: ($binary_op_ty),
                  first: OperandRef,
                  second: OperandRef,
              ) }
              #[allow(unused_parens)]
              { fn assign_unary_op(dest: PlaceRef, operator: ($unary_op_ty), operand: OperandRef) }

              { fn set_discriminant(dest: PlaceRef, variant_index: u32) }
              { fn assign_discriminant(dest: PlaceRef, place: PlaceRef) }

              #[allow(unused_parens)]
              { fn assign_aggregate_array(
                  dest: PlaceRef,
                  items: ($slice_ty!(OperandRef)),
                  align: Alignment,
              ) }
              #[allow(unused_parens)]
              { fn assign_aggregate_tuple(dest: PlaceRef, fields: ($slice_ty!(OperandRef))) }
              #[allow(unused_parens)]
              { fn assign_aggregate_struct(dest: PlaceRef, fields: ($slice_ty!(OperandRef))) }
              #[allow(unused_parens)]
              { fn assign_aggregate_enum(
                  dest: PlaceRef,
                  fields: ($slice_ty!(OperandRef)),
                  variant: VariantIndex,
              ) }
              { fn assign_aggregate_union(dest: PlaceRef, active_field: FieldIndex, value: OperandRef) }
              #[allow(unused_parens)]
              { fn assign_aggregate_closure(dest: PlaceRef, upvars: ($slice_ty!(OperandRef))) }
              #[allow(unused_parens)]
              { fn assign_aggregate_coroutine(dest: PlaceRef, upvars: ($slice_ty!(OperandRef))) }
              #[allow(unused_parens)]
              { fn assign_aggregate_coroutine_closure(dest: PlaceRef, upvars: ($slice_ty!(OperandRef))) }
              { fn assign_aggregate_raw_ptr(dest: PlaceRef, data_ptr: OperandRef, metadata: OperandRef, is_mutable: bool) }

              #[allow(unused_parens)]
              { fn assign_shallow_init_box(_dest: PlaceRef, _operand: OperandRef, _dst_type_id: ($type_id_ty)) }

              #[allow(unused_parens)]
              { fn new_branching_info(
                  node_location: BasicBlockIndex,
                  discriminant: OperandRef,
                  discr_bit_size: u64,
                  discr_is_signed: bool,
              ) -> ($branching_info_ty) }
              #[allow(unused_parens)]
              { fn take_branch_true(info: ($branching_info_ty)) }
              #[allow(unused_parens)]
              { fn take_branch_false(info: ($branching_info_ty)) }

              #[allow(unused_parens)]
              { fn take_branch_int(info: ($branching_info_ty), value_bit_rep: ($u128_ty)) }
              #[allow(unused_parens)]
              { fn take_branch_ow_int(info: ($branching_info_ty), non_values: ($slice_ty!($u128_ty))) }

              #[allow(unused_parens)]
              { fn take_branch_char(info: ($branching_info_ty), value: (($char_ty))) }
              #[allow(unused_parens)]
              { fn take_branch_ow_char(info: ($branching_info_ty), non_values: ($slice_ty!($char_ty))) }

              #[allow(unused_parens)]
              { fn take_branch_enum_discriminant(info: ($branching_info_ty), index: VariantIndex) }
              #[allow(unused_parens)]
              { fn take_branch_ow_enum_discriminant(
                  info: ($branching_info_ty),
                  non_indices: ($slice_ty!(VariantIndex)),
              ) }

              #[allow(unused_parens)]
              { fn before_call_func(func: OperandRef, args: ($slice_ty!(OperandRef)), are_args_tupled: bool) }

              { fn preserve_special_local_metadata(place: PlaceRef) }

              #[allow(unused_parens)]
              { fn try_untuple_argument(arg_index: LocalIndex, tuple_type_id: ($type_id_ty)) }

              { fn enter_func(func: OperandRef) }

              { fn return_from_func() }

              { fn override_return_value(operand: OperandRef) }

              { fn after_call_func(destination: PlaceRef) }

              { fn check_assert_bounds_check(
                  cond: OperandRef,
                  expected: bool,
                  len: OperandRef,
                  index: OperandRef,
              ) }
              #[allow(unused_parens)]
              { fn check_assert_overflow(
                  cond: OperandRef,
                  expected: bool,
                  operator: ($binary_op_ty),
                  first: OperandRef,
                  second: OperandRef,
              ) }
              { fn check_assert_overflow_neg(cond: OperandRef, expected: bool, operand: OperandRef) }
              { fn check_assert_div_by_zero(cond: OperandRef, expected: bool, operand: OperandRef) }
              { fn check_assert_rem_by_zero(cond: OperandRef, expected: bool, operand: OperandRef) }
              { fn check_assert_misaligned_ptr_deref(
                  cond: OperandRef,
                  expected: bool,
                  required: OperandRef,
                  found: OperandRef,
              ) }
            }
        };
    }

    macro_rules! make_pass_func_names_to_macro {
        ($($(#[$($attr: meta)*])* {fn $name:ident ($($arg:tt)*) $(-> $($ret_ty:tt)*)?})*) => {
            #[cfg_attr(not(core_build), macro_export)]
            macro_rules! pass_func_names_to {
                ($$macro:ident, one_by_one) => {
                    $(
                        $$macro!($name);
                    )*
                };
                ($$macro:ident, all_comma_separated) => {
                    $$macro! {
                        $($name),*
                    }
                };
            }
        };
    }
    pass_func_decls_to!(make_pass_func_names_to_macro);

    #[allow(unused_macros)]
    macro_rules! make_list_func_decls_macro {
        ($($(#[$($attr: meta)*])*{$($sig:tt)+})+) => {
            #[cfg_attr(not(core_build), macro_export)]
            macro_rules! list_func_decls {
                (
                    modifier: $$modifier:path,
                    (
                        u128: $$u128_ty:ty,
                        char: $$char_ty:ty,
                        &str: $$str_ty:ty,
                        &[u8]: $$byte_str_ty:ty,
                        // NOTE: Slice is received as a macro to enable having [T].
                        slice: $$slice_ty:path,
                        branching_info: $$branching_info_ty:ty,
                        type_id: $$type_id_ty:ty,
                        binary_op: $$binary_op_ty:ty,
                        unary_op: $$unary_op_ty:ty $$(,)?
                    )
                ) => {
                    $(
                        $modifier! { $(#[$($attr)*])* $($sig)+ ; }
                    )+
                };
                (modifier: $$modifier:path) => {
                    $$crate::leaf::common::pri::macros::list_func_decls! {
                        modifier: $$modifier,
                        (
                            u128: (),
                            char: (),
                            &str: (),
                            &[u8]: (),
                            slice: $crate::leaf::common::utils::identity,
                            branching_info: (),
                            type_id: (),
                            binary_op:(),
                            unary_op:(),
                        )
                    }
                };
                (modifier: $$modifier:path, (from Self)) => {
                    $$crate::leaf::common::pri::macros::list_func_decls! {
                        modifier: $$modifier,
                        (
                            u128: Self::U128,
                            char: Self::Char,
                            &str: Self::ConstStr,
                            &[u8]: Self::ConstByteStr,
                            slice: $crate::leaf::common::pri::macros::self_slice_of,
                            branching_info: Self::BranchingInfo,
                            type_id: Self::TypeId,
                            binary_op: Self::BinaryOp,
                            unary_op: Self::UnaryOp,
                        )
                    }
                };
                (modifier: $$modifier:path, (from common::ffi)) => {
                    $$crate::leaf::common::pri::macros::list_func_decls! {
                        modifier: $$modifier,
                        (
                            u128: U128Pack,
                            char: CharPack,
                            &str: ConstStrPack,
                            &[u8]: ConstByteStrPack,
                            slice: $crate::leaf::common::pri::macros::slice_pack_of,
                            branching_info: BranchingInfo,
                            type_id: U128Pack<TypeId>,
                            binary_op: common::pri::BinaryOp,
                            unary_op: common::pri::UnaryOp,
                        )
                    }
                };
            }
        }
    }
    // NOTE: Because of a bug in the compiler, we need to perform the expansion manually.
    // pass_func_decls_to!(make_list_func_decls_macro);

    // Recursive expansion of pass_func_decls_to! macro
    // =================================================

    #[cfg_attr(not(core_build), macro_export)]
    macro_rules! list_func_decls {
        (modifier: $modifier:path,(u128: $u128_ty:ty,char: $char_ty:ty, &str: $str_ty:ty, &[u8]: $byte_str_ty:ty,slice: $slice_ty:path,branching_info: $branching_info_ty:ty,type_id: $type_id_ty:ty,binary_op: $binary_op_ty:ty,unary_op: $unary_op_ty:ty$(,)?)) => {
            $modifier!{
                fn init_runtime_lib();
            }
            $modifier!{
                fn shutdown_runtime_lib();
            }
            $modifier!{
                fn ref_place_return_value()->PlaceRef;
            }$modifier!{
                fn ref_place_argument(local_index:LocalIndex)->PlaceRef;
            }$modifier!{
                fn ref_place_local(local_index:LocalIndex)->PlaceRef;
            }$modifier!{
                fn ref_place_deref(place:PlaceRef);
            }$modifier!{
                fn ref_place_field(place:PlaceRef,field:FieldIndex);
            }$modifier!{
                fn ref_place_index(place:PlaceRef,index_place:PlaceRef);
            }$modifier!{
                fn ref_place_constant_index(place:PlaceRef,offset:u64,min_length:u64,from_end:bool);
            }$modifier!{
                fn ref_place_subslice(place:PlaceRef,from:u64,to:u64,from_end:bool);
            }$modifier!{
                fn ref_place_downcast(place:PlaceRef,variant_index:u32);
            }$modifier!{
                fn ref_place_opaque_cast(place:PlaceRef);
            }$modifier!{
                fn ref_place_subtype(place:PlaceRef);
            }$modifier!{
                fn set_place_address(place:PlaceRef,raw_ptr:RawPointer);
            }$modifier!{
                #[allow(unused_parens)]fn set_place_type_id(place:PlaceRef,type_id:($type_id_ty));
            }$modifier!{
                fn set_place_type_bool(place:PlaceRef);
            }$modifier!{
                fn set_place_type_char(place:PlaceRef);
            }$modifier!{
                fn set_place_type_int(place:PlaceRef,bit_size:u64,is_signed:bool);
            }$modifier!{
                fn set_place_type_float(place:PlaceRef,e_bits:u64,s_bits:u64);
            }$modifier!{
                fn set_place_size(place:PlaceRef,byte_size:TypeSize);
            }$modifier!{
                fn ref_operand_copy(place:PlaceRef)->OperandRef;
            }$modifier!{
                fn ref_operand_move(place:PlaceRef)->OperandRef;
            }$modifier!{
                fn ref_operand_const_bool(value:bool)->OperandRef;
            }$modifier!{
                #[allow(unused_parens)]fn ref_operand_const_int(bit_rep:($u128_ty),bit_size:u64,is_signed:bool)->OperandRef;
            }$modifier!{
                #[allow(unused_parens)]fn ref_operand_const_float(bit_rep:($u128_ty),e_bits:u64,s_bits:u64)->OperandRef;
            }$modifier!{
                #[allow(unused_parens)]fn ref_operand_const_char(value:($char_ty))->OperandRef;
            }$modifier!{
                fn ref_operand_const_func(id:FuncId)->OperandRef;
            }$modifier!{
                #[allow(unused_parens)]fn ref_operand_const_str(value:($str_ty))->OperandRef;
            }$modifier!{
                #[allow(unused_parens)]fn ref_operand_const_byte_str(value:($byte_str_ty))->OperandRef;
            }$modifier!{
                fn ref_operand_const_zst()->OperandRef;
            }$modifier!{
                #[cfg(abs_concrete)]fn ref_operand_const_some()->OperandRef;
            }$modifier!{
                fn new_sym_value_bool()->OperandRef;
            }$modifier!{
                fn new_sym_value_char()->OperandRef;
            }$modifier!{
                fn new_sym_value_int(bit_size:u64,is_signed:bool)->OperandRef;
            }$modifier!{
                fn new_sym_value_float(e_bits:u64,s_bits:u64)->OperandRef;
            }$modifier!{
                fn assign_use(dest:PlaceRef,operand:OperandRef);
            }$modifier!{
                fn assign_repeat(dest:PlaceRef,operand:OperandRef,count:usize);
            }$modifier!{
                fn assign_ref(dest:PlaceRef,place:PlaceRef,is_mutable:bool);
            }$modifier!{
                fn assign_thread_local_ref(dest:PlaceRef);
            }$modifier!{
                fn assign_address_of(dest:PlaceRef,place:PlaceRef,is_mutable:bool);
            }$modifier!{
                fn assign_len(dest:PlaceRef,place:PlaceRef);
            }$modifier!{
                fn assign_cast_char(dest:PlaceRef,operand:OperandRef);
            }$modifier!{
                fn assign_cast_integer(dest:PlaceRef,operand:OperandRef,bit_size:u64,is_signed:bool);
            }$modifier!{
                fn assign_cast_float(dest:PlaceRef,operand:OperandRef,e_bits:u64,s_bits:u64);
            }$modifier!{
                fn assign_cast_expose_prov(dest:PlaceRef,operand:OperandRef);
            }$modifier!{
                #[allow(unused_parens)]fn assign_cast_with_exposed_prov(dest:PlaceRef,operand:OperandRef,dst_type_id:($type_id_ty));
            }$modifier!{
                #[allow(unused_parens)]fn assign_cast_to_another_ptr(dest:PlaceRef,operand:OperandRef,dst_type_id:($type_id_ty));
            }$modifier!{
                fn assign_cast_unsize(dest:PlaceRef,operand:OperandRef);
            }$modifier!{
                fn assign_cast_sized_dyn(dest:PlaceRef,operand:OperandRef);
            }$modifier!{
                #[allow(unused_parens)]fn assign_cast_transmute(dest:PlaceRef,operand:OperandRef,dst_type_id:($type_id_ty));
            }$modifier!{
                #[allow(unused_parens)]fn assign_binary_op(dest:PlaceRef,operator:($binary_op_ty),first:OperandRef,second:OperandRef,);
            }$modifier!{
                #[allow(unused_parens)]fn assign_unary_op(dest:PlaceRef,operator:($unary_op_ty),operand:OperandRef);
            }$modifier!{
                fn set_discriminant(dest:PlaceRef,variant_index:u32);
            }$modifier!{
                fn assign_discriminant(dest:PlaceRef,place:PlaceRef);
            }$modifier!{
                #[allow(unused_parens)]fn assign_aggregate_array(dest:PlaceRef,items:($slice_ty!(OperandRef)), align:Alignment,);
            }$modifier!{
                #[allow(unused_parens)]fn assign_aggregate_tuple(dest:PlaceRef,fields:($slice_ty!(OperandRef)));
            }$modifier!{
                #[allow(unused_parens)]fn assign_aggregate_struct(dest:PlaceRef,fields:($slice_ty!(OperandRef)));
            }$modifier!{
                #[allow(unused_parens)]fn assign_aggregate_enum(dest:PlaceRef,fields:($slice_ty!(OperandRef)),variant:VariantIndex,);
            }$modifier!{
                fn assign_aggregate_union(dest:PlaceRef,active_field:FieldIndex,value:OperandRef);
            }$modifier!{
                #[allow(unused_parens)]fn assign_aggregate_closure(dest:PlaceRef,upvars:($slice_ty!(OperandRef)));
            }$modifier!{
                #[allow(unused_parens)]fn assign_aggregate_coroutine(dest:PlaceRef,upvars:($slice_ty!(OperandRef)));
            }$modifier!{
                #[allow(unused_parens)]fn assign_aggregate_coroutine_closure(dest:PlaceRef,upvars:($slice_ty!(OperandRef)));
            }$modifier!{
                fn assign_aggregate_raw_ptr(dest:PlaceRef,data_ptr:OperandRef,metadata:OperandRef,is_mutable: bool);
            }$modifier!{
                #[allow(unused_parens)]fn assign_shallow_init_box(_dest:PlaceRef,_operand:OperandRef,_dst_type_id:($type_id_ty));
            }$modifier!{
                #[allow(unused_parens)]fn new_branching_info(node_location:BasicBlockIndex,discriminant:OperandRef,discr_bit_size:u64,discr_is_signed:bool,)->($branching_info_ty);
            }$modifier!{
                #[allow(unused_parens)]fn take_branch_true(info:($branching_info_ty));
            }$modifier!{
                #[allow(unused_parens)]fn take_branch_false(info:($branching_info_ty));
            }$modifier!{
                #[allow(unused_parens)]fn take_branch_int(info:($branching_info_ty),value_bit_rep:($u128_ty));
            }$modifier!{
                #[allow(unused_parens)]fn take_branch_ow_int(info:($branching_info_ty),non_values:($slice_ty!($u128_ty)));
            }$modifier!{
                #[allow(unused_parens)]fn take_branch_char(info:($branching_info_ty),value:(($char_ty)));
            }$modifier!{
                #[allow(unused_parens)]fn take_branch_ow_char(info:($branching_info_ty),non_values:($slice_ty!($char_ty)));
            }$modifier!{
                #[allow(unused_parens)]fn take_branch_enum_discriminant(info:($branching_info_ty),index:VariantIndex);
            }$modifier!{
                #[allow(unused_parens)]fn take_branch_ow_enum_discriminant(info:($branching_info_ty),non_indices:($slice_ty!(VariantIndex)),);
            }$modifier!{
                #[allow(unused_parens)]fn before_call_func(func:OperandRef,args:($slice_ty!(OperandRef)),are_args_tupled:bool);
            }$modifier!{
                fn preserve_special_local_metadata(place:PlaceRef);
            }$modifier!{
                #[allow(unused_parens)]fn try_untuple_argument(arg_index:LocalIndex,tuple_type_id:($type_id_ty));
            }$modifier!{
                fn enter_func(func:OperandRef);
            }$modifier!{
                fn return_from_func();
            }$modifier!{
                fn override_return_value(operand:OperandRef);
            }$modifier!{
                fn after_call_func(destination:PlaceRef);
            }$modifier!{
                fn check_assert_bounds_check(cond:OperandRef,expected:bool,len:OperandRef,index:OperandRef,);
            }$modifier!{
                #[allow(unused_parens)]fn check_assert_overflow(cond:OperandRef,expected:bool,operator:($binary_op_ty),first:OperandRef,second:OperandRef,);
            }$modifier!{
                fn check_assert_overflow_neg(cond:OperandRef,expected:bool,operand:OperandRef);
            }$modifier!{
                fn check_assert_div_by_zero(cond:OperandRef,expected:bool,operand:OperandRef);
            }$modifier!{
                fn check_assert_rem_by_zero(cond:OperandRef,expected:bool,operand:OperandRef);
            }$modifier!{
                fn check_assert_misaligned_ptr_deref(cond:OperandRef,expected:bool,required:OperandRef,found:OperandRef,);
            }
        };
        (modifier: $modifier:path) => {
            $crate::leaf::common::pri::macros::list_func_decls!{
                modifier: $modifier,(u128:(),char:(), &str:(), &[u8]:(),slice:$crate::leaf::common::utils::identity,branching_info:(),type_id:(),binary_op:(),unary_op:(),)
            }
        };
        (modifier: $modifier:path,(from Self)) => {
            $crate::leaf::common::pri::macros::list_func_decls!{
                modifier: $modifier,(u128:Self::U128,char:Self::Char, &str:Self::ConstStr, &[u8]:Self::ConstByteStr,slice:$crate::leaf::common::pri::macros::self_slice_of,branching_info:Self::BranchingInfo,type_id:Self::TypeId,binary_op:Self::BinaryOp,unary_op:Self::UnaryOp,)
            }
        };
        (modifier: $modifier:path,(from common::ffi)) => {
            $crate::leaf::common::pri::macros::list_func_decls!{
                modifier: $modifier,(u128:U128Pack,char:CharPack, &str:ConstStrPack, &[u8]:ConstByteStrPack,slice:$crate::leaf::common::pri::macros::slice_pack_of,branching_info:BranchingInfo,type_id:U128Pack<TypeId>,binary_op:common::pri::BinaryOp,unary_op:common::pri::UnaryOp,)
            }
        };
    }

    #[cfg(not(core_build))]
    pub use {list_func_decls, pass_func_names_to, self_slice_of, slice_pack_of};
    #[cfg(core_build)]
    #[stable(feature = "rust1", since = "1.0.0")]
    pub(crate) use {list_func_decls, pass_func_names_to, self_slice_of, slice_pack_of};
}
#[cfg(not(core_build))]
pub use macros::{list_func_decls, pass_func_names_to};
#[cfg(core_build)]
pub(crate) use macros::{list_func_decls, pass_func_names_to};
