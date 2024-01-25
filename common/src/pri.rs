use super::*;

pub type Ref = u64;
pub type PlaceRef = Ref;
pub type OperandRef = Ref;

#[repr(transparent)]
pub struct BinaryOp(pub u8);
#[repr(transparent)]
pub struct UnaryOp(pub u8);

impl BinaryOp {
    pub const ADD: Self = Self(1);
    pub const SUB: Self = Self(2);
    pub const MUL: Self = Self(3);
    pub const DIV: Self = Self(4);
    pub const REM: Self = Self(5);
    pub const BIT_XOR: Self = Self(6);
    pub const BIT_AND: Self = Self(7);
    pub const BIT_OR: Self = Self(8);
    pub const SHL: Self = Self(9);
    pub const SHR: Self = Self(10);
    pub const EQ: Self = Self(11);
    pub const LT: Self = Self(12);
    pub const LE: Self = Self(13);
    pub const NE: Self = Self(14);
    pub const GE: Self = Self(15);
    pub const GT: Self = Self(16);
    pub const OFFSET: Self = Self(17);

    pub const fn from_raw(raw: u8) -> Self {
        if raw == BinaryOp::ADD.as_u8() {
            BinaryOp::ADD
        } else if raw == BinaryOp::SUB.as_u8() {
            BinaryOp::SUB
        } else if raw == BinaryOp::MUL.as_u8() {
            BinaryOp::MUL
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
        } else if raw == BinaryOp::SHR.as_u8() {
            BinaryOp::SHR
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
        } else if raw == BinaryOp::OFFSET.as_u8() {
            BinaryOp::OFFSET
        } else {
            unreachable!()
        }
    }

    pub const fn as_u8(self) -> u8 {
        self.0
    }
}

impl UnaryOp {
    pub const NOT: Self = Self(31);
    pub const NEG: Self = Self(32);

    pub const fn from_raw(raw: u8) -> Self {
        if raw == UnaryOp::NOT.as_u8() {
            UnaryOp::NOT
        } else if raw == UnaryOp::NEG.as_u8() {
            UnaryOp::NEG
        } else {
            unreachable!()
        }
    }

    pub const fn as_u8(self) -> u8 {
        self.0
    }
}

#[repr(C)]
pub struct BranchingInfo {
    node_location: BasicBlockIndex,
    discriminant: OperandRef,
    discr_bit_size: u64,
    discr_is_signed: bool,
}

macro_rules! make_list_func_decls_macro {
    ($($(#[$($attr: meta)*])*{$($sig:tt)+})+) => {
        #[macro_export]
        macro_rules! list_func_decls {
            (modifier: $$modifier:ident, (u128: $u128_ty:ty, char: $char_ty:ty, &str: $str_ty:ty, [u8]: $byte_str_ty:ty, slice: $slice_ty:path, branching_info: $branching_info_ty:ty, type_id: $type_id_ty:ty)) => {
                $(
                    $modifier! { $(#[$($attr)*])* $($sig)+ ; }
                )+
            };
            (modifier: $$modifier:ident) => {
              common::pri::list_func_decls! {
                  modifier: $$modifier,
                  (u128: (), char: (), &str: (), [u8]: (), slice: common::utils::identity, branching_info: (), type_id: ())
                }
            };
        }
    }
}

// NOTE: Because of a bug in the compiler, we need to perform the expansion manually.
/*
make_list_func_decls_macro! {
    { fn init_runtime_lib() }

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

    #[cfg(place_addr)]
    { fn set_place_address(place: PlaceRef, raw_ptr: RawPointer) }
    #[cfg(place_addr)]
    { fn set_place_type_id(place: PlaceRef, type_id: $type_id_ty) }
    #[cfg(place_addr)]
    { fn set_place_type_bool(place: PlaceRef) }
    #[cfg(place_addr)]
    { fn set_place_type_char(place: PlaceRef) }
    #[cfg(place_addr)]
    { fn set_place_type_int(place: PlaceRef, bit_size: u64, is_signed: bool) }
    #[cfg(place_addr)]
    { fn set_place_type_float(place: PlaceRef, e_bits: u64, s_bits: u64) }

    #[cfg(place_addr)]
    { fn set_place_size(place: PlaceRef, byte_size: TypeSize) }

    { fn ref_operand_copy(place: PlaceRef) -> OperandRef }
    { fn ref_operand_move(place: PlaceRef) -> OperandRef }

    { fn ref_operand_const_bool(value: bool) -> OperandRef }
    { fn ref_operand_const_int(bit_rep: ($u128_ty), bit_size: u64, is_signed: bool) -> OperandRef }
    { fn ref_operand_const_float(bit_rep: ($u128_ty), e_bits: u64, s_bits: u64) -> OperandRef }
    { fn ref_operand_const_char(value: ($char_ty)) -> OperandRef }
    { fn ref_operand_const_func(id: FuncId) -> OperandRef }
    { fn ref_operand_const_str(value: ($str_ty)) -> OperandRef }
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
    { fn assign_cast_expose_addr(dest: PlaceRef, operand: OperandRef) }
    { fn assign_cast_from_exposed_addr(dest: PlaceRef, operand: OperandRef, dst_type_id: $type_id_ty) }
    { fn assign_cast_to_another_ptr(dest: PlaceRef, operand: OperandRef, dst_type_id: $type_id_ty) }

    { fn assign_cast_unsize(dest: PlaceRef, operand: OperandRef) }
    { fn assign_cast_sized_dyn(dest: PlaceRef, operand: OperandRef) }
    { fn assign_cast_transmute(dest: PlaceRef, operand: OperandRef, dst_type_id: $type_id_ty) }

    { fn assign_binary_op(
        dest: PlaceRef,
        operator: BinaryOp,
        first: OperandRef,
        second: OperandRef,
        checked: bool,
    ) }
    { fn assign_unary_op(dest: PlaceRef, operator: UnaryOp, operand: OperandRef) }

    { fn set_discriminant(dest: PlaceRef, variant_index: u32) }
    { fn assign_discriminant(dest: PlaceRef, place: PlaceRef) }

    // We use slice to simplify working with the interface.
    { fn assign_aggregate_array(
        dest: PlaceRef,
        items: ($slice_ty!(OperandRef)),
        #[cfg(place_addr)] align: Alignment,
    ) }

    { fn assign_aggregate_tuple(dest: PlaceRef, fields: ($slice_ty!(OperandRef))) }
    { fn assign_aggregate_struct(dest: PlaceRef, fields: ($slice_ty!(OperandRef))) }
    { fn assign_aggregate_enum(
        dest: PlaceRef,
        fields: ($slice_ty!(OperandRef)),
        variant: VariantIndex,
    ) }
    { fn assign_aggregate_union(dest: PlaceRef, active_field: FieldIndex, value: OperandRef) }
    { fn assign_aggregate_closure(dest: PlaceRef, upvars: ($slice_ty!(OperandRef))) }

    { fn new_branching_info(
        node_location: BasicBlockIndex,
        discriminant: OperandRef,
        discr_bit_size: u64,
        discr_is_signed: bool,
    ) -> ($branching_info_ty) }
    { fn take_branch_true(info: ($branching_info_ty)) }
    { fn take_branch_false(info: ($branching_info_ty)) }

    { fn take_branch_int(info: ($branching_info_ty), value_bit_rep: ($u128_ty)) }
    { fn take_branch_ow_int(info: ($branching_info_ty), non_values: ($slice_ty!($u128_ty))) }

    { fn take_branch_char(info: ($branching_info_ty), value: (($char_ty))) }
    { fn take_branch_ow_char(info: ($branching_info_ty), non_values: ($slice_ty!($char_ty))) }

    { fn take_branch_enum_discriminant(info: ($branching_info_ty), index: VariantIndex) }
    { fn take_branch_ow_enum_discriminant(
        info: ($branching_info_ty),
        non_indices: ($slice_ty!(VariantIndex)),
    ) }

    { fn before_call_func(func: OperandRef, args: ($slice_ty!(OperandRef)), are_args_tupled: bool) }

    #[cfg(place_addr)]
    { fn preserve_special_local_metadata(place: PlaceRef) }

    { fn try_untuple_argument(arg_index: LocalIndex, tuple_type_id: $type_id_ty) }

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
    { fn check_assert_overflow(
        cond: OperandRef,
        expected: bool,
        operator: BinaryOp,
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
*/
// Recursive expansion of make_list_func_decls_macro! macro
// =========================================================

#[macro_export]
macro_rules! list_func_decls {
  (modifier: $modifier:ident,(u128: $u128_ty:ty,char: $char_ty:ty, &str: $str_ty:ty,[u8]: $byte_str_ty:ty,slice: $slice_ty:path,branching_info: $branching_info_ty:ty,type_id: $type_id_ty:ty)) => {
    $modifier!{
      fn init_runtime_lib();
    }$modifier!{
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
      #[cfg(place_addr)]fn set_place_address(place:PlaceRef,raw_ptr:RawPointer);
    }$modifier!{
      #[cfg(place_addr)]fn set_place_type_id(place:PlaceRef,type_id: $type_id_ty);
    }$modifier!{
      #[cfg(place_addr)]fn set_place_type_bool(place:PlaceRef);
    }$modifier!{
      #[cfg(place_addr)]fn set_place_type_char(place:PlaceRef);
    }$modifier!{
      #[cfg(place_addr)]fn set_place_type_int(place:PlaceRef,bit_size:u64,is_signed:bool);
    }$modifier!{
      #[cfg(place_addr)]fn set_place_type_float(place:PlaceRef,e_bits:u64,s_bits:u64);
    }$modifier!{
      #[cfg(place_addr)]fn set_place_size(place:PlaceRef,byte_size:TypeSize);
    }$modifier!{
      fn ref_operand_copy(place:PlaceRef)->OperandRef;
    }$modifier!{
      fn ref_operand_move(place:PlaceRef)->OperandRef;
    }$modifier!{
      fn ref_operand_const_bool(value:bool)->OperandRef;
    }$modifier!{
      fn ref_operand_const_int(bit_rep:($u128_ty),bit_size:u64,is_signed:bool)->OperandRef;
    }$modifier!{
      fn ref_operand_const_float(bit_rep:($u128_ty),e_bits:u64,s_bits:u64)->OperandRef;
    }$modifier!{
      fn ref_operand_const_char(value:($char_ty))->OperandRef;
    }$modifier!{
      fn ref_operand_const_func(id:FuncId)->OperandRef;
    }$modifier!{
      fn ref_operand_const_str(value:($str_ty))->OperandRef;
    }$modifier!{
      fn ref_operand_const_byte_str(value:($byte_str_ty))->OperandRef;
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
      fn assign_cast_expose_addr(dest:PlaceRef,operand:OperandRef);
    }$modifier!{
      fn assign_cast_from_exposed_addr(dest:PlaceRef,operand:OperandRef,dst_type_id: $type_id_ty);
    }$modifier!{
      fn assign_cast_to_another_ptr(dest:PlaceRef,operand:OperandRef,dst_type_id: $type_id_ty);
    }$modifier!{
      fn assign_cast_unsize(dest:PlaceRef,operand:OperandRef);
    }$modifier!{
      fn assign_cast_sized_dyn(dest:PlaceRef,operand:OperandRef);
    }$modifier!{
      fn assign_cast_transmute(dest:PlaceRef,operand:OperandRef,dst_type_id: $type_id_ty);
    }$modifier!{
      fn assign_binary_op(dest:PlaceRef,operator:BinaryOp,first:OperandRef,second:OperandRef,checked:bool,);
    }$modifier!{
      fn assign_unary_op(dest:PlaceRef,operator:UnaryOp,operand:OperandRef);
    }$modifier!{
      fn set_discriminant(dest:PlaceRef,variant_index:u32);
    }$modifier!{
      fn assign_discriminant(dest:PlaceRef,place:PlaceRef);
    }$modifier!{
      fn assign_aggregate_array(dest:PlaceRef,items:($slice_ty!(OperandRef)), #[cfg(place_addr)]align:Alignment,);
    }$modifier!{
      fn assign_aggregate_tuple(dest:PlaceRef,fields:($slice_ty!(OperandRef)));
    }$modifier!{
      fn assign_aggregate_struct(dest:PlaceRef,fields:($slice_ty!(OperandRef)));
    }$modifier!{
      fn assign_aggregate_enum(dest:PlaceRef,fields:($slice_ty!(OperandRef)),variant:VariantIndex,);
    }$modifier!{
      fn assign_aggregate_union(dest:PlaceRef,active_field:FieldIndex,value:OperandRef);
    }$modifier!{
      fn assign_aggregate_closure(dest:PlaceRef,upvars:($slice_ty!(OperandRef)));
    }$modifier!{
      fn new_branching_info(node_location:BasicBlockIndex,discriminant:OperandRef,discr_bit_size:u64,discr_is_signed:bool,)->($branching_info_ty);
    }$modifier!{
      fn take_branch_true(info:($branching_info_ty));
    }$modifier!{
      fn take_branch_false(info:($branching_info_ty));
    }$modifier!{
      fn take_branch_int(info:($branching_info_ty),value_bit_rep:($u128_ty));
    }$modifier!{
      fn take_branch_ow_int(info:($branching_info_ty),non_values:($slice_ty!($u128_ty)));
    }$modifier!{
      fn take_branch_char(info:($branching_info_ty),value:(($char_ty)));
    }$modifier!{
      fn take_branch_ow_char(info:($branching_info_ty),non_values:($slice_ty!($char_ty)));
    }$modifier!{
      fn take_branch_enum_discriminant(info:($branching_info_ty),index:VariantIndex);
    }$modifier!{
      fn take_branch_ow_enum_discriminant(info:($branching_info_ty),non_indices:($slice_ty!(VariantIndex)),);
    }$modifier!{
      fn before_call_func(func:OperandRef,args:($slice_ty!(OperandRef)),are_args_tupled:bool);
    }$modifier!{
      #[cfg(place_addr)]fn preserve_special_local_metadata(place:PlaceRef);
    }$modifier!{
      fn try_untuple_argument(arg_index:LocalIndex,tuple_type_id: $type_id_ty);
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
      fn check_assert_overflow(cond:OperandRef,expected:bool,operator:BinaryOp,first:OperandRef,second:OperandRef,);
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
  (modifier: $modifier:ident) => {
    common::pri::list_func_decls!{
      modifier: $modifier,(u128:(),char:(), &str:(),[u8]:(),slice:common::utils::identity,branching_info:(),type_id:())
    }
  };
}

pub use list_func_decls;
