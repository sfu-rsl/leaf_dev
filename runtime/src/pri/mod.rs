mod instance;
mod utils;

use crate::abs::{
    backend::*, Alignment, AssertKind, BasicBlockIndex, BinaryOp, BranchingMetadata, CastKind,
    FieldIndex, FloatType, FuncId, IntType, Local, LocalIndex, PointerOffset, RawPointer, TypeId,
    TypeSize, UnaryOp, ValueType, VariantIndex,
};

use self::instance::*;

pub type Ref = u64;
pub type PlaceRef = Ref;
pub type OperandRef = Ref;

/*
 * This field serves as a marker to find the module in the compiler easier.
 */
pub static MODULE_MARKER: u8 = 0;

pub fn init_runtime_lib() {
    log::info!("Initializing the runtime library.");
    init_backend();
}

pub fn ref_place_return_value() -> PlaceRef {
    push_place_ref(|p| p.of_local(Local::ReturnValue))
}
pub fn ref_place_argument(local_index: LocalIndex) -> PlaceRef {
    push_place_ref(|p| p.of_local(Local::Argument(local_index)))
}
pub fn ref_place_local(local_index: LocalIndex) -> PlaceRef {
    push_place_ref(|p| p.of_local(Local::Normal(local_index)))
}

pub fn ref_place_deref(place: PlaceRef) {
    mut_place_ref(place, |p, place| p.project_on(place).deref())
}
pub fn ref_place_field(place: PlaceRef, field: FieldIndex /*, type */) {
    mut_place_ref(place, |p, place| p.project_on(place).for_field(field))
}
pub fn ref_place_index(place: PlaceRef, index_place: PlaceRef) {
    let index = take_back_place_ref(index_place)
        .try_into()
        .unwrap_or_else(|p| panic!("The place used as an index should be just a local. {:?}", p));
    mut_place_ref(place, |p, place| p.project_on(place).at_index(index))
}
pub fn ref_place_constant_index(place: PlaceRef, offset: u64, min_length: u64, from_end: bool) {
    mut_place_ref(place, |p, place| {
        p.project_on(place)
            .at_constant_index(offset, min_length, from_end)
    })
}
pub fn ref_place_subslice(place: PlaceRef, from: u64, to: u64, from_end: bool) {
    mut_place_ref(place, |p, place| {
        p.project_on(place).subslice(from, to, from_end)
    })
}
pub fn ref_place_downcast(place: PlaceRef, variant_index: u32 /*, type */) {
    mut_place_ref(place, |p, place| {
        p.project_on(place).downcast(variant_index)
    })
}
pub fn ref_place_opaque_cast(place: PlaceRef /*, type */) {
    mut_place_ref(place, |p, place| p.project_on(place).opaque_cast())
}
pub fn ref_place_subtype(place: PlaceRef /*, type */) {
    mut_place_ref(place, |p, place| p.project_on(place).subtype())
}
#[cfg(place_addr)]
pub fn set_place_address_typed<T>(place: PlaceRef, ptr: *const T) {
    set_place_address(place, ptr.expose_addr() as RawPointer)
}
#[cfg(place_addr)]
pub fn set_place_address(place: PlaceRef, raw_ptr: RawPointer) {
    mut_place_ref(place, |p, place| p.metadata(place).set_address(raw_ptr));
}
#[cfg(place_addr)]
pub fn set_place_type_id(place: PlaceRef, type_id: TypeId) {
    mut_place_ref(place, |h, p| h.metadata(p).set_type_id(type_id))
}
#[cfg(place_addr)]
pub fn set_place_type_bool(place: PlaceRef) {
    set_place_type(place, ValueType::Bool)
}
#[cfg(place_addr)]
pub fn set_place_type_char(place: PlaceRef) {
    set_place_type(place, ValueType::Char)
}
#[cfg(place_addr)]
pub fn set_place_type_int(place: PlaceRef, bit_size: u64, is_signed: bool) {
    set_place_type(place, ValueType::new_int(bit_size, is_signed))
}
#[cfg(place_addr)]
pub fn set_place_type_float(place: PlaceRef, e_bits: u64, s_bits: u64) {
    set_place_type(place, ValueType::new_float(e_bits, s_bits))
}
#[cfg(place_addr)]
fn set_place_type(place: PlaceRef, ty: ValueType) {
    mut_place_ref(place, |p, place| p.metadata(place).set_primitive_type(ty));
}
#[cfg(place_addr)]
pub fn set_place_size(place: PlaceRef, byte_size: TypeSize) {
    mut_place_ref(place, |h, p| h.metadata(p).set_size(byte_size))
}

pub fn ref_operand_copy(place: PlaceRef) -> OperandRef {
    push_operand_ref(|o| o.copy_of(take_back_place_ref(place)))
}
pub fn ref_operand_move(place: PlaceRef) -> OperandRef {
    push_operand_ref(|o| o.move_of(take_back_place_ref(place)))
}

pub fn ref_operand_const_bool(value: bool) -> OperandRef {
    push_operand_ref(|o| o.const_from().bool(value))
}
pub fn ref_operand_const_int(bit_rep: u128, bit_size: u64, is_signed: bool) -> OperandRef {
    push_operand_ref(|o| {
        o.const_from().int(
            bit_rep,
            IntType {
                bit_size,
                is_signed,
            },
        )
    })
}
pub fn ref_operand_const_float(bit_rep: u128, e_bits: u64, s_bits: u64) -> OperandRef {
    push_operand_ref(|o| o.const_from().float(bit_rep, FloatType { e_bits, s_bits }))
}
pub fn ref_operand_const_char(value: char) -> OperandRef {
    push_operand_ref(|o| o.const_from().char(value))
}
pub fn ref_operand_const_func(id: FuncId) -> OperandRef {
    push_operand_ref(|o| o.const_from().func(id))
}
pub fn ref_operand_const_str(value: &'static str) -> OperandRef {
    push_operand_ref(|o| o.const_from().str(value))
}
pub fn ref_operand_const_byte_str(value: &'static [u8]) -> OperandRef {
    push_operand_ref(|o| o.const_from().byte_str(value))
}
pub fn ref_operand_const_zst() -> OperandRef {
    push_operand_ref(|o| o.const_from().zst())
}
#[cfg(abs_concrete)]
pub fn ref_operand_const_some() -> OperandRef {
    push_operand_ref(|o| o.const_from().some())
}

pub fn new_sym_value_bool() -> OperandRef {
    push_operand_ref(|o| o.new_symbolic(ValueType::Bool))
}
pub fn new_sym_value_char() -> OperandRef {
    push_operand_ref(|o| o.new_symbolic(ValueType::Char))
}
pub fn new_sym_value_int(bit_size: u64, is_signed: bool) -> OperandRef {
    push_operand_ref(|o| o.new_symbolic(ValueType::new_int(bit_size, is_signed)))
}
pub fn new_sym_value_float(e_bits: u64, s_bits: u64) -> OperandRef {
    push_operand_ref(|o| o.new_symbolic(ValueType::new_float(e_bits, s_bits)))
}

pub fn assign_use(dest: PlaceRef, operand: OperandRef) {
    assign_to(dest, |h| h.use_of(take_back_operand_ref(operand)))
}
pub fn assign_repeat(dest: PlaceRef, operand: OperandRef, count: usize) {
    assign_to(dest, |h| h.repeat_of(take_back_operand_ref(operand), count))
}
pub fn assign_ref(dest: PlaceRef, place: PlaceRef, is_mutable: bool) {
    assign_to(dest, |h| h.ref_to(take_back_place_ref(place), is_mutable))
}
pub fn assign_thread_local_ref(
    dest: PlaceRef, /* TODO: Complicated. MIRAI has some works on it. */
) {
    assign_to(dest, |h| h.thread_local_ref_to())
}
pub fn assign_address_of(dest: PlaceRef, place: PlaceRef, is_mutable: bool) {
    assign_to(dest, |h| {
        h.address_of(take_back_place_ref(place), is_mutable)
    })
}
pub fn assign_len(dest: PlaceRef, place: PlaceRef) {
    // To be investigated. Not obvious whether it appears at all in the later stages.
    assign_to(dest, |h| h.len_of(take_back_place_ref(place)))
}

pub fn assign_cast_char(dest: PlaceRef, operand: OperandRef) {
    assign_to(dest, |h| {
        h.cast_of(take_back_operand_ref(operand), CastKind::ToChar)
    })
}
pub fn assign_cast_integer(dest: PlaceRef, operand: OperandRef, bit_size: u64, is_signed: bool) {
    assign_to(dest, |h| {
        h.cast_of(
            take_back_operand_ref(operand),
            CastKind::ToInt(IntType {
                bit_size,
                is_signed,
            }),
        )
    })
}
pub fn assign_cast_float(dest: PlaceRef, operand: OperandRef, e_bits: u64, s_bits: u64) {
    assign_to(dest, |h| {
        h.cast_of(
            take_back_operand_ref(operand),
            CastKind::ToFloat(FloatType { e_bits, s_bits }),
        )
    })
}

pub fn assign_cast_expose_addr(dest: PlaceRef, operand: OperandRef, dst_type_id: TypeId) {
    assign_to(dest, |h| {
        h.cast_of(
            take_back_operand_ref(operand),
            CastKind::ExposeAddress(dst_type_id),
        )
    })
}
pub fn assign_cast_from_addr(dest: PlaceRef, operand: OperandRef, dst_type_id: TypeId) {
    assign_to(dest, |h| {
        h.cast_of(
            take_back_operand_ref(operand),
            CastKind::FromAddress(dst_type_id),
        )
    })
}

pub fn assign_cast_to_ptr(dest: PlaceRef, operand: OperandRef, dst_type_id: TypeId) {
    assign_to(dest, |h| {
        h.cast_of(
            take_back_operand_ref(operand),
            CastKind::ToPointer(dst_type_id),
        )
    })
}

pub fn assign_cast_unsize(dest: PlaceRef, operand: OperandRef) {
    assign_to(dest, |h| {
        h.cast_of(take_back_operand_ref(operand), CastKind::PointerUnsize)
    })
}

pub fn assign_binary_op(
    dest: PlaceRef,
    operator: BinaryOp,
    first: OperandRef,
    second: OperandRef,
    checked: bool,
) {
    assign_to(dest, |h| {
        h.binary_op_between(
            operator,
            take_back_operand_ref(first),
            take_back_operand_ref(second),
            checked,
        )
    })
}
pub fn assign_unary_op(dest: PlaceRef, operator: UnaryOp, operand: OperandRef) {
    assign_to(dest, |h| {
        h.unary_op_on(operator, take_back_operand_ref(operand))
    })
}

pub fn set_discriminant(dest: PlaceRef, variant_index: u32) {
    assign_to(dest, |h| h.variant_index(variant_index))
}
pub fn assign_discriminant(dest: PlaceRef, place: PlaceRef) {
    assign_to(dest, |h| h.discriminant_of(take_back_place_ref(place)))
}

// We use slice to simplify working with the interface.
pub fn assign_aggregate_array(
    dest: PlaceRef,
    items: &[OperandRef],
    #[cfg(place_addr)] align: Alignment,
) {
    #[cfg(place_addr)]
    log::warn!(
        "Align is going to be supported based on types. Ignoring it {}.",
        align
    );

    assign_to(dest, |h| {
        h.array_from(items.iter().map(|o| take_back_operand_ref(*o)))
    })
}
pub fn assign_aggregate_tuple(dest: PlaceRef, fields: &[OperandRef]) {
    assign_to(dest, |h| {
        let fields = take_fields(fields);
        h.tuple_from(fields.into_iter())
    })
}
pub fn assign_aggregate_struct(dest: PlaceRef, fields: &[OperandRef]) {
    assign_to(dest, |h| {
        let fields = take_fields(fields);
        h.adt_from(fields.into_iter(), None)
    })
}
pub fn assign_aggregate_enum(dest: PlaceRef, fields: &[OperandRef], variant: VariantIndex) {
    assign_to(dest, |h| {
        let fields = take_fields(fields);
        h.adt_from(fields.into_iter(), Some(variant))
    })
}
pub fn assign_aggregate_union(dest: PlaceRef, active_field: FieldIndex, value: OperandRef) {
    assign_to(dest, |h| {
        let field = take_fields(&[value]).pop().unwrap();
        h.union_from(active_field, field)
    })
}
fn take_fields(fields: &[OperandRef]) -> Vec<FieldImpl> {
    let fields = fields.iter().map(|o| take_back_operand_ref(*o));
    fields.map(Into::<FieldImpl>::into).collect()
}

pub fn take_branch_true(info: BranchingInfo) {
    conditional(info, |h| h.on_bool().take(true))
}
pub fn take_branch_false(info: BranchingInfo) {
    conditional(info, |h| h.on_bool().take(false))
}

pub fn take_branch_int(info: BranchingInfo, value_bit_rep: u128) {
    conditional(info, |h| h.on_int().take(value_bit_rep))
}
pub fn take_branch_ow_int(info: BranchingInfo, non_values: &[u128]) {
    conditional(info, |h| h.on_int().take_otherwise(non_values))
}

pub fn take_branch_char(info: BranchingInfo, value: char) {
    conditional(info, |h| h.on_char().take(value))
}
pub fn take_branch_ow_char(info: BranchingInfo, non_values: &[char]) {
    conditional(info, |h| h.on_char().take_otherwise(non_values))
}

pub fn take_branch_enum_discriminant(info: BranchingInfo, index: VariantIndex) {
    conditional(info, |h| h.on_enum().take(index))
}
pub fn take_branch_ow_enum_discriminant(info: BranchingInfo, non_indices: &[VariantIndex]) {
    conditional(info, |h| h.on_enum().take_otherwise(non_indices))
}

pub fn before_call_func(func: OperandRef, args: &[OperandRef]) {
    func_control(|h| {
        h.before_call(
            take_back_operand_ref(func),
            args.iter().map(|o| take_back_operand_ref(*o)),
        )
    });
}

pub fn enter_func(func: OperandRef) {
    func_control(|h| h.enter(take_back_operand_ref(func)))
}
pub fn internal_enter_func() {
    func_control(|h| h.internal_enter())
}

pub fn return_from_func() {
    func_control(|h| h.ret())
}

/// Overrides (forces) the return value of a function.
/// In an external call chain, the value will be kept as the return value
/// until it is consumed at the point of return to an internal caller.
pub fn override_return_value(operand: OperandRef) {
    func_control(|h| h.override_return_value(take_back_operand_ref(operand)))
}

pub fn after_call_func(destination: PlaceRef) {
    func_control(|h| h.after_call(take_back_place_ref(destination)))
}

#[cfg(place_addr)]
pub fn preserve_special_local_metadata(place: PlaceRef) {
    func_control(|h| h.metadata().preserve_metadata(take_back_place_ref(place)))
}

pub fn check_assert_bounds_check(
    cond: OperandRef,
    expected: bool,
    len: OperandRef,
    index: OperandRef,
) {
    let assert_kind = AssertKind::BoundsCheck {
        len: take_back_operand_ref(len),
        index: take_back_operand_ref(index),
    };
    check_assert(cond, expected, assert_kind)
}
pub fn check_assert_overflow(
    cond: OperandRef,
    expected: bool,
    operator: BinaryOp,
    first: OperandRef,
    second: OperandRef,
) {
    let assert_kind = AssertKind::Overflow(
        operator,
        take_back_operand_ref(first),
        take_back_operand_ref(second),
    );
    check_assert(cond, expected, assert_kind)
}
pub fn check_assert_overflow_neg(cond: OperandRef, expected: bool, operand: OperandRef) {
    let assert_kind = AssertKind::OverflowNeg(take_back_operand_ref(operand));
    check_assert(cond, expected, assert_kind)
}
pub fn check_assert_div_by_zero(cond: OperandRef, expected: bool, operand: OperandRef) {
    let assert_kind = AssertKind::DivisionByZero(take_back_operand_ref(operand));
    check_assert(cond, expected, assert_kind)
}
pub fn check_assert_rem_by_zero(cond: OperandRef, expected: bool, operand: OperandRef) {
    let assert_kind = AssertKind::RemainderByZero(take_back_operand_ref(operand));
    check_assert(cond, expected, assert_kind)
}
fn check_assert(cond: OperandRef, expected: bool, assert_kind: AssertKind<OperandImpl>) {
    branch(|h| h.assert(take_back_operand_ref(cond), expected, assert_kind))
}

pub struct BranchingInfo {
    pub discriminant: OperandRef,
    metadata: BranchingMetadata,
}

impl BranchingInfo {
    pub fn new(
        node_location: BasicBlockIndex,
        discriminant: OperandRef,
        discr_bit_size: u64,
        discr_is_signed: bool,
    ) -> Self {
        Self {
            discriminant,
            metadata: BranchingMetadata {
                node_location,
                discr_as_int: IntType {
                    bit_size: discr_bit_size,
                    is_signed: discr_is_signed,
                },
            },
        }
    }
}

pub mod compiler_helpers {
    use super::*;

    pub static MODULE_MARKER: u8 = 0;

    /* These fields serve as exported symbols to get the types of the desired
     * arguments easier in the compiler. */
    pub static PLACE_REF_TYPE_HOLDER: PlaceRef = 0;
    pub static OPERAND_REF_TYPE_HOLDER: OperandRef = 0;
    pub static BINARY_OP_TYPE_HOLDER: BinaryOp = BinaryOp::Add;
    pub static UNARY_OP_TYPE_HOLDER: UnaryOp = UnaryOp::Neg;
    pub static RAW_PTR_TYPE_HOLDER: RawPointer = 0;
    pub static FUNC_ID_TYPE_HOLDER: FuncId = 0;

    pub const fn f32_to_bits(value: f32) -> u128 {
        value.to_bits() as u128
    }

    pub const fn f64_to_bits(value: f64) -> u128 {
        value.to_bits() as u128
    }

    pub const fn mark_as_nctfe() {}

    /* NOTE:
     * This is a workaround to prevent the compiler from removing the generic
     * functions from the exported symbols as they are unused.
     */

    #[used]
    #[cfg(place_addr)]
    static _SET_PLACE_ADDR_TYPED_REFERENCER: fn(PlaceRef, *const u32) -> () =
        set_place_address_typed::<u32>;

    #[used]
    #[cfg(place_addr)]
    static _TYPE_ID_OF_REFERENCER: fn() -> TypeId = type_id_of::<u32>;

    #[used]
    static _SIZE_OF_REFERENCER: fn() -> TypeSize = size_of::<u32>;

    #[cfg(place_addr)]
    pub fn type_id_of<T: ?Sized + 'static>() -> TypeId {
        /* NOTE: Once this function is const in stable build, we can mark this
         * function as constant as well. */
        /* NOTE: Do we need to bother about inlining?
         * Based on the last checks, LLVM is smart enough to inline this function
         * automatically and even replace everything with u128.
         * Also, giving this function the `inline` attribute will cause it to
         * not be exported. */
        crate::utils::type_id_of::<T>()
    }

    pub fn size_of<T>() -> TypeSize {
        core::mem::size_of::<T>() as TypeSize
    }
}
