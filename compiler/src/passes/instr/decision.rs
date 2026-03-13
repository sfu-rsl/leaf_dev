use const_format::concatcp;

use rustc_hir::{def_id::DefId, definitions::DefPathData};
use rustc_middle::{
    mir::Body,
    ty::{InstanceKind, IntrinsicDef, TyCtxt},
};
use rustc_span::Symbol;

use common::{log_debug, log_info, log_warn};

use crate::{config::WholeBodyFilter, passes::Storage, utils::mir::TyCtxtExt};

pub(super) const TAG_INSTR_DECISION: &str = concatcp!(super::TAG_INSTRUMENTATION, "::decision");

const TOOL_NAME: &str = crate::constants::TOOL_LEAF;
const ATTR_NAME: &str = "instrument";

pub(super) fn should_instrument<'tcx>(
    tcx: TyCtxt<'tcx>,
    body: &mut Body<'tcx>,
    storage: &mut dyn Storage,
) -> bool {
    let def_id = body.source.def_id();

    if !decide_instance_kind(&body.source.instance) {
        return false;
    }

    rules::bake_rules(storage, get_exceptional_exclusions);
    let rules = rules::get_baked_body_rules(storage);
    if let Some((decision, item)) =
        find_inheritable_first_filtered(tcx, def_id, move |tcx, def_id| {
            rules.accept(&(tcx, def_id))
        })
    {
        log_debug!(
            target: TAG_INSTR_DECISION,
            "Found a rule for instrumentation of {:?} on {:?} with decision: {}",
            def_id,
            item,
            decision
        );
        return decision;
    }

    if is_lang_start_item(tcx, def_id) {
        return false;
    }

    // To be removed once we ensure it is working correctly.
    if false && is_drop_fn(tcx, def_id) {
        return false;
    }

    // Some intrinsic functions have body.
    if tcx.intrinsic(def_id).is_some() {
        return false;
    }

    true
}

fn decide_instance_kind(kind: &InstanceKind) -> bool {
    use InstanceKind::*;
    match kind {
        Item(..)
        | FnPtrShim(..)
        | ClosureOnceShim { .. }
        | CloneShim(..)
        | ReifyShim(..)
        | DropGlue(_, Some(..)) => true,
        Intrinsic(..)
        | VTableShim(..)
        | Virtual(..)
        | ConstructCoroutineInClosureShim { .. }
        | ThreadLocalShim(..)
        | FutureDropPollShim(..)
        | FnPtrAddrShim(..)
        | AsyncDropGlue(..)
        | DropGlue(_, None)
        | AsyncDropGlueCtorShim(..) => false,
    }
}

/// Returns a set of filters to exclude some functions (mostly in the standard library)
/// that are currently problematic to instrument.
fn get_exceptional_exclusions() -> Vec<WholeBodyFilter> {
    use crate::config::{
        EntityLocationFilter,
        rules::{AllFormula, AnyFormula, LogicFormula::*, PatternMatch},
    };

    fn def_path_pattern(pattern: &str) -> EntityLocationFilter {
        EntityLocationFilter::DefPathMatch(PatternMatch::from(pattern.to_owned()))
    }

    fn crate_name(name: &str) -> EntityLocationFilter {
        EntityLocationFilter::Crate(crate::config::CrateFilter::Name(name.to_owned()))
    }

    vec![
        Atom(def_path_pattern(".*panicking.*")),
        Any(AnyFormula::from(vec![
            Atom(def_path_pattern(".*core_arch.*")),
            All(vec![Atom(crate_name("core")), Atom(def_path_pattern(".*arch.*"))].into()),
        ])),
        All(AllFormula::from(vec![
            Atom(crate_name("std")),
            Any(vec![
                Atom(def_path_pattern(".*thread.*")),
                Atom(def_path_pattern(".*sync.*")),
                Atom(def_path_pattern(".*arch.*")),
            ]
            .into()),
        ])),
    ]
    .into_iter()
    .map(Into::into)
    .collect()
}

fn find_inheritable_first_filtered<'tcx>(
    tcx: TyCtxt<'tcx>,
    def_id: DefId,
    rules: impl Fn(TyCtxt<'tcx>, DefId) -> Option<bool>,
) -> Option<(bool, DefId)> {
    let mut current = def_id;
    loop {
        // Attributes take precedence over filters.
        if let Some(explicit) = opt_instrument_attr(tcx, current) {
            log_info!(
                target: TAG_INSTR_DECISION,
                "Found explicit instrumentation attribute for {:?} on {:?} with value: {}",
                def_id,
                current,
                explicit
            );
            return Some((explicit, current));
        }

        if let Some(include) = rules(tcx, current) {
            return Some((include, current));
        }

        let parent = tcx.opt_parent(current);
        current = match parent {
            Some(parent) => parent,
            None => return None,
        };
    }
}

/// Returns the value of the `instrument` attribute if it is placed on the item.
/// If the attribute is not found, or the argument passed to the attribute is invalid
/// returns `None`.
fn opt_instrument_attr<'tcx>(tcx: TyCtxt<'tcx>, def_id: DefId) -> Option<bool> {
    use rustc_hir::{AttrArgs, Attribute};
    // Avoid possibly problematic const items.
    // See https://github.com/rust-lang/rust/issues/128145
    if matches!(
        tcx.def_key(def_id).disambiguated_data.data,
        DefPathData::AnonConst
    ) {
        return None;
    }

    tcx.get_attrs_by_path(
        def_id,
        &[Symbol::intern(TOOL_NAME), Symbol::intern(ATTR_NAME)],
    )
    .next()
    .and_then(|attr| match attr {
        Attribute::Unparsed(attr) => Some(attr),
        _ => None,
    })
    .and_then(|attr| match &attr.args {
        AttrArgs::Delimited(delim_args) => Some(delim_args.tokens.iter().next().cloned()),
        AttrArgs::Empty | AttrArgs::Eq { .. } => None,
    })
    .and_then(|token| {
        match token {
            // No argument means it's enabled.
            None => Some(true),
            Some(token) => {
                let as_bool = match &token {
                    rustc_ast::tokenstream::TokenTree::Token(token, ..) => token
                        .is_bool_lit()
                        .then(|| token.ident().unwrap().0.name == rustc_span::symbol::kw::True),
                    _ => None,
                };
                if as_bool.is_none() {
                    log_warn!(
                        "Invalid argument for attribute `{}`: {:?}",
                        ATTR_NAME,
                        token
                    );
                }
                as_bool
            }
        }
    })
}

fn is_lang_start_item(tcx: TyCtxt<'_>, def_id: DefId) -> bool {
    // It is in the module defining lang_start items (std rt module)
    tcx.lang_items()
        .start_fn()
        .map(|id| tcx.module_of(id).collect::<Vec<_>>())
        .zip(Some(tcx.module_of(def_id)))
        .is_some_and(|(start_mod, this_mod)| {
            let n = start_mod.len();
            start_mod.into_iter().eq(this_mod.take(n))
        })
}

fn is_drop_fn(tcx: TyCtxt<'_>, def_id: DefId) -> bool {
    let mut drop_fn_ids = {
        use rustc_hir::LanguageItems as Items;
        [Items::drop_in_place_fn, Items::async_drop_in_place_fn]
            .iter()
            .filter_map(|item| item(tcx.lang_items()))
    };
    drop_fn_ids.any(|id| id == def_id)
        || tcx
            .lang_items()
            .drop_trait()
            .zip(
                tcx.impl_of_assoc(def_id)
                    .and_then(|id| tcx.impl_opt_trait_id(id)),
            )
            .is_some_and(|(t1, t2)| t1 == t2)
}

mod intrinsics {
    use common::pri::AtomicBinaryOp;

    use super::super::pri_utils;

    use super::*;

    pub(crate) enum IntrinsicDecision {
        OneToOneAssign(pri_utils::sym::intrinsics::LeafIntrinsicSymbol),
        Atomic(AtomicIntrinsicKind),
        Memory {
            kind: MemoryIntrinsicKind,
            is_volatile: bool,
        },
        NoOp,
        ConstEvaluated,
        Contract,
        ToDo,
        NotPlanned,
        Unsupported,
        Unexpected,
    }

    pub(crate) enum AtomicIntrinsicKind {
        Load,
        Store,
        Exchange,
        CompareExchange { weak: bool },
        BinOp(AtomicBinaryOp),
        Fence { single_thread: bool },
    }

    pub(crate) enum MemoryIntrinsicKind {
        Load { is_ptr_aligned: bool },
        Store { is_ptr_aligned: bool },
        Copy { is_overlapping: bool },
        Set,
        Swap,
    }

    macro_rules! of_mir_translated_funcs {
        ($macro:ident) => {
            // These functions are expected to be translated to MIR elements and not appear at the
            // phase we perform the instrumentation.
            $macro!(
                transmute,
                transmute_unchecked,
                aggregate_raw_ptr,
                ptr_metadata,
                discriminant_value,
                offset,
                align_of,
                add_with_overflow,
                sub_with_overflow,
                mul_with_overflow,
                three_way_compare,
                size_of,
                slice_get_unchecked,
                unchecked_add,
                unchecked_div,
                unchecked_mul,
                unchecked_rem,
                unchecked_shl,
                unchecked_shr,
                unchecked_sub,
                wrapping_add,
                wrapping_mul,
                wrapping_sub,
                write_via_move,
                read_via_copy,
                ub_checks,
            )
        };
    }

    macro_rules! of_const_evaluated_funcs {
        ($macro:ident) => {
            // These functions are expected to be evaluated and not appear at the
            // phase we perform the instrumentation.
            $macro!(
                variant_count,
                type_name,
                type_id,
                type_id_eq,
                ptr_guaranteed_cmp,
                needs_drop,
                align_of_val,
                // FIXME: These two are probably not intrinsics anymore.
                likely,
                unlikely,
                forget,
                const_allocate,
                const_eval_select,
                const_make_global,
                const_deallocate,
                caller_location,
                assert_zero_valid,
                assert_mem_uninitialized_valid,
                assume,
            )
        };
    }

    macro_rules! of_contract_funcs {
        ($macro:ident) => {
            $macro!(
                contract_check_requires,
                contract_check_ensures,
                contract_checks
            )
        };
    }

    macro_rules! of_noop_funcs {
        ($macro:ident) => {
            $macro!(
                unreachable,
                rustc_peek,
                prefetch_write_instruction,
                prefetch_read_instruction,
                prefetch_write_data,
                prefetch_read_data,
                breakpoint,
                assert_inhabited,
                cold_path,
            )
        };
    }

    macro_rules! of_float_arith_funcs {
        ($macro:ident) => {
            $macro!(
                truncf16,
                truncf32,
                truncf64,
                truncf128,
                sqrtf128,
                sqrtf64,
                sqrtf16,
                sqrtf32,
                sinf128,
                sinf32,
                sinf16,
                sinf64,
                roundf32,
                roundf64,
                roundf128,
                roundf16,
                round_ties_even_f16,
                round_ties_even_f32,
                round_ties_even_f64,
                round_ties_even_f128,
                powif64,
                powif128,
                powif16,
                powif32,
                powf32,
                powf64,
                powf128,
                powf16,
                minimumf16,
                minimumf32,
                minimumf64,
                minimumf128,
                minnumf16,
                minnumf32,
                minnumf64,
                minnumf128,
                logf128,
                logf64,
                maximumf16,
                maximumf32,
                maximumf64,
                maximumf128,
                maxnumf16,
                maxnumf32,
                maxnumf64,
                maxnumf128,
                log10f128,
                log10f32,
                log10f16,
                logf16,
                log10f64,
                logf32,
                log2f128,
                log2f16,
                log2f32,
                log2f64,
                fsub_fast,
                frem_fast,
                frem_algebraic,
                fsub_algebraic,
                fmul_algebraic,
                fmul_fast,
                fmuladdf16,
                fmuladdf32,
                fmuladdf64,
                fmuladdf128,
                fmaf128,
                fmaf64,
                floorf64,
                floorf128,
                floorf32,
                fmaf32,
                fmaf16,
                floorf16,
                fdiv_fast,
                fadd_fast,
                fdiv_algebraic,
                fabsf32,
                fadd_algebraic,
                fabsf128,
                fabsf64,
                fabsf16,
                expf64,
                exp2f128,
                expf32,
                expf128,
                expf16,
                exp2f64,
                exp2f32,
                exp2f16,
                cosf128,
                cosf64,
                cosf32,
                copysignf32,
                copysignf64,
                copysignf128,
                cosf16,
                copysignf16,
                ceilf128,
                ceilf64,
                ceilf16,
                ceilf32,
                float_to_int_unchecked,
            )
        };
    }

    macro_rules! of_atomic_load_funcs {
        ($macro:ident) => {
            $macro!(atomic_load,)
        };
    }

    macro_rules! of_atomic_store_funcs {
        ($macro:ident) => {
            $macro!(atomic_store,)
        };
    }

    macro_rules! of_atomic_xchg_funcs {
        ($macro:ident) => {
            $macro!(atomic_xchg,)
        };
    }

    macro_rules! of_atomic_cxchg_funcs {
        ($macro:ident) => {
            $macro!(atomic_cxchg, atomic_cxchgweak,)
        };
    }

    macro_rules! of_atomic_binop_funcs {
        ($macro:ident) => {
            $macro!(
                atomic_and,
                atomic_max,
                atomic_min,
                atomic_nand,
                atomic_or,
                atomic_umax,
                atomic_umin,
                atomic_xadd,
                atomic_xor,
                atomic_xsub,
            )
        };
    }

    macro_rules! of_atomic_fence_funcs {
        ($macro:ident) => {
            $macro!(atomic_fence, atomic_singlethreadfence,)
        };
    }

    macro_rules! of_simd_op_funcs {
        ($macro:ident) => {
            $macro!(
                simd_add,
                simd_and,
                simd_arith_offset,
                simd_as,
                simd_bitmask,
                simd_bitreverse,
                simd_bswap,
                simd_cast,
                simd_cast_ptr,
                simd_ceil,
                simd_ctlz,
                simd_ctpop,
                simd_cttz,
                simd_div,
                simd_eq,
                simd_expose_provenance,
                simd_extract,
                simd_extract_dyn,
                simd_fabs,
                simd_fcos,
                simd_fexp,
                simd_fexp2,
                simd_flog,
                simd_flog2,
                simd_flog10,
                simd_floor,
                simd_fma,
                simd_fmax,
                simd_fmin,
                simd_fsin,
                simd_fsqrt,
                simd_funnel_shl,
                simd_funnel_shr,
                simd_gather,
                simd_ge,
                simd_gt,
                simd_insert,
                simd_insert_dyn,
                simd_le,
                simd_lt,
                simd_masked_load,
                simd_masked_store,
                simd_mul,
                simd_ne,
                simd_neg,
                simd_or,
                simd_reduce_add_ordered,
                simd_reduce_add_unordered,
                simd_reduce_all,
                simd_reduce_and,
                simd_reduce_any,
                simd_reduce_max,
                simd_reduce_min,
                simd_reduce_mul_ordered,
                simd_reduce_mul_unordered,
                simd_reduce_or,
                simd_reduce_xor,
                simd_relaxed_fma,
                simd_rem,
                simd_round,
                simd_round_ties_even,
                simd_saturating_add,
                simd_saturating_sub,
                simd_scatter,
                simd_select,
                simd_select_bitmask,
                simd_shl,
                simd_shr,
                simd_shuffle,
                simd_sub,
                simd_trunc,
                simd_with_exposed_provenance,
                simd_xor,
            )
        };
    }

    macro_rules! of_memory_funcs {
        ($macro:ident) => {
            $macro!(
                volatile_load,
                volatile_store,
                unaligned_volatile_load,
                unaligned_volatile_store,
                nontemporal_store,
                copy,
                copy_nonoverlapping,
                volatile_copy_nonoverlapping_memory,
                volatile_copy_memory,
                write_bytes,
                volatile_set_memory,
                typed_swap_nonoverlapping,
            )
        };
    }

    macro_rules! of_to_be_supported_funcs {
        ($macro:ident) => {
            $macro!(
                vtable_size,
                vtable_align,
                select_unpredictable,
                raw_eq,
                ptr_mask,
                ptr_offset_from_unsigned,
                ptr_offset_from,
                compare_bytes,
                catch_unwind,
                abort,
                size_of_val,
                is_val_statically_known,
                arith_offset,
                carrying_mul_add,
                autodiff,
                va_arg,
                va_copy,
                va_end,
            )
        };
    }

    macro_rules! of_one_to_one_funcs {
        ($macro:ident) => {
            $macro!(
                rotate_left,
                rotate_right,
                saturating_sub,
                saturating_add,
                disjoint_bitor,
                exact_div,
                bitreverse,
                cttz_nonzero,
                cttz,
                ctpop,
                ctlz_nonzero,
                ctlz,
                bswap,
                black_box,
            )
        };
    }

    // To make sure all intrinsics are covered.
    mod sanity_check {
        #![allow(unused)]

        macro_rules! str_array {
            ($($intrinsic:ident),*$(,)?) => {
                [$(stringify!($intrinsic)),*]
            };
        }

        macro_rules! count_all {
            ($($macro:ident),*$(,)?) => {
                0 $(+count($macro!(str_array)))*
            };
        }

        const fn count<const N: usize>(_: [&str; N]) -> usize {
            N
        }

        const LISTED_COUNT: usize = count_all!(
            of_mir_translated_funcs,
            of_const_evaluated_funcs,
            of_noop_funcs,
            of_contract_funcs,
            of_float_arith_funcs,
            of_atomic_load_funcs,
            of_atomic_store_funcs,
            of_atomic_xchg_funcs,
            of_atomic_binop_funcs,
            of_atomic_cxchg_funcs,
            of_atomic_fence_funcs,
            of_simd_op_funcs,
            of_to_be_supported_funcs,
            of_one_to_one_funcs,
            of_memory_funcs,
        );

        /* NTOE: This is used as a test to make sure that the list do not contain duplicates.
         * Do not change the count unless some intrinsics are added or removed to Rust.
         */
        const EXPECTED_COUNT: usize = 293;
        const _ALL_INTRINSICS: [(); EXPECTED_COUNT] = [(); LISTED_COUNT];
    }

    use pri_utils::sym::intrinsics as psym;
    use rustc_span::sym as rsym;

    pub(crate) fn decide_intrinsic_call<'tcx>(intrinsic: IntrinsicDef) -> IntrinsicDecision {
        macro_rules! any_of {
            ($($intrinsic:ident),*$(,)?) => {
                $(rsym::$intrinsic)|*
            };
        }

        match intrinsic.name {
            of_one_to_one_funcs!(any_of) => decide_one_to_one_intrinsic_call(intrinsic),
            of_noop_funcs!(any_of) => IntrinsicDecision::NoOp,
            of_contract_funcs!(any_of) => IntrinsicDecision::Contract,
            of_const_evaluated_funcs!(any_of) => IntrinsicDecision::ConstEvaluated,
            of_to_be_supported_funcs!(any_of) => IntrinsicDecision::ToDo,
            of_float_arith_funcs!(any_of) => IntrinsicDecision::NotPlanned,
            of_mir_translated_funcs!(any_of) => IntrinsicDecision::Unexpected,
            of_simd_op_funcs!(any_of) => IntrinsicDecision::Unsupported,
            other if other.as_str().starts_with("atomic") => {
                decide_atomic_intrinsic_call(intrinsic)
            }
            of_memory_funcs!(any_of) => decide_memory_intrinsic_call(intrinsic),
            _ => panic!("Uncovered intrinsic: {:?}", intrinsic),
        }
    }

    fn decide_one_to_one_intrinsic_call(intrinsic: IntrinsicDef) -> IntrinsicDecision {
        let pri_sym = match intrinsic.name {
            rsym::rotate_left => psym::intrinsic_assign_rotate_left,
            rsym::rotate_right => psym::intrinsic_assign_rotate_right,
            rsym::saturating_add => psym::intrinsic_assign_saturating_add,
            rsym::saturating_sub => psym::intrinsic_assign_saturating_sub,
            rsym::disjoint_bitor => psym::intrinsic_assign_disjoint_bitor,
            rsym::exact_div => psym::intrinsic_assign_exact_div,
            rsym::bitreverse => psym::intrinsic_assign_bitreverse,
            rsym::cttz_nonzero => psym::intrinsic_assign_cttz_nonzero,
            rsym::cttz => psym::intrinsic_assign_cttz,
            rsym::ctpop => psym::intrinsic_assign_ctpop,
            rsym::ctlz_nonzero => psym::intrinsic_assign_ctlz_nonzero,
            rsym::ctlz => psym::intrinsic_assign_ctlz,
            rsym::bswap => psym::intrinsic_assign_bswap,
            rsym::black_box => psym::intrinsic_assign_identity,
            _ => unreachable!(),
        };
        IntrinsicDecision::OneToOneAssign(pri_sym)
    }

    fn decide_memory_intrinsic_call(intrinsic: IntrinsicDef) -> IntrinsicDecision {
        let (kind, is_volatile) = match intrinsic.name {
            rsym::volatile_load => (
                MemoryIntrinsicKind::Load {
                    is_ptr_aligned: true,
                },
                true,
            ),
            rsym::unaligned_volatile_load => (
                MemoryIntrinsicKind::Load {
                    is_ptr_aligned: false,
                },
                true,
            ),
            rsym::volatile_store => (
                MemoryIntrinsicKind::Store {
                    is_ptr_aligned: true,
                },
                true,
            ),
            rsym::unaligned_volatile_store => (
                MemoryIntrinsicKind::Store {
                    is_ptr_aligned: false,
                },
                true,
            ),
            rsym::nontemporal_store => (
                MemoryIntrinsicKind::Store {
                    is_ptr_aligned: true,
                },
                false,
            ),
            rsym::copy => (
                MemoryIntrinsicKind::Copy {
                    is_overlapping: true,
                },
                false,
            ),
            rsym::copy_nonoverlapping => (
                MemoryIntrinsicKind::Copy {
                    is_overlapping: false,
                },
                false,
            ),
            rsym::volatile_copy_memory => (
                MemoryIntrinsicKind::Copy {
                    is_overlapping: true,
                },
                true,
            ),
            rsym::volatile_copy_nonoverlapping_memory => (
                MemoryIntrinsicKind::Copy {
                    is_overlapping: false,
                },
                true,
            ),
            rsym::write_bytes => (MemoryIntrinsicKind::Set, false),
            rsym::volatile_set_memory => (MemoryIntrinsicKind::Set, true),
            rsym::typed_swap_nonoverlapping => (MemoryIntrinsicKind::Swap, false),
            _ => unreachable!(),
        };
        IntrinsicDecision::Memory { kind, is_volatile }
    }

    fn decide_atomic_intrinsic_call<'tcx>(intrinsic: IntrinsicDef) -> IntrinsicDecision {
        macro_rules! str_any_of {
            ($($intrinsic:ident),*$(,)?) => {
                $(stringify!($intrinsic))|*
            };
        }

        let name = intrinsic.name.as_str();
        let parts = name.split('_').skip(1).collect::<Vec<_>>();
        let operation = parts[0];
        let kind = match name {
            of_atomic_load_funcs!(str_any_of) => AtomicIntrinsicKind::Load,
            of_atomic_store_funcs!(str_any_of) => AtomicIntrinsicKind::Store,
            of_atomic_xchg_funcs!(str_any_of) => AtomicIntrinsicKind::Exchange,
            of_atomic_cxchg_funcs!(str_any_of) => AtomicIntrinsicKind::CompareExchange {
                weak: operation.contains("weak"),
            },
            of_atomic_binop_funcs!(str_any_of) => {
                AtomicIntrinsicKind::BinOp(atomic_binop_from_str(&operation))
            }
            of_atomic_fence_funcs!(str_any_of) => AtomicIntrinsicKind::Fence {
                single_thread: operation.contains("singlethread"),
            },
            _ => unreachable!(),
        };
        IntrinsicDecision::Atomic(kind)
    }

    fn atomic_binop_from_str(binop: &str) -> AtomicBinaryOp {
        match binop {
            "and" => AtomicBinaryOp::AND,
            "max" => AtomicBinaryOp::MAX,
            "min" => AtomicBinaryOp::MIN,
            "nand" => AtomicBinaryOp::NAND,
            "or" => AtomicBinaryOp::OR,
            "umax" => AtomicBinaryOp::MAX,
            "umin" => AtomicBinaryOp::MIN,
            "xadd" => AtomicBinaryOp::ADD,
            "xor" => AtomicBinaryOp::XOR,
            "xsub" => AtomicBinaryOp::SUB,
            _ => unreachable!(),
        }
    }
}
pub(super) use intrinsics::{
    AtomicIntrinsicKind, IntrinsicDecision, MemoryIntrinsicKind, decide_intrinsic_call,
};

pub(super) mod rules {
    use std::ops::DerefMut;

    use delegate::delegate;

    use crate::{
        config::{
            AssignmentFilter, AssignmentKind, AssignmentLocationFilter, CallFlowFilter,
            CallFlowLocationFilter, CallFlowPartKind, ConstantType, ConstantTypeFilter,
            CrateFilter, DropFilter, DropLocationFilter, DropPartKind, EntityFilter,
            EntityLocationFilter, InstrumentationRules, MethodDynDefinitionFilter,
            OperandInfoFilter, OperandInfoLocationFilter, OperandKind, PlaceAddressFilter,
            PlaceInfoFilter, PlaceInfoStructureFilter, PlaceStructurePiece, PlaceTypeFilter,
            StorageLifetimeLocationFilter, StorageLifetimeMarker, StorageLifetimeMarkerFilter,
            WholeBodyFilter, rules::LogicFormula,
        },
        passes::StorageExt,
        utils::rules::{InclusionPredicate, Predicate, ToPredicate},
    };

    use super::*;

    pub(crate) const KEY_RULES: &str = "instr_rules";
    pub(crate) const KEY_BAKED_BODY_RULES: &str = "i_r_b_body";
    pub(crate) const KEY_BAKED_DYN_DEF_RULES: &str = "i_r_b_dyn_def";
    pub(crate) const KEY_BAKED_PLACE_INFO_RULES: &str = "i_r_b_place_info";
    pub(crate) const KEY_BAKED_OPERAND_INFO_RULES: &str = "i_r_b_operand_info";
    pub(crate) const KEY_BAKED_CONST_TYPE_RULES: &str = "i_r_b_const_ty";
    pub(crate) const KEY_BAKED_ASSIGNMENT_RULES: &str = "i_r_b_assignment";
    pub(crate) const KEY_BAKED_ASSIGNMENT_INFO_RULES: &str = "i_r_b_assignment_info";
    pub(crate) const KEY_BAKED_STORAGE_LIFETIME_RULES: &str = "i_r_b_storage_lifetime";
    pub(crate) const KEY_BAKED_CALL_FLOW_RULES: &str = "i_r_b_call_flow";
    pub(crate) const KEY_BAKED_DROP_RULES: &str = "i_r_b_drop";

    type LocationQuery<'tcx> = (TyCtxt<'tcx>, DefId);

    type EntityLocationFilterPredicate<'tcx> =
        <LogicFormula<EntityLocationFilter> as ToPredicate<LocationQuery<'tcx>>>::Predicate;

    type BakedEntityLocationFilterRules<'tcx> =
        InclusionPredicate<EntityLocationFilterPredicate<'tcx>>;

    type PlaceStructureQuery<'tcx> = (PlaceStructurePiece, LocationQuery<'tcx>);

    type BakedPlaceStructureFilterRules<'tcx> = InclusionPredicate<
        <PlaceInfoStructureFilter as ToPredicate<PlaceStructureQuery<'tcx>>>::Predicate,
    >;

    pub(crate) struct PlaceInfoRules<TS, TA = TS, TT = TA> {
        pub structure: TS,
        pub address: TA,
        pub ty: TT,
    }

    pub(crate) struct PlaceStructurePieceRules<T> {
        pub local: T,
        pub deref: T,
        pub field: T,
        pub index: T,
        pub constant_index: T,
        pub subslice: T,
        pub downcast: T,
        pub opaque_cast: T,
        pub unwrap_unsafe_binder: T,
    }

    impl<T> PlaceStructurePieceRules<T> {
        pub(crate) fn map<U>(self, f: impl Fn(T) -> U) -> PlaceStructurePieceRules<U> {
            PlaceStructurePieceRules {
                local: f(self.local),
                deref: f(self.deref),
                field: f(self.field),
                index: f(self.index),
                constant_index: f(self.constant_index),
                subslice: f(self.subslice),
                downcast: f(self.downcast),
                opaque_cast: f(self.opaque_cast),
                unwrap_unsafe_binder: f(self.unwrap_unsafe_binder),
            }
        }
    }

    type BakedPlaceInfoFilterRules<'tcx> =
        PlaceInfoRules<BakedPlaceStructureFilterRules<'tcx>, BakedEntityLocationFilterRules<'tcx>>;

    pub(crate) struct OperandInfoRules<T, TC = T> {
        pub copy: T,
        pub mov: T,
        pub constant: TC,
    }

    type OperandInfoQuery<'tcx> = (OperandKind, LocationQuery<'tcx>);

    type BakedOperandInfoFilterRules<'tcx> =
        InclusionPredicate<<OperandInfoFilter as ToPredicate<OperandInfoQuery<'tcx>>>::Predicate>;

    pub(crate) struct ConstantTypeRules<T> {
        pub bool: T,
        pub char: T,
        pub int: T,
        pub float: T,
        pub str: T,
        pub byte_str: T,
        pub ptr: T,
        pub zst: T,
    }

    impl<T> ConstantTypeRules<T> {
        pub(crate) fn map<U>(self, f: impl Fn(T) -> U) -> ConstantTypeRules<U> {
            ConstantTypeRules {
                bool: f(self.bool),
                char: f(self.char),
                int: f(self.int),
                float: f(self.float),
                str: f(self.str),
                byte_str: f(self.byte_str),
                ptr: f(self.ptr),
                zst: f(self.zst),
            }
        }
    }

    type ConstantTypeQuery<'tcx> = (ConstantType, LocationQuery<'tcx>);

    type BakedConstantTypeFilterRules<'tcx> =
        InclusionPredicate<<ConstantTypeFilter as ToPredicate<ConstantTypeQuery<'tcx>>>::Predicate>;

    pub(crate) struct AssignmentKindRules<T> {
        pub use_: T,
        pub repeat: T,
        pub ref_: T,
        pub thread_local_ref: T,
        pub raw_ptr: T,
        pub cast: T,
        pub binary_op: T,
        pub unary_op: T,
        pub discriminant: T,
        pub aggregate: T,
        pub shallow_init_box: T,
        pub wrap_unsafe_binder: T,
    }

    impl<T> AssignmentKindRules<T> {
        pub(crate) fn map<U>(self, f: impl Fn(T) -> U) -> AssignmentKindRules<U> {
            AssignmentKindRules {
                use_: f(self.use_),
                repeat: f(self.repeat),
                ref_: f(self.ref_),
                thread_local_ref: f(self.thread_local_ref),
                raw_ptr: f(self.raw_ptr),
                cast: f(self.cast),
                binary_op: f(self.binary_op),
                unary_op: f(self.unary_op),
                discriminant: f(self.discriminant),
                aggregate: f(self.aggregate),
                shallow_init_box: f(self.shallow_init_box),
                wrap_unsafe_binder: f(self.wrap_unsafe_binder),
            }
        }
    }

    type AssignmentQuery<'tcx> = (AssignmentKind, LocationQuery<'tcx>);

    type BakedAssignmentFilterRules<'tcx> =
        InclusionPredicate<<AssignmentFilter as ToPredicate<AssignmentQuery<'tcx>>>::Predicate>;

    type StorageLifetimeMarkerQuery<'tcx> = (StorageLifetimeMarker, LocationQuery<'tcx>);

    type BakedStorageLifetimeMarkerFilterRules<'tcx> = InclusionPredicate<
        <StorageLifetimeMarkerFilter as ToPredicate<StorageLifetimeMarkerQuery<'tcx>>>::Predicate,
    >;

    pub(crate) struct StorageLifetimeRules<T> {
        pub live: T,
        pub dead: T,
    }

    pub(crate) struct CallFlowRules<T> {
        pub call_control: T,
        pub call_input: T,
        pub func_data: T,
    }

    impl<T> CallFlowRules<T> {
        pub(crate) fn map<U>(self, f: impl Fn(T) -> U) -> CallFlowRules<U> {
            CallFlowRules {
                call_control: f(self.call_control),
                call_input: f(self.call_input),
                func_data: f(self.func_data),
            }
        }
    }

    type CallFlowQuery<'tcx> = (CallFlowPartKind, LocationQuery<'tcx>);

    type BakedCallFlowFilterRules<'tcx> =
        InclusionPredicate<<CallFlowFilter as ToPredicate<CallFlowQuery<'tcx>>>::Predicate>;

    pub(crate) struct DropRules<T> {
        pub control: T,
        pub input: T,
    }

    impl<T> DropRules<T> {
        pub(crate) fn map<U>(self, f: impl Fn(T) -> U) -> DropRules<U> {
            DropRules {
                control: f(self.control),
                input: f(self.input),
            }
        }
    }

    type DropQuery<'tcx> = (DropPartKind, LocationQuery<'tcx>);

    type BakedDropFilterRules<'tcx> =
        InclusionPredicate<<DropFilter as ToPredicate<DropQuery<'tcx>>>::Predicate>;

    pub(crate) fn get_baked_body_rules<'tcx>(
        storage: &mut dyn Storage,
    ) -> impl DerefMut<Target = BakedEntityLocationFilterRules<'tcx>> + '_ {
        storage
            .get_mut::<BakedEntityLocationFilterRules<'tcx>>(&KEY_BAKED_BODY_RULES.to_owned())
            .expect("Filter rules are expected to be baked at this point.")
    }

    pub(crate) fn accept_dyn_def_filter_rules<'tcx, I>(
        storage: &mut dyn Storage,
        item: &I,
    ) -> Option<bool>
    where
        EntityLocationFilterPredicate<'tcx>: Predicate<I>,
    {
        storage
            .get_mut::<BakedEntityLocationFilterRules<'tcx>>(&KEY_BAKED_DYN_DEF_RULES.to_owned())
            .expect("Filter rules are expected to be baked at this point.")
            .accept(item)
    }

    pub(crate) type PlaceInfoFilterResult =
        PlaceInfoRules<PlaceStructurePieceRules<Option<bool>>, Option<bool>>;

    pub(crate) fn accept_place_info_rules<'tcx>(
        storage: &mut dyn Storage,
        item: &LocationQuery<'tcx>,
    ) -> PlaceInfoFilterResult
    where
        BakedPlaceInfoFilterRules<'tcx>: core::any::Any,
        EntityLocationFilterPredicate<'tcx>: Predicate<LocationQuery<'tcx>>,
    {
        let rules = storage
            .get_mut::<BakedPlaceInfoFilterRules<'tcx>>(&KEY_BAKED_PLACE_INFO_RULES.to_owned())
            .expect("Filter rules are expected to be baked at this point.");
        use PlaceStructurePiece::*;
        PlaceInfoRules {
            structure: PlaceStructurePieceRules {
                local: rules.structure.accept(&(Local, *item)),
                deref: rules.structure.accept(&(Deref, *item)),
                field: rules.structure.accept(&(Field, *item)),
                index: rules.structure.accept(&(Index, *item)),
                constant_index: rules.structure.accept(&(ConstantIndex, *item)),
                subslice: rules.structure.accept(&(Subslice, *item)),
                downcast: rules.structure.accept(&(Downcast, *item)),
                opaque_cast: rules.structure.accept(&(OpaqueCast, *item)),
                unwrap_unsafe_binder: rules.structure.accept(&(UnwrapUnsafeBinder, *item)),
            },
            address: rules.address.accept(item),
            ty: rules.ty.accept(item),
        }
    }

    pub(crate) type OperandInfoFilterResult = OperandInfoRules<Option<bool>>;

    pub(crate) fn accept_operand_info_rules<'tcx>(
        storage: &mut dyn Storage,
        item: &LocationQuery<'tcx>,
    ) -> OperandInfoFilterResult
    where
        BakedOperandInfoFilterRules<'tcx>: core::any::Any,
        EntityLocationFilterPredicate<'tcx>: Predicate<LocationQuery<'tcx>>,
    {
        let rules = storage
            .get_mut::<BakedOperandInfoFilterRules<'tcx>>(&KEY_BAKED_OPERAND_INFO_RULES.to_owned())
            .expect("Filter rules are expected to be baked at this point.");

        use OperandKind::*;
        OperandInfoRules {
            copy: rules.accept(&(Copy, *item)),
            mov: rules.accept(&(Move, *item)),
            constant: rules.accept(&(Constant, *item)),
        }
    }

    pub(crate) type ConstantTypeFilterResult = ConstantTypeRules<Option<bool>>;

    pub(crate) fn accept_constant_type_rules<'tcx>(
        storage: &mut dyn Storage,
        item: &LocationQuery<'tcx>,
    ) -> ConstantTypeFilterResult
    where
        BakedConstantTypeFilterRules<'tcx>: core::any::Any,
        EntityLocationFilterPredicate<'tcx>: Predicate<LocationQuery<'tcx>>,
    {
        let rules = storage
            .get_mut::<BakedConstantTypeFilterRules<'tcx>>(&KEY_BAKED_CONST_TYPE_RULES.to_owned())
            .expect("Filter rules are expected to be baked at this point.");

        use ConstantType::*;
        ConstantTypeRules {
            bool: rules.accept(&(Bool, *item)),
            char: rules.accept(&(Char, *item)),
            int: rules.accept(&(Int, *item)),
            float: rules.accept(&(Float, *item)),
            str: rules.accept(&(Str, *item)),
            byte_str: rules.accept(&(ByteStr, *item)),
            ptr: rules.accept(&(Ptr, *item)),
            zst: rules.accept(&(Zst, *item)),
        }
    }

    pub(crate) type AssignmentFilterResult = AssignmentKindRules<Option<bool>>;

    pub(crate) fn accept_assignment_rules<'tcx>(
        storage: &mut dyn Storage,
        item: &LocationQuery<'tcx>,
        info: bool,
    ) -> AssignmentFilterResult
    where
        BakedAssignmentFilterRules<'tcx>: core::any::Any,
        EntityLocationFilterPredicate<'tcx>: Predicate<LocationQuery<'tcx>>,
    {
        let rules = storage
            .get_mut::<BakedAssignmentFilterRules<'tcx>>(&if !info {
                KEY_BAKED_ASSIGNMENT_RULES.to_owned()
            } else {
                KEY_BAKED_ASSIGNMENT_INFO_RULES.to_owned()
            })
            .expect("Filter rules are expected to be baked at this point.");

        use AssignmentKind::*;
        AssignmentKindRules {
            use_: rules.accept(&(Use, *item)),
            repeat: rules.accept(&(Repeat, *item)),
            ref_: rules.accept(&(Ref, *item)),
            thread_local_ref: rules.accept(&(ThreadLocalRef, *item)),
            raw_ptr: rules.accept(&(RawPtr, *item)),
            cast: rules.accept(&(Cast, *item)),
            binary_op: rules.accept(&(BinaryOp, *item)),
            unary_op: rules.accept(&(UnaryOp, *item)),
            discriminant: rules.accept(&(Discriminant, *item)),
            aggregate: rules.accept(&(Aggregate, *item)),
            shallow_init_box: rules.accept(&(ShallowInitBox, *item)),
            wrap_unsafe_binder: rules.accept(&(WrapUnsafeBinder, *item)),
        }
    }

    pub(crate) type StorageLifetimeFilterResult = StorageLifetimeRules<Option<bool>>;

    pub(crate) fn accept_storage_lifetime_rules<'tcx>(
        storage: &mut dyn Storage,
        item: &LocationQuery<'tcx>,
    ) -> StorageLifetimeFilterResult
    where
        BakedStorageLifetimeMarkerFilterRules<'tcx>: 'static,
        EntityLocationFilterPredicate<'tcx>: Predicate<LocationQuery<'tcx>>,
    {
        let rules = storage
            .get_mut::<BakedStorageLifetimeMarkerFilterRules<'tcx>>(
                &KEY_BAKED_STORAGE_LIFETIME_RULES.to_owned(),
            )
            .expect("Filter rules are expected to be baked at this point.");
        use StorageLifetimeMarker::*;
        StorageLifetimeRules {
            live: rules.accept(&(Live, *item)),
            dead: rules.accept(&(Dead, *item)),
        }
    }

    pub(crate) type CallFlowFilterResult = CallFlowRules<Option<bool>>;

    pub(crate) fn accept_call_flow_rules<'tcx>(
        storage: &mut dyn Storage,
        item: &LocationQuery<'tcx>,
    ) -> CallFlowFilterResult
    where
        BakedCallFlowFilterRules<'tcx>: 'static,
        EntityLocationFilterPredicate<'tcx>: Predicate<LocationQuery<'tcx>>,
    {
        let rules = storage
            .get_mut::<BakedCallFlowFilterRules<'tcx>>(&KEY_BAKED_CALL_FLOW_RULES.to_owned())
            .expect("Filter rules are expected to be baked at this point.");
        use CallFlowPartKind::*;
        CallFlowRules {
            call_control: rules.accept(&(CallControl, *item)),
            call_input: rules.accept(&(CallInput, *item)),
            func_data: rules.accept(&(FunctionData, *item)),
        }
    }

    pub(crate) type DropFilterResult = DropRules<Option<bool>>;

    pub(crate) fn accept_drop_rules<'tcx>(
        storage: &mut dyn Storage,
        item: &LocationQuery<'tcx>,
    ) -> DropFilterResult
    where
        BakedDropFilterRules<'tcx>: 'static,
        EntityLocationFilterPredicate<'tcx>: Predicate<LocationQuery<'tcx>>,
    {
        let rules = storage
            .get_mut::<BakedDropFilterRules<'tcx>>(&KEY_BAKED_DROP_RULES.to_owned())
            .expect("Filter rules are expected to be baked at this point.");
        use DropPartKind::*;
        DropRules {
            control: rules.accept(&(CallControl, *item)),
            input: rules.accept(&(CallInput, *item)),
        }
    }

    pub(super) fn bake_rules(
        storage: &mut dyn Storage,
        additional_exclusions: impl FnOnce() -> Vec<WholeBodyFilter>,
    ) {
        // We use explicit types to ensure not using the wrong type by mistake.
        let _ = storage.get_or_insert_with_acc(
            KEY_BAKED_DYN_DEF_RULES.to_owned(),
            |storage| -> BakedEntityLocationFilterRules<'_> {
                let rules = storage.get_or_default::<InstrumentationRules>(KEY_RULES.to_owned());
                rules
                    .clone()
                    .filter_map(|r| match r {
                        EntityFilter::MethodDynDefinition(filter) => Some(filter),
                        _ => None,
                    })
                    .to_baked()
            },
        );
        let _ = storage.get_or_insert_with_acc(
            KEY_BAKED_BODY_RULES.to_owned(),
            |storage| -> BakedEntityLocationFilterRules<'_> {
                let rules = storage.get_or_default::<InstrumentationRules>(KEY_RULES.to_owned());
                let mut rules = rules.clone().filter_map(|r| match r {
                    EntityFilter::WholeBody(filter) => Some(filter),
                    _ => None,
                });
                rules.exclude.extend(additional_exclusions());
                rules.to_baked()
            },
        );
        let _ = storage.get_or_insert_with_acc(
            KEY_BAKED_PLACE_INFO_RULES.to_owned(),
            |storage| -> BakedPlaceInfoFilterRules<'_> {
                let rules = storage.get_or_default::<InstrumentationRules>(KEY_RULES.to_owned());
                let rules = rules.clone().filter_map(|r| match r {
                    EntityFilter::PlaceInfo(filter) => Some(filter),
                    _ => None,
                });
                BakedPlaceInfoFilterRules {
                    structure: rules
                        .clone()
                        .filter_map(|r| match r {
                            PlaceInfoFilter::Structure(filter) => Some(filter),
                            _ => None,
                        })
                        .to_baked(),
                    address: rules
                        .clone()
                        .filter_map(|r| match r {
                            PlaceInfoFilter::Address(filter) => Some(filter),
                            _ => None,
                        })
                        .to_baked(),
                    ty: rules
                        .clone()
                        .filter_map(|r| match r {
                            PlaceInfoFilter::Type(filter) => Some(filter),
                            _ => None,
                        })
                        .to_baked(),
                }
            },
        );
        let _ = storage.get_or_insert_with_acc(
            KEY_BAKED_OPERAND_INFO_RULES.to_owned(),
            |storage| -> BakedOperandInfoFilterRules<'_> {
                let rules = storage.get_or_default::<InstrumentationRules>(KEY_RULES.to_owned());
                let rules = rules.clone().filter_map(|r| match r {
                    EntityFilter::OperandInfo(filter) => Some(filter),
                    _ => None,
                });
                rules.to_baked()
            },
        );
        let _ = storage.get_or_insert_with_acc(
            KEY_BAKED_CONST_TYPE_RULES.to_owned(),
            |storage| -> BakedConstantTypeFilterRules<'_> {
                let rules = storage.get_or_default::<InstrumentationRules>(KEY_RULES.to_owned());
                let rules = rules.clone().filter_map(|r| match r {
                    EntityFilter::ConstantType(filter) => Some(filter),
                    _ => None,
                });
                rules.to_baked()
            },
        );
        let _ = storage.get_or_insert_with_acc(
            KEY_BAKED_ASSIGNMENT_RULES.to_owned(),
            |storage| -> BakedAssignmentFilterRules<'_> {
                let rules = storage.get_or_default::<InstrumentationRules>(KEY_RULES.to_owned());
                let rules = rules.clone().filter_map(|r| match r {
                    EntityFilter::Assignment(filter) => Some(filter),
                    _ => None,
                });
                rules.to_baked()
            },
        );
        let _ = storage.get_or_insert_with_acc(
            KEY_BAKED_ASSIGNMENT_INFO_RULES.to_owned(),
            |storage| -> BakedAssignmentFilterRules<'_> {
                let rules = storage.get_or_default::<InstrumentationRules>(KEY_RULES.to_owned());
                let rules = rules.clone().filter_map(|r| match r {
                    EntityFilter::AssignmentInfo(filter) => Some(filter),
                    _ => None,
                });
                rules.to_baked()
            },
        );
        let _ = storage.get_or_insert_with_acc(
            KEY_BAKED_STORAGE_LIFETIME_RULES.to_owned(),
            |storage| -> BakedStorageLifetimeMarkerFilterRules<'_> {
                let rules = storage.get_or_default::<InstrumentationRules>(KEY_RULES.to_owned());
                let rules = rules.clone().filter_map(|r| match r {
                    EntityFilter::StorageLifetime(filter) => Some(filter),
                    _ => None,
                });
                rules.to_baked()
            },
        );
        let _ = storage.get_or_insert_with_acc(
            KEY_BAKED_CALL_FLOW_RULES.to_owned(),
            |storage| -> BakedCallFlowFilterRules<'_> {
                let rules = storage.get_or_default::<InstrumentationRules>(KEY_RULES.to_owned());
                let rules = rules.clone().filter_map(|r| match r {
                    EntityFilter::CallFlow(filter) => Some(filter),
                    _ => None,
                });
                rules.to_baked()
            },
        );
        let _ = storage.get_or_insert_with_acc(
            KEY_BAKED_DROP_RULES.to_owned(),
            |storage| -> BakedDropFilterRules<'_> {
                let rules = storage.get_or_default::<InstrumentationRules>(KEY_RULES.to_owned());
                let rules = rules.clone().filter_map(|r| match r {
                    EntityFilter::Drop(filter) => Some(filter),
                    _ => None,
                });
                rules.to_baked()
            },
        );
    }

    macro_rules! impl_to_predicate_for_newtype {
        ($name:ty, $query:ty, $filter:ty) => {
            impl<'tcx> ToPredicate<$query> for $name {
                type Predicate = <$filter as ToPredicate<$query>>::Predicate;

                delegate! {
                    to self.0 {
                        fn to_predicate(&self) -> Self::Predicate;
                    }
                }
            }
        };

        (loc: $($name:ty),+$(,)?) => {
            $(
                 impl_to_predicate_for_newtype!($name, LocationQuery<'tcx>, LogicFormula<EntityLocationFilter>);
            )*
        };
    }

    impl_to_predicate_for_newtype!(loc:
        WholeBodyFilter,
        MethodDynDefinitionFilter,
        PlaceAddressFilter,
        PlaceTypeFilter,
        OperandInfoLocationFilter,
        AssignmentLocationFilter,
        StorageLifetimeLocationFilter,
        CallFlowLocationFilter,
        DropLocationFilter,
    );

    macro_rules! impl_to_predicate_by_eq {
        ($($name:ty),+$(,)?) => {
            $(
                 impl ToPredicate<$name> for $name {
                     type Predicate = Box<dyn Fn(&Self) -> bool>;

                     fn to_predicate(&self) -> Self::Predicate {
                         let this = *self;
                         Box::new(move |other| this.eq(other))
                     }
                 }
            )*
        };
    }

    impl_to_predicate_by_eq!(
        PlaceStructurePiece,
        OperandKind,
        ConstantType,
        AssignmentKind,
        StorageLifetimeMarker,
        CallFlowPartKind,
        DropPartKind,
    );

    impl ToPredicate<PlaceStructureQuery<'_>> for PlaceInfoStructureFilter {
        type Predicate = Box<dyn Fn(&PlaceStructureQuery<'_>) -> bool>;

        fn to_predicate(&self) -> Self::Predicate {
            let piece = self.piece.to_predicate();
            let loc = self.loc.to_predicate();
            Box::new(move |(q_piece, q_loc)| piece.accept(q_piece) && loc.accept(q_loc))
        }
    }

    impl ToPredicate<LocationQuery<'_>> for EntityLocationFilter {
        type Predicate = Box<dyn Fn(&LocationQuery<'_>) -> bool>;

        fn to_predicate(&self) -> Self::Predicate {
            match self {
                EntityLocationFilter::Crate(crate_filter) => match crate_filter.clone() {
                    CrateFilter::Externality(is_external) => {
                        Box::new(move |(_, def_id)| def_id.is_local() != is_external)
                    }
                    CrateFilter::Name(name) => {
                        Box::new(move |(tcx, def_id)| tcx.crate_name(def_id.krate).as_str() == name)
                    }
                },
                EntityLocationFilter::DefPathMatch(pattern) => {
                    let pred = pattern.to_predicate();
                    Box::new(move |(tcx, def_id)| {
                        let def_path = tcx.def_path_str(def_id);
                        pred.accept(&def_path)
                    })
                }
            }
        }
    }

    impl ToPredicate<OperandInfoQuery<'_>> for OperandInfoFilter {
        type Predicate = Box<dyn Fn(&OperandInfoQuery<'_>) -> bool>;

        fn to_predicate(&self) -> Self::Predicate {
            let kind = self.kind.to_predicate();
            let loc = self.loc.to_predicate();
            Box::new(move |(q_kind, q_loc)| kind.accept(q_kind) && loc.accept(q_loc))
        }
    }

    impl ToPredicate<ConstantTypeQuery<'_>> for ConstantTypeFilter {
        type Predicate = Box<dyn Fn(&ConstantTypeQuery<'_>) -> bool>;

        fn to_predicate(&self) -> Self::Predicate {
            let ty = self.ty.to_predicate();
            let loc = self.loc.to_predicate();
            Box::new(move |(q_ty, q_loc)| ty.accept(q_ty) && loc.accept(q_loc))
        }
    }

    impl ToPredicate<AssignmentQuery<'_>> for AssignmentFilter {
        type Predicate = Box<dyn Fn(&AssignmentQuery<'_>) -> bool>;

        fn to_predicate(&self) -> Self::Predicate {
            let kind = self.kind.to_predicate();
            let loc = self.loc.to_predicate();
            Box::new(move |(q_kind, q_loc)| kind.accept(q_kind) && loc.accept(q_loc))
        }
    }

    impl ToPredicate<StorageLifetimeMarkerQuery<'_>> for StorageLifetimeMarkerFilter {
        type Predicate = Box<dyn Fn(&StorageLifetimeMarkerQuery<'_>) -> bool>;

        fn to_predicate(&self) -> Self::Predicate {
            let kind = self.kind.to_predicate();
            let loc = self.loc.to_predicate();
            Box::new(move |(q_kind, q_loc)| kind.accept(q_kind) && loc.accept(q_loc))
        }
    }

    impl ToPredicate<CallFlowQuery<'_>> for CallFlowFilter {
        type Predicate = Box<dyn Fn(&CallFlowQuery<'_>) -> bool>;

        fn to_predicate(&self) -> Self::Predicate {
            let kind = self.kind.to_predicate();
            let loc = self.loc.to_predicate();
            Box::new(move |(q_kind, q_loc)| kind.accept(q_kind) && loc.accept(q_loc))
        }
    }

    impl ToPredicate<DropQuery<'_>> for DropFilter {
        type Predicate = Box<dyn Fn(&DropQuery<'_>) -> bool>;

        fn to_predicate(&self) -> Self::Predicate {
            let kind = self.kind.to_predicate();
            let loc = self.loc.to_predicate();
            Box::new(move |(q_kind, q_loc)| kind.accept(q_kind) && loc.accept(q_loc))
        }
    }
}
