use const_format::concatcp;

use rustc_hir::{def_id::DefId, definitions::DefPathData};
use rustc_middle::{
    mir::Body,
    ty::{InstanceKind, IntrinsicDef, TyCtxt},
};
use rustc_span::Symbol;

use common::{log_info, log_warn};

use crate::utils::mir::TyCtxtExt;

pub(super) const TAG_INSTR_DECISION: &str = concatcp!(super::TAG_INSTRUMENTATION, "::skip");

const TOOL_NAME: &str = crate::constants::TOOL_LEAF;
const ATTR_NAME: &str = "instrument";

pub(super) fn should_instrument<'tcx>(tcx: TyCtxt<'tcx>, body: &mut Body<'tcx>) -> bool {
    let def_id = body.source.def_id();

    if !decide_instance_kind(&body.source.instance) {
        return false;
    }

    if let Some((explicit, item)) = opt_instrument_attr_inheritable(tcx, def_id) {
        log_info!(
            target: TAG_INSTR_DECISION,
            "Found explicit instrumentation attribute for {:?} on {:?} with value: {}",
            def_id,
            item,
            explicit
        );
        return explicit;
    }

    // It is in the module defining lang_start items (std rt module)
    if tcx
        .lang_items()
        .start_fn()
        .map(|id| tcx.module_of(id).collect::<Vec<_>>())
        .zip(Some(tcx.module_of(def_id)))
        .is_some_and(|(start_mod, this_mod)| {
            let n = start_mod.len();
            start_mod.into_iter().eq(this_mod.take(n))
        })
    {
        return false;
    }

    // FIXME: Drops are important for bug detection, however we avoid instrumenting them for now.
    let mut drop_fn_ids = {
        use rustc_hir::LanguageItems as Items;
        [
            Items::drop_in_place_fn,
            Items::async_drop_in_place_fn,
            Items::surface_async_drop_in_place_fn,
            Items::async_drop_surface_drop_in_place_fn,
            Items::async_drop_slice_fn,
            Items::async_drop_chain_fn,
            Items::async_drop_noop_fn,
            Items::async_drop_deferred_drop_in_place_fn,
            Items::async_drop_fuse_fn,
            Items::async_drop_defer_fn,
            Items::async_drop_either_fn,
        ]
        .iter()
        .filter_map(|item| item(tcx.lang_items()))
    };
    if drop_fn_ids.any(|id| id == def_id)
        || tcx
            .lang_items()
            .drop_trait()
            .zip(
                tcx.impl_of_method(def_id)
                    .and_then(|id| tcx.trait_id_of_impl(id)),
            )
            .is_some_and(|(t1, t2)| t1 == t2)
    {
        return false;
    }

    // FIXME: To be replaced with a better-specified list.
    let def_path = &tcx.def_path_debug_str(def_id);
    if def_path.contains("panicking")
        || (def_path.contains("core_arch")
            || (def_path.starts_with("core") && def_path.contains("arch::")))
        || (def_path.starts_with("std")
            && (def_path.contains("thread")
                || def_path.contains("sync")
                || def_path.contains("arch::")))
    {
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
        Item(..) | FnPtrShim(..) | ClosureOnceShim { .. } => true,
        Intrinsic(..)
        | VTableShim(..)
        | ReifyShim(..)
        | Virtual(..)
        | ConstructCoroutineInClosureShim { .. }
        | CoroutineKindShim { .. }
        | ThreadLocalShim(..)
        | DropGlue(..)
        | CloneShim(..)
        | FnPtrAddrShim(..)
        | AsyncDropGlueCtorShim(..) => false,
    }
}

/// Returns the value of the `instrument` attribute if it is placed on the item or one of its ancestors.
fn opt_instrument_attr_inheritable<'tcx>(
    tcx: TyCtxt<'tcx>,
    def_id: DefId,
) -> Option<(bool, DefId)> {
    let mut current = def_id;
    loop {
        let attr = opt_instrument_attr(tcx, current);
        if attr.is_some() {
            return attr.map(|v| (v, current));
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
    .and_then(|attr| match &attr.kind {
        rustc_ast::AttrKind::Normal(attr) => Some(attr),
        _ => None,
    })
    .map(|attr| attr.item.args.inner_tokens())
    .map(|t| t.into_trees().next_ref().cloned())
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

mod intrinsics {
    use crate::pri_utils::sym::intrinsics::LeafIntrinsicSymbol;

    use super::*;

    pub(crate) enum IntrinsicDecision {
        PriFunc(LeafIntrinsicSymbol),
        NoOp,
        ConstEvaluated,
        ToDo,
        NotPlanned,
        Unsupported,
        Unexpected,
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
                float_to_int_unchecked,
                min_align_of,
                add_with_overflow,
                sub_with_overflow,
                mul_with_overflow,
                three_way_compare,
                size_of,
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
                ptr_guaranteed_cmp,
                pref_align_of,
                needs_drop,
                min_align_of_val,
                likely,
                forget,
                const_allocate,
                const_eval_select,
                const_deallocate,
                caller_location,
                assert_zero_valid,
                assert_mem_uninitialized_valid,
                assume,
            )
        };
    }

    macro_rules! of_noop_funcs {
        ($macro:ident) => {
            $macro!(
                unreachable,
                unlikely,
                rustc_peek,
                prefetch_write_instruction,
                prefetch_read_instruction,
                prefetch_write_data,
                prefetch_read_data,
                // const_deallocate,
                breakpoint,
                black_box,
                assert_inhabited,
            )
        };
    }

    macro_rules! of_float_arith_funcs {
        ($macro:ident) => {
            $macro!(
                truncf64,
                truncf128,
                truncf32,
                truncf16,
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
                roundevenf32,
                roundevenf128,
                roundevenf64,
                roundevenf16,
                rintf64,
                rintf128,
                rintf16,
                rintf32,
                powif64,
                powif128,
                powif16,
                powif32,
                powf32,
                powf64,
                powf128,
                powf16,
                nearbyintf128,
                nearbyintf32,
                nearbyintf64,
                minnumf64,
                nearbyintf16,
                minnumf32,
                minnumf128,
                minnumf16,
                maxnumf128,
                logf128,
                logf64,
                maxnumf16,
                maxnumf32,
                maxnumf64,
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
            )
        };
    }

    macro_rules! of_atomic_op_funcs {
        ($macro:ident) => {
            $macro!(
                atomic_xsub_seqcst,
                atomic_xsub_release,
                atomic_xsub_relaxed,
                atomic_xor_seqcst,
                atomic_xsub_acqrel,
                atomic_xsub_acquire,
                atomic_xor_relaxed,
                atomic_xor_acqrel,
                atomic_xor_release,
                atomic_xchg_seqcst,
                atomic_xor_acquire,
                atomic_xadd_seqcst,
                atomic_xchg_relaxed,
                atomic_xchg_acqrel,
                atomic_xchg_acquire,
                atomic_xchg_release,
                atomic_xadd_release,
                atomic_xadd_acqrel,
                atomic_umin_seqcst,
                atomic_xadd_relaxed,
                atomic_xadd_acquire,
                atomic_umin_acquire,
                atomic_umax_seqcst,
                atomic_umin_acqrel,
                atomic_umin_relaxed,
                atomic_umin_release,
                atomic_umax_relaxed,
                atomic_umax_release,
                atomic_umax_acqrel,
                atomic_umax_acquire,
                atomic_store_release,
                atomic_singlethreadfence_seqcst,
                atomic_store_relaxed,
                atomic_store_unordered,
                atomic_store_seqcst,
                atomic_or_release,
                atomic_singlethreadfence_acquire,
                atomic_singlethreadfence_acqrel,
                atomic_or_seqcst,
                atomic_singlethreadfence_release,
                atomic_or_relaxed,
                atomic_or_acqrel,
                atomic_nand_release,
                atomic_or_acquire,
                atomic_nand_seqcst,
                atomic_min_seqcst,
                atomic_nand_acquire,
                atomic_nand_acqrel,
                atomic_nand_relaxed,
                atomic_min_release,
                atomic_max_seqcst,
                atomic_max_release,
                atomic_min_relaxed,
                atomic_min_acqrel,
                atomic_min_acquire,
                atomic_max_acqrel,
                atomic_load_unordered,
                atomic_max_relaxed,
                atomic_load_seqcst,
                atomic_max_acquire,
                atomic_load_relaxed,
                atomic_fence_seqcst,
                atomic_fence_acquire,
                atomic_fence_release,
                atomic_load_acquire,
                atomic_cxchgweak_seqcst_acquire,
                atomic_cxchgweak_release_seqcst,
                atomic_fence_acqrel,
                atomic_cxchgweak_seqcst_seqcst,
                atomic_cxchgweak_seqcst_relaxed,
                atomic_cxchgweak_relaxed_seqcst,
                atomic_cxchgweak_relaxed_relaxed,
                atomic_cxchgweak_relaxed_acquire,
                atomic_cxchgweak_release_relaxed,
                atomic_cxchgweak_release_acquire,
                atomic_cxchgweak_acquire_acquire,
                atomic_cxchgweak_acquire_seqcst,
                atomic_cxchgweak_acqrel_seqcst,
                atomic_cxchgweak_acquire_relaxed,
                atomic_cxchgweak_acqrel_acquire,
                atomic_cxchg_seqcst_relaxed,
                atomic_cxchgweak_acqrel_relaxed,
                atomic_cxchg_seqcst_seqcst,
                atomic_cxchg_release_seqcst,
                atomic_cxchg_release_relaxed,
                atomic_cxchg_release_acquire,
                atomic_cxchg_seqcst_acquire,
                atomic_cxchg_relaxed_acquire,
                atomic_cxchg_acquire_seqcst,
                atomic_cxchg_relaxed_seqcst,
                atomic_cxchg_relaxed_relaxed,
                atomic_cxchg_acquire_acquire,
                atomic_cxchg_acquire_relaxed,
                atomic_cxchg_acqrel_relaxed,
                atomic_and_seqcst,
                atomic_cxchg_acqrel_seqcst,
                atomic_cxchg_acqrel_acquire,
                atomic_and_release,
                atomic_and_relaxed,
                atomic_and_acquire,
                atomic_and_acqrel,
            )
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
                simd_gather,
                simd_ge,
                simd_gt,
                simd_insert,
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
                simd_reduce_xor,
                simd_rem,
                simd_round,
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

    macro_rules! of_to_be_supported_funcs {
        ($macro:ident) => {
            $macro!(
                vtable_size,
                vtable_align,
                volatile_set_memory,
                volatile_copy_nonoverlapping_memory,
                volatile_load,
                volatile_store,
                volatile_copy_memory,
                unaligned_volatile_store,
                unaligned_volatile_load,
                typed_swap,
                select_unpredictable,
                raw_eq,
                ptr_mask,
                ptr_offset_from_unsigned,
                ptr_offset_from,
                nontemporal_store,
                cttz_nonzero,
                cttz,
                ctpop,
                ctlz_nonzero,
                ctlz,
                compare_bytes,
                bswap,
                catch_unwind,
                bitreverse,
                abort,
                drop_in_place,
                write_bytes,
                copy,
                copy_nonoverlapping,
                size_of_val,
                is_val_statically_known,
                arith_offset,
            )
        };
    }

    macro_rules! of_supported_funcs {
        ($macro:ident) => {
            $macro!(
                rotate_left,
                rotate_right,
                saturating_sub,
                saturating_add,
                exact_div,
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

        const TOTAL_COUNT: usize = count_all!(
            of_mir_translated_funcs,
            of_const_evaluated_funcs,
            of_noop_funcs,
            of_float_arith_funcs,
            of_atomic_op_funcs,
            of_simd_op_funcs,
            of_to_be_supported_funcs,
            of_supported_funcs,
        );

        /* NTOE: This is used as a test to make sure that the list do not contain duplicates.
         * Do not change the count unless some intrinsics are added or removed to Rust.
         */
        const _ALL_INTRINSICS: [u8; 354] = [0; TOTAL_COUNT];
    }

    pub(crate) fn decide_intrinsic_call<'tcx>(intrinsic: IntrinsicDef) -> IntrinsicDecision {
        use crate::pri_utils::sym::intrinsics as psym;
        use rustc_span::sym as rsym;

        macro_rules! any_of {
            ($($intrinsic:ident),*$(,)?) => {
                $(rsym::$intrinsic)|*
            };
        }

        macro_rules! str_any_of {
            ($($intrinsic:ident),*$(,)?) => {
                $(stringify!($intrinsic))|*
            };
        }

        match intrinsic.name {
            rsym::rotate_left => IntrinsicDecision::PriFunc(psym::intrinsic_assign_rotate_left),
            rsym::rotate_right => IntrinsicDecision::PriFunc(psym::intrinsic_assign_rotate_right),
            rsym::saturating_add => {
                IntrinsicDecision::PriFunc(psym::intrinsic_assign_saturating_add)
            }
            rsym::saturating_sub => {
                IntrinsicDecision::PriFunc(psym::intrinsic_assign_saturating_sub)
            }
            rsym::exact_div => IntrinsicDecision::PriFunc(psym::intrinsic_assign_exact_div),
            of_noop_funcs!(any_of) => IntrinsicDecision::NoOp,
            of_const_evaluated_funcs!(any_of) => IntrinsicDecision::ConstEvaluated,
            of_to_be_supported_funcs!(any_of) => IntrinsicDecision::ToDo,
            of_float_arith_funcs!(any_of) => IntrinsicDecision::NotPlanned,
            of_mir_translated_funcs!(any_of) => IntrinsicDecision::Unexpected,
            of_simd_op_funcs!(any_of) => IntrinsicDecision::Unsupported,
            other if matches!(other.as_str(), of_atomic_op_funcs!(str_any_of)) => {
                IntrinsicDecision::Unsupported
            }
            #[allow(unreachable_patterns)]
            of_supported_funcs!(any_of) => unreachable!(),
            _ => panic!("Uncovered intrinsic: {:?}", intrinsic),
        }
    }
}
pub(super) use intrinsics::{decide_intrinsic_call, IntrinsicDecision};
