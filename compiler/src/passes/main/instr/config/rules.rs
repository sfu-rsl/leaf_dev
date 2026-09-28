use core::ops::Deref;

use delegate::delegate;
use paste::paste;

use rustc_hir::def_id::DefId;
use rustc_middle::ty::TyCtxt;

use crate::{
    config::rules::{InclusionRules, LogicFormula},
    passes::{Storage, StorageExt},
    utils::rules::{InclusionPredicate, Predicate, ToPredicate},
};

use super::super::config::*;

pub(crate) const KEY_RULES: &str = "instr_rules";
pub(crate) const KEY_BAKED_POLICY: &str = "instr_baked_policy";

type LocationQuery<'tcx> = (TyCtxt<'tcx>, DefId);

type EntityLocationFilterPredicate<'tcx> =
    <LogicFormula<EntityLocationFilter> as ToPredicate<LocationQuery<'tcx>>>::Predicate;

type BakedEntityLocationFilterRules<'tcx> = InclusionPredicate<EntityLocationFilterPredicate<'tcx>>;

type BakedWholeBodyFilterRules<'tcx> = BakedEntityLocationFilterRules<'tcx>;

type BakedMethodDynDefinitionFilterRules<'tcx> = BakedEntityLocationFilterRules<'tcx>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BodyDecision {
    Instrument,
    Skip,
}

impl BodyDecision {
    pub(crate) fn from_rule(rule: Option<bool>) -> Option<Self> {
        rule.map(|include| {
            if include {
                Self::Instrument
            } else {
                Self::Skip
            }
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum EventDecision {
    Omit,
    Opaque,
    Detailed,
}

impl EventDecision {
    pub(crate) fn from_rules(event: Option<bool>, detail: Option<bool>) -> Self {
        if !event.unwrap_or(true) {
            Self::Omit
        } else if detail.unwrap_or(true) {
            Self::Detailed
        } else {
            Self::Opaque
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum DetailDecision {
    Disabled,
    Enabled,
}

impl DetailDecision {
    pub(crate) fn from_rule(rule: Option<bool>, default: bool) -> Self {
        if rule.unwrap_or(default) {
            Self::Enabled
        } else {
            Self::Disabled
        }
    }

    pub(crate) fn is_enabled(self) -> bool {
        matches!(self, Self::Enabled)
    }
}

macro_rules! query_and_baked_aliases {
    ($stem:ident, $part:ty) => {
        paste! {
            type [<$stem Query>]<'tcx> = ($part, LocationQuery<'tcx>);
            type [<Baked $stem FilterRules>]<'tcx> =
                InclusionPredicate<<[<$stem Filter>] as ToPredicate<[<$stem Query>]<'tcx>>>::Predicate>;
        }
    };
}

macro_rules! rules_struct_from_enum {
    (
        $(#[$meta:meta])*
        $name:ident match $enum:path {
            $(
                $variant:ident => $field:ident
            ),+ $(,)?
        }
    ) => {
        $(#[$meta])*
        pub(crate) struct $name<T> {
            $(
                pub $field: T,
            )+
        }

        impl<T> $name<T> {
            #[allow(dead_code)]
            pub(crate) fn accept(mut f: impl FnMut($enum) -> T) -> Self {
                Self {
                    $(
                        $field: f(<$enum>::$variant),
                    )+
                }
            }

            #[allow(unused)]
            pub(crate) fn map<U>(self, f: impl Fn(T) -> U) -> $name<U> {
                $name {
                    $($field: f(self.$field),)+
                }
            }
        }

        // Ensuring exhaustiveness
        const _: fn($enum) = |value| match value {
            $(
                <$enum>::$variant => (),
            )+
        };
    };
}

/// Defines a filter-rule group from an enum.
///
/// For a given `$stem` and enum mapping, this macro generates:
/// - `{$stem}Rules<T>`: a struct with one field per mapping entry.
/// - `{$stem}Query<'tcx>`: `(EnumVariantType, LocationQuery<'tcx>)`.
/// - `Baked{$stem}FilterRules<'tcx>`: baked inclusion predicate rules.
///
/// Example:
///
/// ```rust,ignore
/// define_filter_rule_group!(
///     Switch match SwitchPartKind {
///         Control => control,
///         Data => data,
///     }
/// );
///
/// // Generated (conceptually):
/// // pub(crate) struct SwitchRules<T> { pub control: T, pub data: T }
/// // type SwitchQuery<'tcx> = (SwitchPartKind, LocationQuery<'tcx>);
/// // type BakedSwitchFilterRules<'tcx> = InclusionPredicate<<SwitchFilter as ToPredicate<SwitchQuery<'tcx>>>::Predicate>;
/// ```
macro_rules! define_filter_rule_group {
    (
        $(#[$meta:meta])*
        $stem:ident match $enum:path {
            $(
                $variant:ident => $field:ident
            ),+ $(,)?
        }
    ) => {
        paste! {
            rules_struct_from_enum!(
                $(#[$meta])*
                [<$stem Rules>] match $enum {
                    $(
                        $variant => $field
                    ),+
                }
            );
        }
        query_and_baked_aliases!($stem, $enum);
    };
}

pub(crate) struct PlaceInfoRules<TS, TA = TS, TT = TA> {
    pub structure: TS,
    pub address: TA,
    pub ty: TT,
}

define_filter_rule_group!(
    PlaceStructure match PlaceStructurePiece {
        Local => local,
        Deref => deref,
        Field => field,
        Index => index,
        ConstantIndex => constant_index,
        Subslice => subslice,
        Downcast => downcast,
        OpaqueCast => opaque_cast,
        UnwrapUnsafeBinder => unwrap_unsafe_binder,
    }
);

pub(crate) struct OperandKindRules<T, TC = T> {
    pub copy: T,
    pub mov: T,
    pub constant: TC,
}

query_and_baked_aliases!(OperandKind, OperandKind);

define_filter_rule_group!(
    ConstantType match ConstantType {
        Bool => bool,
        Char => char,
        Int => int,
        Float => float,
        Str => str,
        ByteStr => byte_str,
        Ptr => ptr,
        Zst => zst,
    }
);

define_filter_rule_group!(
    Assignment match AssignmentKind {
        Use => use_,
        Repeat => repeat,
        Ref => ref_,
        ThreadLocalRef => thread_local_ref,
        RawPtr => raw_ptr,
        Cast => cast,
        BinaryOp => binary_op,
        UnaryOp => unary_op,
        Discriminant => discriminant,
        Aggregate => aggregate,
        WrapUnsafeBinder => wrap_unsafe_binder,
        IntrinsicUnaryOp => intrinsic_unary_op,
        IntrinsicBinaryOp => intrinsic_binary_op,
        IntrinsicTernaryOp => intrinsic_ternary_op,
        IntrinsicMiscOp => intrinsic_misc_op,
        IntrinsicMemoryOp => intrinsic_memory_op,
        AtomicBinaryOp => atomic_binary_op,
        AtomicMemoryOp => atomic_memory_op,
    }
);
type BakedAssignmentInfoFilterRules<'tcx> = BakedAssignmentFilterRules<'tcx>;

define_filter_rule_group!(
    StorageLifetimeMarker match StorageLifetimeMarkerKind {
        Live => live,
        Dead => dead,
    }
);

define_filter_rule_group!(
    CallFlow match CallFlowPartKind {
        CallControl => call_control,
        CallInput => call_input,
        CallAddress => call_address,
        FunctionData => func_data,
        FunctionAddress => func_address,
    }
);

define_filter_rule_group!(
    Drop match DropPartKind {
        CallControl => control,
        CallInput => input,
        CallAddress => call_address,
    }
);

define_filter_rule_group!(
    Switch match SwitchPartKind {
        Control => control,
        Data => data,
    }
);

pub(crate) struct BakedInstrumentationPolicy<'tcx>
where
    Self: 'static,
{
    body: BakedWholeBodyFilterRules<'tcx>,
    dyn_def: BakedMethodDynDefinitionFilterRules<'tcx>,
    place_info:
        PlaceInfoRules<BakedPlaceStructureFilterRules<'tcx>, BakedEntityLocationFilterRules<'tcx>>,
    operand_info: BakedOperandKindFilterRules<'tcx>,
    constant_type: BakedConstantTypeFilterRules<'tcx>,
    assignment: BakedAssignmentFilterRules<'tcx>,
    assignment_info: BakedAssignmentInfoFilterRules<'tcx>,
    storage_lifetime: BakedStorageLifetimeMarkerFilterRules<'tcx>,
    call_flow: BakedCallFlowFilterRules<'tcx>,
    drop: BakedDropFilterRules<'tcx>,
    switch: BakedSwitchFilterRules<'tcx>,
}

impl BakedInstrumentationPolicy<'_> {
    fn body<'tcx>(&self) -> &BakedWholeBodyFilterRules<'tcx> {
        &self.body
    }

    fn dyn_def<'tcx>(&self) -> &BakedMethodDynDefinitionFilterRules<'tcx> {
        &self.dyn_def
    }

    fn place_structure<'tcx>(&self) -> &BakedPlaceStructureFilterRules<'tcx> {
        &self.place_info.structure
    }

    fn place_address<'tcx>(&self) -> &BakedEntityLocationFilterRules<'tcx> {
        &self.place_info.address
    }

    fn place_type<'tcx>(&self) -> &BakedEntityLocationFilterRules<'tcx> {
        &self.place_info.ty
    }

    fn operand_info<'tcx>(&self) -> &BakedOperandKindFilterRules<'tcx> {
        &self.operand_info
    }

    fn constant_type<'tcx>(&self) -> &BakedConstantTypeFilterRules<'tcx> {
        &self.constant_type
    }

    fn assignment<'tcx>(&self) -> &BakedAssignmentFilterRules<'tcx> {
        &self.assignment
    }

    fn assignment_info<'tcx>(&self) -> &BakedAssignmentInfoFilterRules<'tcx> {
        &self.assignment_info
    }

    fn storage_lifetime<'tcx>(&self) -> &BakedStorageLifetimeMarkerFilterRules<'tcx> {
        &self.storage_lifetime
    }

    fn call_flow<'tcx>(&self) -> &BakedCallFlowFilterRules<'tcx> {
        &self.call_flow
    }

    fn drop<'tcx>(&self) -> &BakedDropFilterRules<'tcx> {
        &self.drop
    }

    fn switch<'tcx>(&self) -> &BakedSwitchFilterRules<'tcx> {
        &self.switch
    }

    pub(crate) fn body_decision<'tcx>(&self, item: &LocationQuery<'tcx>) -> Option<BodyDecision> {
        BodyDecision::from_rule(self.body().accept(item))
    }

    pub(crate) fn dynamic_definition_decision<'tcx>(
        &self,
        item: &LocationQuery<'tcx>,
    ) -> Option<BodyDecision> {
        BodyDecision::from_rule(self.dyn_def().accept(item))
    }

    pub(crate) fn place_info_decisions<'tcx>(
        &self,
        item: &LocationQuery<'tcx>,
    ) -> PlaceInfoRules<PlaceStructureRules<DetailDecision>, DetailDecision> {
        PlaceInfoRules {
            structure: PlaceStructureRules::accept(|piece| {
                DetailDecision::from_rule(self.place_structure().accept(&(piece, *item)), true)
            }),
            address: DetailDecision::from_rule(self.place_address().accept(item), true),
            ty: DetailDecision::from_rule(self.place_type().accept(item), true),
        }
    }

    pub(crate) fn operand_info_decisions<'tcx>(
        &self,
        item: &LocationQuery<'tcx>,
    ) -> OperandKindRules<DetailDecision> {
        use OperandKind::*;
        OperandKindRules {
            copy: DetailDecision::from_rule(self.operand_info().accept(&(Copy, *item)), true),
            mov: DetailDecision::from_rule(self.operand_info().accept(&(Move, *item)), true),
            constant: DetailDecision::from_rule(
                self.operand_info().accept(&(Constant, *item)),
                true,
            ),
        }
    }

    pub(crate) fn constant_type_decisions<'tcx>(
        &self,
        item: &LocationQuery<'tcx>,
    ) -> ConstantTypeRules<DetailDecision> {
        ConstantTypeRules::accept(|kind| {
            DetailDecision::from_rule(self.constant_type().accept(&(kind, *item)), true)
        })
    }

    pub(crate) fn assignment_decisions<'tcx>(
        &self,
        item: &LocationQuery<'tcx>,
    ) -> AssignmentRules<EventDecision> {
        AssignmentRules::accept(|kind| {
            EventDecision::from_rules(
                self.assignment().accept(&(kind, *item)),
                self.assignment_info().accept(&(kind, *item)),
            )
        })
    }

    pub(crate) fn storage_lifetime_decisions<'tcx>(
        &self,
        item: &LocationQuery<'tcx>,
    ) -> StorageLifetimeMarkerRules<DetailDecision> {
        StorageLifetimeMarkerRules::accept(|kind| {
            let default = match kind {
                StorageLifetimeMarkerKind::Live => false,
                StorageLifetimeMarkerKind::Dead => true,
            };
            DetailDecision::from_rule(self.storage_lifetime().accept(&(kind, *item)), default)
        })
    }

    pub(crate) fn call_flow_decisions<'tcx>(
        &self,
        item: &LocationQuery<'tcx>,
    ) -> CallFlowRules<DetailDecision> {
        CallFlowRules::accept(|kind| {
            DetailDecision::from_rule(self.call_flow().accept(&(kind, *item)), true)
        })
    }

    pub(crate) fn drop_decisions<'tcx>(
        &self,
        item: &LocationQuery<'tcx>,
    ) -> DropRules<DetailDecision> {
        DropRules::accept(|kind| {
            DetailDecision::from_rule(self.drop().accept(&(kind, *item)), true)
        })
    }

    pub(crate) fn switch_decisions<'tcx>(
        &self,
        item: &LocationQuery<'tcx>,
    ) -> SwitchRules<DetailDecision> {
        SwitchRules::accept(|kind| {
            DetailDecision::from_rule(self.switch().accept(&(kind, *item)), true)
        })
    }
}

pub(crate) fn get_baked_policy<'tcx>(
    storage: &mut dyn Storage,
) -> impl Deref<Target = BakedInstrumentationPolicy<'tcx>> + '_
where
    BakedInstrumentationPolicy<'tcx>: 'static,
{
    storage
        .get_mut::<BakedInstrumentationPolicy<'tcx>>(&KEY_BAKED_POLICY.to_owned())
        .expect("Instrumentation policy is expected to be baked at this point.")
}

fn filter_rules<T>(
    all_rules: &InstrumentationRules,
    select: impl Fn(EntityFilter) -> Option<T> + Clone,
) -> InclusionRules<T> {
    all_rules.clone().filter_map(select)
}

pub(crate) fn bake_rules(
    storage: &mut dyn Storage,
    additional_exclusions: impl FnOnce() -> Vec<WholeBodyFilter>,
) {
    let _ = storage.get_or_insert_with_acc(
        KEY_BAKED_POLICY.to_owned(),
        |storage| -> BakedInstrumentationPolicy<'_> {
            let all_rules = storage.get_or_default::<InstrumentationRules>(KEY_RULES.to_owned());

            let mut body = filter_rules(&*all_rules, |rule| match rule {
                EntityFilter::WholeBody(filter) => Some(filter),
                _ => None,
            });
            body.exclude.extend(additional_exclusions());

            let place_rules = filter_rules(&*all_rules, |rule| match rule {
                EntityFilter::PlaceInfo(filter) => Some(filter),
                _ => None,
            });

            BakedInstrumentationPolicy {
                body: body.to_baked(),
                dyn_def: filter_rules(&*all_rules, |rule| match rule {
                    EntityFilter::MethodDynDefinition(filter) => Some(filter),
                    _ => None,
                })
                .to_baked(),
                place_info: PlaceInfoRules {
                    structure: place_rules
                        .clone()
                        .filter_map(|filter| match filter {
                            PlaceInfoFilter::Structure(filter) => Some(filter),
                            _ => None,
                        })
                        .to_baked(),
                    address: place_rules
                        .clone()
                        .filter_map(|filter| match filter {
                            PlaceInfoFilter::Address(filter) => Some(filter),
                            _ => None,
                        })
                        .to_baked(),
                    ty: place_rules
                        .filter_map(|filter| match filter {
                            PlaceInfoFilter::Type(filter) => Some(filter),
                            _ => None,
                        })
                        .to_baked(),
                },
                operand_info: filter_rules(&*all_rules, |rule| match rule {
                    EntityFilter::OperandKind(filter) => Some(filter),
                    _ => None,
                })
                .to_baked(),
                constant_type: filter_rules(&*all_rules, |rule| match rule {
                    EntityFilter::ConstantType(filter) => Some(filter),
                    _ => None,
                })
                .to_baked(),
                assignment: filter_rules(&*all_rules, |rule| match rule {
                    EntityFilter::Assignment(filter) => Some(filter),
                    _ => None,
                })
                .to_baked(),
                assignment_info: filter_rules(&*all_rules, |rule| match rule {
                    EntityFilter::AssignmentInfo(filter) => Some(filter),
                    _ => None,
                })
                .to_baked(),
                storage_lifetime: filter_rules(&*all_rules, |rule| match rule {
                    EntityFilter::StorageLifetimeMarker(filter) => Some(filter),
                    _ => None,
                })
                .to_baked(),
                call_flow: filter_rules(&*all_rules, |rule| match rule {
                    EntityFilter::CallFlow(filter) => Some(filter),
                    _ => None,
                })
                .to_baked(),
                drop: filter_rules(&*all_rules, |rule| match rule {
                    EntityFilter::Drop(filter) => Some(filter),
                    _ => None,
                })
                .to_baked(),
                switch: filter_rules(&*all_rules, |rule| match rule {
                    EntityFilter::Switch(filter) => Some(filter),
                    _ => None,
                })
                .to_baked(),
            }
        },
    );
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
                    let pred = name.to_predicate();
                    Box::new(move |(tcx, def_id)| {
                        pred.accept(tcx.crate_name(def_id.krate).as_str())
                    })
                }
            },
            EntityLocationFilter::DefPathMatch(pattern) => {
                let pred = pattern.to_predicate();
                Box::new(move |(tcx, def_id)| {
                    let def_path = tcx.def_path_str(*def_id);
                    pred.accept(&def_path)
                })
            }
            EntityLocationFilter::DefId(def_id) => {
                let def_id = *def_id;
                Box::new(move |(_, query_def_id)| {
                    query_def_id.krate.as_u32() == def_id.0
                        && query_def_id.index.as_u32() == def_id.1
                })
            }
        }
    }
}

macro_rules! impl_to_predicate_for_loc_type {
    ($name:ty, $field:ident, $query:ty, $filter:ty) => {
        impl<'tcx> ToPredicate<$query> for $name {
            type Predicate = <$filter as ToPredicate<$query>>::Predicate;

            delegate! {
                to self.$field {
                    fn to_predicate(&self) -> Self::Predicate;
                }
            }
        }
    };

    ($($name:ty),+$(,)?) => {
        $(
            impl_to_predicate_for_loc_type!($name, loc, LocationQuery<'tcx>, LogicFormula<EntityLocationFilter>);
        )*
    };
}

impl_to_predicate_for_loc_type!(
    WholeBodyFilter,
    MethodDynDefinitionFilter,
    PlaceAddressFilter,
    PlaceTypeFilter,
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
    StorageLifetimeMarkerKind,
    CallFlowPartKind,
    DropPartKind,
    SwitchPartKind,
);

macro_rules! impl_to_predicate_for_filter {
    ($filter:ty, $query:ty, $other:ident) => {
        impl ToPredicate<$query> for $filter {
            type Predicate = Box<dyn Fn(&$query) -> bool>;

            fn to_predicate(&self) -> Self::Predicate {
                let pred = (
                    self.$other.to_predicate(),
                    self.loc.to_predicate(),
                );
                Box::new(move |(q0, q1)| {
                    pred.0.accept(q0) && pred.1.accept(q1)
                })
            }
        }
    };

    ($filter:ty, $query:ty) => { impl_to_predicate_for_filter!($filter, $query, kind); };

    ($({ $filter:ty, $query:ty $(,$other:ident)? }),+ $(,)?) => {
        $(impl_to_predicate_for_filter!($filter, $query $(, $other)?);)+
    };
}
impl_to_predicate_for_filter!(
    { PlaceStructureFilter, PlaceStructureQuery<'_>, piece },
    { OperandKindFilter,  OperandKindQuery<'_> },
    { ConstantTypeFilter, ConstantTypeQuery<'_>, ty },
    { AssignmentFilter, AssignmentQuery<'_> },
    { StorageLifetimeMarkerFilter, StorageLifetimeMarkerQuery<'_> },
    { CallFlowFilter, CallFlowQuery<'_> },
    { DropFilter, DropQuery<'_> },
    { SwitchFilter, SwitchQuery<'_> },
);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn body_rules_preserve_unmatched_state() {
        assert_eq!(BodyDecision::from_rule(None), None);
        assert_eq!(
            BodyDecision::from_rule(Some(true)),
            Some(BodyDecision::Instrument)
        );
        assert_eq!(
            BodyDecision::from_rule(Some(false)),
            Some(BodyDecision::Skip)
        );
    }

    #[test]
    fn event_rules_separate_omission_from_opaque_payloads() {
        assert_eq!(
            EventDecision::from_rules(Some(false), Some(true)),
            EventDecision::Omit
        );
        assert_eq!(
            EventDecision::from_rules(Some(true), Some(false)),
            EventDecision::Opaque
        );
        assert_eq!(
            EventDecision::from_rules(None, None),
            EventDecision::Detailed
        );
    }

    #[test]
    fn detail_rules_keep_entity_defaults() {
        assert_eq!(
            DetailDecision::from_rule(None, false),
            DetailDecision::Disabled
        );
        assert_eq!(
            DetailDecision::from_rule(None, true),
            DetailDecision::Enabled
        );
        assert!(!DetailDecision::from_rule(Some(false), true).is_enabled());
        assert!(DetailDecision::from_rule(Some(true), false).is_enabled());
    }
}
