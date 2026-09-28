use rustc_middle::ty::TyCtxt;
use rustc_span::def_id::DefId;

pub(super) use super::super::config::rules::{
    ConstantTypeRules, DetailDecision, PlaceStructureRules,
};

use super::super::config::rules::{
    AssignmentRules, BakedInstrumentationPolicy, BodyDecision, CallFlowRules, DropRules,
    EventDecision, OperandKindRules, PlaceInfoRules, StorageLifetimeMarkerRules, SwitchRules,
};

pub(crate) struct BodyConfig {
    pub(crate) place_info_filter:
        PlaceInfoRules<PlaceStructureRules<DetailDecision>, DetailDecision>,
    pub(crate) operand_info_filter:
        OperandKindRules<DetailDecision, Option<ConstantTypeRules<DetailDecision>>>,
    pub(crate) assignment_filter: AssignmentRules<EventDecision>,
    pub(crate) storage_lifetime_filter: StorageLifetimeMarkerRules<DetailDecision>,
    pub(crate) call_flow_filter: CallFlowRules<DetailDecision>,
    pub(crate) drop_filter: DropRules<DetailDecision>,
    pub(crate) switch_filter: SwitchRules<DetailDecision>,
    dynamic_definition: BodyDecision,
}

impl BodyConfig {
    pub(crate) fn for_body<'tcx, 'policy>(
        policy: &BakedInstrumentationPolicy<'policy>,
        tcx: TyCtxt<'tcx>,
        def_id: DefId,
    ) -> Self
    where
        BakedInstrumentationPolicy<'policy>: 'static,
    {
        let item = &(tcx, def_id);
        let operand_info = policy.operand_info_decisions(item);
        let operand_info_filter = OperandKindRules {
            copy: operand_info.copy,
            mov: operand_info.mov,
            constant: if operand_info.constant.is_enabled() {
                Some(policy.constant_type_decisions(item))
            } else {
                None
            },
        };

        Self {
            place_info_filter: policy.place_info_decisions(item),
            operand_info_filter,
            assignment_filter: policy.assignment_decisions(item),
            storage_lifetime_filter: policy.storage_lifetime_decisions(item),
            call_flow_filter: policy.call_flow_decisions(item),
            drop_filter: policy.drop_decisions(item),
            switch_filter: policy.switch_decisions(item),
            dynamic_definition: policy.dynamic_definition_decision(item),
        }
    }

    pub(crate) fn defines_dyn_compatible_method_as_static(&self) -> bool {
        self.dynamic_definition == BodyDecision::Skip
    }
}
