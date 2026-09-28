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
    pub(crate) place_info: PlaceInfoRules<PlaceStructureRules<DetailDecision>, DetailDecision>,
    pub(crate) operand_info:
        OperandKindRules<DetailDecision, Option<ConstantTypeRules<DetailDecision>>>,
    pub(crate) assignment: AssignmentRules<EventDecision>,
    pub(crate) storage_lifetime: StorageLifetimeMarkerRules<DetailDecision>,
    pub(crate) call_flow: CallFlowRules<DetailDecision>,
    pub(crate) drop: DropRules<DetailDecision>,
    pub(crate) switch: SwitchRules<DetailDecision>,
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
        let operand_info = OperandKindRules {
            copy: operand_info.copy,
            mov: operand_info.mov,
            constant: if operand_info.constant.is_enabled() {
                Some(policy.constant_type_decisions(item))
            } else {
                None
            },
        };

        Self {
            place_info: policy.place_info_decisions(item),
            operand_info,
            assignment: policy.assignment_decisions(item),
            storage_lifetime: policy.storage_lifetime_decisions(item),
            call_flow: policy.call_flow_decisions(item),
            drop: policy.drop_decisions(item),
            switch: policy.switch_decisions(item),
            dynamic_definition: policy.dynamic_definition_decision(item),
        }
    }

    pub(crate) fn defines_dyn_compatible_method_as_static(&self) -> bool {
        self.dynamic_definition == BodyDecision::Skip
    }
}
