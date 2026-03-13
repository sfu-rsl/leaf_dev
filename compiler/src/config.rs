use derive_more::{Deref, derive::From};
use serde::Deserialize;

use crate::CONFIG_ENV_PREFIX;
use common::log_info;

#[derive(Debug, Default, Clone, Deserialize)]
pub(crate) struct LeafCompilerConfig {
    #[serde(default)]
    pub runtime_shim: RuntimeShimConfig,
    #[serde(default)]
    pub building_core: bool,
    #[serde(default = "default_override_sysroot")]
    pub override_sysroot: bool,
    #[serde(default = "default_codegen_all_mir")]
    pub codegen_all_mir: bool,
    #[serde(default = "default_marker_cfg_name")]
    pub marker_cfg_name: String,
    #[serde(default)]
    #[serde(alias = "rules")]
    pub instr_rules: InstrumentationRules,
    #[serde(default)]
    pub internalization_rules: InternalizationRules,
}

fn default_override_sysroot() -> bool {
    true
}

fn default_codegen_all_mir() -> bool {
    true
}

fn default_marker_cfg_name() -> String {
    "leafc".to_string()
}

impl LeafCompilerConfig {
    const F_RUNTIME_SHIM: &'static str = "runtime_shim";
}

#[derive(Debug, Default, Clone, Deserialize)]
pub(crate) struct RuntimeShimConfig {
    pub location: RuntimeShimLocation,
}

impl RuntimeShimConfig {
    const F_LOCATION: &'static str = "location";
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum RuntimeShimLocation {
    #[serde(alias = "core")]
    CoreLib,
    External {
        #[serde(default = "default_runtime_shim_crate_name")]
        crate_name: String,
        search_path: RuntimeShimExternalLocation,
    },
}

impl RuntimeShimLocation {
    const V_EXTERNAL: &'static str = "external";

    const F_CRATE_NAME: &'static str = "crate_name";
    const F_SEARCH_PATH: &'static str = "search_path";
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum RuntimeShimExternalLocation {
    #[default]
    #[serde(alias = "default")]
    Sysroot,
    /// Treated as a normal dependency,
    /// i.e., the crate is expected to be in the sysroot or other provided search paths.
    #[serde(alias = "deps")]
    CrateDeps,
    Compiler,
    #[serde(alias = "exact")]
    Exact(String),
}

impl RuntimeShimExternalLocation {
    const V_SYSROOT: &'static str = "sysroot";
}

impl Default for RuntimeShimLocation {
    fn default() -> Self {
        RuntimeShimLocation::External {
            crate_name: default_runtime_shim_crate_name(),
            search_path: RuntimeShimExternalLocation::Sysroot,
        }
    }
}

fn default_runtime_shim_crate_name() -> String {
    "leaf".to_string()
}

pub(crate) type InstrumentationRules = InclusionRules<EntityFilter>;

#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "entity")]
#[serde(rename_all = "snake_case")]
pub(crate) enum EntityFilter {
    #[serde(alias = "body")]
    WholeBody(WholeBodyFilter),
    #[serde(alias = "dyn_def")]
    MethodDynDefinition(MethodDynDefinitionFilter),
    PlaceInfo(PlaceInfoFilter),
    OperandInfo(OperandInfoFilter),
    #[serde(alias = "const_type")]
    ConstantType(ConstantTypeFilter),
    Assignment(AssignmentFilter),
    AssignmentInfo(AssignmentFilter),
    StorageLifetime(StorageLifetimeMarkerFilter),
    CallFlow(CallFlowFilter),
    Drop(DropFilter),
}

#[derive(Debug, Clone, Deserialize, From)]
pub(crate) struct WholeBodyFilter(pub(crate) LogicFormula<EntityLocationFilter>);

#[derive(Debug, Clone, Deserialize)]
pub(crate) struct MethodDynDefinitionFilter(pub(crate) LogicFormula<EntityLocationFilter>);

#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "kind")]
#[serde(rename_all = "snake_case")]
pub(crate) enum PlaceInfoFilter {
    #[serde(alias = "struct")]
    Structure(PlaceInfoStructureFilter),
    #[serde(alias = "addr")]
    Address(PlaceAddressFilter),
    #[serde(alias = "ty")]
    Type(PlaceTypeFilter),
}

#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "kind")]
#[serde(rename_all = "snake_case")]
pub(crate) struct StorageLifetimeMarkerFilter {
    pub(crate) kind: LogicFormula<StorageLifetimeMarker>,
    #[serde(flatten)]
    pub(crate) loc: LogicFormula<StorageLifetimeLocationFilter>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum EntityLocationFilter {
    #[serde(alias = "def_path")]
    DefPathMatch(PatternMatch),
    Crate(CrateFilter),
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum CrateFilter {
    #[serde(alias = "is_external")]
    Externality(bool),
    Name(String),
}

#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "kind")]
#[serde(rename_all = "snake_case")]
pub(crate) struct PlaceInfoStructureFilter {
    pub(crate) piece: LogicFormula<PlaceStructurePiece>,
    #[serde(flatten)]
    pub(crate) loc: LogicFormula<EntityLocationFilter>,
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub(crate) enum PlaceStructurePiece {
    Local,
    Deref,
    Field,
    Index,
    ConstantIndex,
    Subslice,
    Downcast,
    OpaqueCast,
    UnwrapUnsafeBinder,
}

#[derive(Debug, Clone, Deserialize)]
pub(crate) struct PlaceAddressFilter(pub(crate) LogicFormula<EntityLocationFilter>);

#[derive(Debug, Clone, Deserialize)]
pub(crate) struct PlaceTypeFilter(pub(crate) LogicFormula<EntityLocationFilter>);

#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "kind")]
#[serde(rename_all = "snake_case")]
pub(crate) struct OperandInfoFilter {
    pub(crate) kind: LogicFormula<OperandKind>,
    #[serde(flatten)]
    pub(crate) loc: OperandInfoLocationFilter,
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub(crate) enum OperandKind {
    Copy,
    Move,
    #[serde(alias = "const")]
    Constant,
}

#[derive(Debug, Clone, Deserialize)]
pub(crate) struct ConstantTypeFilter {
    #[serde(alias = "type")]
    pub(crate) ty: LogicFormula<ConstantType>,
    #[serde(flatten)]
    pub(crate) loc: OperandInfoLocationFilter,
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub(crate) enum ConstantType {
    Bool,
    Char,
    Int,
    Float,
    Str,
    ByteStr,
    Ptr,
    Zst,
}

#[derive(Debug, Clone, Deserialize)]
pub(crate) struct OperandInfoLocationFilter(pub(crate) LogicFormula<EntityLocationFilter>);

#[derive(Debug, Clone, Deserialize)]
pub(crate) struct AssignmentFilter {
    pub(crate) kind: LogicFormula<AssignmentKind>,
    #[serde(flatten)]
    pub(crate) loc: AssignmentLocationFilter,
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub(crate) enum AssignmentKind {
    Use,
    Repeat,
    Ref,
    ThreadLocalRef,
    RawPtr,
    Cast,
    BinaryOp,
    UnaryOp,
    Discriminant,
    Aggregate,
    ShallowInitBox,
    WrapUnsafeBinder,
}

#[derive(Debug, Clone, Deserialize)]
pub(crate) struct AssignmentLocationFilter(pub(crate) LogicFormula<EntityLocationFilter>);

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub(crate) enum StorageLifetimeMarker {
    Live,
    Dead,
}

#[derive(Debug, Clone, Deserialize)]
pub(crate) struct StorageLifetimeLocationFilter(pub(crate) LogicFormula<EntityLocationFilter>);

#[derive(Debug, Clone, Deserialize)]
pub(crate) struct CallFlowFilter {
    pub(crate) kind: LogicFormula<CallFlowPartKind>,
    #[serde(flatten)]
    pub(crate) loc: CallFlowLocationFilter,
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub(crate) enum CallFlowPartKind {
    CallControl,
    CallInput,
    #[serde(alias = "func_data")]
    FunctionData,
}

#[derive(Debug, Clone, Deserialize)]
pub(crate) struct CallFlowLocationFilter(pub(crate) LogicFormula<EntityLocationFilter>);

#[derive(Debug, Clone, Deserialize)]
pub(crate) struct DropFilter {
    pub(crate) kind: LogicFormula<DropPartKind>,
    #[serde(flatten)]
    pub(crate) loc: DropLocationFilter,
}

#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub(crate) enum DropPartKind {
    CallControl,
    CallInput,
}

#[derive(Debug, Clone, Deserialize)]
pub(crate) struct DropLocationFilter(pub(crate) LogicFormula<EntityLocationFilter>);

pub(crate) type InternalizationRules = InclusionRules<LogicFormula<PatternMatch>>;

const CONFIG_FILENAME: &str = "leafc_config";

pub(super) fn load_config() -> LeafCompilerConfig {
    common::config::load_config(CONFIG_FILENAME, CONFIG_ENV_PREFIX, |b| {
        Ok(b)
            .and_then(|b| {
                b.set_default(
                    format!(
                        "{}.{}.{}.{}",
                        LeafCompilerConfig::F_RUNTIME_SHIM,
                        RuntimeShimConfig::F_LOCATION,
                        RuntimeShimLocation::V_EXTERNAL,
                        RuntimeShimLocation::F_CRATE_NAME,
                    ),
                    default_runtime_shim_crate_name(),
                )
            })
            .and_then(|b| {
                b.set_default(
                    format!(
                        "{}.{}.{}.{}",
                        LeafCompilerConfig::F_RUNTIME_SHIM,
                        RuntimeShimConfig::F_LOCATION,
                        RuntimeShimLocation::V_EXTERNAL,
                        RuntimeShimLocation::F_SEARCH_PATH,
                    ),
                    RuntimeShimExternalLocation::V_SYSROOT,
                )
            })
    })
    .and_then(|c| c.try_deserialize())
    .inspect(|c| log_info!("Loaded configurations: {:?}", c))
    .expect("Failed to read configurations")
}

pub(crate) mod rules {
    use super::*;

    #[derive(Debug, Clone, Deserialize)]
    pub(crate) struct InclusionRules<T> {
        #[serde(default = "Vec::default")]
        pub(crate) include: Vec<T>,
        #[serde(default = "Vec::default")]
        pub(crate) exclude: Vec<T>,
    }

    impl<T> Default for InclusionRules<T> {
        fn default() -> Self {
            InclusionRules {
                include: Vec::default(),
                exclude: Vec::default(),
            }
        }
    }

    /* NOTE: How is serde's structure is defined?
     * We want to make the rules easy and intuitive to define in TOML.
     * - The default enum representation in serde uses the variant name as the key.
     * - The untagged representation selects the variant based on unique fields matched.
     * We mostly utilize these two and flattening.
     * For example, a `LogicFormula` can be represented as any of the following:
     * ```toml
     * [[f]]
     * crate = { is_external = true }
     * [[f]]
     * not = { crate = { name = "std" } }
     * [[f]]
     * any = [{ crate = { name = "std" } }, { crate = { name = "core" } }]
     * [[f]]
     * all = [{ crate = { is_external = true } }, { crate = { name = "core" } }]
     * ``` */

    #[derive(Debug, Clone, Deserialize, Deref, From)]
    pub(crate) struct PatternMatch(String);

    #[derive(Debug, Clone, Deserialize)]
    #[serde(untagged)]
    pub(crate) enum LogicFormula<T> {
        Not(NotFormula<T>),
        Any(AnyFormula<T>),
        All(AllFormula<T>),
        Atom(T),
        // NOTE: This variant helps with parsing empty tables by preventing the infinite search over the name of fields.
        Empty {},
    }

    impl<T> Default for LogicFormula<T> {
        fn default() -> Self {
            LogicFormula::Empty {}
        }
    }

    #[derive(Debug, Clone, Deserialize, From)]
    pub(crate) struct NotFormula<T> {
        #[serde(rename = "not")]
        pub(crate) of: Box<LogicFormula<T>>,
    }

    #[derive(Debug, Clone, Deserialize, From)]
    pub(crate) struct AnyFormula<T> {
        #[serde(rename = "any")]
        pub(crate) of: Vec<LogicFormula<T>>,
    }

    #[derive(Debug, Clone, Deserialize, From)]
    pub(crate) struct AllFormula<T> {
        #[serde(rename = "all")]
        pub(crate) of: Vec<LogicFormula<T>>,
    }
}
use rules::*;
