mod common;

use crate::common::TestType;
use leafc::RunCompiler;
use leafcommon::statementkind::StatementKind;

#[test]
fn test_assign() {
    let path = common::get_test_file_path(TestType::StatementKind, "assign.rs");
    let (exit_code, statements) =
        RunCompiler::run(&mut std::env::args().collect(), Some(path), true);
    assert_eq!(0, exit_code);
    assert!(statements
        .iter()
        .any(|s| if let StatementKind::Assign(_, _) = s {
            true
        } else {
            false
        }))
}

#[test]
fn test_setdiscriminant() {
    let path = common::get_test_file_path(TestType::StatementKind, "setdiscriminant.rs");
    let (exit_code, statements) =
        RunCompiler::run(&mut std::env::args().collect(), Some(path), true);
    assert_eq!(0, exit_code);
    assert!(statements
        .iter()
        .any(|s| if let StatementKind::SetDiscriminant { .. } = s {
            true
        } else {
            false
        }));
}

#[test]
fn test_storage_live_dead() {
    let path = common::get_test_file_path(TestType::StatementKind, "storage_live_dead.rs");
    let (exit_code, statements) =
        RunCompiler::run(&mut std::env::args().collect(), Some(path), true);
    assert_eq!(0, exit_code);
    assert!(statements
        .iter()
        .any(|s| if let StatementKind::StorageLive(_) = s {
            true
        } else {
            false
        }));
    assert!(statements
        .iter()
        .any(|s| if let StatementKind::StorageDead(_) = s {
            true
        } else {
            false
        }));
}
