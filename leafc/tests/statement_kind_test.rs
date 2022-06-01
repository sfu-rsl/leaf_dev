mod common;

use rc0::{RunCompiler, SimpleStatementKind};
use crate::common::TestType;

#[test]
fn test_assign() {
    let path = common::get_test_file_path(TestType::StatementKind, "assign.rs");
    let (exit_code, statements) = RunCompiler::run(&mut std::env::args().collect(), Some(path));
    assert_eq!(0, exit_code);
    assert!(statements.iter().any(|s| if let SimpleStatementKind::Assign(_) = s { true } else { false }))
}

#[test]
fn test_setdiscriminant() {
    let path = common::get_test_file_path(TestType::StatementKind, "setdiscriminant.rs");
    let (exit_code, statements) = RunCompiler::run(&mut std::env::args().collect(), Some(path));
    assert_eq!(0, exit_code);
    assert!(statements.iter().any(|s| *s == SimpleStatementKind::SetDiscriminant));
}

#[test]
fn test_storage_live_dead() {
    let path = common::get_test_file_path(TestType::StatementKind, "storage_live_dead.rs");
    let (exit_code, statements) = RunCompiler::run(&mut std::env::args().collect(), Some(path));
    assert_eq!(0, exit_code);
    assert!(statements.iter().any(|s| *s == SimpleStatementKind::StorageLive));
    assert!(statements.iter().any(|s| *s == SimpleStatementKind::StorageDead));
}
