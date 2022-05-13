mod common;

use rc0::{RunCompiler, SimpleRvalue, SimpleStatementKind};
use crate::common::TestType;

fn contains_assign_rvalue_type(expected_type: SimpleRvalue, statements: &[SimpleStatementKind]) -> bool {
    statements.iter().any(|s| is_assign_rvalue_type(expected_type.clone(), s))
}

fn is_assign_rvalue_type(expected_type: SimpleRvalue, statement: &SimpleStatementKind) -> bool {
    if let SimpleStatementKind::Assign(x) = statement { *x == expected_type } else { false }
}

fn run_test(file_name: &str, expected_type: SimpleRvalue) {
    let path = common::get_test_file_path(TestType::Rvalue, file_name);
    let (exit_code, statements) = RunCompiler::run(&mut std::env::args().collect(), Some(path));
    assert_eq!(0, exit_code);
    assert!(
        contains_assign_rvalue_type(expected_type, &statements),
        "expected to contain a statement with Rvalue::{:?}, but only statements are {:?}",
        expected_type, statements
    )
}

#[test]
fn test_addressof() {
    run_test("addressof.rs", SimpleRvalue::AddressOf)
}

#[test]
fn test_aggregate() {
    run_test("aggregate.rs", SimpleRvalue::Aggregate)
}

#[test]
fn test_binaryop() {
    run_test("binaryop.rs", SimpleRvalue::BinaryOp)
}

#[test]
fn test_cast() {
    run_test("cast.rs", SimpleRvalue::Cast)
}

#[test]
fn test_checkedbinaryop() {
    run_test("checkedbinaryop.rs", SimpleRvalue::CheckedBinaryOp)
}

#[test]
fn test_len() {
    run_test("len.rs", SimpleRvalue::Len)
}

#[test]
#[ignore]
fn test_nullaryop() {
    // FIXME: Doesn't work
    run_test("nullaryop.rs", SimpleRvalue::NullaryOp)
}

#[test]
fn test_ref() {
    run_test("ref.rs", SimpleRvalue::Ref)
}

#[test]
fn test_repeat() {
    run_test("repeat.rs", SimpleRvalue::Repeat)
}

#[test]
fn test_shallowinitbox() {
    run_test("shallowinitbox.rs", SimpleRvalue::ShallowInitBox)
}

#[test]
fn test_unaryop() {
    run_test("unaryop.rs", SimpleRvalue::UnaryOp)
}

#[test]
fn test_use() {
    run_test("use.rs", SimpleRvalue::Use)
}