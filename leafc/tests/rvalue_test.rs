mod common;

use crate::common::TestType;
use leafc::RunCompiler;
use leafcommon::place::Place;
use leafcommon::rvalue::Rvalue;
use leafcommon::statementkind::StatementKind;

fn run_test(file_name: &str, rvalue_predicate: impl Fn(&Rvalue) -> bool) {
    fn contains_assign_rvalue_type(
        rvalue_predicate: impl Fn(&Rvalue) -> bool,
        statements: &[StatementKind],
    ) -> bool {
        statements
            .iter()
            .any(|s| is_assign_rvalue_type(&rvalue_predicate, s))
    }

    fn is_assign_rvalue_type(
        rvalue_predicate: &dyn Fn(&Rvalue) -> bool,
        statement: &StatementKind,
    ) -> bool {
        if let StatementKind::Assign(_, r) = statement {
            rvalue_predicate(r)
        } else {
            false
        }
    }

    let path = common::get_test_file_path(TestType::Rvalue, file_name);
    let (exit_code, statements) =
        RunCompiler::run(&mut std::env::args().collect(), Some(path), true);
    assert_eq!(0, exit_code);
    assert!(
        contains_assign_rvalue_type(rvalue_predicate, &statements),
        "expected to contain a statement for file {:?}, but only statements are {:?}",
        file_name,
        statements
    )
}

#[test]
fn test_addressof() {
    run_test("addressof.rs", |rv| {
        if let Rvalue::AddressOf(..) = rv {
            true
        } else {
            false
        }
    })
}

#[test]
fn test_aggregate() {
    run_test("aggregate.rs", |rv| {
        if let Rvalue::Aggregate(..) = rv {
            true
        } else {
            false
        }
    })
}

#[test]
fn test_binaryop() {
    run_test("binaryop.rs", |rv| {
        if let Rvalue::BinaryOp(..) = rv {
            true
        } else {
            false
        }
    })
}

#[test]
fn test_cast() {
    run_test("cast.rs", |rv| {
        if let Rvalue::Cast(..) = rv {
            true
        } else {
            false
        }
    })
}

#[test]
fn test_checkedbinaryop() {
    run_test("checkedbinaryop.rs", |rv| {
        if let Rvalue::CheckedBinaryOp(..) = rv {
            true
        } else {
            false
        }
    })
}

#[test]
fn test_len() {
    run_test("len.rs", |rv| {
        if let Rvalue::Len(..) = rv {
            true
        } else {
            false
        }
    })
}

#[test]
#[ignore]
fn test_nullaryop() {
    // FIXME: Doesn't work
    run_test("nullaryop.rs", |rv| {
        if let Rvalue::NullaryOp(..) = rv {
            true
        } else {
            false
        }
    })
}

#[test]
fn test_ref() {
    run_test("ref.rs", |rv| {
        if let Rvalue::Ref(..) = rv {
            true
        } else {
            false
        }
    })
}

#[test]
fn test_repeat() {
    run_test("repeat.rs", |rv| {
        if let Rvalue::Repeat(..) = rv {
            true
        } else {
            false
        }
    })
}

#[test]
fn test_shallowinitbox() {
    run_test("shallowinitbox.rs", |rv| {
        if let Rvalue::ShallowInitBox(..) = rv {
            true
        } else {
            false
        }
    })
}

#[test]
fn test_unaryop() {
    run_test("unaryop.rs", |rv| {
        if let Rvalue::UnaryOp(..) = rv {
            true
        } else {
            false
        }
    })
}

#[test]
fn test_use() {
    run_test("use.rs", |rv| {
        if let Rvalue::Use(..) = rv {
            true
        } else {
            false
        }
    })
}
