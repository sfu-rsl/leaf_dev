use std::path::PathBuf;

pub enum TestType {
    StatementKind,
    Rvalue,
}

pub fn get_test_file_path(test_type: TestType, file_name: &str) -> PathBuf {
    let mut dir = std::env::current_dir()
        .expect("top-level directory")
        .join("tests")
        .join("test_fixtures");
    match test_type {
        TestType::StatementKind => dir.push("statementkind"),
        TestType::Rvalue => dir.push("rvalue"),
    };
    dir.push(file_name);
    dir
}
