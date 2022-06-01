struct TestStatic { string: &'static str, }

static TEST: &'static TestStatic = &TestStatic { string: "I am static" };
