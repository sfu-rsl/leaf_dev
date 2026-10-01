mod errors {
    include!("../examples/errors.rs");
}

mod exports_function {
    include!("../examples/exports_function.rs");
}

mod export_global {
    include!("../examples/exports_global.rs");
}

mod export_memory {
    include!("../examples/exports_memory.rs");
}

mod funcref {
    include!("../examples/funcref.rs");
}

mod hello_world {
    include!("../examples/hello_world.rs");
}

mod http_dynamic_size {
    include!("../examples/http_dynamic_size.rs");
}

mod imports_exports {
    include!("../examples/imports_exports.rs");
}

mod imports_function_env {
    include!("../examples/imports_function_env.rs");
}

mod imports_function {
    include!("../examples/imports_function.rs");
}

mod imports_global {
    include!("../examples/imports_global.rs");
}

mod instance {
    include!("../examples/instance.rs");
}

// Requiring features like cranelift that are not the target of experiments

// mod early_exit {
//     include!("../examples/early_exit.rs");
// }

// mod imports_function_env_global {
//     include!("../examples/imports_function_env_global.rs");
// }

// Not compatible with wamr

// mod memory_grow {
//     include!("../examples/memory_grow.rs");
// }

// mod memory {
//     include!("../examples/memory.rs");
// }

// mod table {
//     include!("../examples/table.rs");
// }