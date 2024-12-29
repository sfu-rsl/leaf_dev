#![cfg_attr(not(leafc), no_main)]

use libfuzzer_sys::hybrid_fuzz_target;

hybrid_fuzz_target!(|data: &[u8]| {
    basic_pro_conc::run_input(data);
});
