const SONAME: &str = "libleafrt.so";

fn set_so_name() {
    /* This ensures that the ELF header SONAME is set to `libleafrt.so` for all flavors.
     * This makes it possible to use different flavors interchangeably without
     * recompiling the target program, otherwise by default the header is set to
     * the so file, which will affect `DT_NEEDED` header in the program's executable. */
    println!("cargo::rustc-cdylib-link-arg=-Wl,-soname={SONAME}");
}
