use ignore::WalkBuilder;
use proc_macro::TokenStream;
use quote::{format_ident, quote};
use std::collections::HashSet;
use std::path::{Path, MAIN_SEPARATOR};
use syn::{parse_macro_input, Ident, ItemFn, LitStr};

#[proc_macro_attribute]
pub fn gen_tests(input_path: TokenStream, input_fn: TokenStream) -> TokenStream {
    let mut tests = proc_macro2::TokenStream::new();
    let mut tests_name: HashSet<Ident> = HashSet::new();
    let test_fn = parse_macro_input!(input_fn as ItemFn);
    let test_name_fn = &test_fn.sig.ident;

    let ignore_filename = format!(".{}.ignore", test_name_fn);
    for entry in WalkBuilder::new(Path::new(&parse_macro_input!(input_path as LitStr).value()))
        .hidden(false)
        .ignore(false)
        .add_custom_ignore_filename(ignore_filename.clone())
        .build()
    {
        match entry {
            Ok(entry) => {
                let path = entry.path();
                if path.is_file() {
                    let file_name = path.file_name().expect("invalid file name").to_string_lossy();
                    if
                        if let Some(rs_file_name) = file_name.strip_suffix(".rs") {
                            if rs_file_name.is_empty() {
                                true
                            } else {
                                let test_name = format_ident!(
                                    "{}",
                                    path.parent().expect("invalid parent")
                                        .join(rs_file_name).to_string_lossy()
                                        .replace("-", "_")
                                        .replace(MAIN_SEPARATOR, "__")
                                );
                                if tests_name.contains(&test_name) {
                                    true
                                } else {
                                    tests_name.insert(test_name.clone());
                                    let test_path = &path.display().to_string();
                                    tests.extend(quote! {
                                        #[test]
                                        fn #test_name() {
                                            #test_name_fn(#test_path)
                                        }
                                    });
                                    false
                                }
                            }
                        } else {
                            true
                        }
                    {
                        if file_name == ignore_filename {
                            eprintln!("note: loaded ignore rules `{}`", path.display())
                        } else {
                            eprintln!("note: ignoring testing `{}`", path.display())
                        }
                    }
                    
                }
            }
            Err(err) => {
                eprintln!("error: expecting an existing file or directory");
                panic!("{}", err);
            }
        }
    }

    TokenStream::from(quote! {
        mod #test_name_fn {
            use super::*;
            #test_fn
            #tests
        }
    })
}
