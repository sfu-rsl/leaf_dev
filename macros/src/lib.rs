use ignore::WalkBuilder;
use proc_macro::TokenStream;
use quote::{format_ident, quote};
use std::path::{Path, MAIN_SEPARATOR};
use syn::{parse_macro_input, ItemFn, LitStr};

#[proc_macro_attribute]
pub fn gen_tests_rs(input_path: TokenStream, input_fn: TokenStream) -> TokenStream {
    let mut tests = proc_macro2::TokenStream::new();
    let test_fn = parse_macro_input!(input_fn as ItemFn);
    let test_name_fn = &test_fn.sig.ident;

    let ignore_filename = format!(".{}.ignore", test_name_fn);
    for entry in WalkBuilder::new(Path::new(&parse_macro_input!(input_path as LitStr).value()))
        .hidden(false)
        .ignore(false)
        .add_custom_ignore_filename(ignore_filename.clone())
        .build()
        .filter_map(|entry| entry.ok())
    {
        let path = entry.path();
        if !path.extension().is_some_and(|ext| ext == "rs") {
            continue;
        }

        let test_name = format_ident!("{}", get_test_name(path));
        let test_path = &path.display().to_string();
        tests.extend(quote! {
            #[test]
            fn #test_name() {
                #test_name_fn(#test_path)
            }
        });
    }

    TokenStream::from(quote! {
        mod #test_name_fn {
            use super::*;
            #test_fn
            #tests
        }
    })
}

#[proc_macro_attribute]
pub fn gen_tests_toml(input_path: TokenStream, input_fn: TokenStream) -> TokenStream {
    let mut tests = proc_macro2::TokenStream::new();
    let test_fn = parse_macro_input!(input_fn as ItemFn);
    let test_name_fn = &test_fn.sig.ident;

    let ignore_filename = format!(".{}.ignore", test_name_fn);
    for entry in WalkBuilder::new(Path::new(&parse_macro_input!(input_path as LitStr).value()))
        .hidden(false)
        .ignore(false)
        .add_custom_ignore_filename(ignore_filename.clone())
        .build()
        .filter_map(|entry| entry.ok())
    {
        let path = entry.path();
        if !path.join("Cargo.toml").exists() {
            continue;
        }

        let test_name = format_ident!("{}", get_test_name(path));
        let test_path = &path.display().to_string();
        tests.extend(quote! {
            #[test]
            fn #test_name() {
                #test_name_fn(#test_path)
            }
        });
    }

    TokenStream::from(quote! {
        mod #test_name_fn {
            use super::*;
            #test_fn
            #tests
        }
    })
}

fn get_test_name(path: &Path) -> String {
    path.with_extension("")
        .to_string_lossy()
        .replace("-", "_")
        .replace(MAIN_SEPARATOR, "__")
}
