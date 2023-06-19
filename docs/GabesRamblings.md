# Gabe's Ramblings
 things I've learned about rustc

### notes:
- what is a const parameter in rust?  
  - in a `ParamConst` -> `Symbol` represents an interned string, where interned means uniquely addressed. Symbol is probably variable name.
    - https://rust-lang.github.io/rfcs/2000-const-generics.html
    - it's like a generic parameter (T in `Vec<T>`), except it's a const variable with unknown value. 
    - declared as `const $ident: $ty`
  - const generics
    - https://practice.rs/generics-traits/const-generics.html#:~:text=Const%20generics%20are%20generic%20arguments,type%20T%20and%20N%3A%20usize.
    - generic arguments that are actually just const variables!
    - use `MyType::<278, 43, 43, 44, 45>` syntax to assign them to an instance of the type
    - very likely still exist in MIR
- optimizations run on the MIR
  - https://github.com/rust-lang/rust/blob/master/compiler/rustc_mir_transform/src/lib.rs#L545
  - as of 2023-02-15 we get the `body` we parse from `providers.optimized_mir` (see: https://rustc-dev-guide.rust-lang.org/mir/passes.html), so all of the above optimizations have been applied to it!
  - MIR has no assignments to ZST places
    - this makes sense because one of the big goals of ZSTs is to remove functionality generically (https://doc.rust-lang.org/nomicon/exotic-sizes.html#zero-sized-types-zsts)
  - MIR has constant propagation applied to it already (it's not perfect though?) (at least two passes of constant propagation)
- `rustc_middle::ty::ConstKind` is only used in the HIR
  - https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/ty/enum.ConstKind.html
  - don't be confused with `rustc_middle::mir::ConstantKind`, which is used in println!("hello world")
  - 
- Placeholders & Universes
  - https://rustc-dev-guide.rust-lang.org/borrow_check/region_inference/placeholders_and_universes.html?highlight=Placeholder#subtyping-and-placeholders
  - Placeholders are vaugely for dealing with unknown regions / things?
  - Universes are spaces where a set of names are in scope at the same time
  - this relates to the borrow checker, and so likely happens during MIR (more specifically during the `mir_validated` phase https://rustc-dev-guide.rust-lang.org/mir/passes.html)
- Trait queries
  - trait queries have to do with finding the unambiguous value for a type variable https://rustc-dev-guide.rust-lang.org/traits/canonical-queries.html#a-trait-query-in-rustc
  - rust has a trait solver called chalk which various logic rules https://rustc-dev-guide.rust-lang.org/traits/chalk.html 
  - trait solving (aka trait resolution maybe?) revolves around pairing generic function (or struct) implementations with the parameters passed to them & determining if they satisfy the trait bounds required. https://rustc-dev-guide.rust-lang.org/traits/resolution.html#major-concepts
  - trait solving and type checking are both likely done in the HIR phase https://rustc-dev-guide.rust-lang.org/type-checking.html
- Assertions
  - Automatic assertions such as Overflow or division by zero checks are removes in release mode
  - https://doc.rust-lang.org/beta/nightly-rustc/rustc_middle/mir/enum.AssertKind.html
  - TODO: verify this formally, not just experimentally
  - TODO: also verify this for the assert macro
  
### debugging on windows:
- in order to debug stack overflows, you must use a debugger. On windows, lldb is more availible than gdb.
- in order to link to a rust executable's debug symbols, you must add the following environment variable:
- `$Env:LLDB_USE_NATIVE_PDB_READER="yes"`
- this help lldb find your pdb.
- also omit `.exe` in the command: `lldb exe_name`