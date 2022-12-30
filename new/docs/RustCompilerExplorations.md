# Rust Compiler Expolartion
As the rust compiler is not well-documented and fairly unstable, lots of facts and behaviors should be understood through reading the source codes, (hopefully) comments in them, and compiling various test programs. In this document, we write whatever we find.

## Rvalue

### Aggregates
- Appear as a variant of `Rvalue`.
- Something intermediate for informations that compiler needs. After `Deaggregator` MIR pass, aggregate variants get expanded.
- Seems to be removed in the future. Sources: rust-lang/rust#48052, rust-lang/rust#48193
- It looks like we only need to consider the `Array` variant.
- ADT means Algebraic Data Types. [Useful Doc](https://doc.rust-lang.org/beta/nightly-rustc/rustc_middle/ty/enum.TyKind.html#variant.Adt)
- We could ignore `Generator` variant even if it was present. Because it is "extra-unstable".

### Shallow Init Box
- Appears as a variant of `Rvalue`
- Couldn't trace it in the compiler.
- The most useful information is the MIRAI's comment on this type:
    > This is different from a normal transmute because dataflow analysis will treat the box as initialized but its content as uninitialized.
    >
    > `Box.0 = Unique, Unique.0 = NonNullPtr, NonNullPtr.0 = source thin pointer`
- As we haven't decided yet about boxes, we drop support for this type of Rvalue at least for now.

## Operand

### Const

#### Bool
Booleans, numeric types, chars, pointers, and fn defs are expressed as `Scalar` in the consts. Source: `Display` implementation for scalar type. Or easier: the constants `ScalarInt::TRUE` and `FALSE`.