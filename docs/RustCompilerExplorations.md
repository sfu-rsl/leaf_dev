# Rust Compiler Exploration
As the rust compiler is not well-documented and fairly unstable, lots of facts and behaviors should be understood through reading the source codes, (hopefully) comments in them, and compiling various test programs. In this document, we write whatever we find.

## Source Code

MIR generation codes are located at `rustc_mir_build::build`.

## Rvalue

### Binary
#### Offset
Still not obvious how this type is generated. Checked places:
- MIRAI tests which use `std::intrinsics::offset`
- `as_rvalue.rs` in MIR build.
- Lots of plays with `pointer::offset`, slices, arrays, ...

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

#### Slice
Slice constants are restricted to be of type `&str` or `&[u8]`. As of yet, I have not been able to find any way of creating a u8 variable that is turned into a string const.

- Byte arrays (`b"abc"` for example) are represented using `ConstValue::Scalar`
- I found reference to a constant reference from a raw pointer here https://github.com/rust-lang/rust/issues/105536
  - this was represented using `ConstValue::ZeroSized`
  - the assignment code can be seen below

```rust
const CONST_U8_REF: &[u8] = unsafe { std::slice::from_raw_parts(3 as *const u8, 0) };
```

## Ty (Types)

### Function
- `TyCtxt::type_of` returns the function type. Its kind is `TyKind::FnDef`.
- To get the return type we can query its signature using `TyCtxt::fn_sig`.

## SwitchInt
- `false` is considered as 0 in a fashion hard-coded. Take a look at the implementation of `TerminatorKind::if_`.