////! This is actually TyKind in MIR.
//use serde::{Deserialize, Serialize};
//
///// https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/ty/sty/enum.TyKind.html
//#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
//pub enum TyKind {
//    /// The primitive boolean type. Written as `bool`.
//    Bool,
//
//    /// The primitive character type; holds a Unicode scalar value
//    /// (a non-surrogate code point). Written as `char`.
//    Char,
//
//    /// A primitive signed integer type. For example, `i32`.
//    Int(IntTy),
//
//    /// A primitive unsigned integer type. For example, `u32`.
//    Uint(UintTy),
//
//    /// A primitive floating-point type. For example, `f64`.
//    Float(FloatTy),
//
//    /// Algebraic data types (ADT). For example: structures, enumerations and unions.
//    ///
//    /// For example, the type `List<i32>` would be represented using the `AdtDef`
//    /// for `struct List<T>` and the substs `[i32]`.
//    ///
//    /// Note that generic parameters in fields only get lazily substituted
//    /// by using something like `adt_def.all_fields().map(|field| field.ty(tcx, substs))`.
//    Adt(AdtDef, SubstsRef),
//
//    /// An unsized FFI type that is opaque to Rust. Written as `extern type T`.
//    Foreign(DefId),
//
//    /// The pointee of a string slice. Written as `str`.
//    Str,
//
//    /// An array with the given length. Written as `[T; N]`.
//    Array(Ty, Const),
//
//    /// The pointee of an array slice. Written as `[T]`.
//    Slice(Ty),
//
//    /// A raw pointer. Written as `*mut T` or `*const T`
//    RawPtr(TypeAndMut),
//
//    /// A reference; a pointer with an associated lifetime. Written as
//    /// `&'a mut T` or `&'a T`.
//    Ref(Region, Ty, Mutability),
//
//    /// The anonymous type of a function declaration/definition. Each
//    /// function has a unique type.
//    ///
//    /// For the function `fn foo() -> i32 { 3 }` this type would be
//    /// shown to the user as `fn() -> i32 {foo}`.
//    ///
//    /// For example the type of `bar` here:
//    /// ```rust
//    /// fn foo() -> i32 { 1 }
//    /// let bar = foo; // bar: fn() -> i32 {foo}
//    /// ```
//    FnDef(DefId, SubstsRef),
//
//    /// A pointer to a function. Written as `fn() -> i32`.
//    ///
//    /// Note that both functions and closures start out as either
//    /// [FnDef] or [Closure] which can be then be coerced to this variant.
//    ///
//    /// For example the type of `bar` here:
//    ///
//    /// ```rust
//    /// fn foo() -> i32 { 1 }
//    /// let bar: fn() -> i32 = foo;
//    /// ```
//    FnPtr(PolyFnSig),
//
//    /// A trait object. Written as `dyn for<'b> Trait<'b, Assoc = u32> + Send + 'a`.
//    Dynamic(&List<Binder<ExistentialPredicate>>, Region),
//
//    /// The anonymous type of a closure. Used to represent the type of `|a| a`.
//    ///
//    /// Closure substs contain both the - potentially substituted - generic parameters
//    /// of its parent and some synthetic parameters. See the documentation for
//    /// [ClosureSubsts] for more details.
//    Closure(DefId, SubstsRef),
//
//    /// The anonymous type of a generator. Used to represent the type of
//    /// `|a| yield a`.
//    ///
//    /// For more info about generator substs, visit the documentation for
//    /// [GeneratorSubsts].
//    Generator(DefId, SubstsRef, Movability),
//
//    /// A type representing the types stored inside a generator.
//    /// This should only appear as part of the [GeneratorSubsts].
//    ///
//    /// Note that the captured variables for generators are stored separately
//    /// using a tuple in the same way as for closures.
//    ///
//    /// Unlike upvars, the witness can reference lifetimes from
//    /// inside of the generator itself. To deal with them in
//    /// the type of the generator, we convert them to higher ranked
//    /// lifetimes bound by the witness itself.
//    ///
//    /// Looking at the following example, the witness for this generator
//    /// may end up as something like `for<'a> [Vec<i32>, &'a Vec<i32>]`:
//    ///
//    /// ```rust
//    /// |a| {
//    ///     let x = &vec![3];
//    ///     yield a;
//    ///     yield x[0];
//    /// }
//    /// ```
//    GeneratorWitness(Binder<&List<Ty>>),
//
//    /// The never type `!`.
//    Never,
//
//    /// A tuple type. For example, `(i32, bool)`.
//    Tuple(&List<Ty>),
//
//    /// The projection of an associated type. For example,
//    /// `<T as Trait<..>>::N`.
//    Projection(ProjectionTy),
//
//    /// Opaque (`impl Trait`) type found in a return type.
//    ///
//    /// The `DefId` comes either from
//    /// * the `impl Trait` ast::Ty node,
//    /// * or the `type Foo = impl Trait` declaration
//    ///
//    /// For RPIT the substitutions are for the generics of the function,
//    /// while for TAIT it is used for the generic parameters of the alias.
//    ///
//    /// During codegen, `tcx.type_of(def_id)` can be used to get the underlying type.
//    Opaque(DefId, SubstsRef),
//
//    /// A type parameter; for example, `T` in `fn f<T>(x: T) {}`.
//    Param(ParamTy),
//
//    /// Bound type variable, used to represent the `'a` in `for<'a> fn(&'a ())`.
//    ///
//    /// For canonical queries, we replace inference variables with bound variables,
//    /// so e.g. when checking whether `&'_ (): Trait<_>` holds, we canonicalize that to
//    /// `for<'a, T> &'a (): Trait<T>` and then convert the introduced bound variables
//    /// back to inference variables in a new inference context when inside of the query.
//    ///
//    /// See the `rustc-dev-guide` for more details about
//    /// [higher-ranked trait bounds][1] and [canonical queries][2].
//    ///
//    /// [1]: https://rustc-dev-guide.rust-lang.org/traits/hrtb.html
//    /// [2]: https://rustc-dev-guide.rust-lang.org/traits/canonical-queries.html
//    Bound(DebruijnIndex, BoundTy),
//
//    /// A placeholder type, used during higher ranked subtyping to instantiate
//    /// bound variables.
//    Placeholder(PlaceholderType),
//
//    /// A type variable used during type checking.
//    ///
//    /// Similar to placeholders, inference variables also live in a universe to
//    /// correctly deal with higher ranked types. Though unlike placeholders,
//    /// that universe is stored in the `InferCtxt` instead of directly
//    /// inside of the type.
//    Infer(InferTy),
//
//    /// A placeholder for a type which could not be computed; this is
//    /// propagated to avoid useless error messages.
//    Error(DelaySpanBugEmitted),
//}
//
//#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
//pub enum IntTy {
//    Isize,
//    I8,
//    I16,
//    I32,
//    I64,
//    I128,
//}
//
//#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
//pub enum UintTy {
//    Usize,
//    U8,
//    U16,
//    U32,
//    U64,
//    U128,
//}
//
//#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
//pub enum FloatTy {
//    F32,
//    F64,
//}
//
//#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
//pub struct AdtDef {
//    variants: Vec<VariantDef>,
//    flags: AdtFlags,
//}
//
//#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
//pub struct VariantDef {
//    pub name: Symbol,
//    pub discr: VariantDiscr,
//    pub fields: Vec<FieldDef>,
//    flags: VariantFlags,
//}
//
//#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
//pub enum VariantDiscr {
//    Explicit(Ty),
//    Relative(u32),
//}
//
//#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
//pub struct FieldDef {
//    pub name: Symbol,
//    pub ty: Ty,
//}
//
//#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
//pub struct VariantFlags {
//    bits: u32,
//}
//
//#[derive(Copy, Clone, Debug, Serialize, Deserialize)]
//pub struct AdtFlags {
//    bits: u32,
//}
