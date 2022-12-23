# Reporting vs. Building

## Definitions
The program that leaf generates can communicate and work with the runtime in two fashions. In this page we try to discuss each approach to the best of our knowledge up to now.

### Reporting
A passive program (compiler) that only reports what has happened to the runtime. Tries to handle no state in the compiler. Runtime should handle all the state management.

### Building
A rather active program that creates approapriate expressions using the runtime and then binds them. It also includes reporting. It is inspired by SymCC. In other words, the runtime's interface and its expression builder will be almost the same.

Consider the following program:
```
let x = a + b;
let y = calc(x, c);
```
Which roughly generates the following MIR.
```
_10 = _7 + _8;
_11 = calc(_10, _9);
```
Then the generated additional statements in the program would look like the following.
```
// Reporting
assig_binary_op(10, 7, 8);
assign_return_val(calc, 10, 9);

// Building
_20 = build_addition(_7, _8);
_21 = build_return_val(calc, _20, _9);
```
Which means in the building approach we build an expression for addition, **keep track of that _10 now corresponds to _20**, then use the built expression in place of the usages of that local.

## Challenges
Analyzing challenges will help us get a better insight for the comparison.

### Places
As discussed in Places, we need to handle cases more complicated than simple `Local`s.
For example the following program:
```rust
fn main() -> () {
    let mut a = Test {
        x: [1, 2, 3, 4],
        y: 4
    };
    let b = a.y;
    a.x[2] = 30;
}
```
Generates this MIR:
```
bb0: {
    _2 = [const 1_u32, const 2_u32, const 3_u32, const 4_u32]; // scope 0 at src/main.rs:6:12: 6:24
    Deinit(_1);                      // scope 0 at src/main.rs:5:17: 8:6
    (_1.0: [u32; 4]) = move _2;      // scope 0 at src/main.rs:5:17: 8:6
    (_1.1: u32) = const 4_u32;       // scope 0 at src/main.rs:5:17: 8:6
    _3 = (_1.1: u32);                // scope 1 at src/main.rs:9:13: 9:16
    _4 = const 2_usize;              // scope 2 at src/main.rs:10:9: 10:10
    (_1.0: [u32; 4])[_4] = const 30_u32; // scope 2 at src/main.rs:10:5: 10:16
}
```
As you see:
1. There are constants (as a type of `Operand`).
1. There are two index projections for the local `_1`.
1. These projections may appear as left values or right values.
1. The projections may have a nested/wrapper structure.

### Where is the challenging part at all?
- As an example, for the assignment of `x` in `a`, we have `(_1.0)` (a field projection over the local `_1`) in the left side of the assignment. A bit more complicated one is the last MIR statement that has an indexing projection as well. Either we need to convert them to simple assignments or add first-class support for them in the runtime. The former is almost in possible because of the considerable complications that will be added to the compiler and the programs and more importatntly benefits that we may lose, such as array SMT solving.
- Now that we want to support them in the runtime, how to receive them and how should it work at all? A basic way could be serializing all the information as JSON and passing them as strings to the runtime methods. However, it is not reasonable to have such chunks of static strings for each statement. It will make the generated program large and inefficient.

### Any solution? 🤔
Let's have dedicated methods in runtime for handling places and then use them or their results in combination with other methods. Thus, a totally-passive approach should be left.

Talking more concretely, we consider we have a polymorphic (just for simplicity of this part) method inthe runtime that receives places as input and returns a reference to them to be used in the next expressions. We name it `rp` (`reference_place`).

Let's upgrade the initial example to demonstrate it:
```rust
fn main() {
    let mut a = [1, 2, 3];
    let b = 5;
    let x = a[1] + b;
    a[2] = x;
}
```
The generated MIR (assertions removed):
```
bb0: {
    _1 = [const 1_i32, const 2_i32, const 3_i32]; // scope 0 at src/main.rs:4:17: 4:26
    _2 = const 5_i32;                // scope 1 at src/main.rs:5:13: 5:14
    _5 = const 1_usize;              // scope 2 at src/main.rs:6:15: 6:16
    _4 = _1[_5];                     // scope 2 at src/main.rs:6:13: 6:17
    _6 = _2;                         // scope 2 at src/main.rs:6:20: 6:21
    _7 = CheckedAdd(_4, _6);         // scope 2 at src/main.rs:6:13: 6:21
    
    _3 = move (_7.0: i32);           // scope 2 at src/main.rs:6:13: 6:21
    _8 = _3;                         // scope 3 at src/main.rs:7:12: 7:13
    _9 = const 2_usize;              // scope 3 at src/main.rs:7:7: 7:8
    _1[_9] = move _8;                // scope 3 at src/main.rs:7:5: 7:13
}
```
The augmented program roughly would look like:
```
// Reporting
assign_const(10, 1_i32);
assign_const(11, 2_i32);
...
assign_aggregate(1, [10, 11, 12, 13]);
assign_const(2, 5_i32);
assign_const(5, 1_usize);
_40 = rp("_1[_5]");
assign_use(4, _40);
assign_use(6, 2);
assign_binary_op(7, 4, 6);
assign_use(8, 7);
assign_const(9, 2_usize);
_80 = rp("_1[_9]");
assign_use(_80, 8);

// Building
_11 = build_const(1_i32);
_12 = build_const(2_i32);
...
_10 = build_aggreagate([_11, _12, _13, _14]);
_50 = bulid_const(1_usize);
_40 = rp("_10[_50]");
_60 = build_copy(2);
_70 = build_addition(_40, _60);
_80 = build_copy(_70);
_90 = build_const(2_usize);
_100 = rp("_10[_90]");
_800 = build_move(_80);
assign(_100, _800);
```
Note: Pay attention to what we pass to the runtime methods. Sometimes it is a direct number while sometimes it is a local which means the value it stores.

#### What about `Local`s?
In majority of cases, places are simple locals (with no projection). Therefore, calling `rp` for each place again results in a large program with a basic for each place. Can we work with them directly? It depends on how there references will be expressed. But at least up to now, we aim to express them using numbers. Thus, it is possible to handle them using some encodings. For example, assume we express references as 64 bit integers. Then we can dedicate the first 32 bit numbers to the normal locals, and the rest to the runtime references.

Okay, so we will have this `rp` in the runtime. Is there anything left?

#### What to do with the nested structures (multiple projections)?
We can call `rp` multiple times.

For example:
```
(_1.0: [u32; 4])[_4]
```
Will become:
```
_10 = rp("_1.0: [u32;4]");
_50 = rp("10[_4]");
```
Why not making a powerful `rp` instead that handles everything itself? Because passing complicated structures requires 1) duplication of many compiler structures in a common library between the compiler and the runtime 2) adding complicated MIR statements that generates those structures. The first reason is not a significant issue but the second one is pretty tough if we want to work directly in MIR.

#### Do we need to distinguish between left hand places and the rest that appear at the right hand?
At least for the sake of semantics we need to define two different methods in the runtime interface. They may have the same implementation but this design gives us the freedom to handle them independently if needed. Thus, in the previous example, we will have something like this:
```
_40 = rp_r("_1[_5]");
_80 = rp_l("_1[_9]");
```
Note: Having the next part in mind, probably we use a boolean flag instead. Otherwise, the number of `rp` methods will be doubled.

#### Is this polymorphic `rp` that we assumed before realistic?
No because of the similar reasons mentioned before. In real, the `rp` method corresponds to a group of methods that are made for each type of place.
```
// Reporting
// Target: _40 = rp_r("_1[_5]");
_40 = rp_r_index(1, 5);

// Building
// Target: _40 = rp_r("_10[_50]");
_40 = rp_r_index(_10, _50);
```
This wasn't good enough! Let's check a more nested place.
```rust
fn main() {
    let mut a = Test { x: [10, 20], y: 0 };
    a.x[a.y] = a.x[a.y] + 2;
}

struct Test {
    x: [u32; 2],
    y: usize
}
```
The generated MIR (checks omitted):
```
bb0: {
    _2 = [const 10_u32, const 20_u32]; // scope 0 at src/main.rs:4:27: 4:35
    Deinit(_1);                      // scope 0 at src/main.rs:4:17: 4:43
    (_1.0: [u32; 2]) = move _2;      // scope 0 at src/main.rs:4:17: 4:43
    (_1.1: usize) = const 0_usize;   // scope 0 at src/main.rs:4:17: 4:43
    _4 = const 0_usize;              // scope 1 at src/main.rs:5:20: 5:23
    _3 = (_1.0: [u32; 2])[_4];       // scope 1 at src/main.rs:5:16: 5:24
    _5 = CheckedAdd(_3, const 2_u32); // scope 1 at src/main.rs:5:16: 5:28
    _6 = (_1.1: usize);              // scope 1 at src/main.rs:5:9: 5:12
    (_1.0: [u32; 2])[_6] = move (_5.0: u32); // scope 1 at src/main.rs:5:5: 5:28
}
```
Roughly the additional statements will look like:
```
// Reporting
assign_const(20, 10_u32);
assign_const(21, 20_u32);
assign_aggregate(2, [20, 21]);
_10 = rp_l_field(1, 0);
assign_use(_10, 2);
_11 = rp_l_field(1, 1);
assign_const(11, 0_usize);
assign_const(4, 0_usize);
_30 = rp_r_field(1, 0);
_31 = rp_r_index(_30, 4);
assign_use(3, _31);
assign_const(50, 2_u32);
assign_binary_op(5, 3, 50);
_60 = rp_r_field(1, 1)
assign_use(6, _60);
_100 = rp_l_field(1, 0);
_101 = rp_l_index(_100, 6);
_102 = rp_r_field(5, 0);
assign_use(_101, _102);

// Building
_21 = build_const(10_u32);
_22 = build_const(20_u32);
_20 = build_aggregate([_20, _21]);
_10 = rp_l_field(1, 0);
assign(_10, _20);
_11 = rp_l_field(1, 1);
_110 = build_const(0_usize);
assign(_11, _110);
_40 = bulid_const(0_usize);
_31 = rp_r_field(1, 0);
_30 = rp_r_index(_31, _40);
_51 = bulid_const(2_u32);
_50 = build_binary_op(_30, _51);
_60 = rp_r_field(1, 1);
_100 = rp_l_field(1, 0);
_101 = rp_l_index(_100, 6);
assign(_101, _50);
```

## Verdict
|Reporting|Building|
|------|-----|
|**Advantages**|
|Simpler compiler|Runtime does not need to maintain the state for locals in addition of expressions and constraint solving.|
|No need to maintain the state of locals and their corresponding expressions in the compiler|More functional style|
|Simplicity may give runtimes more freedom and flexibility||
|The generated MIR will be more readable||
|**Disadvantages**|
|Fatter runtime and state management|Not purely functional. There must be assign statements.|
||Not sure if we can express everything in terms of expressions|

## Decision
Reporting