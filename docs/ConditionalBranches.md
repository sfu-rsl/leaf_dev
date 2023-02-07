# Conditional Branches

In MIR, conditional branches are handled using the `SwitchInt` terminator. The jump decision is made based on the int value of a local, called discriminant. In other words, this terminator contains a mapping from int values to basic block indices (branch targets) and an otherwise case.

Boolean discriminants are modeled using 0 and 1 (in fact, only 0 is used and not cases are handled using `Not` unary operator).

Examples:
- Boolean
    ```rust
    if !condition {
        foo();
    }
    ```
    ```
    bb3: {
        _2 = Not(move _3);
        switchInt(move _2) -> [0: bb5, otherwise: bb4];
    }
    ```
- match
    ```rust
    match x % 3 {
        2 => foo(),
        1 => foo(),
        _ => foo(),
    }
    ```
    ```
    bb2: {
        _2 = Rem(move _3, const 3_i32);
        switchInt(_2) -> [2: bb4, 1: bb5, otherwise: bb3];
    }
    ```
- match enum
    ```rust
    match a {
        Bar::First => foo(),
        Bar::Second => foo(),
    }
    ```
    ```
    bb1: {
        _2 = discriminant(_1);
        switchInt(move _2) -> [0: bb4, 1: bb2, otherwise: bb3];
    }
    ```

## Discriminant Types
Although the terminator tries to express the switch using `u128` values, types seem to be considered behind the scenes.

Currently, the following types for the discriminant are detected.
- Booleans
- Integers
- Enum Discriminants (isize)
Update:
Based on the document provided for [`SwitchInt`][switch_int], the discriminant will be of types signed/unsigned integer, char, or bool.

[switch_int]: https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/mir/enum.TerminatorKind.html#variant.SwitchInt

## Important Points
- When in a match expression the arms are exhaustive (no else branch is needed), the otherwise target will be an empty unreachable basic block.


## How to handle them in runtime?
There are two strategies to report branching, i.e. path constraint, to the runtime.

### Strategies
#### 1. Pre-Branching Reporting
In this strategy, we report the discriminant's concrete value as well as the targets. The runtime library will be responsible for comparing the values, detecting the branch that will be taken, and concluding the path constraint.

This approach is similar to what SymCC does; however, in LLVM IR level the jump condition is always a boolean.

Example:
- Boolean
    ```
    // Original
    bb3: {
        _2 = Not(move _3);
        switchInt(move _2) -> [0: bb5, otherwise: bb4];
    }

    // Leaf
    bb1: {
        _2 = Not(move _3);
    }
    bb2: {
        _3 = [(0, 5)]; // targets and values
        notify_switch_int(_2 /* the real value */, 2 /* the local id */, _3, 4 /* otherwise target */);
    }
    bb3: {
        switchInt(move _2) -> [0: bb5, otherwise: bb4];
    }
    ```
- Match with number
    ```
    // Original
    bb3: {
        _2 = Rem(move _3, const 3_i32);
        switchInt(_2) -> [2: bb4, 1: bb5, otherwise: bb6];
    }

    // Leaf
    bb1: {
        _2 = Rem(move _3, const 3_i32);
    }
    bb2: {
        _3 = [(2, 4), (1, 5)]; // targets and values
        notify_switch_int(_2 /* the real value */, 2 /* the local id */, _3, 6 /* otherwise */);
    }
    bb3: {
        switchInt(_2) -> [2: bb4, 1: bb5, otherwise: bb6];
    }
    ```

#### 2. On-Target-Site Reporting
In this strategy, the report is sent to the runtime after the branch is taken. It requires reporting the value of the target branch that is already taken. The otherwise case should report all the other values.

Any shared information such as basic block number will be set in a basic block containing those variables placed on top of the forking (switch-int-terminated) basic block.

Example:
- Boolean
    ```
    // Original
    bb3: {
        _2 = Not(move _3);
        switchInt(move _2) -> [0: bb6, otherwise: bb4];
    }

    // Leaf
    bb3: {
        _2 = Not(move _3);
        switchInt(move _2) -> [0: bb6, otherwise: bb4];
    }
    bb4: {
        _3 = [0]; // all switch values
        notify_otherwise_taken(2 /* the local id */, _3 /* all values */);
    }
    bb5: {
        // the original bb4
    }
    bb6: {
        notify_branch_taken(2 /* the local id */, 0);
    }
    bb7: {
        // the original bb5
    }
    ```
- Match with numbers
    ```
    // Original
    bb3: {
        _2 = Rem(move _3, const 3_i32);
        switchInt(_2) -> [2: bb4, 1: bb5, otherwise: bb6];
    }

    // Leaf
    bb3: {
        _2 = Rem(move _3, const 3_i32);
        switchInt(_2) -> [2: bb4, 1: bb6, otherwise: bb8];
    }
    bb4: {
        notify_branch_taken(2 /* the local id */, 2);
    }
    bb5: {
        // the original bb4
    }
    bb6: {
        notify_branch_taken(2 /* the local id */, 1);
    }
    bb7: {
        // the original bb5
    }
    bb8: {
        _3 = [2, 1]; // all switch values
        notify_otherwise_taken(2 /* the local id */, _3 /* all values */);
    }
    bb9: {
        // the original bb6
    }
    ```

#### Challenges
- In both cases, unless we explicitly determine the discriminant type through different functions, we need to interpret them in the runtime library which doesn't look rational because its against keeping the type information, and we need to perform equivalence checking in the runtime library equivalent to the compiler and the underlying machine code. Different functions means for example in the boolean case there can be corresponding functions for true and false. Also, for enums it also looks rational to have a separate function.<br>
Examples:
    - Pre-Branching
        ```
        bb1: {
            notify_switch_int_bool(_2 /* the real value */, 2 /* the local id */, 5 /* true target */, 4 /* false target */);
        }
        ```
    - On-Target-Site
        ```
        bb1: {
            notify_branch_taken_bool(2 /* the local id */); // true
        }
        ...
        bb3: {
            notify_branch_not_taken_bool(2 /* the local id */); // false
        }
        ```
    This will also help optimizations for the boolean case, which is the most frequent one.

#### Comparison
|Pre-Branching|On-Target-Site|
|------|-----|
|**Advantages**|
|Single basic block gets added.|No need to pass all the target values to the runtime except for the otherwise case.|
|**Disadvantages**|
|Breaks the original forking basic block.|Scattered usage of the discriminant local variable before each target.|
||~~Any shared information for the branching should be replicated in each function call.~~|

#### Conclusion
In both strategies, we may end up adding basic blocks both before the branching block and the target blocks (like what SymCC does however its runtimes don't't use the on-target-site notification.). Therefore, there is not a significant advantage for a strategy over the other.

We will use the on-target-site notifications because it doesn't break the original blocks.