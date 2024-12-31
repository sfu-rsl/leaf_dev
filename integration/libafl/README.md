# Leaf's Integration with LibAFL
This is the information for project management purposes.
Please refer to the documentations if you are looking for how to integrate Leaf
within your fuzzing stack.

The integration consists of the following parts:
- `libafl_leaf`: A helper library containing facilities to generate mutations from a leaf-instrumented program
  or attaching it to a fuzz target.
- Fuzzers: Some fuzzers written based on the helper library for certain use cases.
  For example, `pure_concolic` implements pure concolic execution (looping over all possible diverging inputs)
  on top of LibAFL's infrastructure.
- `libfuzzer`: LibAFL's implementation for `libFuzzer` with support for leaf-instrumented programs to perform hybrid
  fuzzing.

### libFuzzer
We extend LibAFL's implementation of libFuzzer to optionally accept a program for hybrid fuzzing. At the time of writing, LibAFL's repo holds it at `libafl_libfuzzer` directory and we maintain it as a subtree. The subtree lives at
https://github.com/sfu-rsl/LibAFL/tree/libfuzzer.

In order to update the library you need to pull from the aforementioned branch. i.e.:
```
git subtree -P integration/libafl/libfuzzer pull LibAFL libfuzzer
```
where `LibAFL` is the name of the remote pointing to the repo mentioned above. i.e.:
```
$> git remote --verbose
LibAFL  https://github.com/sfu-rsl/LibAFL.git (fetch)
...
```