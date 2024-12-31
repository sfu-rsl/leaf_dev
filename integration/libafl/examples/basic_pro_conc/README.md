A basic example of hybrid fuzzing which particularly benefits from concolic execution.

You can look at the project as a regular library, with fuzz targets generated using `cargo fuzz` under fuzz.

To perform hybrid fuzzing, you need to compile the fuzz target once using leaf and once regularly using `cargo fuzz`.
Use a separate target directory when compiling with leaf then provide the generated instrumented version of the program
to the fuzz target. 