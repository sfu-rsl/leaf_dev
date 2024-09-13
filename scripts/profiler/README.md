### Profiling
The project includes a script to automatically setup profiling for you

1. **Profiling Setup**

   The profiling setup is managed through the `scripts/profiler/profile-setup.py` file. This script is responsible for:

   - **Inserting Code**: Automatically instrumenting functions inside the code.
   - **Checkout**: Checks out to a temporary branch where you can profile code and make changes
   - **Stashing Changes**: Stashes unstaged/ uncommitted changes in a separate stash
   - **Committing**: Creates a separate commit for the automatic insertions 
   - **Pop the Stash**: Apply the stashed changes
   - **Compiling a Test Program**: Builds a test program to gather profiling data.
     - Compiles a Flamegraph `tracing.folded` in the root directory for the sample rust program. 
     - Visualize it on tools like [flamegraph-viewer](https://flamegraph.com/)

   You can run the profiling setup by executing the following command:

   ```bash
   python scripts/profiler/profile-setup.py
   ```

#### Next Steps

After the successful execution of the script, leaf should be instrumented and you can analyze the execution of your programs now.
The workflow goes

##### Workflow

1. **Compile your program** with Leaf as usual

> Note: The runtime library uses feature flags to enable certain functionalities that include 

``` bash
profile_flame 
profile_tracy 
profile_full 
```

> even though it's the script's job to build the runtime when it executes the sample program, it might fail sometimes so make sure runtime library is built with one of these flags. 

2. **Set the logging level**:
    ```bash
    LEAF_LOG=trace,pri=trace,z3=trace
    ```

3. **Run the program** and check the output:

    ```bash
    pri::set_place_address place=42, raw_ptr=140728834465100
    pri::set_place_type_char place=42
    pri::set_place_type_id place=42, type_id=81227397821202392580868386207259837707
    .....

    ```

---

### Environment Variables for Profiling

When executing the sample project for profiling, certain environment variables are set by the script to control the logging and tracing behavior. Here's an explanation of the environment variables being passed to the executable:

#### LEAF_LOG

The `LEAF_LOG` environment variable is set to control the logging levels for different modules . It is set as follows:

```
LEAF_LOG=trace,pri=trace,z3=trace
```

This setting breaks down into three parts:

1. `trace`: Sets the general logging level to 'trace', which is typically the most verbose logging level.
2. `pri=trace`: Sets the logging level for the 'pri' target to 'trace'.
3. `z3=trace`: Sets the logging level for the 'z3' target to 'trace'.


#### ENV_FLAME_GRAPH

The `ENV_FLAME_GRAPH` environment variable controls the name of the flamegraph, if no value is provided it defaults to `tracing.folded`

The script outputs the flamegraph as `tracing.folded` in the same directory where the script was ran.

To visualize the flamegraph you can use [flamegraph-viewer](https://flamegraph.com/)

---

### Profiling with Temporary Branch

When using the profiling script, it automatically creates a **temporary branch** specifically for profiling purposes. During this process, any automatic code insertions made for profiling are committed separately to keep them isolated from your main branch.

### **After Profiling: Cleanup and Push to Upstream**

Once you have finished profiling and want to push your changes upstream, follow these steps to maintain a clean commit history:

1. **Stashing Changes**:
   - If you want to keep your main branch clean, stash the changes made in the temporary branch.
   - Switch to your main branch and apply the stashed changes there.
   - Finally, push the changes to the upstream repository.

   **Example**:
   ```bash
   git stash
   git checkout main
   git stash apply
   git add .
   git commit -m "Your commit message"
   git push origin main
   ```

2. **Committing and Cherry-Picking**:
   - Alternatively, you can commit the changes in the temporary branch.
   - After committing, switch to your main branch and cherry-pick the commit(s) from the temporary branch.

   **Example**:
   ```bash
   git add .
   git commit -m "Commit message in temporary branch"
   git checkout main
   git cherry-pick <commit-hash-from-temp-branch>
   git push origin main
   ```
---

3. **Cargo features**

 This project includes several Cargo features that enable different types of profiling. 

#### Profile Flamegraph

**Feature**: profile-flame

**Description**: Generates a flamegraph for visualizing profiling data.

``` sh
cargo build --features profile_flame

# set the flamegraph file name
#export FLAME_OUTPUT=output.folded
# default flamegraph = tracing.folded

```
#### Profile Tracy

  **Feature**: profile-tracy

  **Description**: Sends profiling data to [Tracy](https://github.com/wolfpld/tracy) for further analysis.

``` sh

cargo build --features profile_tracy

```

#### Profile Full

  **Feature**: profile-full

  **Description**: Enables both profile-flame and profile-tracy features.

``` sh
    cargo build --features profile_full

```

### Understanding Log Output

With the script you will get log output of the sample program, it will look similar to this

```bash
 pri::set_place_address place=44, raw_ptr=140728834466664
 pri::set_place_type_char place=44
 pri::set_place_type_id place=44, type_id=81227397821202392580868386207259837707
 pri::ref_operand_copy place=44
 pri::new_branching_info node_location=25, discriminant=49, discr_bit_size=32, discr_is_signed=false
 pri::take_branch_ow_char info=BranchingInfo(on: 49 as u32 @ 25), non_values=['a']
```

#### General Format

Each line in the tracing output typically follows this format:

```
target::function_name argument_list
```

Where:
- `target` is usually the module name or something else if explicitly set 

> For more details refer to [tracing](https://docs.rs/tracing/latest/tracing/) which is the logging backend 
