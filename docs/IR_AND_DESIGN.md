# IR & Class Design

This document specifies the Intermediate Representation (IR) and the core class responsibilities for Caten.

## 1. Instruction Set Architecture (`caten.ops`)

The IR is designed to be minimal (RISC-style) to simplify the Backend Renderer. Complex operations (like `sin`, `cos`) should be decomposed or handled via high-level constructs in `Tensor`.

### Unary Operations
*   `NEG`: Negation (`-x`)
*   `EXP`: Exponential (`e^x`)
*   `LOG`: Natural logarithm (`ln(x)`)
*   `SQRT`: Square root (`sqrt(x)`)
*   `CAST`: Type casting.

### Binary Operations
*   `ADD`: Addition (`x + y`)
*   `SUB`: Subtraction (`x - y`)
*   `MUL`: Multiplication (`x * y`)
*   `DIV`: Division (`x / y`)
*   `MOD`: Modulo (`x % y`)
*   `MAX`: Maximum (`max(x, y)`)
*   `CMP`: Comparison (returns boolean/int mask).

### Memory Operations
*   `LOAD`: Read from memory (Tensor).
*   `STORE`: Write to memory (Tensor).

### Reduction Operations
*   `SUM`: Sum reduction (can be built from ADD).
*   `PROD`: Product reduction.
*   `MIN`/`MAX`: Min/Max reduction.

### Control Flow (Implicit in Schedule)
The Polyhedral Schedule defines the loops (`FOR`) and conditionals (`IF`). The IR blocks are the "statements" at the leaves of the schedule.

---

## 2. Core Class Design

### `caten.tensor.Tensor`
*   **Responsibility**: High-level user interface.
*   **Properties**:
    *   `shape`: Tuple of dimensions.
    *   `dtype`: Data type.
    *   `device`: Execution target (CPU, CUDA, Metal).
    *   `op`: The operation that produced this tensor (for lazy evaluation).
*   **Methods**:
    *   `schedule()`: Trigger Polyhedral analysis and scheduling for this tensor's computation graph.
    *   `realize()`: Execute the kernel and allocate memory.

### `caten.kernel.Kernel`
*   **Responsibility**: Represents a single compiled unit of computation.
*   **Components**:
    *   `input_tensors`: List of input tensors.
    *   `output_tensors`: List of output tensors.
    *   `schedule`: The `caten.polyhedral.ScheduleTree` driving the execution.
    *   `ir_graph`: The DAG of `ops` representing the computation body.
*   **Methods**:
    *   `optimize()`: Apply transformations (tiling, fusion).
    *   `lower()`: Convert to target-specific code string.

### `caten.polyhedral.ScheduleTree` (Wrappers)
*   **`Domain`**: Root of the tree, defines the iteration space.
*   **`Band`**: Represents a loop nest. Handles tiling, permuting.
*   **`Filter`**: Selects a subset of the domain (or specific statements) for the subtree.
*   **`Sequence`**: Specifies sequential execution order of children.
*   **`Mark`**: Attaches metadata (e.g., "SIMD", "Unroll").

### `caten.render.Renderer`
*   **Responsibility**: Visiting the AST/IR and emitting string code.
*   **Subclasses**: `CRenderer`, `CUDARenderer`, `MetalRenderer`.
*   **Input**: `isl_ast_node` (from Polyhedral) + `ops` graph.
*   **Output**: Source code string.

### `caten.runtime.Runtime`
*   **Responsibility**: Hardware abstraction layer.
*   **Methods**:
    *   `malloc(size)` / `free(ptr)`
    *   `copy_to_device(host_ptr, dev_ptr, size)`
    *   `launch_kernel(kernel_name, args, grid, block)`
