# IR & Class Design

This document specifies the Intermediate Representation (IR) and the core class responsibilities for Caten.

## 1. Instruction Set Architecture (`caten.ops`)

The IR follows a strict RISC-style design, minimizing the number of primitives. Complex operations (like `SUB`, `DIV`) are composed of these primitives (e.g., `SUB(a, b)` -> `ADD(a, NEG(b))`).

### Unary Operations (7 Ops)
*   `NEG`: Negation (`-x`)
*   `RECIP`: Reciprocal (`1/x`)
*   `SIN`: Sine (`sin(x)`)
*   `EXP2`: Base-2 Exponential (`2^x`)
*   `LOG2`: Base-2 Logarithm (`log2(x)`)
*   `SQRT`: Square root (`sqrt(x)`)
*   `NOT`: Bitwise/Logical Not (`~x`)

### Binary Operations (8 Ops)
*   `ADD`: Addition (`x + y`)
*   `MUL`: Multiplication (`x * y`)
*   `IDIV`: Integer Division (`x // y`)
*   `AND`: Bitwise And (`x & y`)
*   `OR`: Bitwise Or (`x | y`)
*   `XOR`: Bitwise Xor (`x ^ y`)
*   `MAX`: Maximum (`max(x, y)`)
*   `MOVE`: Data copy/move.

### Ternary / Comparison Operations (3 Ops)
*   `NEQ`: Not Equal (`x != y`)
*   `LT`: Less Than (`x < y`)
*   `WHERE`: Conditional Select / Mux (`cond ? a : b`)

### JIT / Memory Operations (2 Ops)
*   `REF`: Load from memory / Reference.
*   `STORE`: Store to memory.

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