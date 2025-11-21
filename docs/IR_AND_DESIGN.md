# IR & Design Specification

This document outlines the architecture of the Caten compiler, focusing on the Frontend DSL, Intermediate Representation (IR), Polyhedral integration, and Runtime components.

## 1. Frontend API Design (`caten.kernel`)

The frontend exposes a Pythonic DSL that allows users to define tensor computations naturally. It uses **Tracing** (symbolic execution) with a thread-local context stack to capture the computation structure and build the underlying Polyhedral model.

### 1.1 Type System & Tensors
Input tensors are defined using Python type annotations. This allows the compiler to infer shapes, strides, and data types ahead of execution.

```python
import caten as C

# Define Variables (Symbols) for dynamic shapes
# C.symbol / C.symbols -> C.var / C.vars
N, M = C.vars("N M")

# Kernel Definition with parameters
# @C.jit -> @C.kernel()
@C.kernel()
def matmul(
    # A is a N x M matrix of float32
    A: C.Tensor[C.float32, N, M],
    # B is a fixed-size vector
    B: C.Tensor[C.int32, 128]
):
    ...
```

### 1.2 Loop & Control Flow (The Pipeline Syntax)
Loops are defined using `with C.range(...)` blocks. Loop transformations (scheduling directives) are applied using the pipe `|` operator.

*   **`C.range(stop)`**: Iterates from 0 to `stop`.
*   **`C.range(start, stop, step)`**: Standard range.
*   **`| C.parallel`**: Marks the loop for parallel execution (OpenMP/CUDA blocks).
*   **`| C.vectorize`**: Marks the loop for vectorization (SIMD).
*   **`| C.unroll(factor)`**: Unrolls the loop.
*   **`| C.tile(size)`**: Tiles the loop (modifies the schedule tree structure).

```python
# Example: Parallel outer loop, Vectorized inner loop
with (C.range(N) | C.parallel) as i:
    with (C.range(M) | C.vectorize) as j:
        ...
```

### 1.3 Guards & Conditionals
To ensure valid SCoP (Static Control Parts) construction, data-dependent control flow uses specific context managers instead of Python's `if`.

```python
# Execute only when condition is true
with C.when(i < j):
    ...

# Execute in the complement domain of the previous 'when'
with C.otherwise():
    ...
```

### 1.4 Computation & Tracing Mechanism
Assignments like `Out[i, j] = 0.0` are traced via `__setitem__` and `__getitem__`.

**Tracing Mechanism:**
1.  ` @C.kernel()` initializes a thread-local **`BuilderContext`**.
2.  `with C.range(...)` pushes a loop scope onto the context stack.
3.  Arithmetic operations (`+`, `*`) return intermediate `Op` nodes.
4.  `Out[i, j] = expr` invokes `Tensor.__setitem__`.
5.  `__setitem__` accesses the active `BuilderContext` and registers a `STORE` operation, linking it to the current loop domain (e.g., `{ S[i,j] : ... }`) and creating a Statement in the Polyhedral model.

---

## 2. Intermediate Representation (IR)

### 2.1 Instruction Set (`caten.ops`)
Strict RISC-style primitive operations. Complex comparisons are reduced to primitives to simplify backend logic.

*   **Unary**: `NEG`, `RECIP`, `SIN`, `EXP2`, `LOG2`, `SQRT`, `NOT`, `CAST`
*   **Binary**: `ADD`, `MUL`, `IDIV`, `AND`, `OR`, `XOR`, `MAX`, `MOD`
*   **Comparison**: `NEQ` (Not Equal), `LT` (Less Than)
    *   `EQ(a, b)` $\to$ `NOT(NEQ(a, b))`
    *   `LE(a, b)` $\to$ `NOT(LT(b, a))`
    *   *Note: `EQ` and `LE` are removed to minimize primitives.*
*   **Select**: `WHERE` (Ternary select / Mux)
*   **Memory**: `LOAD`, `STORE` (Reflects tensor access)

### 2.2 Polyhedral Model (`caten.polyhedral`)
The execution structure is managed by the ISL Schedule Tree.

*   **Domain**: The set of all iteration vectors `{ S[i,j] : 0<=i<N ... }`.
*   **Schedule Tree**:
    *   `Band`: Represents loops.
    *   `Filter`: Represents guards/statements.
    *   `Sequence`: Represents execution order.
*   **Access Relations**: Maps from iteration space to memory space.

---

## 3. Core Class Design & Implementation Files

### `caten.kernel.Kernel` (`caten/kernel.py`)
*   **Responsibility**: Represents a single compiled unit of computation (SCoP).
*   **Usage**: Created by the ` @C.kernel` decorator.
*   **Components**:
    *   `schedule`: `caten.polyhedral.ScheduleTree` driving the execution.
    *   `ir_graph`: DAG of `caten.ops` representing the computation body.
*   **Methods**:
    *   `lower(target="c")`: Returns target-specific source code string.
    *   `compile()`: JIT compiles and returns a callable Python executable.

### `caten.tensor.Tensor` (`caten/tensor.py`)
*   **Responsibility**: High-level user interface and tracing proxy.
*   **Properties**:
    *   `shape`: Tuple of dimensions (int or Symbol).
    *   `dtype`: Data type (e.g., `C.float32`).
    *   `strides`: Computed from shape (Row-major default).
*   **Methods**:
    *   `__getitem__`: Emits `LOAD` op.
    *   `__setitem__`: Emits `STORE` op to the active builder context.

### `caten.render.Renderer` (`caten/render.py`)
*   **Responsibility**: Visiting the ScheduleTree and IR Graph to emit source code.
*   **Subclasses**: `CRenderer`, `CUDARenderer`.
*   **Input**: `isl_ast_node` (from Polyhedral codegen) + `ops` graph.
*   **Example**:
    ```python
    renderer = CUDARenderer(kernel)
    source_code = renderer.render()
    ```

### `caten.runtime.Runtime` (`caten/runtime.py`)
*   **Responsibility**: Hardware abstraction layer for memory management and kernel launching.
*   **Methods**:
    *   `malloc(size)`: Allocate device memory.
    *   `copy_to_device(host_ptr, dev_ptr, size)`: Host to Device transfer.
    *   `copy_to_host(dev_ptr, host_ptr, size)`: Device to Host transfer.
    *   `launch(kernel_name, args, grid, block)`: Launch compiled kernel.

---

## 4. Examples

### Example 1: Matrix Multiplication
```python
N, M, K = C.vars("N M K")

@C.kernel()
def matmul(
    A: C.Tensor[C.f32, N, K], 
    B: C.Tensor[C.f32, K, M],
    Out: C.Tensor[C.f32, N, M]
):
    # Initialize
    with (C.range(N) | C.parallel) as i:
        with (C.range(M) | C.vectorize) as j:
            Out[i, j] = 0.0
            
            # Reduction
            with C.range(K) as k:
                # Traced as: Load(Out) + Load(A)*Load(B) -> Store(Out)
                Out[i, j] += A[i, k] * B[k, j]
```

### Example 2: Conv2D + Pool2D Fusion (Manual Schedule View)
This demonstrates how the DSL captures structure, which can then be optimized (fused) by the backend.

```python
@C.kernel()
def conv_pool(
    In: C.Tensor[C.f32, N, H, W, C],
    W: C.Tensor[C.f32, K, K, C, F],
    Out: C.Tensor[C.f32, N, H/2, W/2, F]
):
    # 1. Conv Layer
    # Intermediate buffer (conceptually)
    ConvOut = C.alloc([N, H, W, F]) 
    
    with C.range(N) as n, C.range(H) as h, C.range(W) as w, C.range(F) as f:
        acc = 0.0
        with C.range(K) as kh, C.range(K) as kw, C.range(C) as c:
            acc += In[n, h+kh, w+kw, c] * W[kh, kw, c, f]
        ConvOut[n, h, w, f] = acc

    # 2. Pool Layer
    with C.range(N) as n, C.range(H/2) as h, C.range(W/2) as w, C.range(F) as f:
        max_val = -1e30
        with C.range(2) as ph, C.range(2) as pw:
            val = ConvOut[n, h*2+ph, w*2+pw, f]
            max_val = C.max(max_val, val)
        Out[n, h, w, f] = max_val
```
*Note: The compiler backend will analyze dependencies between `ConvOut` writes and reads, and apply `compute_at` to fuse these loops automatically or via user directives.
