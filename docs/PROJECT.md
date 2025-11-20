# Project Implementation Overview

Caten is a Python-based Polyhedral Compiler framework designed for educational and experimental purposes. It aims to provide a comprehensive, high-level interface for manipulating the Polyhedral Model, enabling users to explore scheduling, loop transformations, and kernel fusion strategies (like Conv+Pool fusion) with ease.

## Architecture Layers

The project is organized into four main layers of abstraction:

### Layer 0: Low-Level Bindings (`caten.isl`)
*   **Role**: Direct Python bindings for the Integer Set Library (ISL).
*   **Features**: Automatically generated from ISL manuals, GC-reachable, typed.
*   **Status**: Implemented.

### Layer 1: Polyhedral Framework (`caten.polyhedral`)
*   **Role**: A high-level Scheduling Language and wrapper around ISL's Schedule Trees and AST.
*   **Goal**: To provide an intuitive Pythonic API (`with P.domain(...)`) for constructing and manipulating schedules without dealing with raw C-types directly.
*   **Key Responsibilities**:
    *   **Schedule Tree Construction**: Context-manager based construction of domains, filters, and bands.
    *   **Transformation API**: Methods for Interchange, Tiling, Unrolling, and Fusion.
    *   **Dependency Analysis**: Tools to compute validity of schedules.
    *   **AST Generation**: Converting schedules to Abstract Syntax Trees with customizable build options.

### Layer 2: Intermediate Representation (`caten.ops`, `caten.kernel`)
*   **Role**: Defines the computation semantics and structure of a single kernel.
*   **`caten.ops`**: A minimal, RISC-style instruction set architecture (ISA) for representing computation graphs (DAGs).
*   **`caten.kernel`**: Represents a compute kernel, holding information about input/output tensors, the computation graph, and the associated Polyhedral Schedule.

### Layer 3: Frontend & Runtime (`caten.tensor`, `caten.runtime`, `caten.render`)
*   **Role**: The user-facing API and execution environment.
*   **`caten.tensor`**: An abstract tensor class supporting various backends (CUDA, Metal, CPU).
*   **`caten.render`**: A backend-agnostic renderer that translates the IR and AST into target-specific code (C, CUDA, Metal Shaders).
*   **`caten.runtime`**: Handles device memory allocation and kernel launching.

---

## Key Implementation Strategies

### 1. Symbolic Tiling Strategy
A common challenge in the Polyhedral Model is tiling loops with symbolic sizes (e.g., `N`) using symbolic tile sizes (e.g., `B`), which results in non-linear constraints not solvable by ISL's core ILP solvers.

**Strategy: Post-Scheduling Tiling**
Caten adopts the "Post-Scheduling" approach:
1.  **Model Phase**: Loops are kept as single logical dimensions in the Polyhedral Model. Tiling is marked as a property or attribute of the Band Node.
2.  **Codegen Phase**: The actual "strip-mining" (splitting one loop into tile-loop and point-loop) is deferred to the AST generation phase. We utilize `isl_ast_build` options or manual AST rewriting to generate the tiled loop structure (e.g., `for (i=0; i<N; i+=B) ...`).

### 2. Kernel Fusion Strategy (e.g., Conv + Pool)
Fusion of kernels with different loop shapes (like Convolution and Pooling) requires complex rescheduling. Caten implements a **Consumer-Driven Embedding** strategy.

**Strategy: Tiled Fusion / Compute-at**
1.  **Dependency Analysis**: Identify the `Read-After-Write` dependency between the Producer (Conv) and Consumer (Pool).
2.  **Pre-image Calculation**: For a given iteration (or tile) of the Consumer $\vec{j}$, calculate the required slice of the Producer's iteration space $S_P(\vec{j})$ using the inverse of the access functions.
    $$ S_P(\vec{j}) = \{ \vec{i} \in D_P \mid \text{Producer}(\vec{i}) \text{ is needed by } \text{Consumer}(\vec{j}) \} $$
3.  **Embedding**: Transform the Schedule Tree to nest the Producer's computation loop *inside* the Consumer's loops (at an appropriate depth to preserve locality and minimize buffer size).
4.  **Buffer Management**: Eliminate intermediate global memory usage by promoting intermediate tensors to registers or shared memory (L1/L2 cache).

### 3. Auto-Scheduling
Future work will include an Auto-Scheduler that explores the space of legal schedules (tiling sizes, loop orders, fusion strategies) to find the theoretically optimal kernel configuration for a given hardware target.
