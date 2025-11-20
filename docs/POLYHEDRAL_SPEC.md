# Polyhedral Loop Transformation Spec

This document outlines the specification for loop transformations supported by `caten.polyhedral`. These transformations operate on the Schedule Tree structure.

## 1. Basic Loop Transformations

These transformations manipulate the `Band` nodes of the schedule tree.

### `Band.interchange(dim1, dim2)`
*   **Description**: Swaps two loops within a band.
*   **Effect**: Changes the execution order. Useful for improving locality (e.g., bringing a stride-1 dimension innermost) or enabling parallelism (moving parallel loop outwards).
*   **Validity**: Requires dependence checking to ensure no lexicographical dependence is violated.

### `Band.skew(dim, factor, target_dim)`
*   **Description**: Shifts the iteration space of `dim` by `factor * target_dim`.
*   **Effect**: Changes the loop bounds and shape (e.g., rectangular to parallelogram). Used to enable tiling (wavefront parallelism) for loops with dependences.

### `Band.split(pos)` / `Band.fission`
*   **Description**: Splits a single band node into nested band nodes at `pos`.
*   **Effect**: Allows treating inner dimensions separately (e.g., to apply different transformations).

### `Band.tile(sizes)`
*   **Description**: Applies loop tiling (blocking) to the band.
*   **Mechanism**:
    *   **Static Sizes**: Can be strictly modeled in ISL if sizes are constants.
    *   **Parametric/Symbolic Sizes**: Implemented via **Strip-mining** deferred to the AST generation phase (see `PROJECT.md`). The Band node is split into "Tile Loops" and "Point Loops".
*   **Effect**: Improves cache locality and coarse-grained parallelism.

### `Band.unroll(dim, factor)`
*   **Description**: Fully or partially unrolls a loop dimension.
*   **Mechanism**: Marked in the Schedule Tree. ISL AST generator emits unrolled C code.
*   **Effect**: Reduces loop overhead and enables instruction-level parallelism (ILP).

## 2. Advanced Transformations (Fusion & Rescheduling)

These transformations involve structural changes to the Schedule Tree (Sequence nodes, Domain filtering).

### `Fusion (Compute-at)`
*   **Target**: Two kernels (statements) with a producer-consumer relationship.
*   **Algorithm**: **Consumer-Driven Embedding**.
    1.  Identify the Consumer loop band $\vec{j}$.
    2.  Calculate the Pre-image of dependencies to find the Producer slice $S_P(\vec{j})$.
    3.  Restructure the tree:
        *   Create a `SequenceNode` inside the Consumer's loop body.
        *   **Child 1**: Producer computation restricted to $S_P(\vec{j})$.
        *   **Child 2**: Consumer computation for $\vec{j}$.
*   **Effect**: Improves locality by producing data immediately before consumption, potentially keeping data in registers/L1 cache and avoiding global memory round-trips.

### `Distribution (Loop Fission)`
*   **Target**: A single loop nest containing multiple independent statements.
*   **Algorithm**: Split the statements into separate loops (SequenceNode of Bands).
*   **Effect**: Reduces register pressure or separates memory-bound and compute-bound parts.

### `Index Set Splitting`
*   **Description**: Splits a loop iteration domain into pieces (e.g., Boundary vs. Core).
*   **Effect**: Removes conditional checks (`if` statements) from the inner "Core" loop, enabling vectorization.

## 3. API Design (Draft)

```python
import caten.polyhedral as P

# Example: Creating a schedule
with P.domain("{ S[i,j] : 0 <= i,j < N }") as dom:
    with P.band("{ S[i,j] -> [i,j] }") as band:
        # Transformations
        band.tile([32, 32]) # Tiles i,j
        
        # Accessing child nodes
        outer, inner = band.split(1) # Split tile loops and point loops
        
        inner.unroll(1) # Unroll the j-point loop
        
# Codegen
ast = dom.get_ast()
print(ast.to_c())
```
