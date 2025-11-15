
# Caten.py

High Level ISL Wrapper + Polyhedral Compiler Infrastructure.

```python
import caten as C
@C.kernel()
def gemm(A: caten.Tensor[:, M, N], B: caten.Tensor[:, N, K] C: caten.Tensor[:, M, K]):
  for i in C.band(M):
    for j in C.band(N):
      C.reduction(ADD, A[i:i+10], axis=-1)
      for k in C.band(K):
        # can place @caten.kernel function here
        caten.gemm(A, B, C)
# while having smth like
import caten.polyhedral as P
with P.domain():
  for i in P.schedule(...):
    for j in P.schedule(...):
      for k in P.schedule(...):
        P.filter(...)
        
import caten.isl as I
with I.context():
  A = I.Set("A[i, j]") + I.Constraint("0 <= i <= 512", "0 <= j <= 512")
  B = I.Set("B[i, j]") + I.Constraint("0 <= i <= 512", "0 <= j <= 512")
  C = I.Set("C[i, j]") + I.Constraint("0 <= i <= 512", "0 <= j <= 512")
  with P.domain(A | B | C) as dom: # ctx = extra information to generate kernel
    with P.filter(" { A }"):
      with P.band("{ A[i, j] -> [(i)] }"):
        with P.band():
          # ...
    kernel = dom.finalize(op_context=...) # Finalize Schedule Construction
    C.auto_schedule(kernel)
    result = C.retrive_kernel(kernel)
    return result
  # all allocated items inside I.context() is freed?
  
# caten.py provides extended isl schedule_tree
# supporting:
# - Symbolic Tile
# - Reduction Coincidence Computation while detecting parallel legality
# - Symbolic Band Mod (A=B*C)
# - Memory Layout Optimization

```

```python
# Design
[caten/poly_ir] <= Translator => [caten/ops]
^ Extended ISL Schedule Tree

# Note
- caten/polyhedral
- 
```

- [ ] `class Kernel`
  - [ ] `class Domain`
- [ ] Syntax Sugar for supporting Symbolic Tile (Loop D is divided by A*B=D)
- [ ] Loop Coalesce Support
- [ ] Less Dependencies (except for ISL)
- [ ] Enhancement on Symbolic Reduction Coincidence Computataion
- [ ] AutoScheduler Infrastructure
- [ ] Memory Layout Optimization
- [ ] ISL Object GC Extension
- [ ] Ruff, mypy, pytest in CI.
