
# Caten.py

WIP: Enpowerments on ISL, Polyhedral Compiler Infrastructure.

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
