
# Caten.py

WIP: Enpowerments on ISL, Polyhedral Compiler Infrastructure.

```python
@C.kernel()
def gemm(A: caten.Tensor[:, M, N], B: caten.Tensor[:, N, K] C: caten.Tensor[:, M, K]):
  for i in C.range(M):
    for j in C.range(N):
      C.reduction(ADD, A[i:i+10], axis=-1)
      for k in C.range(K):
        # can place @caten.kernel function here
        caten.gemm(A, B, C)
        caten
```

- [ ] `class Kernel`
  - [ ] `class Domain`
- [ ] Syntax Sugar for supporting Symbolic Tile (Loop D is divided by A*B=D)
- [ ] Loop Coalesce Support
- [ ] Less Dependencies (except for ISL)
- [ ] Enhancement on Symbolic Reduction Coincidence Computataion
- [ ] AutoScheduler Infrastructure
- [ ] Memory Layout Optimization
