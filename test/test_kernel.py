import caten as C

def test_tensor():
    print(C.Tensor.from_shape([10, 10], dtype=C.float32))

def test_matmul_kernel():
    @C.kernel()
    def matmul(A: C.Tensor[N, K], B: C.Tensor[K, M]):
        Out = C.Tensor(N, M, dtype=A.dtype)
        with C.range(N) as i:
            with C.range(M) as j:
                acc = C.Const(0.0)
                with C.range(K) as k:
                    acc += + A[i, k] * B[k, j]
                Out[i, j] = C.tanh(acc)
        return Out

    # TODO:
    # 1. VMAP
    # 2. Symbolic
    N = C.param("N")
    tmp = C.randn(N, 10, 10)
    a, b, c = C.randn(10, 10), C.randn(10, 10), C.randn(10, 10)
    c = matmul(a, b, c)
    tmp * c
