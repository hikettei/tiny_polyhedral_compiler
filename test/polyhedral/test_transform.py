import pytest

import caten.isl as I
import caten.polyhedral as P

@pytest.fixture()
def matmul():
    A, B, C = map(I.expr, ("A", "B", "C"))
    zero = I.expr(0)

    with P.parameter("M, N, K"):
        with P.domain("{ WMMA[i,j,k] : 0<=i<M and 0<=j<N and 0<=k<K }") as gemm:
            with P.band("{ WMMA[i,j,k] -> [i, j, k] }"):
                P.stmt("C[j,i] = C[j,i], A[k,i], B[j,k]")[
                    lambda i, j, k: C[j, i].assign(C[j, i] + A[k, i] * B[j, k])
                ]
    return gemm.finalize()

def test_dependency_analysys(matmul):
    print(matmul)
