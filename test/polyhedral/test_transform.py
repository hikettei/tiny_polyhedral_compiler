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
# TODO:
# - Repro of the article
# - test tile (padding)
def test_matmul_dispatcher(matmul):
    with matmul.editor() as mm:
        assert isinstance(mm, P.Dispatcher)
        mm: P.DomainEditor = mm.domain()[0]
        with mm.band() as mm: # todo: interchange
            mm = mm @ [128, 128, 128]
            with mm[0].band() as mm:
                mm @ [32, 32, 32]
                print(mm)
                print(mm.to_c())

# def test_matmul_tile(matmul):
