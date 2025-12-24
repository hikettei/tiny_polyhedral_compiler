import pytest

import caten.isl as I
import caten.polyhedral as P

# === Conv+Pool ====================================================================
S_POOL = 4
H_OUT = 128
W_OUT = 128
H_POOL = (H_OUT - S_POOL) // S_POOL + 1
W_POOL = (W_OUT - S_POOL) // S_POOL + 1

@pytest.fixture()
def conv2d():
    out, inp, wei = map(I.expr, ("Out", "In", "W"))
    zero = I.expr(0)

    with P.parameter("N, K_out, Cin, KH, KW"):
        d1 = P.domain(
            f"{{ S_conv_init[n,k,h,w] : "
            f"0<=n<N and 0<=k<K_out and 0<=h<{H_OUT} and 0<=w<{W_OUT} }}"
        )
        d2 = P.domain(
            f"{{ S_conv[n,k,h,w,c,kh,kw] : "
            f"0<=n<N and 0<=k<K_out and 0<=h<{H_OUT} and 0<=w<{W_OUT} and "
            f"0<=c<Cin and 0<=kh<KH and 0<=kw<KW }}"
        )

        with (d1 | d2) as conv:
            with P.filter("{ S_conv_init[n,k,h,w] }"):
                with P.band("{ S_conv_init[n,k,h,w] -> [n, k, h, w] }"):
                    P.stmt("Out[n,k,h,w] = 0")[
                        lambda n, k, h, w: out[n, k, h, w].assign(zero)
                    ]

            with P.filter("{ S_conv[n,k,h,w,c,kh,kw] }"):
                with P.band("{ S_conv[n,k,h,w,c,kh,kw] -> [n, k, h, w, c, kh, kw] }"):
                    P.stmt("Out[n,k,h,w] = Out[n,k,h,w], In[n,c,h+kh,w+kw], W[k,c,kh,kw]")[
                        lambda n, k, h, w, c, kh, kw:
                            out[n, k, h, w].assign(
                                out[n, k, h, w] + inp[n, c, h + kh, w + kw] * wei[k, c, kh, kw]
                            )
                    ]

    return conv.finalize()


@pytest.fixture()
def pool2d():
    S_pool = I.expr(S_POOL)
    pool_buf, out = map(I.expr, ("PoolBuf", "Out"))
    neg_inf = I.expr("NEG_INF")

    with P.parameter("N, K_out"):
        d1 = P.domain(
            f"{{ S_pool_init[n,k,h,w] : "
            f"0<=n<N and 0<=k<K_out and 0<=h<{H_POOL} and 0<=w<{W_POOL} }}"
        )
        d2 = P.domain(
            f"{{ S_pool[n,k,h,w,rh,rw] : "
            f"0<=n<N and 0<=k<K_out and 0<=h<{H_POOL} and 0<=w<{W_POOL} and "
            f"0<=rh<{S_POOL} and 0<=rw<{S_POOL} }}"
        )

        with (d1 | d2) as pool:
            with P.filter("{ S_pool_init[n,k,h,w] }"):
                with P.band("{ S_pool_init[n,k,h,w] -> [n, k, h, w] }"):
                    P.stmt("PoolBuf[n,k,h,w] = -INF")[
                        lambda n, k, h, w: pool_buf[n, k, h, w].assign(neg_inf)
                    ]

            with P.filter("{ S_pool[n,k,h,w,rh,rw] }"):
                with P.band("{ S_pool[n,k,h,w,rh,rw] -> [n, k, h, w, rh, rw] }"):
                    P.stmt(f"PoolBuf[n,k,h,w] = PoolBuf[n,k,h,w], Out[n,k,h*{S_POOL}+rh,w*{S_POOL}+rw]")[
                        lambda n, k, h, w, rh, rw:
                            pool_buf[n, k, h, w].assign(
                                pool_buf[n, k, h, w].max(
                                    out[n, k, h * S_pool + rh, w * S_pool + rw]
                                )
                            )
                    ]
    return pool.finalize()

def test_conv_pool_manual_fusion(conv2d, pool2d):
    with (conv2d+pool2d).editor() as kernel:
        # Kernel Fusion
        with kernel.domain()[0] as dom:
            print(dom.to_c())
            # maximize fusion?
            with dom.sequence() as nk:
                nk[0].filter()[0].band().split(2)
                nk[1].filter()[0].band().split(2)
                nk[2].filter()[0].band().split(2)
                nk[3].filter()[0].band().split(2)
                nk.fuse()
            dom.band()[0].sequence().reorder(2, 0, 1, 3)
            with dom.band()[0].sequence() as hw:
                hw[0].filter()[0].band().split(2)
                hw[1].filter()[0].band().split(2).tile([S_POOL, S_POOL]).scale_down([S_POOL, S_POOL])
                hw[2].filter()[0].band().split(2).tile([S_POOL, S_POOL]).scale_down([S_POOL, S_POOL])
                hw[3].filter()[0].band().split(2)
                hw.fuse()
            with dom.band()[0].band()[0].sequence().group(1, 3) as fused:
                fused[1].filter()[0].sequence().fuse()
            print(kernel.to_c())
        # Kernel Optimization
# == Flash Attention ==================================================================
@pytest.fixture()
def gemm_qk():
    Q, K, Score = map(I.expr, ("Q", "K", "Score"))
    zero = I.expr(0)

    with P.parameter("M, N, D"):  # DV unused here but keep parameter sets consistent
        d1 = P.domain("{ S_qk_init[i,j] : 0<=i<M and 0<=j<N }")
        d2 = P.domain("{ S_qk[i,j,d] : 0<=i<M and 0<=j<N and 0<=d<D }")

        with (d1 | d2) as qk:
            with P.filter("{ S_qk_init[i,j] }"):
                with P.band("{ S_qk_init[i,j] -> [i, j] }"):
                    P.stmt("Score[i,j] = 0")[
                        lambda i, j: Score[i, j].assign(zero)
                    ]

            with P.filter("{ S_qk[i,j,d] }"):
                with P.band("{ S_qk[i,j,d] -> [i, j, d] }"):
                    P.stmt("Score[i,j] = Score[i,j], Q[i,d], K[j,d]")[
                        lambda i, j, d:
                            Score[i, j].assign(Score[i, j] + Q[i, d] * K[j, d])
                    ]
    return qk.finalize()

@pytest.fixture()
def softmax():
    def _exp(x):
        return I.expr("expr").call(x)
    
    Score, Prob, Tmp, Max, Sum = map(I.expr, ("Score", "Prob", "Tmp", "Max", "Sum"))
    neg_inf = I.expr("NEG_INF")
    zero = I.expr(0)

    with P.parameter("M, N, D, DV"):  # D/DV unused but consistent
        with P.domain("{ S_sm_init_max[i] : 0<=i<M }") as d0:
            with P.filter("{ S_sm_init_max[i] }"):
                with P.band("{ S_sm_init_max[i] -> [i] }"):
                    P.stmt("Max[i] = -INF")[
                        lambda i: Max[i].assign(neg_inf)
                    ]
        
        with P.domain("{ S_sm_max[i,j] : 0<=i<M and 0<=j<N }") as d1:
            with P.filter("{ S_sm_max[i,j] }"):
                with P.band("{ S_sm_max[i,j] -> [i, j] }"):
                    P.stmt("Max[i] = Max[i], Score[i,j]")[
                        lambda i, j: Max[i].assign(Max[i].max(Score[i, j]))
                    ]
        
        with P.domain("{ S_sm_init_sum[i] : 0<=i<M }") as d2:
            with P.filter("{ S_sm_init_sum[i] }"):
                with P.band("{ S_sm_init_sum[i] -> [i] }"):
                    P.stmt("Sum[i] = 0")[
                        lambda i: Sum[i].assign(zero)
                    ]

        with P.domain("{ S_sm_exp_sum[i,j] : 0<=i<M and 0<=j<N }") as d3:
            with P.filter("{ S_sm_exp_sum[i,j] }"):
                with P.band("{ S_sm_exp_sum[i,j] -> [i, j] }"):
                    P.stmt("Tmp[i, j] = Score[i, j], Max[i]")#[
#                        lambda i, j:
#                            (
#                                Tmp[i, j].assign(_exp((Score[i, j] - Max[i]))),
#                            )
#                    ]
                    P.stmt("Sum[i] = Sum[i], Tmp[i, j]")#[
#                        lambda i, j:
#                            (
#                                Sum[i].assign(Sum[i] + Tmp[i, j]),
#                            )
#                    ]

        with P.domain("{ S_sm_norm[i,j] : 0<=i<M and 0<=j<N }") as d4:
            with P.filter("{ S_sm_norm[i,j] }"):
                with P.band("{ S_sm_norm[i,j] -> [i, j] }"):
                    P.stmt("Prob[i,j] = Tmp[i,j], Sum[i]")#[
#                        lambda i, j: Prob[i, j].assign(Tmp[i, j] / Sum[i])
#                   ]
    return d0.finalize() + d1.finalize() + d2.finalize() + d3.finalize() + d4.finalize()

@pytest.fixture()
def gemm_v():
    Prob, V, Out = map(I.expr, ("Prob", "V", "OutAttn"))
    zero = I.expr(0)

    with P.parameter("M, N, D, DV"):
        d1 = P.domain("{ S_pv_init[i,v] : 0<=i<M and 0<=v<DV }")
        d2 = P.domain("{ S_pv[i,v,j] : 0<=i<M and 0<=v<DV and 0<=j<N }")

        with (d1 | d2) as pv:
            with P.filter("{ S_pv_init[i,v] }"):
                with P.band("{ S_pv_init[i,v] -> [i, v] }"):
                    P.stmt("OutAttn[i,v] = 0")[
                        lambda i, v: Out[i, v].assign(zero)
                    ]

            with P.filter("{ S_pv[i,v,j] }"):
                with P.band("{ S_pv[i,v,j] -> [i, v, j] }"):
                    P.stmt("OutAttn[i,v] = OutAttn[i,v], Prob[i,j], V[j,v]")[
                        lambda i, v, j:
                            Out[i, v].assign(Out[i, v] + Prob[i, j] * V[j, v])
                    ]

    return pv.finalize()

def test_flash_attention(gemm_qk, softmax, gemm_v):
    with (gemm_qk+softmax+gemm_v).editor() as flash_attention:
        with flash_attention.domain()[0] as dom:
            with dom.sequence() as seq:
                seq[0].filter()[0].band().split(1)  # [i][j]
                seq[1].filter()[0].band().split(1)  # [i][j,d]
                # seq[2] already [i]
                seq[3].filter()[0].band().split(1)  # [i][j]
                # seq[4] already [i]
                seq[5].filter()[0].band().split(1)  # [i][j]
                seq[6].filter()[0].band().split(1)  # [i][j]
                seq[7].filter()[0].band().split(1)  # [i][v]
                seq[8].filter()[0].band().split(1)  # [i][v,j]
                seq.fuse()
                seq[0].sequence().reorder(0, 1, 2, 3, 4, 5, 6, 7, 8)
        print(dom.to_c())
        # TODO
