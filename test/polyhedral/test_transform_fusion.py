import pytest

import caten.isl as I
import caten.polyhedral as P

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

def test_conv_pool_fusion(conv2d, pool2d):
    with (conv2d+pool2d).editor() as kernel:
        print(kernel.model.deps)
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
