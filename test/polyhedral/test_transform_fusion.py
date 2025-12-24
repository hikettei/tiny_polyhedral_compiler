import pytest

import caten.isl as I
import caten.polyhedral as P

@pytest.fixture()
def conv2d():
    out, inp, wei = map(I.expr, ("Out", "In", "W"))
    zero = I.expr(0)
    with P.parameter("N, K_out, H_out, W_out, Cin, KH, KW"):
        d1 = P.domain("{ S_conv_init[n,k,h,w] : 0<=n<N and 0<=k<K_out and 0<=h<H_out and 0<=w<W_out }")
        d2 = P.domain("{ S_conv[n,k,h,w,c,kh,kw] : "
                     "0<=n<N and 0<=k<K_out and 0<=h<H_out and 0<=w<W_out and "
                     "0<=c<Cin and 0<=kh<KH and 0<=kw<KW }")
        with (d1 | d2) as conv:
            with P.filter("{ S_conv_init[n,k,h,w] }"):
                with P.band("{ S_conv_init[n,k,h,w] -> [n, k, h, w] }"):
                    P.stmt("Out[n,k,h,w] = 0")[lambda n, k, h, w: out[n, k, h, w].assign(zero)]
            with P.filter("{ S_conv[n,k,h,w,c,kh,kw] }"):
                with P.band("{ S_conv[n,k,h,w,c,kh,kw] -> [n, k, h, w, c, kh, kw] }"):
                    P.stmt("Out[n,k,h,w] = Out[n,k,h,w], In[n,c,h+kh,w+kw], W[k,c,kh,kw]")[
                        lambda n, k, h, w, c, kh, kw: out[n, k, h, w].assign(out[n, k, h, w] + inp[n, c, h + kh, w + kw] * wei[k, c, kh, kw])]
    return conv.finalize()

@pytest.fixture()
def pool2d():
    S_pool = I.expr(2)
    pool_buf, out = map(I.expr, ("PoolBuf", "Out"))
    neg_inf = I.expr(0) # TODO: This should be negative inf
    with P.parameter("N, K_out, H_pool, W_pool, KH_pool, KW_pool, H_out, W_out"):
        d1 = P.domain(
            "{ S_pool_init[n,k,h,w] : 0<=n<N and 0<=k<K_out and 0<=h<H_pool and 0<=w<W_pool }"
        )
        d2 = P.domain(
            "{ S_pool[n,k,h,w,rh,rw] : "
            "0<=n<N and 0<=k<K_out and 0<=h<H_pool and 0<=w<W_pool and "
            "0<=rh<KH_pool and 0<=rw<KW_pool }"
        )
        with (d1 | d2) as pool:
            with P.filter("{ S_pool_init[n,k,h,w] }"):
                with P.band("{ S_pool_init[n,k,h,w] -> [n, k, h, w] }"):
                    P.stmt("PoolBuf[n,k,h,w] = -INF")[
                        lambda n, k, h, w:
                            pool_buf[n, k, h, w].assign(neg_inf)
                    ]
            # reduction
            with P.filter("{ S_pool[n,k,h,w,rh,rw] }"):
                with P.band("{ S_pool[n,k,h,w,rh,rw] -> [n, k, h, w, rh, rw] }"):
                    P.stmt("PoolBuf[n,k,h,w] = PoolBuf[n,k,h,w], Out[n,k,h*2+rh,w*2+rw]")[
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
        print(kernel)
        print(kernel.to_c())
