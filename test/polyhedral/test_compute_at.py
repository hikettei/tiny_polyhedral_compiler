import caten.isl as I
import caten.polyhedral as P


def test_compute_at():
    N, C, H, W = 1, 1, 4, 4
    Pool_S = 2
    H_out, W_out = H // Pool_S, W // Pool_S
    
    conv_dom_str = f"{{ Conv[n, c, h, w] : 0 <= n < {N} and 0 <= c < {C} and 0 <= h < {H} and 0 <= w < {W} }}"
    pool_dom_str = f"{{ Pool[n, c, ph, pw] : 0 <= n < {N} and 0 <= c < {C} and 0 <= ph < {H_out} and 0 <= pw < {W_out} }}"
    
    with I.context():
        writes_conv = I.UnionMap("{ Conv[n,c,h,w] -> Out[n,c,h,w] }")
        reads_pool = I.UnionMap("{ Pool[n,c,ph,pw] -> Out[n,c,h,w] : 2*ph <= h < 2*ph + 2 and 2*pw <= w < 2*pw + 2 }")

        # Setup Independent Domains
        with P.domain(conv_dom_str) as conv:
            conv.access(writes=writes_conv)
            
        with P.domain(pool_dom_str) as pool:
            pool.access(reads=reads_pool)
            # Identity schedule for Pool
            with P.band("{ Pool[n,c,ph,pw] -> [n,c,ph,pw] }"):
                pass
        
        # Compute At
        fused = conv.compute_at(pool)
        
        print(fused.schedule)
        s_str = str(fused.schedule)
        
        assert "Conv" in s_str
        assert "Pool" in s_str
        # Should share common band
        assert "schedule" in s_str
        
        # Codegen
        c_code = P.to_c(fused.schedule)
        print(c_code)
        assert "for" in c_code
