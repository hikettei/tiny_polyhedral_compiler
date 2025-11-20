import caten.isl as I
import caten.polyhedral as P


def test_compute_at():
    # Increase size to prevent ISL from unrolling/splitting into if/else blocks
    # We want to see symbolic strides like "2 * c2"
    N, C, H, W = 1, 1, 16, 16
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
            # Explicit schedule to ensure valid C generation
            with P.band("{ Conv[n,c,h,w] -> [n,c,h,w] }"):
                pass
        
        print("\n--- Conv Schedule ---")
        print(P.to_c(conv.schedule))

        with P.domain(pool_dom_str) as pool:
            pool.access(reads=reads_pool)
            with P.band("{ Pool[n,c,ph,pw] -> [n,c,ph,pw] }"):
                pass
        
        print("\n--- Pool Schedule ---")
        print(P.to_c(pool.schedule))
    
        # Compute At
        fused = conv.compute_at(pool)
        
        print("\n--- Fused Schedule Tree ---")
        print(fused.schedule)
        
        # Codegen
        c_code = P.to_c(fused.schedule)
        print("\n--- Fused C Code ---")
        print(c_code)
        
        assert "for" in c_code
        assert "2 *" in c_code or "* 2" in c_code
