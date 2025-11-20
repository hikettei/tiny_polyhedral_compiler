import caten.isl as I
import caten.polyhedral as P


def test_conv_pool_manual_fusion():
    # Parameters
    N, C, H, W = 1, 1, 4, 4
    Pool_S = 2
    H_out, W_out = H // Pool_S, W // Pool_S
    
    # Domains
    conv_dom_str = f"{{ Conv[n, c, h, w] : 0 <= n < {N} and 0 <= c < {C} and 0 <= h < {H} and 0 <= w < {W} }}"
    pool_dom_str = f"{{ Pool[n, c, ph, pw] : 0 <= n < {N} and 0 <= c < {C} and 0 <= ph < {H_out} and 0 <= pw < {W_out} }}"
    
    # 1. Dependency Analysis (Check connectivity)
    with I.context():
        writes = I.UnionMap("{ Conv[n,c,h,w] -> Out[n,c,h,w] }")
        # Pool reads window 2x2
        reads = I.UnionMap("{ Pool[n,c,ph,pw] -> Out[n,c,h,w] : 2*ph <= h < 2*ph + 2 and 2*pw <= w < 2*pw + 2 }")
        
        # Schedule for dependency analysis: Conv happens before Pool
        dep_schedule = I.UnionMap("{ Conv[n,c,h,w] -> [0]; Pool[n,c,ph,pw] -> [1] }")
        
        dep = P.compute_flow(sink=reads, must_source=writes, schedule=dep_schedule)
        # dep: { Conv[...] -> Pool[...] }
        assert not dep.is_empty()
        
        # 2. Construct Fused Schedule Manually (Simulating compute_at)
        full_domain = f"{conv_dom_str}; {pool_dom_str}"
        
        with P.domain(full_domain) as dom:
            # Outer Band: Common loops [n, c, tile_h, tile_w]
            # Pool: [n, c, ph, pw] -> [n, c, ph, pw]
            # Conv: [n, c, h, w]   -> [n, c, h//2, w//2]
            outer_mupa = f"{{ Pool[n,c,ph,pw] -> [n,c,ph,pw]; Conv[n,c,h,w] -> [n,c, floor(h/{Pool_S}), floor(w/{Pool_S})] }}"
            
            with P.band(outer_mupa):
                # Sequence: Execute Conv slice, then Pool body
                with P.sequence(["{ Conv[n,c,h,w] }", "{ Pool[n,c,ph,pw] }"]) as seq:
                    
                    # Child 1: Producer (Conv)
                    # Remaining loops for Conv: [h%2, w%2]
                    with seq.child(0):
                        with P.band(f"{{ Conv[n,c,h,w] -> [h%{Pool_S}, w%{Pool_S}] }}"):
                            pass
                            
                    # Child 2: Consumer (Pool)
                    with seq.child(1):
                        # No inner loops for Pool point (reduction is inside body)
                        pass
        
        sched = dom.finalize()
        print(sched)
        
        s_str = str(sched)
        assert "sequence" in s_str
        assert "Conv" in s_str
        assert "Pool" in s_str