import pytest
import caten.isl as I
import caten.polyhedral as P
from caten.polyhedral.codegen import to_c

def create_conv_schedule(N, K_out, H_out, W_out, Cin, KH, KW):
    dom_str = f"{{ S_conv[n, k, h, w, c, kh, kw] : 0<=n<{N} and 0<=k<{K_out} and 0<=h<{H_out} and 0<=w<{W_out} and 0<=c<{Cin} and 0<=kh<{KH} and 0<=kw<{KW} }}"
    
    with P.domain(dom_str) as conv:
        # Band 0: [n, k] (Outer parallelism)
        with P.band("{ S_conv[n, k, h, w, c, kh, kw] -> [n, k, h, w, c, kh, kw] }"):
            pass
            
    return conv

def create_pool_schedule(N, K_out, H_out, W_out, KH, KW):
    dom_str = f"{{ S_pool[n, k, h, w, kh, kw] : 0<=n<{N} and 0<=k<{K_out} and 0<=h<{H_out} and 0<=w<{W_out} and 0<=kh<{KH} and 0<=kw<{KW} }}"
    
    with P.domain(dom_str) as pool:
        with P.band("{ S_pool[n, k, h, w, kh, kw] -> [n, k, h, w, kh, kw] }"):
            pass
            
    return pool

def test_conv2d_pool2d_fusion_e2e():
    # Parameters
    N = 10
    Cin = 16
    Cout = 32
    H_in, W_in = 32, 32
    KH_conv, KW_conv = 3, 3
    S_conv = 1
    
    # Conv Output Dims
    H_conv = (H_in - KH_conv) // S_conv + 1
    W_conv = (W_in - KW_conv) // S_conv + 1
    
    KH_pool, KW_pool = 2, 2
    S_pool = 2
    
    # Pool Output Dims
    H_pool = (H_conv - KH_pool) // S_pool + 1
    W_pool = (W_conv - KW_pool) // S_pool + 1
    
    Tile_H = S_pool
    Tile_W = S_pool

    with I.context():
        # 1. Create Schedules
        conv = create_conv_schedule(N, Cout, H_conv, W_conv, Cin, KH_conv, KW_conv)
        pool = create_pool_schedule(N, Cout, H_pool, W_pool, KH_pool, KW_pool)
        
        # Define Access Maps for Dependency Analysis
        # Conv writes to Buf
        # Pool reads from Buf
        # Mapping:
        # Pool(n, k, h, w, kh, kw) reads Buf(n, k, h*S + kh, w*S + kw)
        # Conv(n, k, h, w, ...) writes Buf(n, k, h, w)
        
        conv.access(
            writes=f"{{ S_conv[n, k, h, w, c, kh, kw] -> Buf[n, k, h, w] }}"
        )
        
        pool.access(
            reads=f"{{ S_pool[n, k, h, w, kh, kw] -> Buf[n, k, h*{S_pool} + kh, w*{S_pool} + kw] }}"
        )
        
        # 2. Fuse (Compute At)
        # Fuse Conv into Pool
        # We want to compute Conv tiles required for a Pool tile.
        # Target is Pool.
        
        fused_domain = conv.compute_at(pool)
        
        # 3. Verify Schedule Structure
        # The resulting schedule should have fused outer loops.
        
        # Generate C code to verify structural correctness
        sched = fused_domain.finalize()
        code = to_c(sched.schedule)
        
        # Basic assertions on generated code
        assert "for (int c0 = 0; c0 <= 9; c0 += 1)" in code  # N loop
        assert "for (int c1 = 0; c1 <= 31; c1 += 1)" in code # C_out loop
        assert "S_conv(" in code
        assert "S_pool(" in code
        
        # Check that loops are nested/fused correctly
        # (Checking string containment order is weak but indicative)
        conv_idx = code.find("S_conv")
        pool_idx = code.find("S_pool")
        assert conv_idx != -1
        assert pool_idx != -1
        
        # In a fused loop nest for this case, S_conv should be computed just before S_pool 
        # within the tiling loops.
        
        # Check legality
        assert sched.is_legal()
