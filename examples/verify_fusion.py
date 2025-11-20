import numpy as np

import caten.isl as I
import caten.polyhedral as P


def verify_correctness():
    # Parameters
    N, C, H, W = 1, 1, 6, 6
    Pool_S = 2
    Pool_K = 2 # Kernel size
    # Output size
    H_out = (H - Pool_K) // Pool_S + 1
    W_out = (W - Pool_K) // Pool_S + 1
    
    print(f"Input: {H}x{W}, Pool: K={Pool_K}, S={Pool_S} => Output: {H_out}x{W_out}")

    # Input Data
    input_data = np.arange(N*C*H*W).reshape(N,C,H,W).astype(np.float32)
    
    # 1. Naive Implementation (Reference)
    # Conv: 1x1 kernel, stride 1, pad 0
    conv_out = np.zeros((N, C, H, W), dtype=np.float32)
    for n in range(N):
        for c in range(C):
            for h in range(H):
                for w in range(W):
                    conv_out[n,c,h,w] = input_data[n,c,h,w] * 1.0 # 1x1 conv
    
    # Pool: Max pool 2x2
    pool_out_ref = np.zeros((N, C, H_out, W_out), dtype=np.float32)
    for n in range(N):
        for c in range(C):
            for ph in range(H_out):
                for pw in range(W_out):
                    # Window
                    h_start = ph * Pool_S
                    w_start = pw * Pool_S
                    window = conv_out[n, c, h_start:h_start+Pool_K, w_start:w_start+Pool_K]
                    pool_out_ref[n,c,ph,pw] = np.max(window)

    print("Reference Output computed.")

    # 2. Fused Implementation (Manual Simulation of Expected Schedule)
    # Loop over Pool indices, compute necessary Conv on demand
    pool_out_fused = np.zeros((N, C, H_out, W_out), dtype=np.float32)
    
    for n in range(N):
        for c in range(C):
            for ph in range(H_out):
                for pw in range(W_out):
                    # Fused body:
                    h_start = ph * Pool_S
                    w_start = pw * Pool_S
                    
                    local_max = -float("inf")
                    
                    # Iterate over Kernel window (Producer Loop embedded)
                    for kh in range(Pool_K):
                        for kw in range(Pool_K):
                            h = h_start + kh
                            w = w_start + kw
                            
                            # Compute Conv(n,c,h,w)
                            val = input_data[n,c,h,w] * 1.0
                            
                            # Update Pool
                            if val > local_max:
                                local_max = val
                                
                    pool_out_fused[n,c,ph,pw] = local_max

    print("Fused Output computed.")
    
    # Check
    if np.allclose(pool_out_ref, pool_out_fused):
        print("SUCCESS: Manual Fused implementation matches Reference.")
    else:
        print("FAILURE: Manual Fused implementation mismatch!")
        return

    # 3. Polyhedral Schedule Verification
    print("\n--- Checking Caten Schedule ---")
    conv_dom_str = f"{{ Conv[n, c, h, w] : 0 <= n < {N} and 0 <= c < {C} and 0 <= h < {H} and 0 <= w < {W} }}"
    pool_dom_str = f"{{ Pool[n, c, ph, pw] : 0 <= n < {N} and 0 <= c < {C} and 0 <= ph < {H_out} and 0 <= pw < {W_out} }}"
    
    with I.context():
        writes_conv = I.UnionMap(f"{{ Conv[n,c,h,w] -> Out[n,c,h,w] : 0 <= n < {N} and 0 <= c < {C} and 0 <= h < {H} and 0 <= w < {W} }}")
        reads_pool = I.UnionMap(f"{{ Pool[n,c,ph,pw] -> Out[n,c,h,w] : 0 <= n < {N} and 0 <= c < {C} and 0 <= ph < {H_out} and 0 <= pw < {W_out} and {Pool_S}*ph <= h < {Pool_S}*ph + {Pool_K} and {Pool_S}*pw <= w < {Pool_S}*pw + {Pool_K} }}")
        
        with P.domain(conv_dom_str) as conv:
            conv.access(writes=writes_conv)
            with P.band("{ Conv[n,c,h,w] -> [n,c,h,w] }"):
                pass
            
        with P.domain(pool_dom_str) as pool:
            pool.access(reads=reads_pool)
            with P.band("{ Pool[n,c,ph,pw] -> [n,c,ph,pw] }"):
                pass
            
        # Fusion
        fused = conv.compute_at(pool)
        c_code = P.to_c(fused.schedule)
        print(c_code)
        
        # Verification logic
        # Ensure loops exist
        assert "for" in c_code
        
        # Ensure strided access logic exists in C code
        # e.g. 2 * c2
        stride_check = f"{Pool_S} *"
        if stride_check not in c_code and f"* {Pool_S}" not in c_code:
             print(f"WARNING: Stride multiplication {Pool_S} not found in code.")
        else:
             print("SUCCESS: Stride logic found.")

if __name__ == "__main__":
    verify_correctness()
