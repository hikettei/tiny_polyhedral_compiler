import caten.isl as I
import caten.polyhedral as P


def main():
    print("=== Conv2D + Pool2D Register-Level Fusion ===\n")
    
    # Parameters
    N, Cin, Cout = 1, 1, 1
    H_in, W_in = 6, 6
    KH, KW = 3, 3
    S_conv = 1
    H_conv = (H_in - KH) // S_conv + 1 # 4
    W_conv = (W_in - KW) // S_conv + 1 # 4
    
    KH_pool, KW_pool = 2, 2
    S_pool = 2
    H_pool = (H_conv - KH_pool) // S_pool + 1 # 2
    W_pool = (W_conv - KW_pool) // S_pool + 1 # 2
    
    print(f"Conv: {H_conv}x{W_conv}, Pool: {H_pool}x{W_pool}")

    # Domains
    # S_conv[n, k, h, w, c, rh, rw]
    conv_dom_str = f"{{ S_conv[n, k, h, w, c, rh, rw] : 0<=n<{N} and 0<=k<{Cout} and 0<=h<{H_conv} and 0<=w<{W_conv} and 0<=c<{Cin} and 0<=rh<{KH} and 0<=rw<{KW} }}"
    # S_pool[n, k, h, w, rh, rw]
    pool_dom_str = f"{{ S_pool[n, k, h, w, rh, rw] : 0<=n<{N} and 0<=k<{Cout} and 0<=h<{H_pool} and 0<=w<{W_pool} and 0<=rh<{KH_pool} and 0<=rw<{KW_pool} }}"

    with I.context():
        # Create Schedule from Domains
        full_dom = I.UnionSet(conv_dom_str).union(I.UnionSet(pool_dom_str))
        sched = I.Schedule.from_domain(full_dom)
        
        # --- 1. Global Tile Fusion (Outer Loop) ---
        # Fuse N, K, and Outer H/W
        # Conv(h) maps to Pool(floor(h/2))
        
        # Map [n, k, h_tile, w_tile]
        # Conv: [n, k, floor(h/2), floor(w/2)]
        # Pool: [n, k, h,          w         ]
        
        mupa_outer = I.MultiUnionPwAff(
            f"[{{ S_conv[n,k,h,w,c,rh,rw] -> [(n)]; S_pool[n,k,h,w,rh,rw] -> [(n)] }}, "
            f" {{ S_conv[n,k,h,w,c,rh,rw] -> [(k)]; S_pool[n,k,h,w,rh,rw] -> [(k)] }}, "
            f" {{ S_conv[n,k,h,w,c,rh,rw] -> [(floor(h/{S_pool}))]; S_pool[n,k,h,w,rh,rw] -> [(h)] }}, "
            f" {{ S_conv[n,k,h,w,c,rh,rw] -> [(floor(w/{S_pool}))]; S_pool[n,k,h,w,rh,rw] -> [(w)] }}]"
        )
        
        root = sched.get_root()
        band_outer = root.child(0).insert_partial_schedule(mupa_outer)
        
        # --- 2. Register Fusion (Inner Loop) ---
        # This is the key optimization!
        # Fuse Conv's local spatial offset with Pool's reduction window.
        # Conv Local: h % 2, w % 2
        # Pool Local: rh,    rw
        
        mupa_inner = I.MultiUnionPwAff(
            f"[{{ S_conv[n,k,h,w,c,rh,rw] -> [(h%{S_pool})]; S_pool[n,k,h,w,rh,rw] -> [(rh)] }}, "
            f" {{ S_conv[n,k,h,w,c,rh,rw] -> [(w%{S_pool})]; S_pool[n,k,h,w,rh,rw] -> [(rw)] }}]"
        )
        
        # Insert Inner Band under Outer Band
        band_inner = band_outer.child(0).insert_partial_schedule(mupa_inner)
        
        # --- 3. Sequence (Computation Order) ---
        # Inside the Register Loop:
        # 1. Compute Conv Pixel (Producer)
        # 2. Accumulate to Pool (Consumer)
        
        filters = I.UnionSetList.alloc(2)
        filters = filters.add(I.UnionSet(conv_dom_str))
        filters = filters.add(I.UnionSet(pool_dom_str))
        
        seq_node = band_inner.child(0).insert_sequence(filters)
        
        # --- 4. Local Computation Schedules ---
        
        # Conv: Schedule remaining reduction loops [c, rh, rw]
        conv_local_map = I.UnionMap("{ S_conv[n,k,h,w,c,rh,rw] -> [c, rh, rw] }")
        mupa_conv_local = I.MultiUnionPwAff.from_union_map(conv_local_map)
        
        # Pool: No remaining loops! 
        # (n, k, h, w, rh, rw) are all scheduled in Outer/Inner bands.
        # So S_pool is a scalar statement here.
        
        # Finalize
        # We need to get the schedule from the modified tree.
        # seq_node -> Filter -> Band(Conv Local)
        # But we need the root.
        # Since ISL nodes are copies, we can't just use `sched`.
        # But `insert` returns the new node *in the new tree*.
        # We can traverse up to root from the last modified node?
        # Or simpler: just grab the schedule from the last inserted node.
        
        # seq_node.child(0) is Filter.
        # seq_node.child(0).child(0) is Leaf.
        # insert_partial_schedule returns the new Band node.
        final_node = seq_node.child(0).child(0).insert_partial_schedule(mupa_conv_local)
        final_sched = final_node.get_schedule()
        
        # Codegen
        print("\n=== Schedule Tree ===")
        print(final_sched)
        
        print("\n=== Generated C Code (Register Fusion) ===")
        c_code = P.to_c(final_sched)
        print(c_code)

if __name__ == "__main__":
    main()