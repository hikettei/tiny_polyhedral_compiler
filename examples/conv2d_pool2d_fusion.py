import caten.isl as I
import caten.polyhedral as P


def main():
    print("=== Conv2D + Pool2D Fusion via Dimension-wise Scheduling ===\n")
    
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
        # Initial: Set node with both domains
        full_dom = I.UnionSet(conv_dom_str).union(I.UnionSet(pool_dom_str))
        sched = I.Schedule.from_domain(full_dom)
        
        # 1. Fuse N and K dimensions (One-to-One)
        # Map both to [n, k]
        mupa_nk = I.MultiUnionPwAff("[{ S_conv[n,k,h,w,c,rh,rw] -> [(n)]; S_pool[n,k,h,w,rh,rw] -> [(n)] }, { S_conv[n,k,h,w,c,rh,rw] -> [(k)]; S_pool[n,k,h,w,rh,rw] -> [(k)] }]")
        
        root = sched.get_root() # Domain
        # Insert Band(n, k)
        band_nk = root.child(0).insert_partial_schedule(mupa_nk)
        
        # 2. Fuse H and W dimensions via Tiling
        # Map H: Conv->floor(h/2), Pool->h
        aff_h_str = f"[{{ S_conv[n,k,h,w,c,rh,rw] -> [(floor(h/{S_pool}))]; S_pool[n,k,h,w,rh,rw] -> [(h)] }}]"
        mupa_h = I.MultiUnionPwAff(aff_h_str)
        
        # Map W: Conv->floor(w/2), Pool->w
        aff_w_str = f"[{{ S_conv[n,k,h,w,c,rh,rw] -> [(floor(w/{S_pool}))]; S_pool[n,k,h,w,rh,rw] -> [(w)] }}]"
        mupa_w = I.MultiUnionPwAff(aff_w_str)
        
        # Insert Band(h_tile, w_tile) under Band(n, k)
        band_hw = band_nk.child(0).insert_partial_schedule(mupa_h)
        band_hw = band_hw.child(0).insert_partial_schedule(mupa_w)
        
        # Create filters for the Sequence
        filters = I.UnionSetList.alloc(2)
        filters = filters.add(I.UnionSet(conv_dom_str))
        filters = filters.add(I.UnionSet(pool_dom_str))
        
        seq_node = band_hw.child(0).insert_sequence(filters)
        
        # 3. Inner Schedules
        # Sequence Child 0: Conv
        conv_inner_map = I.UnionMap("{ S_conv[n,k,h,w,c,rh,rw] -> [h, w, c, rh, rw] }")
        mupa_conv_inner = I.MultiUnionPwAff.from_union_map(conv_inner_map)
        
        # Insert Band for Conv inner
        # Note: ISL objects are immutable. We get a new node in a new tree.
        conv_band = seq_node.child(0).child(0).insert_partial_schedule(mupa_conv_inner)
        
        # Navigate back to Sequence node in the NEW tree
        # Hierarchy: Sequence -> Filter -> Band(Conv)
        new_seq_node = conv_band.parent().parent()
        
        # Sequence Child 1: Pool
        pool_inner_map = I.UnionMap("{ S_pool[n,k,h,w,rh,rw] -> [rh, rw] }")
        mupa_pool_inner = I.MultiUnionPwAff.from_union_map(pool_inner_map)
        
        # Insert Band for Pool inner
        pool_band = new_seq_node.child(1).child(0).insert_partial_schedule(mupa_pool_inner)
        
        # Get final schedule
        final_sched = pool_band.get_schedule()
        
        # Codegen
        print("\n=== Generated C Code ===")
        c_code = P.to_c(final_sched)
        print(c_code)

if __name__ == "__main__":
    main()
