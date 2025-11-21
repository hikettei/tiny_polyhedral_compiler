import caten.isl as I
import caten.polyhedral as P
from caten.polyhedral.analysis import compute_dependence_relation, schedule_is_legal_p
from caten.polyhedral.transformations import schedule_node_sequence_full_fuse


def main():
    print("=== Conv2D + Pool2D Fusion (Robust Implementation) ===\n")
    
    # Parameters
    N = 10
    Cin = 16
    Cout = 32
    H_in, W_in = 32, 32
    
    KH_conv, KW_conv = 3, 3
    S_conv = 1
    H_conv = (H_in - KH_conv) // S_conv + 1
    W_conv = (W_in - KW_conv) // S_conv + 1
    
    KH_pool, KW_pool = 2, 2
    S_pool = 2
    H_pool = (H_conv - KH_pool) // S_pool + 1
    W_pool = (W_conv - KW_pool) // S_pool + 1
    
    # Tile sizes
    Tile_H = S_pool
    Tile_W = S_pool
    
    print(f"Conv: {H_conv}x{W_conv}, Pool: {H_pool}x{W_pool}")

    # Domains
    conv_dom_str = f"{{ S_conv[n, k, h, w, c, kh, kw] : 0<=n<{N} and 0<=k<{Cout} and 0<=h<{H_conv} and 0<=w<{W_conv} and 0<=c<{Cin} and 0<=kh<{KH_conv} and 0<=kw<{KW_conv} }}"
    pool_dom_str = f"{{ S_pool[n, k, h, w, rh, rw] : 0<=n<{N} and 0<=k<{Cout} and 0<=h<{H_pool} and 0<=w<{W_pool} and 0<=rh<{KH_pool} and 0<=rw<{KW_pool} }}"

    with I.context():
        # Access Relations (including Reduction Dependencies)
        # Conv Writes: Out (accumulates). Reads In, Weight, Out (for accumulation).
        writes_conv = I.UnionMap(
            f"{{ S_conv[n, k, h, w, c, kh, kw] -> Out[n, k, h, w] : "
            f"0<=n<{N} and 0<=k<{Cout} and 0<=h<{H_conv} and 0<=w<{W_conv} }}"
        )
        reads_conv_acc = I.UnionMap(
            f"{{ S_conv[n, k, h, w, c, kh, kw] -> Out[n, k, h, w] : "
            f"0<=n<{N} and 0<=k<{Cout} and 0<=h<{H_conv} and 0<=w<{W_conv} }}"
        )
        
        reads_pool = I.UnionMap(
            f"{{ S_pool[n, k, h, w, rh, rw] -> Out[n, k, h*{S_pool} + rh, w*{S_pool} + rw] : "
            f"0<=n<{N} and 0<=k<{Cout} and 0<=h<{H_pool} and 0<=w<{W_pool} and 0<=rh<{KH_pool} and 0<=rw<{KW_pool} }}"
        )
        # Pool also accumulates (max reduction)
        writes_pool = I.UnionMap(
            "{ S_pool[n, k, h, w, rh, rw] -> PoolBuf[n, k, h, w] }"
        )
        reads_pool_acc = I.UnionMap(
            "{ S_pool[n, k, h, w, rh, rw] -> PoolBuf[n, k, h, w] }"
        )
        
        all_writes = writes_conv.union(writes_pool)
        all_reads = reads_pool.union(reads_conv_acc).union(reads_pool_acc)
        
        # Initial Schedule
        filters = I.UnionSetList.alloc(2)
        filters = filters.add(I.UnionSet(conv_dom_str))
        filters = filters.add(I.UnionSet(pool_dom_str))
        
        full_dom = I.UnionSet(conv_dom_str).union(I.UnionSet(pool_dom_str))
        sched = I.Schedule.from_domain(full_dom)
        
        root = sched.get_root()
        seq_node = root.child(0).insert_sequence(filters)
        
        conv_filter = seq_node.child(0)
        conv_mupa = I.MultiUnionPwAff.from_union_map(I.UnionMap("{ S_conv[n, k, h, w, c, kh, kw] -> [n, k, h, w, c, kh, kw] }"))
        conv_node = conv_filter.child(0).insert_partial_schedule(conv_mupa)
        
        seq_node = conv_node.parent().parent()
        pool_filter = seq_node.child(1)
        pool_mupa = I.MultiUnionPwAff.from_union_map(I.UnionMap("{ S_pool[n, k, h, w, rh, rw] -> [n, k, h, w, rh, rw] }"))
        pool_node = pool_filter.child(0).insert_partial_schedule(pool_mupa)
        
        initial_sched = pool_node.get_schedule()
        print("--- Initial Schedule ---")
        
        # --- Dependence Analysis ---
        print("Computing Dependence...")
        total_dep, raw, waw, war = compute_dependence_relation(
            read=all_reads,
            write=all_writes,
            schedule=initial_sched
        )
        print("Dependencies Found: RAW={not raw.is_empty()}, WAW={not waw.is_empty()}")
        
        legal = schedule_is_legal_p(initial_sched, total_dep)
        print(f"Initial Schedule Legal? {legal}")
        
        # --- Transformations ---
        def get_seq_from_band(band):
            return band.parent().parent()
            
        # Split Bands
        seq_node = get_seq_from_band(pool_node)
        conv_band = seq_node.child(0).child(0)
        conv_band = conv_band.band_split(2)
        conv_band_2 = conv_band.child(0)
        conv_band_2 = conv_band_2.band_split(2)
        
        # conv_band_2 is Band(HW). Parent is Band(NK). Parent is Filter. Parent is Sequence.
        seq_node = conv_band_2.parent().parent().parent()
        
        pool_band = seq_node.child(1).child(0)
        pool_band = pool_band.band_split(2)
        pool_band_2 = pool_band.child(0)
        pool_band_2 = pool_band_2.band_split(2)
        
        seq_node = pool_band_2.parent().parent().parent()
        
        # Fuse NK
        print("--- Fusing NK ---")
        nk_band = schedule_node_sequence_full_fuse(seq_node)
        
        # Tile Conv HW
        seq_node = nk_band.child(0)
        conv_hw = seq_node.child(0).child(0)
        conv_hw = conv_hw.band_set_permutable(1)
        space = conv_hw.band_get_space()
        
        mv = I.MultiVal.zero(space)
        mv = mv.set_val(0, I.Val.int_from_si(Tile_H))
        mv = mv.set_val(1, I.Val.int_from_si(Tile_W))
        
        conv_tiled = conv_hw.band_tile(mv)
        conv_tiled = conv_tiled.band_scale_down(mv)
        
        # Replace Inner Band
        inner = conv_tiled.child(0)
        replaced = inner.delete()
        new_mupa = I.MultiUnionPwAff.from_union_map(I.UnionMap(f"{{ S_conv[n, k, h, w, c, kh, kw] -> [(h%{Tile_H}), (w%{Tile_W})] }}"))
        conv_tiled_inner = replaced.insert_partial_schedule(new_mupa) # noqa: F841
        
        # conv_tiled_inner is Inner Band. Parent is Outer Band. Parent is Filter. Parent is Sequence.
        seq_node = conv_tiled_inner.parent().parent().parent()
        
        # Fuse HW Tiles
        print("--- Fusing HW Tiles ---")
        hw_tile_band = schedule_node_sequence_full_fuse(seq_node)
        
        # Fuse Inner
        seq_node = hw_tile_band.child(0)
        print("--- Fusing Inner ---")
        inner_band = schedule_node_sequence_full_fuse(seq_node)
        
        final_sched = inner_band.get_schedule()
        
        print("\n=== Generated C Code ===")
        print(P.to_c(final_sched))
        
        # --- Validation ---
        print("\n=== Validation ===")
        legal_fused = schedule_is_legal_p(final_sched, total_dep)
        print(f"Fused Schedule Legal? {legal_fused}")

if __name__ == "__main__":
    main()
