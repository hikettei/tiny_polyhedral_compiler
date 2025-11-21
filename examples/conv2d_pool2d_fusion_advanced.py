import caten.isl as I
import caten.polyhedral as P
from caten.polyhedral.transformations import schedule_node_sequence_full_fuse


def main():
    print("=== Advanced Conv2D+Pool2D Fusion (Iterative Full-Fuse) ===\n")
    
    # Parameters
    N, K_out = 1, 1
    # Conv Input H, W = 6, 6
    # Conv Kernel 3x3, Stride 1 -> Out 4x4
    # Pool Kernel 2x2, Stride 2 -> Out 2x2
    
    # Domains
    conv_dom_str = f"{{ S_conv[n, k, h, w, c, kh, kw] : 0<=n<{N} and 0<=k<{K_out} and 0<=h<4 and 0<=w<4 and 0<=c<1 and 0<=kh<3 and 0<=kw<3 }}"
    pool_dom_str = f"{{ S_pool[n, k, h, w, rh, rw] : 0<=n<{N} and 0<=k<{K_out} and 0<=h<2 and 0<=w<2 and 0<=rh<2 and 0<=rw<2 }}"
    
    with I.context():
        # --- 1. Initial Separate Schedules ---
        filters = I.UnionSetList.alloc(2)
        filters = filters.add(I.UnionSet(conv_dom_str))
        filters = filters.add(I.UnionSet(pool_dom_str))
        
        full_dom = I.UnionSet(conv_dom_str).union(I.UnionSet(pool_dom_str))
        sched = I.Schedule.from_domain(full_dom)
        
        # Insert Sequence
        root = sched.get_root()
        seq_node = root.child(0).insert_sequence(filters)
        
        # Conv Identity: [n, k, h, w, c, kh, kw]
        conv_filter = seq_node.child(0)
        conv_mupa = I.MultiUnionPwAff.from_union_map(I.UnionMap("{ S_conv[n, k, h, w, c, kh, kw] -> [n, k, h, w, c, kh, kw] }"))
        conv_node = conv_filter.child(0).insert_partial_schedule(conv_mupa)
        
        # Child 1 (Pool): [n, k, h, w, rh, rw]
        # We must navigate from the returned conv_node (which is in a new tree)
        seq_node = conv_node.parent().parent()
        pool_filter = seq_node.child(1)
        pool_mupa = I.MultiUnionPwAff.from_union_map(I.UnionMap("{ S_pool[n, k, h, w, rh, rw] -> [n, k, h, w, rh, rw] }"))
        pool_node = pool_filter.child(0).insert_partial_schedule(pool_mupa)
        
        print("--- Initial Separate Schedules ---")
        print(P.to_c(pool_node.get_schedule()))
        
        # --- 2. Split Bands (Isolate Dimensions) ---
        
        # Conv: [n, k] | [h, w] | [c, kh, kw]
        seq_node = pool_node.parent().parent()
        conv_band = seq_node.child(0).child(0)
        conv_band = conv_band.band_split(2) # Split NK
        conv_band_2 = conv_band.child(0)
        conv_band_2 = conv_band_2.band_split(2) # Split HW
        
        # Pool: [n, k] | [h, w] | [rh, rw]
        # Note: conv_band is in a new tree. Update ref.
        seq_node = conv_band_2.parent().parent().parent() # Band->Band->Filter->Sequence ? No.
        # Hierarchy: Sequence -> Filter -> Band(NK) -> Band(HW) -> Band(Rest)
        # conv_band_2 is Band(HW). parent is Band(NK). parent is Filter.
        seq_node = conv_band_2.parent().parent().parent()
        
        pool_band = seq_node.child(1).child(0)
        pool_band = pool_band.band_split(2) # Split NK
        pool_band_2 = pool_band.child(0)
        pool_band_2 = pool_band_2.band_split(2) # Split HW
        
        # --- 3. Fuse NK ---
        # We have compatible [n, k] bands at the top of sequence children.
        seq_node = pool_band_2.parent().parent().parent()
        
        print("--- Fusing NK ---")
        nk_band = schedule_node_sequence_full_fuse(seq_node)
        # Result: Band(NK) -> Sequence -> Filter -> Band(HW)...
        
        # --- 4. Tile Conv HW ---
        # Conv is child 0 of Sequence (under NK Band)
        seq_node = nk_band.child(0)
        conv_hw = seq_node.child(0).child(0) # Band(HW)
        
        # Tile size [2, 2]
        conv_hw = conv_hw.band_set_permutable(1)
        # We need MultiVal. 
        # schedule_node_band_get_space returns Space.
        space = conv_hw.band_get_space()
        # Create MultiVal [2, 2]
        # Using helper or raw?
        # I.MultiVal.zero(space).set_val(0, 2).set_val(1, 2) ...
        # Let's assume I.MultiVal.from_val_list exists or construct manually.
        
        # Since from_val_list might not be bound, let's use parsing
        # Context is needed for Val.
        # Actually MultiVal isn't easily constructable from string.
        # Let's try tile with integer list if supported?
        # The binding might support list.
        # Let's try: conv_hw.band_tile([2, 2])?
        # If not, we use ISL C interface style.
        
        # Checking caten/isl/specs/schedule_node.py for band_tile signature.
        # Assuming it takes MultiVal.
        
        # Workaround for MultiVal construction:
        # mv = I.MultiVal.zero(space)
        # mv = mv.set_val(0, I.Val.int_from_si(2))
        # mv = mv.set_val(1, I.Val.int_from_si(2))
        
        
        mv = I.MultiVal.zero(space)
        mv = mv.set_val(0, I.Val.int_from_si(2))
        mv = mv.set_val(1, I.Val.int_from_si(2))
        
        conv_tiled = conv_hw.band_tile(mv)
        
        # Split Tile and Point loops
        # ISL band_tile ALREADY splits into Tile and Point bands!
        # So we don't need to split again.
        
        # Replace Inner Band (Absolute h, w) with Relative (h%2, w%2)
        # This is crucial for fusing with Pool's relative rh, rw loops!
        inner = conv_tiled.child(0)
        replaced = inner.delete()
        new_mupa = I.MultiUnionPwAff("{ S_conv[n, k, h, w, c, kh, kw] -> [(h%2), (w%2)] }")
        conv_tiled_inner = replaced.insert_partial_schedule(new_mupa) # noqa: F841
        
        # --- 5. Fuse Outer HW (Tiles) ---
        # Update seq_node ref
        seq_node = conv_tiled_inner.parent().parent().parent()
        
        print("--- Fusing HW Tiles ---")
        hw_band = schedule_node_sequence_full_fuse(seq_node)
        # Result: Band(NK) -> Band(HW_Tile) -> Sequence -> Filter -> Inner Bands
        
        # --- 6. Fuse Inner (Point vs Reduction) ---
        seq_node = hw_band.child(0)
        
        # Conv: Band(H_p, W_p) -> Band(C, KH, KW)
        # Pool: Band(RH, RW)
        
        # Conv H_p, W_p (0..1) matches Pool RH, RW (0..1).
        # Fuse them!
        print("--- Fusing Inner Loops (Register Fusion) ---")
        inner_band = schedule_node_sequence_full_fuse(seq_node)
        
        # Result: Band(NK) -> Band(HW_T) -> Band(Fused_Inner) -> Sequence -> ...
        # Sequence contains:
        # 1. Conv Remaining: Band(C, KH, KW)
        # 2. Pool Remaining: Leaf (since Band(RH,RW) was consumed by fuse)
        
        final_sched = inner_band.get_schedule()
        
        print("\n=== Final Fused C Code ===")
        print(P.to_c(final_sched))

if __name__ == "__main__":
    main()
