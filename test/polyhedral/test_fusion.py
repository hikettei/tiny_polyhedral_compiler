import caten.isl as I
import caten.polyhedral as P
from caten.polyhedral.transformations import schedule_node_sequence_full_fuse


def create_conv_schedule(N, K_out, H_out, W_out, Cin, KH, KW):
    dom_str = f"{{ S_conv[n, k, h, w, c, kh, kw] : 0<=n<{N} and 0<=k<{K_out} and 0<=h<{H_out} and 0<=w<{W_out} and 0<=c<{Cin} and 0<=kh<{KH} and 0<=kw<{KW} }}"
    
    with P.domain(dom_str) as conv:
        with P.band("{ S_conv[n, k, h, w, c, kh, kw] -> [n, k, h, w, c, kh, kw] }"):
            P.stmt("Out[n, k, h, w] = Out[n, k, h, w], In[n, c, h, w], W[k, c, kh, kw]")
            
    return conv.finalize()

def create_pool_schedule(N, K_out, H_pool, W_pool, S_pool, KH_pool, KW_pool):
    dom_str = f"{{ S_pool[n, k, h, w, rh, rw] : 0<=n<{N} and 0<=k<{K_out} and 0<=h<{H_pool} and 0<=w<{W_pool} and 0<=rh<{KH_pool} and 0<=rw<{KW_pool} }}"
    
    with P.domain(dom_str) as pool:
        with P.band("{ S_pool[n, k, h, w, rh, rw] -> [n, k, h, w, rh, rw] }"):
            P.stmt(f"PoolBuf[n, k, h, w] = PoolBuf[n, k, h, w], Out[n, k, h*{S_pool} + rh, w*{S_pool} + rw]")
            
    return pool.finalize()

def test_conv2d_pool2d_fusion():
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
    Tile_H = S_pool
    Tile_W = S_pool
    
    conv = create_conv_schedule(N, Cout, H_conv, W_conv, Cin, KH_conv, KW_conv)
    pool = create_pool_schedule(N, Cout, H_pool, W_pool, S_pool, KH_pool, KW_pool)
    
    psched = conv.sequence(pool)
    
    root = psched.get_root()
    seq_node = root.child(0)
    
    # 1. Split Bands
    conv_band = seq_node.child(0).child(0)
    conv_band = conv_band.band_split(2) # [n, k]
    conv_band_2 = conv_band.child(0)
    conv_band_2 = conv_band_2.band_split(2) # [h, w]
    
    seq_node = conv_band_2.parent().parent().parent()
    
    pool_band = seq_node.child(1).child(0)
    pool_band = pool_band.band_split(2) # [n, k]
    pool_band_2 = pool_band.child(0)
    pool_band_2 = pool_band_2.band_split(2) # [h, w]
    
    seq_node = pool_band_2.parent().parent().parent()
    
    # 2. Fuse NK
    nk_band = schedule_node_sequence_full_fuse(seq_node)
    psched.update(nk_band)
    
    # 3. Tile Conv HW
    seq_node = nk_band.child(0)
    conv_hw = seq_node.child(0).child(0)
    conv_hw = conv_hw.band_set_permutable(1)
    space = conv_hw.band_get_space()
    mv = I.MultiVal.zero(space)
    mv = mv.set_val(0, I.Val.int_from_si(Tile_H))
    mv = mv.set_val(1, I.Val.int_from_si(Tile_W))
    
    conv_tiled = conv_hw.band_tile(mv)
    conv_tiled = conv_tiled.band_scale_down(mv)
    
    # Replace Inner Band with Relative
    inner = conv_tiled.child(0)
    replaced = inner.delete()
    new_mupa = I.MultiUnionPwAff.from_union_map(I.UnionMap(f"{{ S_conv[n, k, h, w, c, kh, kw] -> [(h%{Tile_H}), (w%{Tile_W})] }}"))
    conv_tiled_inner = replaced.insert_partial_schedule(new_mupa) # noqa: F841
    
    seq_node = conv_tiled_inner.parent().parent().parent()
    
    # 4. Fuse HW Tiles
    hw_tile_band = schedule_node_sequence_full_fuse(seq_node)
    psched.update(hw_tile_band)
    
    # 5. Fuse Inner
    seq_node = hw_tile_band.child(0)
    inner_band = schedule_node_sequence_full_fuse(seq_node)
    psched.update(inner_band)
    
    assert psched.is_legal()
    c_code = psched.to_c()
    assert "S_conv" in c_code
    assert "S_pool" in c_code
