#!/usr/bin/env python3
"""
Test script demonstrating automatic loop fusion analysis.

This script shows how the affine relation algebra can detect fusion opportunities
for operations like Conv+Pool, element-wise chains, and reshape sequences.
"""
import caten as C
from caten.aff import (
    AffExpr,
    BasicMap,
    UnionMap,
    attempt_fusion,
)


def test_identity_fusion():
    """Test perfect fusion with identity dependencies."""
    print("=" * 60)
    print("Test 1: Identity Fusion (element-wise operations)")
    print("=" * 60)

    # Simulate: y = sin(x), z = sin(y)
    # Both operate element-wise on same iteration space
    gid0, gid1 = "gid0", "gid1"

    # Access pattern: addr = 10*gid0 + gid1 (for a 10xN array)
    addr_expr = 10 * AffExpr.var(gid0) + AffExpr.var(gid1)

    # Both producer and consumer access the same pattern
    producer_write = BasicMap.from_access((gid0, gid1), addr_expr)
    producer_read = BasicMap.from_access((gid0, gid1), addr_expr)
    consumer_write = BasicMap.from_access((gid0, gid1), addr_expr)
    consumer_read = BasicMap.from_access((gid0, gid1), addr_expr)

    result = attempt_fusion(
        UnionMap.from_maps([producer_write]),
        UnionMap.from_maps([producer_read]),
        UnionMap.from_maps([consumer_write]),
        UnionMap.from_maps([consumer_read]),
    )

    print(f"Fusion type: {result.fusion_type}")
    print(f"Success: {result.success}")
    print(f"Message: {result.message}")
    print(f"RAW deps: {result.dep_info.raw}")
    print()


def test_reshape_fusion():
    """Test fusion across reshape operation."""
    print("=" * 60)
    print("Test 2: Reshape Fusion Challenge")
    print("=" * 60)

    # Producer: operates on 4D space [2,5,2,5]
    # Consumer: operates on 2D space [10,10] (after reshape)
    # These typically cannot be fused due to dimension mismatch

    gid0, gid1, gid2, gid3 = "gid0", "gid1", "gid2", "gid3"

    # 4D access: 50*gid0 + 10*gid1 + 5*gid2 + gid3
    addr_4d = 50 * AffExpr.var(gid0) + 10 * AffExpr.var(gid1) + 5 * AffExpr.var(gid2) + AffExpr.var(gid3)

    # 2D access: 10*gid0 + gid1
    addr_2d = 10 * AffExpr.var("gid0") + AffExpr.var("gid1")

    producer_write = BasicMap.from_access((gid0, gid1, gid2, gid3), addr_4d)
    consumer_read = BasicMap.from_access(("gid0", "gid1"), addr_2d)
    consumer_write = BasicMap.from_access(("gid0", "gid1"), addr_2d)

    result = attempt_fusion(
        UnionMap.from_maps([producer_write]),
        UnionMap.from_maps([]),
        UnionMap.from_maps([consumer_write]),
        UnionMap.from_maps([consumer_read]),
    )

    print(f"Fusion type: {result.fusion_type}")
    print(f"Success: {result.success}")
    print(f"Message: {result.message}")
    print(f"RAW deps: {result.dep_info.raw}")
    print()


def test_reduction_fusion():
    """Test fusion with reduction operations."""
    print("=" * 60)
    print("Test 3: Reduction + Element-wise Fusion")
    print("=" * 60)

    # Producer: reduces over gid2 dimension
    # Consumer: operates on reduced result

    gid0, gid1, gid2 = "gid0", "gid1", "gid2"

    # Producer reads from 3D: 100*gid0 + 10*gid1 + gid2
    producer_read = BasicMap.from_access(
        (gid0, gid1, gid2),
        100 * AffExpr.var(gid0) + 10 * AffExpr.var(gid1) + AffExpr.var(gid2)
    )
    # Producer writes to 2D: 10*gid0 + gid1
    producer_write = BasicMap.from_access(
        (gid0, gid1, gid2),
        10 * AffExpr.var(gid0) + AffExpr.var(gid1)
    )

    # Consumer operates on 2D
    consumer_read = BasicMap.from_access(
        ("gid0", "gid1"),
        10 * AffExpr.var("gid0") + AffExpr.var("gid1")
    )
    consumer_write = BasicMap.from_access(
        ("gid0", "gid1"),
        10 * AffExpr.var("gid0") + AffExpr.var("gid1")
    )

    result = attempt_fusion(
        UnionMap.from_maps([producer_write]),
        UnionMap.from_maps([producer_read]),
        UnionMap.from_maps([consumer_write]),
        UnionMap.from_maps([consumer_read]),
    )

    print(f"Fusion type: {result.fusion_type}")
    print(f"Success: {result.success}")
    print(f"Message: {result.message}")
    print()


def test_conv_pool_style():
    """
    Test Conv+Pool style fusion with proper strided access patterns.

    Based on examples/polyhedral_compiler.ipynb:
    - Conv2D outputs: Out[n,k,h,w] where 0≤h<128, 0≤w<128
    - Pool2D reads: Out[n,k,h*S_POOL+rh,w*S_POOL+rw] where:
        - 0≤h<32, 0≤w<32 (pool output spatial dims)
        - 0≤rh<4, 0≤rw<4 (reduction dims within pool window)

    The fusion requires tiling conv by [S_POOL, S_POOL] to align
    with pool's strided access pattern.
    """
    print("=" * 60)
    print("Test 4: Conv+Pool Style (Strided Access Pattern)")
    print("=" * 60)

    S_POOL = 4
    H_OUT = 128
    W_OUT = 128

    # Conv iteration space: (n, k, h, w) where 0≤h<128, 0≤w<128
    n, k, h, w = "n", "k", "h", "w"

    # Conv output address: linear in (n,k,h,w)
    # Strides: n*K*H*W + k*H*W + h*W + w
    conv_addr = (
        W_OUT * H_OUT * AffExpr.var(n) +
        H_OUT * AffExpr.var(k) +
        W_OUT * AffExpr.var(h) +
        AffExpr.var(w)
    )

    conv_write = BasicMap.from_access((n, k, h, w), conv_addr)
    conv_read = BasicMap.from_access((n, k, h, w), conv_addr)  # reads input, but same domain

    # Pool iteration space: (n, k, hp, wp, rh, rw)
    # where 0≤hp<32, 0≤wp<32, 0≤rh<4, 0≤rw<4
    hp, wp, rh, rw = "hp", "wp", "rh", "rw"

    # Pool reads at Out[n,k,hp*4+rh,wp*4+rw]
    # Address: n*K*H*W + k*H*W + (hp*4+rh)*W + (wp*4+rw)
    #        = n*K*H*W + k*H*W + hp*4*W + rh*W + wp*4 + rw
    pool_read_addr = (
        W_OUT * H_OUT * AffExpr.var(n) +
        H_OUT * AffExpr.var(k) +
        W_OUT * S_POOL * AffExpr.var(hp) +
        W_OUT * AffExpr.var(rh) +
        S_POOL * AffExpr.var(wp) +
        AffExpr.var(rw)
    )

    pool_read = BasicMap.from_access((n, k, hp, wp, rh, rw), pool_read_addr)

    # Pool writes to PoolBuf[n,k,hp,wp] - different buffer
    pool_out_addr = (
        32 * 32 * AffExpr.var(n) +
        32 * AffExpr.var(k) +
        32 * AffExpr.var(hp) +
        AffExpr.var(wp)
    )
    pool_write = BasicMap.from_access((n, k, hp, wp, rh, rw), pool_out_addr)

    print("Conv writes: Out[n,k,h,w]")
    print(f"  Access map: {conv_write.pretty_str()}")
    print("Pool reads:  Out[n,k,hp*4+rh,wp*4+rw]")
    print(f"  Access map: {pool_read.pretty_str()}")

    result = attempt_fusion(
        UnionMap.from_maps([conv_write]),
        UnionMap.from_maps([conv_read]),
        UnionMap.from_maps([pool_write]),
        UnionMap.from_maps([pool_read]),
    )

    print(f"\nFusion type: {result.fusion_type}")
    print(f"Success: {result.success}")
    print(f"Message: {result.message}")
    print(f"RAW deps: {result.dep_info.raw}")

    # Show tiling info if available
    if result.tiling_info:
        print("\n--- Detected Tiling Strategy ---")
        print(f"  Shared dimensions: {result.tiling_info.shared_dims}")
        for dim, (tile_size, red_dim) in result.tiling_info.tile_dims.items():
            print(f"  Tile {dim} by {tile_size}, reduction over {red_dim}")
        print("\nExplanation:")
        print(f"  For each pool output (hp,wp), compute conv tile [{S_POOL}x{S_POOL}] on-the-fly")
        print(f"  Conv dims (h,w) map to (hp*{S_POOL}+rh, wp*{S_POOL}+rw)")
    elif result.fusion_type != "none":
        print("\n--- Fusion Strategy ---")
        print(f"To fuse Conv+Pool with S_POOL={S_POOL}:")
        print(f"1. Tile Conv output dims (h,w) by [{S_POOL},{S_POOL}]")
        print(f"2. Scale down tiled dims by [{S_POOL},{S_POOL}]")
        print("3. This aligns conv tiles with pool's strided reads")
        print("4. For each pool output (hp,wp), compute conv tile on-the-fly")
    print()


def test_matmul_chain():
    """Test matrix multiplication chain fusion."""
    print("=" * 60)
    print("Test 5: Matmul Chain (GEMM fusion)")
    print("=" * 60)

    # C = A @ B, D = C @ E
    # GEMM with reduction over K dimension

    i, j, k = "i", "j", "k"

    # First GEMM writes C[i,j]
    gemm1_write = BasicMap.from_access((i, j, k), 50 * AffExpr.var(i) + AffExpr.var(j))
    gemm1_read_a = BasicMap.from_access((i, j, k), 30 * AffExpr.var(i) + AffExpr.var(k))
    gemm1_read_b = BasicMap.from_access((i, j, k), 50 * AffExpr.var(k) + AffExpr.var(j))

    # Second GEMM reads C[i,k2] and writes D[i,j2]
    k2, j2 = "k2", "j2"
    gemm2_read = BasicMap.from_access((i, k2, j2), 50 * AffExpr.var(i) + AffExpr.var(k2))
    gemm2_write = BasicMap.from_access((i, k2, j2), 100 * AffExpr.var(i) + AffExpr.var(j2))

    result = attempt_fusion(
        UnionMap.from_maps([gemm1_write]),
        UnionMap.from_maps([gemm1_read_a, gemm1_read_b]),
        UnionMap.from_maps([gemm2_write]),
        UnionMap.from_maps([gemm2_read]),
    )

    print(f"Fusion type: {result.fusion_type}")
    print(f"Success: {result.success}")
    print(f"Message: {result.message}")
    print(f"RAW deps: {result.dep_info.raw}")
    print()


def test_ir_fusion():
    """Test actual IR transformation for element-wise fusion."""
    print("=" * 60)
    print("Test 6: IR Transformation (sin(sin(x)) fusion)")
    print("=" * 60)

    # Create: y = sin(x), z = sin(y)
    # Should fuse into a single kernel: z = sin(sin(x))
    x = C.Tensor([10, 10])
    y = x.sin()
    z = y.sin()

    print("Before fusion:")
    print(f"  x.shape = {x.shape}")
    print("  z = sin(sin(x))")

    # Access the IR (lowering happens automatically during construction)
    ir = z.op

    print("\nAfter lowering (fusion applied):")
    print(ir.viz())

    # Check that we have a single Sync (fused)
    # or analyze the structure
    def count_endranges(node, seen=None):
        if seen is None:
            seen = set()
        if id(node) in seen:
            return 0
        seen.add(id(node))

        from caten.ir import Sync
        count = 1 if isinstance(node, Sync) else 0
        if hasattr(node, "args"):
            for arg in node.args:
                count += count_endranges(arg, seen)
        return count

    num_endranges = count_endranges(ir)
    print(f"\nNumber of Sync nodes: {num_endranges}")
    if num_endranges == 1:
        print("  -> Successfully fused into single kernel!")
    else:
        print("  -> Multiple kernels (fusion pending or not applicable)")
    print()


def main():
    print("\n" + "=" * 60)
    print("  AUTOMATIC LOOP FUSION ANALYSIS - TEST SUITE")
    print("=" * 60 + "\n")

    test_identity_fusion()
    test_reshape_fusion()
    test_reduction_fusion()
    test_conv_pool_style()
    test_matmul_chain()
    test_ir_fusion()

    print("=" * 60)
    print("Summary:")
    print("- Identity dependencies enable perfect fusion")
    print("- Dimension mismatches block fusion")
    print("- Reduction patterns require special handling")
    print("- Conv+Pool fusion is possible when iteration spaces match")
    print("- IR transformation inlines producer into consumer")
    print("=" * 60)


if __name__ == "__main__":
    main()
