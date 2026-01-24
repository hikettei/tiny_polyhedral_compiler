"""
Test Conv+Pool style fusion with ir.py's BasicMap/UnionMap API.

Demonstrates dependency analysis and tiling relationship extraction
for polyhedral loop fusion.
"""
from __future__ import annotations

import caten.ir as ir
from typing import List, Tuple, Dict, Optional, Set
from dataclasses import dataclass


# ============================================================
# Helper classes for analysis
# ============================================================


@dataclass
class TileRelation:
    """Represents a tiling relationship: var = tile_factor * tile_var + offset_var"""
    var: str           # Original variable (e.g., "h")
    tile_factor: int   # Tile size (e.g., 4)
    tile_var: str      # Tile coordinate (e.g., "hp")
    offset_var: str    # Offset within tile (e.g., "rh")


@dataclass
class DepInfo:
    """Dependency information."""
    raw: ir.UnionMap   # Read-After-Write dependencies
    war: ir.UnionMap   # Write-After-Read dependencies
    waw: ir.UnionMap   # Write-After-Write dependencies


@dataclass 
class FusionResult:
    """Result of fusion analysis."""
    success: bool
    fusion_type: str   # "elementwise", "tiled", "none"
    message: str
    dep_info: DepInfo
    tile_relations: List[TileRelation]
    shared_dims: List[str]


def extract_tile_relations(
    cst: ir.Constraint,
    producer_dims: Tuple[str, ...],
    consumer_dims: Tuple[str, ...]
) -> Tuple[List[TileRelation], List[str]]:
    """
    Extract tiling relationships from a dependency constraint.
    
    For constraint: -128*h + 512*hp + 128*rh + 4*wp + rw - w = 0
    We identify:
      - h has coefficient -128 (producer)
      - hp has coefficient +512 = 128 * 4  (tile factor = 4)
      - rh has coefficient +128 = 128 * 1  (offset factor = 1)
    
    Opposite signs indicate: -128*h + 512*hp + 128*rh = 0
                          => 128*h = 512*hp + 128*rh
                          => h = 4*hp + rh
    """
    tile_relations = []
    shared_dims = []
    
    producer_set = set(producer_dims)
    consumer_set = set(consumer_dims)
    
    # Find shared dimensions (same variable name, coefficients cancel)
    for pdim in producer_dims:
        if pdim in consumer_set:
            pcoef = cst.get_coefficient_of(pdim).item
            if pcoef == 0:
                shared_dims.append(pdim)
    
    # Find tiling relationships for non-shared producer dims
    for pdim in producer_dims:
        if pdim in shared_dims:
            continue
            
        pcoef = cst.get_coefficient_of(pdim).item
        if pcoef == 0:
            continue
        
        abs_pcoef = abs(pcoef)
        
        # Look for consumer dims that form tiling pattern
        # Constraint form: pcoef*pdim + ccoef1*tile_var + ccoef2*offset_var = 0
        # If pcoef < 0 and ccoef > 0: |pcoef|*pdim = ccoef*cdim
        tile_candidates = []
        offset_candidates = []
        
        for cdim in consumer_dims:
            if cdim in producer_set:
                continue  # Skip shared dims
                
            ccoef = cst.get_coefficient_of(cdim).item
            if ccoef == 0:
                continue
            
            # Opposite signs indicate relationship:
            # pcoef*p + ccoef*c = 0 => |pcoef|*p = |ccoef|*c (when signs differ)
            signs_opposite = (pcoef < 0 and ccoef > 0) or (pcoef > 0 and ccoef < 0)
            if not signs_opposite:
                continue
                
            # Check ratio to determine if tile or offset variable
            ratio = abs(ccoef) / abs_pcoef
            if ratio == int(ratio):
                int_ratio = int(ratio)
                if int_ratio > 1:
                    tile_candidates.append((cdim, int_ratio, abs(ccoef)))
                elif int_ratio == 1:
                    offset_candidates.append((cdim, abs(ccoef)))
        
        # Match tile and offset by coefficient relationship
        # Pick the smallest tile_factor (most likely the correct one)
        best_match = None
        for tile_var, tile_factor, tile_coef in tile_candidates:
            for offset_var, offset_coef in offset_candidates:
                # Verify: tile_coef = abs_pcoef * tile_factor
                #         offset_coef = abs_pcoef * 1
                if tile_coef == abs_pcoef * tile_factor and offset_coef == abs_pcoef:
                    if best_match is None or tile_factor < best_match[1]:
                        best_match = (tile_var, tile_factor, offset_var)
        
        if best_match:
            tile_relations.append(TileRelation(
                var=pdim,
                tile_factor=best_match[1],
                tile_var=best_match[0],
                offset_var=best_match[2]
            ))
    return tile_relations, shared_dims
    return tile_relations, shared_dims


def analyze_dependencies(
    producer_writes: ir.UnionMap,
    producer_reads: ir.UnionMap,
    consumer_writes: ir.UnionMap,
    consumer_reads: ir.UnionMap
) -> DepInfo:
    """Compute RAW, WAR, WAW dependencies between producer and consumer."""
    
    # RAW: consumer reads what producer writes
    # D_RAW = W_producer ∘ R_consumer^{-1}
    raw_maps = []
    for pw in producer_writes.args:
        for cr in consumer_reads.args:
            cr_rev = cr.reverse()
            dep = pw.apply_range(cr_rev)
            if not dep.is_empty():
                raw_maps.append(dep)
    
    # WAR: producer reads what consumer writes (less common for fusion)
    war_maps = []
    
    # WAW: both write to same location (less common)
    waw_maps = []
    
    return DepInfo(
        raw=ir.UnionMap(tuple(raw_maps)),
        war=ir.UnionMap(tuple(war_maps)),
        waw=ir.UnionMap(tuple(waw_maps))
    )


def attempt_fusion(
    producer_writes: ir.UnionMap,
    producer_reads: ir.UnionMap,
    consumer_writes: ir.UnionMap,
    consumer_reads: ir.UnionMap
) -> FusionResult:
    """Attempt to fuse producer and consumer, returning fusion strategy."""
    
    dep_info = analyze_dependencies(
        producer_writes, producer_reads,
        consumer_writes, consumer_reads
    )
    
    # Analyze RAW dependencies for fusion strategy
    if not dep_info.raw.args:
        return FusionResult(
            success=True,
            fusion_type="independent",
            message="No RAW dependencies - independent operations",
            dep_info=dep_info,
            tile_relations=[],
            shared_dims=[]
        )
    
    # Get first RAW dependency for analysis
    raw_dep = dep_info.raw.args[0]
    
    if not raw_dep.args:
        return FusionResult(
            success=True,
            fusion_type="elementwise",
            message="Elementwise fusion - no constraints",
            dep_info=dep_info,
            tile_relations=[],
            shared_dims=list(raw_dep.dom_vars)
        )
    
    # Extract tiling relationships
    cst = raw_dep.args[0]
    tile_rels, shared = extract_tile_relations(
        cst,
        raw_dep.dom_vars,
        raw_dep.rng_vars
    )
    
    if tile_rels:
        tile_desc = ", ".join(
            f"{r.var}=[{r.tile_factor}x{r.tile_factor}]"
            for r in tile_rels
        )
        return FusionResult(
            success=True,
            fusion_type="tiled",
            message=f"Tiled fusion possible - tile producer dims: {tile_desc}",
            dep_info=dep_info,
            tile_relations=tile_rels,
            shared_dims=shared
        )
    
    return FusionResult(
        success=False,
        fusion_type="none",
        message="Could not determine fusion strategy",
        dep_info=dep_info,
        tile_relations=[],
        shared_dims=shared
    )


# ============================================================
# Test: Conv+Pool Style Fusion
# ============================================================

def test_conv_pool_style():
    """
    Test Conv+Pool style fusion with proper strided access patterns.

    Based on examples/polyhedral_compiler.ipynb:
    - Conv2D outputs: Out[n,k,h,w] where 0≤h<128, 0≤w<128
    - Pool2D reads: Out[n,k,hp*S_POOL+rh,wp*S_POOL+rw] where:
        - 0≤hp<32, 0≤wp<32 (pool output spatial dims)
        - 0≤rh<4, 0≤rw<4 (reduction dims within pool window)

    The fusion requires tiling conv by [S_POOL, S_POOL] to align
    with pool's strided access pattern.
    """
    print("=" * 60)
    print("Test: Conv+Pool Style (Strided Access Pattern)")
    print("=" * 60)

    S_POOL = 4
    H_OUT = 128
    W_OUT = 128

    # Conv iteration space: (n, k, h, w) where 0≤h<128, 0≤w<128
    n, k, h, w = "n", "k", "h", "w"

    # Conv output address: linear in (n,k,h,w)
    # Strides: n*K*H*W + k*H*W + h*W + w
    # Using H_OUT=W_OUT=128, so: 16384*n + 128*k + 128*h + w
    conv_write = ir.BasicMap.define(
        dom=(n, k, h, w),
        mapping={"addr": (
            ir.Aff.term(H_OUT * W_OUT, n),  # 16384*n
            ir.Aff.term(H_OUT, k),           # 128*k
            ir.Aff.term(W_OUT, h),           # 128*h
            ir.Aff.term(1, w),               # w
        )},
        dom_name="S"
    )
    
    conv_read = ir.BasicMap.define(
        dom=(n, k, h, w),
        mapping={"addr": (
            ir.Aff.term(H_OUT * W_OUT, n),
            ir.Aff.term(H_OUT, k),
            ir.Aff.term(W_OUT, h),
            ir.Aff.term(1, w),
        )},
        dom_name="S"
    )

    # Pool iteration space: (n, k, hp, wp, rh, rw)
    # where 0≤hp<32, 0≤wp<32, 0≤rh<4, 0≤rw<4
    hp, wp, rh, rw = "hp", "wp", "rh", "rw"

    # Pool reads at Out[n,k,hp*4+rh,wp*4+rw]
    # Address: n*16384 + k*128 + (hp*4+rh)*128 + (wp*4+rw)
    #        = n*16384 + k*128 + hp*512 + rh*128 + wp*4 + rw
    pool_read = ir.BasicMap.define(
        dom=(n, k, hp, wp, rh, rw),
        mapping={"addr": (
            ir.Aff.term(H_OUT * W_OUT, n),        # 16384*n
            ir.Aff.term(H_OUT, k),                 # 128*k
            ir.Aff.term(W_OUT * S_POOL, hp),       # 512*hp
            ir.Aff.term(W_OUT, rh),                # 128*rh
            ir.Aff.term(S_POOL, wp),               # 4*wp
            ir.Aff.term(1, rw),                    # rw
        )},
        dom_name="S"
    )

    # Pool writes to PoolBuf[n,k,hp,wp] - different buffer
    pool_write = ir.BasicMap.define(
        dom=(n, k, hp, wp, rh, rw),
        mapping={"addr": (
            ir.Aff.term(32 * 32, n),
            ir.Aff.term(32, k),
            ir.Aff.term(32, hp),
            ir.Aff.term(1, wp),
        )},
        dom_name="S"
    )

    print("\nConv writes: Out[n,k,h,w]")
    print(f"  Access map: {conv_write}")
    print("\nPool reads:  Out[n,k,hp*4+rh,wp*4+rw]")
    print(f"  Access map: {pool_read}")

    result = attempt_fusion(
        ir.UnionMap((conv_write,)),
        ir.UnionMap((conv_read,)),
        ir.UnionMap((pool_write,)),
        ir.UnionMap((pool_read,)),
    )

    print(f"\nFusion type: {result.fusion_type}")
    print(f"Success: {result.success}")
    print(f"Message: {result.message}")
    
    if result.dep_info.raw.args:
        print(f"RAW deps: {result.dep_info.raw.args[0]}")

    print("\n--- Detected Tiling Strategy ---")
    print(f"  Shared dimensions: {result.shared_dims}")
    
    for rel in result.tile_relations:
        print(f"  Tile {rel.var} by {rel.tile_factor}, reduction over {rel.offset_var}")

    print("\nExplanation:")
    print("  For each pool output (hp,wp), compute conv tile [4x4] on-the-fly")
    print("  Conv dims (h,w) map to (hp*4+rh, wp*4+rw)")
    
    # Verify tiling relationships
    if result.dep_info.raw.args:
        cst = result.dep_info.raw.args[0].args[0]
        print("\n--- Algebraic Verification ---")
        
        h_coef = cst.get_coefficient_of("h").item
        hp_coef = cst.get_coefficient_of("hp").item
        rh_coef = cst.get_coefficient_of("rh").item
        
        w_coef = cst.get_coefficient_of("w").item
        wp_coef = cst.get_coefficient_of("wp").item
        rw_coef = cst.get_coefficient_of("rw").item
        
        print(f"  h relationship: {-h_coef}*h = {hp_coef}*hp + {rh_coef}*rh")
        print(f"    => h = {hp_coef // (-h_coef)}*hp + {rh_coef // (-h_coef)}*rh")
        
        print(f"  w relationship: {-w_coef}*w = {wp_coef}*wp + {rw_coef}*rw")
        print(f"    => w = {wp_coef // (-w_coef)}*wp + {rw_coef // (-w_coef)}*rw")
    
    # Assertions for testing
    assert result.success, "Fusion should succeed"
    assert result.fusion_type == "tiled", f"Expected tiled fusion, got {result.fusion_type}"
    assert len(result.tile_relations) == 2, f"Expected 2 tile relations, got {len(result.tile_relations)}"
    
    # Check tile factors
    tile_map = {r.var: r for r in result.tile_relations}
    assert "h" in tile_map, "h should have tile relation"
    assert "w" in tile_map, "w should have tile relation"
    assert tile_map["h"].tile_factor == 4, f"h tile factor should be 4, got {tile_map['h'].tile_factor}"
    assert tile_map["w"].tile_factor == 4, f"w tile factor should be 4, got {tile_map['w'].tile_factor}"
    
    print("\n✓ All assertions passed!")
    return result


if __name__ == "__main__":
    test_conv_pool_style()
