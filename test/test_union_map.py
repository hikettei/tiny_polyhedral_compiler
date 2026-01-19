import pytest
import caten.ir as ir

def test_basic_map_apply_range():
    # Test apply_range composition: map1.apply_range(map2)
    # map1: S[i] -> T[2*i]     (scale by 2)
    # map2: T[j] -> U[j+3]     (offset by 3)
    # Result: S[i] -> U[2*i+3] (composed)

    # Create map1: S[i] -> T[x] where x = 2*i
    # Constraint: 2*i - x = 0
    map1 = ir.BasicMap.from_affine(
        dom_vars=("i",),
        rng_vars=("x",),
        rng_exprs=((ir.Aff.var("i"), ir.Aff.var("i")),),  # 2*i = i + i
        dom_name="S",
        rng_name="T"
    )

    # Create map2: T[j] -> U[y] where y = j + 3
    # Constraint: j + 3 - y = 0
    map2 = ir.BasicMap.from_affine(
        dom_vars=("j",),
        rng_vars=("y",),
        rng_exprs=((ir.Aff.var("j"), ir.Aff.var("3")),),  # j + 3
        dom_name="T",
        rng_name="U"
    )

    # Compose: should give S[i] -> U[2*i + 3]
    composed = map1.apply_range(map2)

    # Verify structure
    assert composed.dom_vars == ("i",), f"Expected dom_vars ('i',), got {composed.dom_vars}"
    assert composed.rng_vars == ("y",), f"Expected rng_vars ('y',), got {composed.rng_vars}"
    assert composed.dom_name == "S", f"Expected dom_name 'S', got {composed.dom_name}"
    assert composed.rng_name == "U", f"Expected rng_name 'U', got {composed.rng_name}"
