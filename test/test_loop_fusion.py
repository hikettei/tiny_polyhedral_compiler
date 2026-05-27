"""
Tests for the ir.py-native loop fusion engine.

The fusion engine is exercised entirely through Polyhedron + UnionMap +
BasicMap, with no ISL dependency.  Steps tested:

1. _per_dim_access produces a multi-dim access map from a (Band, ATenOpType).
2. _symbolize_bmap converts graph Affs (Dim references) to symbolic form.
3. _solve_producer_dims solves a dependency relation for substitution.
4. _substitute_dims rewrites a body under a Dim substitution.
5. End-to-end Polyhedron.__add__: elementwise + elementwise (sin∘sin) is
   inlined; the Load(MemoryOf(producer), …) disappears.
6. End-to-end: non-fusible cases return self unchanged.
7. End-to-end: strided dependency (Pool-like over a producer) derives the
   correct substitution h_p = S*hp + rh, w_p = S*wp + rw.
"""
from __future__ import annotations

import caten.ir as ir
from caten.dtype import float32, index

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _band(*sizes: int) -> ir.Band:
    return ir.Band(tuple(ir.Range((ir.Const.new(s, index),)) for s in sizes))


def _named_band(name: str, *sizes: int) -> ir.Band:
    """Build a Band whose Ranges are distinguished by `name` so the metaclass
    cache does not collapse two bands of the same shape."""
    return ir.Band(tuple(ir.Range((ir.Const.new(s, index),), name=f"{name}{i}")
                         for i, s in enumerate(sizes)))


# ---------------------------------------------------------------------------
# Step 1-2: per-dim access map + symbolization
# ---------------------------------------------------------------------------

class TestPerDimAccess:
    def test_per_dim_matches_band_ndim(self):
        band = _band(10, 20)
        T = ir.ATenOpType.from_shape((10, 20), float32)
        bmap, rng = ir._per_dim_access(band, T, dom_prefix="x")
        assert len(rng) == 2
        assert bmap.dom_vars == ("x_0", "x_1")
        assert bmap.rng_vars == ("d_0", "d_1")
        # one constraint per axis
        assert len(bmap.constraints) == 2

    def test_symbolize_makes_solver_see_dim_vars(self):
        band = _band(10, 20)
        T = ir.ATenOpType.from_shape((10, 20), float32)
        bmap, _ = ir._per_dim_access(band, T, dom_prefix="x")
        dim_to_name = {ir.Dim((band,), dim=i): f"x_{i}" for i in range(2)}
        sym = ir._symbolize_bmap(bmap, dim_to_name)
        # _per_dim_access uses logical (unit-stride) access, so coefficients are ±1.
        c0 = sym.constraints[0]
        # constraint 0 encodes d_0 = x_0
        assert c0.get_coefficient_of("x_0").simplify().item == 1
        assert c0.get_coefficient_of("d_0").simplify().item == -1


# ---------------------------------------------------------------------------
# Step 3: dependency solving
# ---------------------------------------------------------------------------

class TestSolveProducerDims:
    def test_identity_substitution(self):
        """sin∘sin on a (10,20) tensor: producer and consumer have identical access
        ⇒ substitution is the identity p_i = c_i."""
        p_band = _band(10, 20)
        c_band = _band(10, 20)
        T = ir.ATenOpType.from_shape((10, 20), float32)
        W_per, _ = ir._per_dim_access(p_band, T, dom_prefix="p")
        R_per, _ = ir._per_dim_access(c_band, T, dom_prefix="c")
        p_dim_to_name = {ir.Dim((p_band,), dim=i): f"p_{i}" for i in range(2)}
        c_dim_to_name = {ir.Dim((c_band,), dim=j): f"c_{j}" for j in range(2)}
        W_sym = ir._symbolize_bmap(W_per, p_dim_to_name)
        R_sym = ir._symbolize_bmap(R_per, c_dim_to_name)
        D = W_sym.apply_range(R_sym.reverse())
        sub = ir._solve_producer_dims(D, p_band, "p", c_band, "c")
        assert sub is not None
        assert 0 in sub and 1 in sub
        # sub[0] should simplify to Dim(c_band, 0); sub[1] to Dim(c_band, 1).
        # We check by evaluating ATenOp.eql against the expected Dim node.
        expected_c0 = ir.Dim((c_band,), dim=0)
        expected_c1 = ir.Dim((c_band,), dim=1)
        # The substitution may have algebraic noise (coefficients 1, additive 0);
        # compare via simplify→equality semantics.
        assert ir.ATenOp.eql(sub[0], expected_c0), f"sub[0]={sub[0].render()} expected {expected_c0.render()}"
        assert ir.ATenOp.eql(sub[1], expected_c1), f"sub[1]={sub[1].render()} expected {expected_c1.render()}"


# ---------------------------------------------------------------------------
# Step 4: Dim substitution rewriter
# ---------------------------------------------------------------------------

class TestSubstituteDims:
    def test_substitute_dim_in_expression(self):
        p_band = _named_band("p", 10, 20)
        c_band = _named_band("c", 10, 20)
        # Expression: Dim(p_band, 0) + 2 * Dim(p_band, 1)
        d0_p = ir.Dim((p_band,), dim=0)
        d1_p = ir.Dim((p_band,), dim=1)
        expr = ir.Add((d0_p, ir.Mul((ir.Const.new(2, index), d1_p))))
        # Substitute p_0 → Dim(c_band, 0), p_1 → Dim(c_band, 1)
        sub = {0: ir.Dim((c_band,), dim=0), 1: ir.Dim((c_band,), dim=1)}
        out = ir._substitute_dims(expr, p_band, sub)
        # The result should reference c_band's Dims, not p_band's
        seen_bands: set[int] = set()
        def _scan(n: ir.ATenOp) -> None:
            if isinstance(n, ir.Dim):
                seen_bands.add(id(n.args[0]))
            for a in n.args: _scan(a)
        _scan(out)
        assert id(c_band) in seen_bands
        assert id(p_band) not in seen_bands

    def test_substitute_leaves_unrelated_band(self):
        p_band = _named_band("p", 10, 20)
        other = _named_band("o", 5)
        sub = {0: ir.Const.new(7, index), 1: ir.Const.new(9, index)}
        # An expression with a Dim referring to another band must NOT be substituted.
        expr = ir.Dim((other,), dim=0)
        out = ir._substitute_dims(expr, p_band, sub)
        assert out is expr


# ---------------------------------------------------------------------------
# Step 5: End-to-end elementwise fusion via Polyhedron.__add__
# ---------------------------------------------------------------------------

def _build_elementwise_chain():
    """Build sin(sin(x)) at the IR level.  Returns (final_polyhedron, producer_polyhedron).

    The lowering should produce:
        producer = Polyhedron over band_p, body = Store(Load(out_p, …), Sin(Load(x_mem, …)))
        consumer = Polyhedron over band_c, body = Store(Load(out_c, …), Sin(Load(MemoryOf(producer), …)))
    """
    band1 = _band(10, 20)
    band2 = _band(10, 20)
    x_mem = ir.Memory.defglobal((10, 20), float32)
    out_p = ir.Memory.defglobal((10, 20), float32, tmp=True)
    out_c = ir.Memory.defglobal((10, 20), float32, tmp=True)
    # producer body: out_p[i,j] = sin(x[i,j])
    p_load_x = ir.Load.from_tensor(x_mem, band1)
    p_load_out = ir.Load.from_tensor(out_p, band1)
    p_body = ir.Store.new(p_load_out, ir.Sin((p_load_x,)))
    producer = ir.Polyhedron.schedule(band1.all_dimensions(), (out_p,), p_body)
    # consumer body: out_c[i,j] = sin(MemoryOf(producer)[i,j])
    c_load_p = ir.Load.from_tensor(ir.MemoryOf((producer,), nth=0, T=(out_p.T[0],)), band2)
    c_load_out = ir.Load.from_tensor(out_c, band2)
    c_body = ir.Store.new(c_load_out, ir.Sin((c_load_p,)))
    consumer = ir.Polyhedron.schedule(band2.all_dimensions(), (out_c,), c_body)
    return consumer, producer


def _has_load_from(node: ir.ATenOp, target: ir.ATenOp) -> bool:
    seen: set[int] = set()
    def _w(n: ir.ATenOp) -> bool:
        if id(n) in seen: return False
        seen.add(id(n))
        if isinstance(n, ir.Load):
            src = n.args[0]
            if isinstance(src, ir.MemoryOf) and src.args[0] is target:
                return True
        return any(_w(a) for a in n.args)
    return _w(node)


class TestEndToEndElementwise:
    def test_sin_of_sin_fuses(self):
        consumer, producer = _build_elementwise_chain()
        fused = consumer + producer
        assert isinstance(fused, ir.Polyhedron)
        # The fused body should NOT contain a Load reading from the producer anymore
        assert not _has_load_from(fused.args[2], producer), \
            "Fusion did not inline the producer; Load(MemoryOf(producer), …) still present"

    def test_sin_of_sin_fused_body_has_two_sins(self):
        consumer, producer = _build_elementwise_chain()
        fused = consumer + producer
        # Count Sin ops in the fused body
        sin_count = 0
        seen: set[int] = set()
        def _walk(n: ir.ATenOp) -> None:
            nonlocal sin_count
            if id(n) in seen: return
            seen.add(id(n))
            if isinstance(n, ir.Sin):
                sin_count += 1
            for a in n.args: _walk(a)
        _walk(fused.args[2])
        assert sin_count == 2, f"Expected 2 Sin ops after fusion (sin∘sin), got {sin_count}"


# ---------------------------------------------------------------------------
# Step 6: Strided / Pool-like dependency – substitution should be linear in
# consumer dims even when the producer access is purely from_tensor_type and
# the consumer iterates more dims.  For this we set up the per-dim maps
# manually to exercise the analytical core.
# ---------------------------------------------------------------------------

class TestStridedSubstitution:
    def test_pool_like_substitution_solves(self):
        """Producer writes Out[h, w] over a 16x16 band.
        Consumer (Pool) iterates (hp, wp, rh, rw) and reads Out at (4*hp+rh, 4*wp+rw).
        The dep solver should yield: p_0 = 4*hp + rh, p_1 = 4*wp + rw."""
        S = 4
        H, W = 16, 16
        Hp, Wp = H // S, W // S

        p_band = _band(H, W)
        c_band = _band(Hp, Wp, S, S)

        # Build producer's per-dim write: d_0 = h, d_1 = w (after stride normalization for
        # shape (H, W) the stride[0] = W, stride[1] = 1; we use unit-stride per-dim form
        # by encoding each tensor dim as its own rng_var).
        # For per-dim access we use the same _per_dim_access as elementwise.
        # Build T whose axes have stride=1, incf=1, offset=0 so per-dim is identity per dim.
        def _identity_T(shape: tuple[int, ...]) -> ir.ATenOpType:
            axes = tuple(
                ir.ATenAxis(size=ir.Const.new(s, index),
                            stride=ir.Const.new(1, index),
                            offset=ir.Const.new(0, index),
                            incf=ir.Const.new(1, index)) for s in shape
            )
            return ir.ATenOpType(axes=axes, dtype=float32)

        prod_T = _identity_T((H, W))
        W_per, _ = ir._per_dim_access(p_band, prod_T, dom_prefix="p")

        # Build consumer's read access manually: d_0 = S * c_0 + c_2, d_1 = S * c_1 + c_3
        # where c_0=hp, c_1=wp, c_2=rh, c_3=rw.
        # Construct the constraints directly as graph Affs.
        hp = ir.Dim((c_band,), dim=0)
        wp = ir.Dim((c_band,), dim=1)
        rh = ir.Dim((c_band,), dim=2)
        rw = ir.Dim((c_band,), dim=3)
        # Aff(stride=S, dim=hp, offset=0, incf=1) + Aff(stride=1, dim=rh, offset=0, incf=1) - d_0 = 0
        def _aff(coef: int, d: ir.Dim) -> ir.Aff:
            return ir.Aff((ir.Const.new(coef, index), d, ir.Const.new(0, index), ir.Const.new(1, index)))
        c0 = ir.Constraint((_aff(S, hp), _aff(1, rh), ir.Aff.var("d_0", flip=True)))
        c1 = ir.Constraint((_aff(S, wp), _aff(1, rw), ir.Aff.var("d_1", flip=True)))
        R_per = ir.BasicMap((c0, c1), dom_vars=("c_0", "c_1", "c_2", "c_3"),
                            rng_vars=("d_0", "d_1"), dom_name="S", rng_name="")

        p_dim_to_name = {ir.Dim((p_band,), dim=i): f"p_{i}" for i in range(2)}
        c_dim_to_name = {ir.Dim((c_band,), dim=j): f"c_{j}" for j in range(4)}
        W_sym = ir._symbolize_bmap(W_per, p_dim_to_name)
        R_sym = ir._symbolize_bmap(R_per, c_dim_to_name)

        D = W_sym.apply_range(R_sym.reverse())
        sub = ir._solve_producer_dims(D, p_band, "p", c_band, "c")
        assert sub is not None, "Pool-like dependency should be solvable"

        # Expected: sub[0] simplifies to S*Dim(c_band,0) + Dim(c_band,2)
        # We verify by structural check: render() should mention all four c_band dims correctly.
        # Easier: substitute concrete values for the consumer dims and check the result.
        # sub[0] = ?  with hp=2, rh=3 should give S*2 + 3 = 11. With wp=1, rw=2 sub[1] = 6.
        # We do this by substituting again via _substitute_dims with concrete consts.
        const_sub = {0: ir.Const.new(2, index),  # hp = 2
                     1: ir.Const.new(1, index),  # wp = 1
                     2: ir.Const.new(3, index),  # rh = 3
                     3: ir.Const.new(2, index)}  # rw = 2
        val0 = ir._substitute_dims(sub[0], c_band, const_sub).simplify()
        val1 = ir._substitute_dims(sub[1], c_band, const_sub).simplify()
        assert val0.item == S * 2 + 3, f"sub[0] eval mismatch: got {val0.item}, expected {S*2+3}"
        assert val1.item == S * 1 + 2, f"sub[1] eval mismatch: got {val1.item}, expected {S*1+2}"


# ---------------------------------------------------------------------------
# Non-fusion fallback: when no Load(MemoryOf(producer), …) is reachable from
# consumer, __add__ returns self unchanged.
# ---------------------------------------------------------------------------

class TestNonFusion:
    def test_unrelated_polyhedra_no_fusion(self):
        """If consumer doesn't reference producer at all, __add__ falls through to self."""
        band = _band(10)
        x_mem = ir.Memory.defglobal((10,), float32)
        out_p = ir.Memory.defglobal((10,), float32, tmp=True)
        out_c = ir.Memory.defglobal((10,), float32, tmp=True)
        # producer
        p_body = ir.Store.new(ir.Load.from_tensor(out_p, band),
                              ir.Sin((ir.Load.from_tensor(x_mem, band),)))
        producer = ir.Polyhedron.schedule(band.all_dimensions(), (out_p,), p_body)
        # consumer reads x directly, not producer's output
        c_body = ir.Store.new(ir.Load.from_tensor(out_c, band),
                              ir.Sin((ir.Load.from_tensor(x_mem, band),)))
        consumer = ir.Polyhedron.schedule(band.all_dimensions(), (out_c,), c_body)
        fused = consumer + producer
        # No fusion: same object back
        assert fused is consumer
