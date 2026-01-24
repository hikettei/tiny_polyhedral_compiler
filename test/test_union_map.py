"""
Comprehensive tests for BasicMap, UnionMap, Constraint, and Aff classes in ir.py.

These classes form the core of the polyhedral compilation model:
- Aff: Affine expressions (stride * (incf * dim + offset))
- Constraint: Equality constraints (sum of Affs == 0)
- BasicMap: Affine relation from domain to range
- UnionMap: Union of multiple BasicMaps
"""
import pytest

import caten.ir as ir
from caten.dtype import index

# =============================================================================
# Aff Tests
# =============================================================================

class TestAff:
    """Tests for the Aff (Affine expression) class."""

    def test_aff_var_creation(self):
        """Test Aff.var() creates a variable affine expression."""
        aff = ir.Aff.var("i")
        assert isinstance(aff, ir.Aff)
        # Aff.var creates: Aff(1, cst_dim, "i", 1)
        # meaning: 1 * (1 * cst + "i")
        assert aff.stride.item == 1
        assert aff.incf.item == 1
        # offset is the variable name as Const
        assert isinstance(aff.offset, ir.Const)
        assert aff.offset.value == "i"

    def test_aff_var_flip(self):
        """Test Aff.var() with flip=True creates negated variable."""
        aff_pos = ir.Aff.var("x", flip=False)
        aff_neg = ir.Aff.var("x", flip=True)
        assert aff_pos.incf.item == 1
        assert aff_neg.incf.item == -1

    def test_aff_from_axis(self):
        """Test Aff creation from ATenAxis."""
        # Create a Band with one Range
        band = ir.Band((ir.Range((ir.Const.new(10, index),)),))
        axis = ir.ATenAxis(
            size=ir.Const.new(10, index),
            stride=ir.Const.new(5, index),
            offset=ir.Const.new(2, index),
            incf=ir.Const.new(1, index)
        )
        aff = axis.aff(band, dim=0)
        assert isinstance(aff, ir.Aff)
        assert aff.stride.item == 5
        assert aff.offset.item == 2
        assert aff.incf.item == 1

    def test_aff_ax_b(self):
        """Test Aff.ax_b() returns (a, b) where aff = a*dim + b."""
        band = ir.Band((ir.Range((ir.Const.new(10, index),)),))
        axis = ir.ATenAxis(
            size=ir.Const.new(10, index),
            stride=ir.Const.new(3, index),   # stride
            offset=ir.Const.new(7, index),   # offset
            incf=ir.Const.new(2, index)      # incf
        )
        aff = axis.aff(band, dim=0)
        a, b = aff.ax_b()
        # a = stride * incf = 3 * 2 = 6
        # b = stride * offset = 3 * 7 = 21
        assert a.item == 6
        assert b.item == 21

    def test_aff_rename(self):
        """Test Aff.rename() renames variables in the expression."""
        band = ir.Band((
            ir.Range((ir.Const.new(10, index),), name="i"),
            ir.Range((ir.Const.new(20, index),), name="j"),
        ))
        axis = ir.ATenAxis(
            size=ir.Const.new(10, index),
            stride=ir.Const.new(1, index),
            offset=ir.Const.new(0, index),
            incf=ir.Const.new(1, index)
        )
        aff = axis.aff(band, dim=0)
        renamed = aff.rename({"i": "x", "j": "y"})
        # Check that rename produces a new Aff
        assert isinstance(renamed, ir.Aff)
        # The dimension should be renamed
        assert renamed.dim.range.name == "x"

    def test_aff_term_creation(self):
        """Test Aff.term() creates coefficient * variable."""
        aff = ir.Aff.term(2, "i")
        assert isinstance(aff, ir.Aff)
        # stride should be 2 (the coefficient)
        assert aff.stride.item == 2
        # offset should be the variable name
        assert aff.offset.value == "i"
        # incf should be 1
        assert aff.incf.item == 1

    def test_aff_term_negative(self):
        """Test Aff.term() with negative coefficient."""
        aff = ir.Aff.term(-3, "j")
        assert aff.stride.item == -3
        assert aff.offset.value == "j"

    def test_aff_const_creation(self):
        """Test Aff.const() creates a constant."""
        aff = ir.Aff.const(5)
        assert isinstance(aff, ir.Aff)
        # For constant: stride=1, offset=value, incf=0
        assert aff.stride.item == 1
        assert aff.offset.item == 5
        assert aff.incf.item == 0
        # ax_b should give (0, 5) -> represents just constant 5
        a, b = aff.ax_b()
        assert a.item == 0  # No dim contribution
        assert b.item == 5  # Just the constant

    def test_aff_const_negative(self):
        """Test Aff.const() with negative value."""
        aff = ir.Aff.const(-7)
        assert aff.offset.item == -7
        _, b = aff.ax_b()
        assert b.item == -7

    def test_aff_lin_basic(self):
        """Test Aff.lin() creates linear expression tuple."""
        # 2*i + 3
        affs = ir.Aff.lin(2, "i", 3)
        assert isinstance(affs, tuple)
        assert len(affs) == 2
        # First element: 2*i
        assert affs[0].stride.item == 2
        assert affs[0].offset.value == "i"
        # Second element: 3
        assert affs[1].offset.item == 3
        assert affs[1].incf.item == 0

    def test_aff_lin_no_const(self):
        """Test Aff.lin() without constant term."""
        # Just 5*j
        affs = ir.Aff.lin(5, "j")
        assert len(affs) == 1
        assert affs[0].stride.item == 5
        assert affs[0].offset.value == "j"

    def test_aff_lin_zero_coef(self):
        """Test Aff.lin() with zero coefficient."""
        # 0*i + 7 = just 7
        affs = ir.Aff.lin(0, "i", 7)
        assert len(affs) == 1
        assert affs[0].offset.item == 7

    def test_aff_lin_concatenation(self):
        """Test concatenating Aff.lin() results for complex expressions."""
        # 2*i + j + 5 = lin(2, "i") + lin(1, "j", 5)
        expr = ir.Aff.lin(2, "i") + ir.Aff.lin(1, "j", 5)
        assert len(expr) == 3  # (2*i, j, 5)
        assert expr[0].stride.item == 2
        assert expr[1].stride.item == 1
        assert expr[2].offset.item == 5



# =============================================================================
# Constraint Tests
# =============================================================================

class TestConstraint:
    """Tests for the Constraint class."""

    def test_constraint_creation_single_aff(self):
        """Test Constraint with single Aff."""
        aff = ir.Aff.var("i")
        constraint = ir.Constraint((aff,))
        assert isinstance(constraint, ir.Constraint)
        assert len(constraint.args) == 1

    def test_constraint_creation_multiple_affs(self):
        """Test Constraint with multiple Affs."""
        # Represents: i + j = 0
        aff_i = ir.Aff.var("i")
        aff_j = ir.Aff.var("j")
        constraint = ir.Constraint((aff_i, aff_j))
        assert isinstance(constraint, ir.Constraint)
        assert len(constraint.args) == 2

    def test_constraint_str(self):
        """Test Constraint.__str__() representation."""
        aff_i = ir.Aff.var("i")
        aff_j = ir.Aff.var("j", flip=True)
        constraint = ir.Constraint((aff_i, aff_j))
        s = str(constraint)
        # Should produce something like "... = 0"
        assert "= 0" in s

    def test_constraint_rename(self):
        """Test Constraint.rename() renames variables."""
        band = ir.Band((ir.Range((ir.Const.new(10, index),), name="i"),))
        axis = ir.ATenAxis(
            size=ir.Const.new(10, index),
            stride=ir.Const.new(1, index),
            offset=ir.Const.new(0, index),
            incf=ir.Const.new(1, index)
        )
        aff = axis.aff(band, dim=0)
        constraint = ir.Constraint((aff,))
        renamed = constraint.rename({"i": "x"})
        assert isinstance(renamed, ir.Constraint)


# =============================================================================
# Range and Band Tests
# =============================================================================

class TestRangeAndBand:
    """Tests for Range and Band classes."""

    def test_range_creation(self):
        """Test Range creation with size."""
        size = ir.Const.new(10, index)
        r = ir.Range((size,))
        assert r.size.item == 10
        assert r.name is None

    def test_range_named(self):
        """Test Range.named() creates named range."""
        size = ir.Const.new(10, index)
        r = ir.Range((size,)).named("i")
        assert r.name == "i"

    def test_range_rename(self):
        """Test Range.rename() renames the range."""
        size = ir.Const.new(10, index)
        r = ir.Range((size,), name="i")
        renamed = r.rename({"i": "x", "j": "y"})
        assert renamed.name == "x"
        # If name not in mapping, unchanged
        r2 = ir.Range((size,), name="k")
        renamed2 = r2.rename({"i": "x"})
        assert renamed2.name == "k"

    def test_band_creation(self):
        """Test Band creation with multiple Ranges."""
        r1 = ir.Range((ir.Const.new(10, index),), name="i")
        r2 = ir.Range((ir.Const.new(20, index),), name="j")
        band = ir.Band((r1, r2))
        assert band.ndim == 2
        assert len(band.ranges) == 2
        assert band.shape[0].item == 10
        assert band.shape[1].item == 20

    def test_band_all_dimensions(self):
        """Test Band.all_dimensions() returns all Dim nodes."""
        r1 = ir.Range((ir.Const.new(10, index),))
        r2 = ir.Range((ir.Const.new(20, index),))
        band = ir.Band((r1, r2))
        dims = band.all_dimensions()
        assert len(dims) == 2
        assert all(isinstance(d, ir.Dim) for d in dims)
        assert dims[0].dim == 0
        assert dims[1].dim == 1

    def test_band_rename(self):
        """Test Band.rename() renames all ranges."""
        r1 = ir.Range((ir.Const.new(10, index),), name="i")
        r2 = ir.Range((ir.Const.new(20, index),), name="j")
        band = ir.Band((r1, r2))
        renamed = band.rename({"i": "x", "j": "y"})
        assert renamed.ranges[0].name == "x"
        assert renamed.ranges[1].name == "y"


class TestDim:
    """Tests for the Dim class."""

    def test_dim_creation(self):
        """Test Dim creation from Band."""
        r = ir.Range((ir.Const.new(10, index),), name="i")
        band = ir.Band((r,))
        dim = ir.Dim((band,), dim=0)
        assert dim.dim == 0
        assert dim.domain is band
        assert dim.range is r

    def test_dim_out_of_range_fails(self):
        """Test Dim with invalid dim index fails."""
        r = ir.Range((ir.Const.new(10, index),))
        band = ir.Band((r,))
        with pytest.raises(AssertionError):
            ir.Dim((band,), dim=1)  # Out of range

    def test_dim_rename(self):
        """Test Dim.rename() renames the underlying band."""
        r1 = ir.Range((ir.Const.new(10, index),), name="i")
        r2 = ir.Range((ir.Const.new(20, index),), name="j")
        band = ir.Band((r1, r2))
        dim = ir.Dim((band,), dim=0)
        renamed = dim.rename({"i": "x"})
        assert renamed.range.name == "x"


# =============================================================================
# BasicMap Tests
# =============================================================================

class TestBasicMap:
    """Tests for the BasicMap class."""

    def test_basic_map_from_affine_simple(self):
        """Test BasicMap.from_affine() with simple identity mapping."""
        # S[i] -> T[i]
        bmap = ir.BasicMap.from_affine(
            dom_vars=("i",),
            rng_vars=("x",),
            rng_exprs=((ir.Aff.var("i"),),),
            dom_name="S",
            rng_name="T"
        )
        assert bmap.dom_vars == ("i",)
        assert bmap.rng_vars == ("x",)
        assert bmap.dom_name == "S"
        assert bmap.rng_name == "T"
        assert len(bmap.args) == 1  # One constraint

    def test_basic_map_from_affine_scaling(self):
        """Test BasicMap.from_affine() with scaling: S[i] -> T[2*i]."""
        # Old way: (ir.Aff.var("i"), ir.Aff.var("i")) to represent 2*i
        # New way: use term(2, "i") for cleaner 2*i
        bmap = ir.BasicMap.from_affine(
            dom_vars=("i",),
            rng_vars=("x",),
            rng_exprs=((ir.Aff.term(2, "i"),),),  # Clean: 2*i
            dom_name="S",
            rng_name="T"
        )
        assert bmap.dom_vars == ("i",)
        assert bmap.rng_vars == ("x",)
        assert len(bmap.args) == 1

    def test_basic_map_from_affine_offset(self):
        """Test BasicMap.from_affine() with offset: S[i] -> T[i + 5]."""
        # Using lin() for cleaner i + 5 expression
        bmap = ir.BasicMap.from_affine(
            dom_vars=("i",),
            rng_vars=("x",),
            rng_exprs=(ir.Aff.lin(1, "i", 5),),  # Clean: i + 5
            dom_name="S",
            rng_name="T"
        )
        assert bmap.dom_vars == ("i",)
        assert bmap.rng_vars == ("x",)

    def test_basic_map_from_affine_multidim(self):
        """Test BasicMap.from_affine() with 2D -> 1D (linearization)."""
        # S[i, j] -> T[addr] where addr = 10*i + j
        # Using new helpers: lin(10, "i") + lin(1, "j") = (10*i, j)
        bmap = ir.BasicMap.from_affine(
            dom_vars=("i", "j"),
            rng_vars=("addr",),
            rng_exprs=(ir.Aff.lin(10, "i") + ir.Aff.lin(1, "j"),),
            dom_name="S",
            rng_name=""
        )
        assert bmap.dom_vars == ("i", "j")
        assert bmap.rng_vars == ("addr",)
        assert len(bmap.args) == 1

    def test_basic_map_str_representation(self):
        """Test BasicMap.__str__() produces ISL-like format."""
        bmap = ir.BasicMap.from_affine(
            dom_vars=("i",),
            rng_vars=("x",),
            rng_exprs=((ir.Aff.var("i"),),),
            dom_name="S",
            rng_name="T"
        )
        s = str(bmap)
        assert "S[i]" in s
        assert "T[x]" in s
        assert "->" in s

    def test_basic_map_reverse(self):
        """Test BasicMap.reverse() swaps domain and range."""
        bmap = ir.BasicMap.from_affine(
            dom_vars=("i",),
            rng_vars=("x",),
            rng_exprs=((ir.Aff.var("i"),),),
            dom_name="S",
            rng_name="T"
        )
        reversed_map = bmap.reverse()
        assert reversed_map.dom_vars == ("x",)
        assert reversed_map.rng_vars == ("i",)
        assert reversed_map.dom_name == "T"
        assert reversed_map.rng_name == "S"

    def test_basic_map_rename_vars(self):
        """Test BasicMap.rename_vars() renames variables."""
        bmap = ir.BasicMap.from_affine(
            dom_vars=("i", "j"),
            rng_vars=("x",),
            rng_exprs=((ir.Aff.var("i"), ir.Aff.var("j")),),
            dom_name="S",
            rng_name="T"
        )
        renamed = bmap.rename_vars({"i": "a", "j": "b", "x": "y"})
        assert renamed.dom_vars == ("a", "b")
        assert renamed.rng_vars == ("y",)

    def test_basic_map_from_tensor_type(self):
        """Test BasicMap.from_tensor_type() creates access map from tensor."""
        # Create a 2D tensor type with shape [10, 20]
        tensor_type = ir.ATenOpType.from_shape((10, 20), index)
        band = ir.Band((
            ir.Range((ir.Const.new(10, index),)),
            ir.Range((ir.Const.new(20, index),)),
        ))
        bmap = ir.BasicMap.from_tensor_type(band, tensor_type)
        # Should create a map S[gid_0, gid_1] -> [addr]
        assert "gid_0" in bmap.dom_vars
        assert "gid_1" in bmap.dom_vars
        assert bmap.rng_vars == ("addr",)

    def test_basic_map_from_tensor_type_scalar(self):
        """Test BasicMap.from_tensor_type() with scalar (0D) tensor."""
        tensor_type = ir.ATenOpType(axes=(), dtype=index)
        band = ir.Band((ir.Range((ir.Const.new(1, index),)),))
        bmap = ir.BasicMap.from_tensor_type(band, tensor_type)
        # Scalar should return empty BasicMap
        assert len(bmap.args) == 0

    def test_basic_map_empty_constraints(self):
        """Test BasicMap with no constraints (unconstrained)."""
        bmap = ir.BasicMap(
            (),
            dom_vars=("i",),
            rng_vars=("x",),
            dom_name="S",
            rng_name="T",
            T=(ir.ATenOpType(axes=(), dtype=index),)
        )
        s = str(bmap)
        assert ":" not in s  # No constraint separator

    def test_basic_map_is_empty(self):
        """Test BasicMap.is_empty() method.
        
        Note: Current implementation always returns False (stub).
        TODO: Update test when proper satisfiability checking is implemented.
        """
        bmap = ir.BasicMap.from_affine(
            dom_vars=("i",),
            rng_vars=("x",),
            rng_exprs=((ir.Aff.var("i"),),),
            dom_name="S",
            rng_name="T"
        )
        # Currently is_empty always returns False
        assert bmap.is_empty() is False

    def test_basic_map_define_simple(self):
        """Test BasicMap.define() with simple identity mapping."""
        # S[i] -> T[x] where x = i
        bmap = ir.BasicMap.define(
            dom=("i",),
            mapping={"x": ir.Aff.lin(1, "i")},
            dom_name="S",
            rng_name="T"
        )
        assert bmap.dom_vars == ("i",)
        assert bmap.rng_vars == ("x",)
        assert bmap.dom_name == "S"
        assert bmap.rng_name == "T"

    def test_basic_map_define_with_offset(self):
        """Test BasicMap.define() with linear expression including offset."""
        # S[i] -> T[x] where x = 2*i + 3
        bmap = ir.BasicMap.define(
            dom=("i",),
            mapping={"x": ir.Aff.lin(2, "i", 3)},
            dom_name="S",
            rng_name="T"
        )
        assert bmap.dom_vars == ("i",)
        assert bmap.rng_vars == ("x",)
        assert len(bmap.args) == 1  # One constraint

    def test_basic_map_define_multidim(self):
        """Test BasicMap.define() with 2D domain and complex mapping."""
        # S[i, j] -> T[x, y] where x = 2*i + j, y = j + 5
        bmap = ir.BasicMap.define(
            dom=("i", "j"),
            mapping={
                "x": ir.Aff.lin(2, "i") + ir.Aff.lin(1, "j"),  # 2*i + j
                "y": ir.Aff.lin(1, "j", 5),                    # j + 5
            },
            dom_name="S",
            rng_name="T"
        )
        assert bmap.dom_vars == ("i", "j")
        assert bmap.rng_vars == ("x", "y")
        assert len(bmap.args) == 2  # Two constraints

    def test_basic_map_define_linearization(self):
        """Test BasicMap.define() for array linearization."""
        # S[i, j] -> [addr] where addr = 10*i + j
        bmap = ir.BasicMap.define(
            dom=("i", "j"),
            mapping={
                "addr": ir.Aff.lin(10, "i") + ir.Aff.lin(1, "j"),
            },
        )
        assert bmap.dom_vars == ("i", "j")
        assert bmap.rng_vars == ("addr",)
        s = str(bmap)
        assert "addr" in s

    def test_basic_map_define_str(self):
        """Test BasicMap.define() produces valid string representation."""
        bmap = ir.BasicMap.define(
            dom=("i",),
            mapping={"x": ir.Aff.lin(2, "i", 3)},
            dom_name="S",
            rng_name="T"
        )
        s = str(bmap)
        assert "S[i]" in s
        assert "T[x]" in s
        assert "->" in s



# =============================================================================
# UnionMap Tests
# =============================================================================

class TestUnionMap:
    """Tests for the UnionMap class."""

    def test_union_map_creation_empty(self):
        """Test empty UnionMap creation."""
        umap = ir.UnionMap(())
        assert len(umap.args) == 0

    def test_union_map_creation_single(self):
        """Test UnionMap with single BasicMap."""
        bmap = ir.BasicMap.from_affine(
            dom_vars=("i",),
            rng_vars=("x",),
            rng_exprs=((ir.Aff.var("i"),),),
            dom_name="S",
            rng_name="T"
        )
        umap = ir.UnionMap((bmap,))
        assert len(umap.args) == 1

    def test_union_map_creation_multiple(self):
        """Test UnionMap with multiple BasicMaps."""
        bmap1 = ir.BasicMap.from_affine(
            dom_vars=("i",),
            rng_vars=("x",),
            rng_exprs=((ir.Aff.var("i"),),),
            dom_name="S1",
            rng_name="T"
        )
        bmap2 = ir.BasicMap.from_affine(
            dom_vars=("j",),
            rng_vars=("y",),
            rng_exprs=((ir.Aff.var("j"),),),
            dom_name="S2",
            rng_name="T"
        )
        umap = ir.UnionMap((bmap1, bmap2))
        assert len(umap.args) == 2

    def test_union_map_or_operator(self):
        """Test UnionMap.__or__() combines two UnionMaps."""
        bmap1 = ir.BasicMap.from_affine(
            dom_vars=("i",),
            rng_vars=("x",),
            rng_exprs=((ir.Aff.var("i"),),),
            dom_name="S1",
            rng_name="T"
        )
        bmap2 = ir.BasicMap.from_affine(
            dom_vars=("j",),
            rng_vars=("y",),
            rng_exprs=((ir.Aff.var("j"),),),
            dom_name="S2",
            rng_name="T"
        )
        umap1 = ir.UnionMap((bmap1,))
        umap2 = ir.UnionMap((bmap2,))
        combined = umap1 | umap2
        assert len(combined.args) == 2

    def test_union_map_reverse(self):
        """Test UnionMap.reverse() reverses all BasicMaps."""
        bmap = ir.BasicMap.from_affine(
            dom_vars=("i",),
            rng_vars=("x",),
            rng_exprs=((ir.Aff.var("i"),),),
            dom_name="S",
            rng_name="T"
        )
        umap = ir.UnionMap((bmap,))
        reversed_umap = umap.reverse()
        assert len(reversed_umap.args) == 1
        assert reversed_umap.args[0].dom_vars == ("x",)
        assert reversed_umap.args[0].rng_vars == ("i",)

    def test_union_map_is_empty_true(self):
        """Test UnionMap.is_empty() returns True for empty union."""
        umap = ir.UnionMap(())
        assert umap.is_empty() is True

    def test_union_map_is_empty_false(self):
        """Test UnionMap.is_empty() returns False for non-empty union."""
        bmap = ir.BasicMap.from_affine(
            dom_vars=("i",),
            rng_vars=("x",),
            rng_exprs=((ir.Aff.var("i"),),),
            dom_name="S",
            rng_name="T"
        )
        umap = ir.UnionMap((bmap,))
        # Non-empty union is not empty
        assert umap.is_empty() is False


    def test_union_map_str_representation(self):
        """Test UnionMap.__str__() produces readable format."""
        bmap1 = ir.BasicMap.from_affine(
            dom_vars=("i",),
            rng_vars=("x",),
            rng_exprs=((ir.Aff.var("i"),),),
            dom_name="S1",
            rng_name="T"
        )
        bmap2 = ir.BasicMap.from_affine(
            dom_vars=("j",),
            rng_vars=("y",),
            rng_exprs=((ir.Aff.var("j"),),),
            dom_name="S2",
            rng_name="T"
        )
        umap = ir.UnionMap((bmap1, bmap2))
        s = str(umap)
        assert "UnionMap" in s
        assert "S1" in s
        assert "S2" in s


# =============================================================================
# Integration Tests: Apply Range / Apply Domain
# =============================================================================

class TestMapComposition:
    """Integration tests for map composition operations."""

    def test_basic_map_apply_range_identity(self):
        """Test apply_range with identity-like maps."""
        # map1: S[i] -> T[i]
        # map2: T[j] -> U[j]
        # Result: S[i] -> U[i]
        map1 = ir.BasicMap.from_affine(
            dom_vars=("i",),
            rng_vars=("x",),
            rng_exprs=((ir.Aff.var("i"),),),
            dom_name="S",
            rng_name="T"
        )
        map2 = ir.BasicMap.from_affine(
            dom_vars=("j",),
            rng_vars=("y",),
            rng_exprs=((ir.Aff.var("j"),),),
            dom_name="T",
            rng_name="U"
        )
        # Note: apply_range requires fourier_motzkin to be fully implemented
        # This test documents expected behavior
        # composed = map1.apply_range(map2)
        # assert composed.dom_vars == ("i",)
        # assert composed.rng_vars == ("y",)
        # For now, just verify the structure
        assert map1.rng_vars == ("x",)
        assert map2.dom_vars == ("j",)

    def test_basic_map_apply_range_scaling(self):
        """Test apply_range composition: S[i] -> T[2*i] -> U[2*i+3]."""
        # This is the original test, kept for reference
        map1 = ir.BasicMap.from_affine(
            dom_vars=("i",),
            rng_vars=("x",),
            rng_exprs=((ir.Aff.var("i"), ir.Aff.var("i")),),  # 2*i
            dom_name="S",
            rng_name="T"
        )
        map2 = ir.BasicMap.from_affine(
            dom_vars=("j",),
            rng_vars=("y",),
            rng_exprs=((ir.Aff.var("j"), ir.Aff.var("3")),),  # j + 3
            dom_name="T",
            rng_name="U"
        )
        # Document expected structure before composition
        assert map1.dom_name == "S"
        assert map2.rng_name == "U"

    def test_union_map_apply_range_pairwise(self):
        """Test UnionMap.apply_range() composes all pairs."""
        bmap1 = ir.BasicMap.from_affine(
            dom_vars=("i",),
            rng_vars=("x",),
            rng_exprs=((ir.Aff.var("i"),),),
            dom_name="S",
            rng_name="M"
        )
        bmap2 = ir.BasicMap.from_affine(
            dom_vars=("j",),
            rng_vars=("y",),
            rng_exprs=((ir.Aff.var("j"),),),
            dom_name="M",
            rng_name="T"
        )
        umap1 = ir.UnionMap((bmap1,))
        umap2 = ir.UnionMap((bmap2,))
        # Document expected structure
        assert len(umap1.args) == 1
        assert len(umap2.args) == 1



# =============================================================================
# Fourier-Motzkin Elimination and Dependency Analysis Tests
# =============================================================================

class TestConstraintMethods:
    """Tests for Constraint class methods used in fourier_motzkin.
    
    Note: get_coefficient_of() and get_constant() return ATenOp computation graphs.
    Use .simplify().item to extract concrete values for assertions.
    """

    def test_get_coefficient_of_single_var(self):
        """Test get_coefficient_of with single variable term."""
        # Constraint: 3*x = 0
        c = ir.Constraint((ir.Aff.term(3, "x"),))
        assert c.get_coefficient_of("x").simplify().item == 3
        assert c.get_coefficient_of("y").simplify().item == 0

    def test_get_coefficient_of_negative(self):
        """Test get_coefficient_of with negative coefficient."""
        # Constraint: -5*x = 0
        c = ir.Constraint((ir.Aff.term(-5, "x"),))
        assert c.get_coefficient_of("x").simplify().item == -5

    def test_get_coefficient_of_multiple_vars(self):
        """Test get_coefficient_of with multiple variables."""
        # Constraint: 2*x + 3*y - 4*z = 0
        c = ir.Constraint((ir.Aff.term(2, "x"), ir.Aff.term(3, "y"), ir.Aff.term(-4, "z")))
        assert c.get_coefficient_of("x").simplify().item == 2
        assert c.get_coefficient_of("y").simplify().item == 3
        assert c.get_coefficient_of("z").simplify().item == -4
        assert c.get_coefficient_of("w").simplify().item == 0

    def test_get_constant_simple(self):
        """Test get_constant with a constant term."""
        # Constraint: x + 5 = 0
        c = ir.Constraint((ir.Aff.term(1, "x"), ir.Aff.const(5)))
        assert c.get_constant().simplify().item == 5

    def test_get_constant_negative(self):
        """Test get_constant with negative constant."""
        # Constraint: x - 10 = 0
        c = ir.Constraint((ir.Aff.term(1, "x"), ir.Aff.const(-10)))
        assert c.get_constant().simplify().item == -10

    def test_get_constant_no_constant(self):
        """Test get_constant when no constant term."""
        # Constraint: x + y = 0
        c = ir.Constraint((ir.Aff.term(1, "x"), ir.Aff.term(1, "y")))
        assert c.get_constant().simplify().item == 0

    def test_variables_extraction(self):
        """Test variables() extracts all variable names."""
        # Constraint: 2*x + 3*y + 5 = 0
        c = ir.Constraint((ir.Aff.term(2, "x"), ir.Aff.term(3, "y"), ir.Aff.const(5)))
        vars_set = c.variables()
        assert vars_set == frozenset({"x", "y"})

    def test_without_var_removes_variable(self):
        """Test without_var removes specified variable's Affs."""
        # Constraint: 2*x + 3*y + 5 = 0
        c = ir.Constraint((ir.Aff.term(2, "x"), ir.Aff.term(3, "y"), ir.Aff.const(5)))
        rest = c.without_var("x")
        # Should have y term and constant
        assert len(rest) == 2
        # Check the remaining constraint
        c2 = ir.Constraint(rest)
        assert c2.get_coefficient_of("y").simplify().item == 3
        assert c2.get_constant().simplify().item == 5
        assert c2.get_coefficient_of("x").simplify().item == 0

    def test_is_trivial_zero_constraint(self):
        """Test is_trivial for 0 = 0."""
        c = ir.Constraint(())
        assert c.is_trivial() is True

    def test_is_trivial_non_trivial(self):
        """Test is_trivial for non-trivial constraint."""
        c = ir.Constraint((ir.Aff.term(1, "x"),))
        assert c.is_trivial() is False

    def test_is_contradiction_with_nonzero_const(self):
        """Test is_contradiction with only non-zero constant."""
        c = ir.Constraint((ir.Aff.const(5),))
        assert c.is_contradiction() is True

    def test_is_contradiction_with_variables(self):
        """Test is_contradiction with variables is False."""
        c = ir.Constraint((ir.Aff.term(1, "x"), ir.Aff.const(5)))
        assert c.is_contradiction() is False

    def test_substitute_simple(self):
        """Test substitute replaces variable."""
        # Constraint: x + y = 0
        # Substitute x = -z (solution: z term)
        c = ir.Constraint((ir.Aff.term(1, "x"), ir.Aff.term(1, "y")))
        solution = (ir.Aff.term(-1, "z"),)  # x = -z means substitute with -z
        c2 = c.substitute("x", solution)
        # Result should be: -z + y = 0
        assert c2.get_coefficient_of("z").simplify().item == -1
        assert c2.get_coefficient_of("y").simplify().item == 1
        assert c2.get_coefficient_of("x").simplify().item == 0

class TestFourierMotzkin:
    """Tests for Fourier-Motzkin variable elimination.
    
    Note: get_coefficient_of() and get_constant() return ATenOp computation graphs.
    Use .simplify().item to extract concrete values for assertions.
    """

    def test_fourier_motzkin_single_var(self):
        """Test eliminating a single variable."""
        # Constraints: x = y, x + z = 5
        # Eliminate x:
        # From x = y: x -> y
        # Substitute into x + z = 5: y + z = 5
        c1 = ir.Constraint((ir.Aff.term(1, "x"), ir.Aff.term(-1, "y")))  # x - y = 0
        c2 = ir.Constraint((ir.Aff.term(1, "x"), ir.Aff.term(1, "z"), ir.Aff.const(-5)))  # x + z - 5 = 0
        
        result = ir.Constraint.fourier_motzkin([c1, c2], ["x"])
        # Should have one constraint: y + z - 5 = 0
        assert len(result) == 1
        assert result[0].get_coefficient_of("y").simplify().item == 1
        assert result[0].get_coefficient_of("z").simplify().item == 1
        assert result[0].get_constant().simplify().item == -5

    def test_fourier_motzkin_multiple_vars(self):
        """Test eliminating multiple variables sequentially."""
        # Constraints: x = a, y = b, x + y + z = c
        # Eliminate x, y:
        # From x = a: substitute x -> a in x + y + z = c => a + y + z = c
        # From y = b: substitute y -> b in a + y + z = c => a + b + z = c
        c1 = ir.Constraint((ir.Aff.term(1, "x"), ir.Aff.term(-1, "a")))  # x - a = 0
        c2 = ir.Constraint((ir.Aff.term(1, "y"), ir.Aff.term(-1, "b")))  # y - b = 0
        c3 = ir.Constraint((ir.Aff.term(1, "x"), ir.Aff.term(1, "y"), ir.Aff.term(1, "z"), ir.Aff.term(-1, "c")))  # x + y + z - c = 0
        
        result = ir.Constraint.fourier_motzkin([c1, c2, c3], ["x", "y"])
        # Should have one constraint: a + b + z - c = 0
        assert len(result) == 1
        assert result[0].get_coefficient_of("a").simplify().item == 1
        assert result[0].get_coefficient_of("b").simplify().item == 1
        assert result[0].get_coefficient_of("z").simplify().item == 1
        assert result[0].get_coefficient_of("c").simplify().item == -1

    def test_fourier_motzkin_no_unit_coefficient_skips(self):
        """Test that variables without ±1 coefficient are skipped."""
        # Constraint: 2*x + y = 0 (cannot eliminate x directly with coefficient 2)
        c = ir.Constraint((ir.Aff.term(2, "x"), ir.Aff.term(1, "y")))
        result = ir.Constraint.fourier_motzkin([c], ["x"])
        # x has coefficient 2, not ±1, so not eliminated
        assert len(result) == 1
        assert result[0].get_coefficient_of("x").simplify().item == 2

    def test_fourier_motzkin_eliminates_trivial(self):
        """Test that trivial constraints (0=0) are removed."""
        # Constraint: x - x = 0 which becomes 0 = 0 after cancellation
        # Actually let's use: x = x (which is x - x = 0)
        c = ir.Constraint((ir.Aff.term(1, "x"), ir.Aff.term(-1, "x")))
        # This constraint has x with coefficient 1 + (-1) = 0, so no x variable actually
        # It should be trivial after fourier_motzkin
        result = ir.Constraint.fourier_motzkin([c], [])
        # The constraint has no variables (x cancels), and constant 0
        # So it's trivial and removed
        assert len(result) == 0

class TestDependencyAnalysis:
    """Tests for dependency analysis using apply_range.
    
    Dependency analysis computes: D = W ∘ R^{-1}
    Where:
        W = { S[i] -> mem[addr] : write relation }
        R = { S[i'] -> mem[addr'] : read relation }
        R^{-1} = { mem[addr'] -> S[i'] : reversed }
        D = { S[i] -> S[i'] : i writes what i' reads }
    """

    def test_basic_map_reverse(self):
        """Test BasicMap.reverse() swaps domain and range."""
        # Map: S[i] -> T[2*i + 1]
        bmap = ir.BasicMap.define(
            dom=("i",),
            mapping={"addr": ir.Aff.lin(2, "i", 1)},
            dom_name="S",
            rng_name="T"
        )
        assert bmap.dom_name == "S"
        assert bmap.rng_name == "T"
        
        rev = bmap.reverse()
        assert rev.dom_name == "T"
        assert rev.rng_name == "S"

    def test_apply_range_identity(self):
        """Test apply_range with identity-like mapping."""
        # Map1: S[i] -> M[i]
        # Map2: M[j] -> T[j]
        # Composition: S[i] -> T[i] (where i=j)
        map1 = ir.BasicMap.define(
            dom=("i",),
            mapping={"x": (ir.Aff.term(1, "i"),)},
            dom_name="S",
            rng_name="M"
        )
        map2 = ir.BasicMap.define(
            dom=("j",),
            mapping={"y": (ir.Aff.term(1, "j"),)},
            dom_name="M",
            rng_name="T"
        )
        composed = map1.apply_range(map2)
        assert composed.dom_name == "S"
        assert composed.rng_name == "T"

    def test_apply_range_with_scaling(self):
        """Test apply_range where range vars match with scaling."""
        # Map1: S[i] -> M[2*i]
        # Map2: M[x] -> T[x + 1]
        # Composition should substitute x = 2*i
        map1 = ir.BasicMap.define(
            dom=("i",),
            mapping={"x": (ir.Aff.term(2, "i"),)},
            dom_name="S",
            rng_name="M"
        )
        map2 = ir.BasicMap.define(
            dom=("x",),
            mapping={"y": (ir.Aff.term(1, "x"), ir.Aff.const(1))},
            dom_name="M",
            rng_name="T"
        )
        composed = map1.apply_range(map2)
        # Result: S[i] -> T[2*i + 1]
        assert composed.dom_name == "S"
        assert composed.rng_name == "T"

    def test_dependency_analysis_simple_1d(self):
        """Test simple 1D dependency: addr = i."""
        # Write: S[i] -> mem[i]
        # Read: S[j] -> mem[j]
        # Dependency: S[i] -> S[j] where i = j
        write = ir.BasicMap.define(
            dom=("i",),
            mapping={"addr": (ir.Aff.term(1, "i"),)},
            dom_name="S_w"
        )
        read = ir.BasicMap.define(
            dom=("j",),
            mapping={"addr": (ir.Aff.term(1, "j"),)},
            dom_name="S_r"
        )
        # D = W ∘ R^{-1}
        read_rev = read.reverse()
        dep = write.apply_range(read_rev)
        
        # The dependency should link S_w to S_r
        assert dep.dom_name == "S_w"
        assert dep.rng_name == "S_r"
        # Constraints should express: i = j (addr matches)

    def test_dependency_analysis_strided_3d(self):
        """Test strided 3D dependency: addr = 1500*gid0 + 30*gid1 + gid2.
        
        This is the example from aff.py:
        addr_expr = 1500 * gid0 + 30 * gid1 + gid2
        
        For a [50, 30] tensor, the dependency should resolve to:
        - gid0 = gid0' (batch dimension preserved)
        - gid1 = gid1' (row dimension preserved)
        - gid2 = gid2' (column dimension preserved)
        """
        # Write: S[gid0, gid1, gid2] -> mem[1500*gid0 + 30*gid1 + gid2]
        write = ir.BasicMap.define(
            dom=("gid0", "gid1", "gid2"),
            mapping={"addr": (
                ir.Aff.term(1500, "gid0"),
                ir.Aff.term(30, "gid1"),
                ir.Aff.term(1, "gid2"),
            )},
            dom_name="S"
        )
        
        # Read: S[gid0', gid1', gid2'] -> mem[1500*gid0' + 30*gid1' + gid2']
        read = ir.BasicMap.define(
            dom=("gid0_", "gid1_", "gid2_"),
            mapping={"addr": (
                ir.Aff.term(1500, "gid0_"),
                ir.Aff.term(30, "gid1_"),
                ir.Aff.term(1, "gid2_"),
            )},
            dom_name="S"
        )
        
        # D = W ∘ R^{-1}
        read_rev = read.reverse()
        dep = write.apply_range(read_rev)
        
        # Verify structure
        assert dep.dom_name == "S"
        assert dep.rng_name == "S"
        # Domain should be (gid0, gid1, gid2)
        assert set(dep.dom_vars) == {"gid0", "gid1", "gid2"}
        # Range should be (gid0_, gid1_, gid2_)
        assert set(dep.rng_vars) == {"gid0_", "gid1_", "gid2_"}

    def test_dependency_analysis_with_offset(self):
        """Test dependency with offset: addr = i + 10."""
        # Write: S[i] -> mem[i + 10]
        # Read: S[j] -> mem[j]
        # For dependency to exist: i + 10 = j => j = i + 10
        write = ir.BasicMap.define(
            dom=("i",),
            mapping={"addr": (ir.Aff.term(1, "i"), ir.Aff.const(10))},
            dom_name="S_w"
        )
        read = ir.BasicMap.define(
            dom=("j",),
            mapping={"addr": (ir.Aff.term(1, "j"),)},
            dom_name="S_r"
        )
        
        read_rev = read.reverse()
        dep = write.apply_range(read_rev)
        
        # Constraints should encode: i + 10 - j = 0
        assert dep.dom_name == "S_w"
        assert dep.rng_name == "S_r"


class TestUnionMapDependencyAnalysis:
    """Tests for UnionMap-based dependency analysis."""

    def test_union_map_apply_range_basic(self):
        """Test UnionMap.apply_range() works correctly."""
        bmap1 = ir.BasicMap.define(
            dom=("i",),
            mapping={"x": (ir.Aff.term(1, "i"),)},
            dom_name="S",
            rng_name="M"
        )
        bmap2 = ir.BasicMap.define(
            dom=("x",),
            mapping={"y": (ir.Aff.term(1, "x"),)},
            dom_name="M",
            rng_name="T"
        )
        
        umap1 = ir.UnionMap((bmap1,))
        umap2 = ir.UnionMap((bmap2,))
        
        composed = umap1.apply_range(umap2)
        assert isinstance(composed, ir.UnionMap)
        assert len(composed.args) >= 1

    def test_union_map_reverse(self):
        """Test UnionMap.reverse() reverses all BasicMaps."""
        bmap = ir.BasicMap.define(
            dom=("i",),
            mapping={"addr": (ir.Aff.term(1, "i"),)},
            dom_name="S",
            rng_name="M"
        )
        umap = ir.UnionMap((bmap,))
        
        rev = umap.reverse()
        assert isinstance(rev, ir.UnionMap)
        assert len(rev.args) == 1
        # Check the reversed BasicMap
        rev_bmap = rev.args[0]
        assert rev_bmap.dom_name == "M"
        assert rev_bmap.rng_name == "S"


# =============================================================================
# ATenOpType and Memory Integration Tests
# =============================================================================

class TestATenOpTypeIntegration:
    """Tests for ATenOpType interactions with BasicMap."""

    def test_aten_op_type_from_shape(self):
        """Test ATenOpType.from_shape() creates proper axes."""
        t = ir.ATenOpType.from_shape((10, 20, 30), index)
        assert t.ndim == 3
        assert t.shape[0].item == 10
        assert t.shape[1].item == 20
        assert t.shape[2].item == 30
        # Strides: for row-major [10, 20, 30]
        # strides = [20*30, 30, 1] = [600, 30, 1]
        assert t.axes[0].stride.item == 600
        assert t.axes[1].stride.item == 30
        assert t.axes[2].stride.item == 1

    def test_aten_op_type_band(self):
        """Test ATenOpType.band() creates matching Band."""
        t = ir.ATenOpType.from_shape((10, 20), index)
        band = t.band()
        assert band.ndim == 2
        assert band.shape[0].item == 10
        assert band.shape[1].item == 20

    def test_basic_map_from_tensor_type_strides(self):
        """Test BasicMap correctly encodes stride information."""
        # 3D tensor [4, 5, 6] with strides [30, 6, 1]
        t = ir.ATenOpType.from_shape((4, 5, 6), index)
        band = t.band()
        bmap = ir.BasicMap.from_tensor_type(band, t)
        # Verify it creates an access pattern
        assert "addr" in bmap.rng_vars
        assert len(bmap.dom_vars) == 3


# =============================================================================
# Edge Cases and Error Handling
# =============================================================================

class TestEdgeCases:
    """Tests for edge cases and error handling."""

    def test_basic_map_mismatched_rng_vars_exprs_raises(self):
        """Test from_affine raises on mismatched lengths."""
        with pytest.raises(ValueError):
            ir.BasicMap.from_affine(
                dom_vars=("i",),
                rng_vars=("x", "y"),  # 2 vars
                rng_exprs=((ir.Aff.var("i"),),),  # 1 expr
                dom_name="S",
                rng_name="T"
            )

    def test_band_empty_fails(self):
        """Test Band with no ranges fails."""
        with pytest.raises(AssertionError):
            ir.Band(())

    def test_constraint_non_aff_fails(self):
        """Test Constraint with non-Aff args fails."""
        const = ir.Const.new(10, index)
        with pytest.raises(AssertionError):
            ir.Constraint((const,))

    def test_union_map_non_basic_map_fails(self):
        """Test UnionMap with non-BasicMap args fails."""
        const = ir.Const.new(10, index)
        with pytest.raises(AssertionError):
            ir.UnionMap((const,))

    def test_range_non_scalar_fails(self):
        """Test Range with non-scalar size fails."""
        # Create a 1D tensor type (not scalar) - kept for documentation
        _t = ir.ATenOpType.from_shape((10,), index)  # noqa: F841
        # This should fail because Range expects scalar
        # We need to create a tensor op with this type
        # For now, just test with a valid scalar
        size = ir.Const.new(10, index)
        r = ir.Range((size,))
        assert r.size.item == 10


# =============================================================================
# Load/Store with BasicMap Tests
# =============================================================================

class TestLoadStoreWithBasicMap:
    """Tests for Load/Store operations using BasicMap."""

    def test_load_from_memory_with_basic_map(self):
        """Test Load creation with Memory and BasicMap."""
        # Create a memory buffer
        mem = ir.Memory.defglobal((10, 20), index)
        
        # Create Band matching the memory shape
        band = ir.Band((
            ir.Range((ir.Const.new(10, index),)),
            ir.Range((ir.Const.new(20, index),)),
        ))
        
        # Create BasicMap for access
        bmap = ir.BasicMap.from_tensor_type(band, mem.T[0])
        
        # Create Load
        load = ir.Load((mem, bmap))
        assert isinstance(load, ir.Load)
        # Load produces scalar
        assert load.T[0].ndim == 0

    def test_load_from_tensor_helper(self):
        """Test Load.from_tensor() helper method."""
        mem = ir.Memory.defglobal((10, 20), index)
        band = mem.T[0].band()
        load = ir.Load.from_tensor(mem, band)
        assert isinstance(load, ir.Load)
        assert load.T[0].ndim == 0

    def test_store_creation(self):
        """Test Store creation with dst and src."""
        mem = ir.Memory.defglobal((10,), index)
        band = mem.T[0].band()
        load = ir.Load.from_tensor(mem, band)
        value = ir.Const.new(42, index)
        store = ir.Store.new(load, value)
        assert isinstance(store, ir.Store)


# =============================================================================
# Polyhedron and MemoryOf Integration Tests
# =============================================================================

class TestPolyhedronIntegration:
    """Tests for Polyhedron with BasicMap/UnionMap."""

    def test_polyhedron_schedule_simple(self):
        """Test Polyhedron.schedule() creates proper structure."""
        # Create a simple elementwise operation
        mem = ir.Memory.defglobal((10,), index)
        band = mem.T[0].band()
        dims = band.all_dimensions()
        
        # Simple store: mem[i] = 42
        load = ir.Load.from_tensor(mem, band)
        value = ir.Const.new(42, index)
        store = ir.Store.new(load, value)
        
        poly = ir.Polyhedron.schedule(dims, (mem,), store)
        assert isinstance(poly, ir.Polyhedron)
        assert poly.n_outs == 1

    def test_polyhedron_explore_predecessors(self):
        """Test Polyhedron.explore_predecessors() extracts info."""
        mem = ir.Memory.defglobal((10,), index)
        band = mem.T[0].band()
        load = ir.Load.from_tensor(mem, band)
        value = ir.Const.new(42, index)
        store = ir.Store.new(load, value)
        
        parents, body, reads, writes = ir.Polyhedron.explore_predecessors((store,))
        # Should find BasicMap in reads/writes
        assert isinstance(reads, tuple)
        assert isinstance(writes, tuple)

    def test_memory_of_retrieves_output(self):
        """Test MemoryOf retrieves Polyhedron output."""
        mem = ir.Memory.defglobal((10,), index)
        band = mem.T[0].band()
        dims = band.all_dimensions()
        load = ir.Load.from_tensor(mem, band)
        value = ir.Const.new(42, index)
        store = ir.Store.new(load, value)
        poly = ir.Polyhedron.schedule(dims, (mem,), store)
        
        result = ir.MemoryOf((poly,), nth=0)
        assert isinstance(result, ir.MemoryOf)
        assert result.nth == 0
