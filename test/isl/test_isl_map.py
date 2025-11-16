import caten.isl as I
from caten.isl.ffi import load_libisl

load_libisl()  # dependency


def test_map_read_copy_and_dim() -> None:
    with I.context():
        m = I.Map("{ [i] -> [j] : i = j }")
        assert m.dim(I.ISLDimType.ISL_DIM_OUT) == 1
        assert m.is_empty() is False
        cp = m.copy()
        assert m.is_equal(cp)
        assert "Map(" in repr(m)


def test_map_ops() -> None:
    with I.context():
        space = I.Space.alloc(0, 1, 1)
        uni = I.Map.universe(space)
        ls = I.LocalSpace.from_space(I.Space.alloc(0, 1, 1))
        c = I.Constraint.equality(ls)
        c = c.set_constant_si(0).set_coefficient_si(I.ISLDimType.ISL_DIM_OUT, 0, 1)
        constrained = uni.add_constraint(c)
        inter = constrained.intersect(I.Map("{ [i] -> [j] : i = j and i = 0 }") )
        assert inter.is_empty() is False
        proj = constrained.project_out(I.ISLDimType.ISL_DIM_OUT, 0, 1)
        assert proj.dim(I.ISLDimType.ISL_DIM_OUT) == 0
        cleaned = constrained.remove_unknown_divs().remove_divs()
        assert cleaned.is_empty() is False
