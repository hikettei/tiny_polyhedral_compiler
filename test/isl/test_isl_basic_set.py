import caten.isl as I
from caten.isl.ffi import load_libisl

load_libisl()  # dependency


def test_basic_set_read_copy_to_set() -> None:
    with I.context():
        bs = I.BasicSet("{ [i] : 0 <= i <= 1 }")
        assert bs.dim(I.ISLDimType.ISL_DIM_SET) == 1
        assert bs.is_empty() is False
        bs2 = bs.copy()
        assert bs.is_equal(bs2)
        s = bs.to_set()
        assert s.is_equal(I.Set("{ [i] : 0 <= i <= 1 }") )


def test_basic_set_ops() -> None:
    with I.context():
        space = I.Space.set_alloc(0, 1)
        empty = I.BasicSet.empty(space)
        assert empty.is_empty()

        u = I.BasicSet.universe(space)
        ls = I.LocalSpace.from_space(space)
        c = I.Constraint.equality(ls)
        c = c.set_constant_si(0).set_coefficient_si(I.ISLDimType.ISL_DIM_SET, 0, 1)
        u2 = u.add_constraint(c)
        assert u2.n_constraint() >= 1

        inter = u2.intersect(bs := I.BasicSet("{ [i] : i = 0 }") )
        assert inter.is_equal(bs)

        proj = u2.project_out(I.ISLDimType.ISL_DIM_SET, 0, 1)
        assert proj.dim(I.ISLDimType.ISL_DIM_SET) == 0

        cleaned = u2.remove_unknown_divs().remove_divs()
        assert cleaned.n_constraint() >= 1
