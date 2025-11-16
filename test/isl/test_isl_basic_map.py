import caten.isl as I
from caten.isl.ffi import load_libisl

load_libisl()  # dependency


def test_basic_map_read_copy_and_dim() -> None:
    with I.context():
        bm = I.BasicMap("{ [i] -> [j] : i = j }")
        assert bm.dim(I.ISLDimType.ISL_DIM_OUT) == 1
        assert bm.is_empty() is False
        bm2 = bm.copy()
        assert bm2.is_equal(bm)
        assert bm.n_constraint() >= 1
        assert "BasicMap(" in repr(bm)


def test_basic_map_ops() -> None:
    with I.context():
        space = I.Space.alloc(0, 1, 1)
        universe = I.BasicMap.universe(space)
        ls = universe.get_local_space()
        c = I.Constraint.equality(ls)
        c = c.set_constant_si(0).set_coefficient_si(I.ISLDimType.ISL_DIM_OUT, 0, 1)
        constrained = universe.add_constraint(c)
        assert constrained.n_constraint() >= 1

        inter = constrained.intersect(I.BasicMap("{ [i] -> [j] : i = j and i = 0 }") )
        assert inter.n_constraint() >= 1

        proj = constrained.project_out(I.ISLDimType.ISL_DIM_OUT, 0, 1)
        assert proj.dim(I.ISLDimType.ISL_DIM_OUT) == 0

        cleaned = constrained.remove_unknown_divs().remove_divs()
        assert cleaned.n_constraint() >= 1
