
import caten.isl as I
from caten.isl.ffi import load_libisl

load_libisl()  # hard dependency


def test_constraint_alloc_and_basic_ops() -> None:
    with I.context():
        sp = I.Space.alloc(0, 0, 1)
        ls = I.LocalSpace.from_space(sp)
        c_eq = I.Constraint.equality(ls)
        assert c_eq.get_local_space().dim(I.ISLDimType.ISL_DIM_SET) == 1
        c_eq = c_eq.set_constant_si(0)
        c_eq = c_eq.set_coefficient_si(I.ISLDimType.ISL_DIM_SET, 0, 1)
        assert isinstance(str(c_eq), str)
        assert "Constraint(" in repr(c_eq)

        c_ineq = I.Constraint.inequality(I.LocalSpace.from_space(I.Space.alloc(0, 0, 1)))
        assert c_ineq.get_space().dim(I.ISLDimType.ISL_DIM_SET) == 1
        assert c_ineq.copy()  # copy works


def test_constraint_dim_name_and_user() -> None:
    with I.context():
        sp = I.Space.set_alloc(0, 1)
        sp = sp.set_tuple_name(I.ISLDimType.ISL_DIM_SET, "S")
        ls = I.LocalSpace.from_space(sp)
        c = I.Constraint.equality(ls)
        assert c.get_space().get_tuple_name(I.ISLDimType.ISL_DIM_SET) == "S"
