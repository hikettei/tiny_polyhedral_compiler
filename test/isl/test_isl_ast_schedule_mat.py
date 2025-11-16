import caten.isl as I
from caten.isl.ffi import load_libisl

load_libisl()  # dependency


def test_ast_expr_from_val() -> None:
    with I.context():
        v = I.Val.int_from_si(5)
        expr = I.ASTExpr.from_val(v)
        assert expr.get_type() >= 0
        expr.copy()


def test_schedule_empty() -> None:
    with I.context():
        space = I.Space.set_alloc(0, 1)
        sched = I.Schedule.empty(space)
        assert sched.get_domain().is_empty()
        sched.copy()


def test_mat_alloc() -> None:
    with I.context():
        m = I.Mat.alloc(1, 1)
        m = m.set_element_si(0, 0, 1)
        assert m.rows() == 1 and m.cols() == 1
