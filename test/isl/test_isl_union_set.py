
import caten.isl as I
from caten.isl.ffi import load_libisl

load_libisl()  # hard dependency


def test_union_set_basic_ops() -> None:
    with I.context():
        a = I.UnionSet("{ [i] : 0 <= i <= 2 }")
        b = I.UnionSet("{ [i] : i = 1 }")

        a_copy = a.copy()
        assert a.is_equal(a_copy)

        uni = a.union(b)
        assert uni.is_equal(a)  # b is subset of a

        inter = a.intersect(b)
        assert inter.is_equal(b)

        diff = a.subtract(b)
        expected = I.UnionSet("{ [i] : i = 0; [i] : i = 2 }")
        assert diff.is_equal(expected)

        prod = a.product(b)
        assert prod.is_empty() is False

        gist = a.gist(b)
        assert isinstance(gist, I.UnionSet)

        assert a.n_set() == 1
        assert a.is_subset(uni)
        assert not a.is_strict_subset(a)


def test_union_set_str_repr_and_params() -> None:
    with I.context():
        u = I.UnionSet("{ [i] : 0 <= i <= 1 }")
        assert isinstance(str(u), str)
        assert "UnionSet(" in repr(u)
        # params() returns Set; for non-param union set keeps same constraint
        params = u.params()
        assert isinstance(params, I.Set)


def test_union_set_regularization_helpers() -> None:
    with I.context():
        u = I.UnionSet("{ [i] : 0 <= i <= 3 }")
        assert u.coalesce().is_equal(u)
        assert u.detect_equalities().is_equal(u)
        assert u.remove_redundancies().is_equal(u)
        assert isinstance(u.project_out_all_params(), I.UnionSet)
        assert isinstance(u.lift(), I.UnionSet)
        try:
            _ = u.drop_unused_params().is_equal(u)
        except NotImplementedError:
            pass
        assert u.lexmin().is_subset(u)
        assert u.lexmax().is_subset(u)
