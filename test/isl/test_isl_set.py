import pytest  # type: ignore

import caten.isl as I
from caten.isl.ffi import load_libisl

load_libisl()  # hard dependency


def test_set_copy_and_free() -> None:
    with I.context():
        s = I.Set("{ [i] : 0 <= i <= 1 }")
        s_copy = s.copy()
        assert s.is_equal(s_copy)
        s.free()
        assert s._handle is None
        with pytest.raises(RuntimeError):
            _ = s.copy()


def test_union_intersect_and_subtract() -> None:
    with I.context():
        a = I.Set("{ [i] : 0 <= i <= 1 }")
        b = I.Set("{ [i] : 1 <= i <= 2 }")

        union = a.union(b)
        expected_union = I.Set("{ [i] : 0 <= i <= 2 }")
        assert union.is_equal(expected_union)

        inter = a.intersect(b)
        expected_inter = I.Set("{ [i] : i = 1 }")
        assert inter.is_equal(expected_inter)

        diff = expected_union.subtract(b)
        expected_diff = I.Set("{ [i] : i = 0 }")
        assert diff.is_equal(expected_diff)

        prod = a.product(b)
        assert prod.tuple_dim() == 2
        assert prod.plain_is_empty() is False

        gist = expected_union.gist(a)
        assert gist.plain_is_empty() is False

        neg = a.neg()
        assert neg.is_equal(I.Set("{ [i] : -1 <= i <= 0 }"))


def test_is_empty_flags() -> None:
    with I.context():
        base = I.Set("{ [i] : i = 3 }")
        empty = base.subtract(base)
        assert empty.is_empty()
        assert base.is_equal(I.Set("{ [i] : i = 3 }") )

def test_tuple_and_wrapping_queries() -> None:
    with I.context():
        s = I.Set("{ [i] : 0 <= i < 4 }")
        assert s.tuple_dim() == 1
        _ = s.has_tuple_name()
        assert s.get_tuple_name() is None or isinstance(s.get_tuple_name(), str)
        assert s.plain_is_universe() is False
        assert s.plain_is_empty() is False


def test_str_and_repr_use_libisl() -> None:
    with I.context():
        spec = "{ [i] : 0 <= i < 2 }"
        s = I.Set(spec)
        rendered = str(s)
        assert rendered.startswith("{ [i] :")
        assert "Set(" in repr(s)
        assert "Set(" in repr(s)
