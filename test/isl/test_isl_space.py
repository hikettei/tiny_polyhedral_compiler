import pytest

import caten.isl as I
from caten.isl.ffi import load_libisl

load_libisl()  # hard dependency


def test_space_alloc_and_copy() -> None:
    with I.context():
        sp = I.Space.alloc(0, 1, 1)
        assert sp.dim(2) == 1  # out
        assert sp.dim(3) == 1  # aggregate set/range
        cp = sp.copy()
        assert cp.dim(2) == 1
        sp.free()
        with pytest.raises(RuntimeError):
            _ = sp.copy()


def test_space_tuple_names() -> None:
    with I.context():
        sp = I.Space.set_alloc(0, 1)
        sp = sp.set_tuple_name(3, "S")
        assert sp.get_tuple_name(3) == "S"
        assert isinstance(str(sp), str)
        assert "Space(" in repr(sp)


def test_local_space_from_space() -> None:
    with I.context():
        sp = I.Space.alloc(0, 0, 1)
        ls = I.LocalSpace.from_space(sp)
        assert ls.dim(3) == 1
        assert isinstance(str(ls), str)
        assert "LocalSpace(" in repr(ls)
