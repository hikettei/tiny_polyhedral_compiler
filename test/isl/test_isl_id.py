import pytest  # type: ignore

import caten.isl as I
from caten.isl.ffi import load_libisl

load_libisl()  # hard dependency


def test_id_alloc_copy_free() -> None:
    with I.context():
        ident = I.Id.alloc("foo")
        assert ident.name() == "foo"
        ident_copy = ident.copy()
        assert ident_copy.name() == "foo"
        ident.free()
        with pytest.raises(RuntimeError):
            _ = ident.copy()


def test_id_str_and_user() -> None:
    with I.context():
        ident = I.Id.from_str("foo")
        assert str(ident) == "foo"
        assert "Id(" in repr(ident)
        assert ident.user() is None
