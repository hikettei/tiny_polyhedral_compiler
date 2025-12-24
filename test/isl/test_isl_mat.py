import pytest  # type: ignore

import caten.isl as I
from caten.isl.ffi import load_libisl

load_libisl()  # dependency


def test_mat_alloc_set_and_dims() -> None:
    with I.context():
        m = I.Mat.alloc(2, 2)
        assert m.rows() == 2
        assert m.cols() == 2
        m2 = m.set_element_si(0, 0, 3)
        assert isinstance(m2, I.Mat)
        m.free()
        with pytest.raises(RuntimeError):
            _ = m.copy()
