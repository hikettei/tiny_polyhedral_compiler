import pytest

import caten.isl as I
from caten.isl.ffi import load_libisl

load_libisl()  # dependency


def test_multi_val_basic() -> None:
    with I.context():
        sp = I.Space.set_alloc(0, 1)
        mv = I.MultiVal.zero(sp)
        assert mv.dim(I.ISLDimType.ISL_DIM_SET) == 1
        assert mv.get_space().dim(I.ISLDimType.ISL_DIM_SET) == 1
        mv.copy()
        assert isinstance(str(mv), str)
        mv.free()
        with pytest.raises(RuntimeError):
            _ = mv.copy()


def test_multi_val_zero() -> None:
    with I.context():
        sp = I.Space.set_alloc(0, 1)
        mv = I.MultiVal.zero(sp)
        assert mv.dim(I.ISLDimType.ISL_DIM_SET) == 1
