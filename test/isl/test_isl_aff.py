import caten.isl as I
from caten.isl.ffi import load_libisl

load_libisl()  # dependency


def test_aff_and_pw_aff_basic() -> None:
    with I.context():
        space = I.Space.set_alloc(0, 1)
        aff = I.Aff.zero_on_domain_space(space)
        assert aff.dim(I.ISLDimType.ISL_DIM_SET) == 1
        assert aff.get_domain_space().dim(I.ISLDimType.ISL_DIM_SET) == 1
        assert aff.is_equal(aff.copy())
        pw = I.PwAff.from_aff(aff)
        assert isinstance(pw, I.PwAff)
        assert pw.is_equal(pw.copy())
        assert "Aff(" in repr(aff)
        assert "PwAff(" in repr(pw)


def test_multi_aff_and_pw_multi_aff() -> None:
    with I.context():
        ma = I.MultiAff.from_str("{ [i] -> [i, i] }")
        assert ma.dim(I.ISLDimType.ISL_DIM_OUT) >= 1
        assert ma.copy()
        pma = I.PwMultiAff.from_str("{ [i] -> [i] }")
        assert pma.dim(I.ISLDimType.ISL_DIM_OUT) == 1
        assert "MultiAff(" in repr(ma)
        assert "PwMultiAff(" in repr(pma)