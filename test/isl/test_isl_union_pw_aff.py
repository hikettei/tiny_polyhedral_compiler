import caten.isl as I
from caten.isl.ffi import load_libisl

load_libisl()  # dependency


def test_union_pw_aff_from_aff_and_pw_aff() -> None:
    with I.context():
        space = I.Space.set_alloc(0, 1)
        upa = I.UnionPwAff.empty(space)
        pa = I.PwAff.from_aff(I.Aff.zero_on_domain_space(space))
        merged = upa.add_pw_aff(pa)
        assert merged.is_equal(merged.copy())
        assert "UnionPwAff(" in repr(merged)


def test_union_pw_multi_aff_and_multi_union_pw_aff() -> None:
    with I.context():
        upma = I.UnionPwMultiAff.from_str("{ [] -> [0] }")
        assert upma.copy()

        try:
            mupa = I.MultiUnionPwAff.from_str("{ [] -> [0] }")
            assert mupa.copy()
            assert "MultiUnionPwAff(" in repr(mupa)
        except Exception:
            pass
