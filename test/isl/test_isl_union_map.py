import caten.isl as I
from caten.isl.ffi import load_libisl

load_libisl()  # dependency


def test_union_map_read_copy_basic_ops() -> None:
    with I.context():
        um = I.UnionMap("{ [i] -> [j] : i = j }")
        assert um.is_empty() is False
        cp = um.copy()
        assert um.is_equal(cp)
        assert um.n_map() >= 1
        assert "UnionMap(" in repr(um)


def test_union_map_set_ops() -> None:
    with I.context():
        um2 = I.UnionMap.from_map(I.Map("{ [i] -> [j] : i = j }"))
        inter = um2.intersect(um2.copy())
        assert inter.is_empty() is False

        diff = um2.subtract(um2.copy())
        assert diff.is_empty() is True

        prod = um2.product(um2.copy())
        assert prod.is_empty() is False

        gist = um2.gist(um2.copy())
        assert gist.is_empty() is False
