from __future__ import annotations

from ctypes import c_int, c_void_p

from ..ffi import FfiPointer, load_libisl
from ..func import ISLFunction
from ..obj import ISLObject
from ..qualifier import Give, Keep, Null, Param, Take
from .basic_map import BasicMap
from .context import Context
from .map import Map

_lib = load_libisl()


class UnionMap(ISLObject):
    """Wrapper around ``isl_union_map``."""

    __slots__ = ()

    def __init__(self, handle_or_spec: str | FfiPointer) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_union_map_read_from_str(handle_or_spec, return_raw_pointer=True)
        else:
            handle = handle_or_spec
        super().__init__(handle)

    @classmethod
    def from_str(cls, spec: str) -> "UnionMap":
        return _isl_union_map_read_from_str(spec)

    @classmethod
    def from_map(cls, m: Map) -> "UnionMap":
        return _isl_union_map_from_map(m)

    @classmethod
    def from_basic_map(cls, bm: BasicMap) -> "UnionMap":
        return _isl_union_map_from_basic_map(bm)

    def copy_handle(self) -> FfiPointer:
        return _isl_union_map_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_union_map_free(handle)

    # queries
    def is_empty(self) -> bool:
        return _isl_union_map_is_empty(self)

    def is_equal(self, other: "UnionMap") -> bool:
        return _isl_union_map_is_equal(self, other)

    def n_map(self) -> int:
        return _isl_union_map_n_map(self)

    # transforms
    def union(self, other: "UnionMap") -> "UnionMap":
        return _isl_union_map_union(self, other)

    def intersect(self, other: "UnionMap") -> "UnionMap":
        return _isl_union_map_intersect(self, other)

    def subtract(self, other: "UnionMap") -> "UnionMap":
        return _isl_union_map_subtract(self, other)

    def product(self, other: "UnionMap") -> "UnionMap":
        return _isl_union_map_product(self, other)

    def gist(self, context: "UnionMap") -> "UnionMap":
        return _isl_union_map_gist(self, context)

    # string
    def __str__(self) -> str:  # pragma: no cover
        return _isl_union_map_to_str(self)

    def __repr__(self) -> str:  # pragma: no cover
        return f"UnionMap({self.__str__()})"


_isl_union_map_read_from_str = ISLFunction.create(
    _lib.isl_union_map_read_from_str,
    Context(),
    Param(str),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_copy = ISLFunction.create(
    _lib.isl_union_map_copy,
    Keep(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_free = ISLFunction.create(
    _lib.isl_union_map_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

_isl_union_map_from_map = ISLFunction.create(
    _lib.isl_union_map_from_map,
    Take(Map),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_from_basic_map = ISLFunction.create(
    _lib.isl_union_map_from_basic_map,
    Take(BasicMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_is_empty = ISLFunction.create(
    _lib.isl_union_map_is_empty,
    Keep(UnionMap),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_map_is_equal = ISLFunction.create(
    _lib.isl_union_map_is_equal,
    Keep(UnionMap),
    Keep(UnionMap),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_map_n_map = ISLFunction.create(
    _lib.isl_union_map_n_map,
    Keep(UnionMap),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_map_union = ISLFunction.create(
    _lib.isl_union_map_union,
    Take(UnionMap),
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_intersect = ISLFunction.create(
    _lib.isl_union_map_intersect,
    Take(UnionMap),
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_subtract = ISLFunction.create(
    _lib.isl_union_map_subtract,
    Take(UnionMap),
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_product = ISLFunction.create(
    _lib.isl_union_map_product,
    Take(UnionMap),
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_gist = ISLFunction.create(
    _lib.isl_union_map_gist,
    Take(UnionMap),
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_to_str = ISLFunction.create(
    _lib.isl_union_map_to_str,
    Keep(UnionMap),
    return_=Param(str),
    lib=_lib,
)

__all__ = ["UnionMap"]
