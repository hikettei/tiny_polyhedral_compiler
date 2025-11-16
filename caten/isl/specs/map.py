from __future__ import annotations

from ctypes import c_int, c_void_p

from ..ffi import FfiPointer, load_libisl
from ..func import ISLFunction
from ..obj import ISLObject
from ..qualifier import Give, Keep, Null, Param, Take
from .basic_map import BasicMap
from .constraint import Constraint
from .context import Context
from .space import Space

_lib = load_libisl()


class Map(ISLObject):
    """Wrapper around ``isl_map``."""

    __slots__ = ()

    def __init__(self, handle_or_spec: str | FfiPointer) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_map_read_from_str(handle_or_spec, return_raw_pointer=True)
        else:
            handle = handle_or_spec
        super().__init__(handle)

    @classmethod
    def from_str(cls, spec: str) -> "Map":
        return _isl_map_read_from_str(spec)

    @classmethod
    def empty(cls, space: Space) -> "Map":
        return _isl_map_empty(space)

    @classmethod
    def universe(cls, space: Space) -> "Map":
        return _isl_map_universe(space)

    @classmethod
    def from_basic_map(cls, bmap: BasicMap) -> "Map":
        return _isl_map_from_basic_map(bmap)

    def copy_handle(self) -> FfiPointer:
        return _isl_map_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_map_free(handle)

    # queries
    def dim(self, dim_type: int) -> int:
        return _isl_map_dim(self, dim_type)

    def is_empty(self) -> bool:
        return _isl_map_is_empty(self)

    def is_equal(self, other: "Map") -> bool:
        return _isl_map_is_equal(self, other)

    # transforms
    def add_constraint(self, constraint: Constraint) -> "Map":
        return _isl_map_add_constraint(self, constraint)

    def intersect(self, other: "Map") -> "Map":
        return _isl_map_intersect(self, other)

    def project_out(self, dim_type: int, first: int, n: int) -> "Map":
        return _isl_map_project_out(self, dim_type, first, n)

    def remove_divs(self) -> "Map":
        return _isl_map_remove_divs(self)

    def remove_unknown_divs(self) -> "Map":
        return _isl_map_remove_unknown_divs(self)

    def __str__(self) -> str:  # pragma: no cover
        return _isl_map_to_str(self)

    def __repr__(self) -> str:  # pragma: no cover
        return f"Map({self.__str__()})"


_isl_map_read_from_str = ISLFunction.create(
    _lib.isl_map_read_from_str,
    Context(),
    Param(str),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_copy = ISLFunction.create(
    _lib.isl_map_copy,
    Keep(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_free = ISLFunction.create(
    _lib.isl_map_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

_isl_map_empty = ISLFunction.create(
    _lib.isl_map_empty,
    Take(Space),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_universe = ISLFunction.create(
    _lib.isl_map_universe,
    Take(Space),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_from_basic_map = ISLFunction.create(
    _lib.isl_map_from_basic_map,
    Take(BasicMap),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_dim = ISLFunction.create(
    _lib.isl_map_dim,
    Keep(Map),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_map_is_empty = ISLFunction.create(
    _lib.isl_map_is_empty,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_equal = ISLFunction.create(
    _lib.isl_map_is_equal,
    Keep(Map),
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_add_constraint = ISLFunction.create(
    _lib.isl_map_add_constraint,
    Take(Map),
    Take(Constraint),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_intersect = ISLFunction.create(
    _lib.isl_map_intersect,
    Take(Map),
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_project_out = ISLFunction.create(
    _lib.isl_map_project_out,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_remove_divs = ISLFunction.create(
    _lib.isl_map_remove_divs,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_remove_unknown_divs = ISLFunction.create(
    _lib.isl_map_remove_unknown_divs,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_to_str = ISLFunction.create(
    _lib.isl_map_to_str,
    Keep(Map),
    return_=Param(str),
    lib=_lib,
)

__all__ = ["Map"]
