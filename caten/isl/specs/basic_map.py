from __future__ import annotations

from ctypes import c_int, c_void_p

from ..ffi import FfiPointer, load_libisl
from ..func import ISLFunction
from ..obj import ISLObject
from ..qualifier import Give, Keep, Null, Param, Take
from .constraint import Constraint
from .context import Context
from .space import LocalSpace, Space

_lib = load_libisl()


class BasicMap(ISLObject):
    """Wrapper around ``isl_basic_map``."""

    __slots__ = ()

    def __init__(self, handle_or_spec: str | FfiPointer) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_basic_map_read_from_str(handle_or_spec, return_raw_pointer=True)
        else:
            handle = handle_or_spec
        super().__init__(handle)

    @classmethod
    def from_str(cls, spec: str) -> "BasicMap":
        return _isl_basic_map_read_from_str(spec)

    @classmethod
    def empty(cls, space: Space) -> "BasicMap":
        return _isl_basic_map_empty(space)

    @classmethod
    def universe(cls, space: Space) -> "BasicMap":
        return _isl_basic_map_universe(space)

    def copy_handle(self) -> FfiPointer:
        return _isl_basic_map_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_basic_map_free(handle)

    # queries
    def get_space(self) -> Space:
        return _isl_basic_map_get_space(self)

    def get_local_space(self) -> LocalSpace:
        return _isl_basic_map_get_local_space(self)

    def dim(self, dim_type: int) -> int:
        return _isl_basic_map_dim(self, dim_type)

    def is_empty(self) -> bool:
        return _isl_basic_map_is_empty(self)

    def is_equal(self, other: "BasicMap") -> bool:
        return _isl_basic_map_is_equal(self, other)

    def n_constraint(self) -> int:
        return _isl_basic_map_n_constraint(self)

    # transforms
    def add_constraint(self, constraint: Constraint) -> "BasicMap":
        return _isl_basic_map_add_constraint(self, constraint)

    def intersect(self, other: "BasicMap") -> "BasicMap":
        return _isl_basic_map_intersect(self, other)

    def project_out(self, dim_type: int, first: int, n: int) -> "BasicMap":
        return _isl_basic_map_project_out(self, dim_type, first, n)

    def remove_divs(self) -> "BasicMap":
        return _isl_basic_map_remove_divs(self)

    def remove_unknown_divs(self) -> "BasicMap":
        return _isl_basic_map_remove_unknown_divs(self)

    # string
    def __str__(self) -> str:  # pragma: no cover
        return _isl_basic_map_to_str(self)

    def __repr__(self) -> str:  # pragma: no cover
        return f"BasicMap({self.__str__()})"


_isl_basic_map_read_from_str = ISLFunction.create(
    _lib.isl_basic_map_read_from_str,
    Context(),
    Param(str),
    return_=Give(BasicMap),
    lib=_lib,
)

_isl_basic_map_copy = ISLFunction.create(
    _lib.isl_basic_map_copy,
    Keep(BasicMap),
    return_=Give(BasicMap),
    lib=_lib,
)

_isl_basic_map_free = ISLFunction.create(
    _lib.isl_basic_map_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

_isl_basic_map_get_space = ISLFunction.create(
    _lib.isl_basic_map_get_space,
    Keep(BasicMap),
    return_=Give(Space),
    lib=_lib,
)

_isl_basic_map_get_local_space = ISLFunction.create(
    _lib.isl_basic_map_get_local_space,
    Keep(BasicMap),
    return_=Give(LocalSpace),
    lib=_lib,
)

_isl_basic_map_dim = ISLFunction.create(
    _lib.isl_basic_map_dim,
    Keep(BasicMap),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_basic_map_is_empty = ISLFunction.create(
    _lib.isl_basic_map_is_empty,
    Keep(BasicMap),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_basic_map_is_equal = ISLFunction.create(
    _lib.isl_basic_map_is_equal,
    Keep(BasicMap),
    Keep(BasicMap),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_basic_map_n_constraint = ISLFunction.create(
    _lib.isl_basic_map_n_constraint,
    Keep(BasicMap),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_basic_map_add_constraint = ISLFunction.create(
    _lib.isl_basic_map_add_constraint,
    Take(BasicMap),
    Take(Constraint),
    return_=Give(BasicMap),
    lib=_lib,
)

_isl_basic_map_project_out = ISLFunction.create(
    _lib.isl_basic_map_project_out,
    Take(BasicMap),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(BasicMap),
    lib=_lib,
)

_isl_basic_map_intersect = ISLFunction.create(
    _lib.isl_basic_map_intersect,
    Take(BasicMap),
    Take(BasicMap),
    return_=Give(BasicMap),
    lib=_lib,
)

_isl_basic_map_remove_divs = ISLFunction.create(
    _lib.isl_basic_map_remove_divs,
    Take(BasicMap),
    return_=Give(BasicMap),
    lib=_lib,
)

_isl_basic_map_remove_unknown_divs = ISLFunction.create(
    _lib.isl_basic_map_remove_unknown_divs,
    Take(BasicMap),
    return_=Give(BasicMap),
    lib=_lib,
)

_isl_basic_map_empty = ISLFunction.create(
    _lib.isl_basic_map_empty,
    Take(Space),
    return_=Give(BasicMap),
    lib=_lib,
)

_isl_basic_map_universe = ISLFunction.create(
    _lib.isl_basic_map_universe,
    Take(Space),
    return_=Give(BasicMap),
    lib=_lib,
)

_isl_basic_map_to_str = ISLFunction.create(
    _lib.isl_basic_map_to_str,
    Keep(BasicMap),
    return_=Param(str),
    lib=_lib,
)

__all__ = ["BasicMap"]

