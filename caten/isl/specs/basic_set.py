from __future__ import annotations

from ctypes import c_int, c_void_p

from ..ffi import FfiPointer, load_libisl
from ..func import ISLFunction
from ..obj import ISLObject
from ..qualifier import Give, Keep, Null, Param, Take
from .constraint import Constraint
from .context import Context
from .set import Set
from .space import LocalSpace, Space

_lib = load_libisl()


class BasicSet(ISLObject):
    """Wrapper around ``isl_basic_set``."""

    __slots__ = ()

    def __init__(self, handle_or_spec: str | FfiPointer) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_basic_set_read_from_str(handle_or_spec, return_raw_pointer=True)
        else:
            handle = handle_or_spec
        super().__init__(handle)

    @classmethod
    def from_str(cls, spec: str) -> "BasicSet":
        return _isl_basic_set_read_from_str(spec)

    @classmethod
    def empty(cls, space: Space) -> "BasicSet":
        return _isl_basic_set_empty(space)

    @classmethod
    def universe(cls, space: Space) -> "BasicSet":
        return _isl_basic_set_universe(space)

    def copy_handle(self) -> FfiPointer:
        return _isl_basic_set_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_basic_set_free(handle)

    # queries
    def get_space(self) -> Space:
        return _isl_basic_set_get_space(self)

    def get_local_space(self) -> LocalSpace:
        return _isl_basic_set_get_local_space(self)

    def dim(self, dim_type: int) -> int:
        return _isl_basic_set_dim(self, dim_type)

    def is_empty(self) -> bool:
        return _isl_basic_set_is_empty(self)

    def is_equal(self, other: "BasicSet") -> bool:
        return _isl_basic_set_is_equal(self, other)

    def n_constraint(self) -> int:
        return _isl_basic_set_n_constraint(self)

    # transforms
    def add_constraint(self, constraint: Constraint) -> "BasicSet":
        return _isl_basic_set_add_constraint(self, constraint)

    def intersect(self, other: "BasicSet") -> "BasicSet":
        return _isl_basic_set_intersect(self, other)

    def project_out(self, dim_type: int, first: int, n: int) -> "BasicSet":
        return _isl_basic_set_project_out(self, dim_type, first, n)

    def remove_divs(self) -> "BasicSet":
        return _isl_basic_set_remove_divs(self)

    def remove_unknown_divs(self) -> "BasicSet":
        return _isl_basic_set_remove_unknown_divs(self)

    # conversions
    def to_set(self) -> Set:
        return _isl_basic_set_to_set(self)

    def __str__(self) -> str:  # pragma: no cover
        return _isl_basic_set_to_str(self)

    def __repr__(self) -> str:  # pragma: no cover
        return f"BasicSet({self.__str__()})"


_isl_basic_set_read_from_str = ISLFunction.create(
    _lib.isl_basic_set_read_from_str,
    Context(),
    Param(str),
    return_=Give(BasicSet),
    lib=_lib,
)

_isl_basic_set_copy = ISLFunction.create(
    _lib.isl_basic_set_copy,
    Keep(BasicSet),
    return_=Give(BasicSet),
    lib=_lib,
)

_isl_basic_set_free = ISLFunction.create(
    _lib.isl_basic_set_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

_isl_basic_set_get_space = ISLFunction.create(
    _lib.isl_basic_set_get_space,
    Keep(BasicSet),
    return_=Give(Space),
    lib=_lib,
)

_isl_basic_set_get_local_space = ISLFunction.create(
    _lib.isl_basic_set_get_local_space,
    Keep(BasicSet),
    return_=Give(LocalSpace),
    lib=_lib,
)

_isl_basic_set_dim = ISLFunction.create(
    _lib.isl_basic_set_dim,
    Keep(BasicSet),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_basic_set_is_empty = ISLFunction.create(
    _lib.isl_basic_set_is_empty,
    Keep(BasicSet),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_basic_set_is_equal = ISLFunction.create(
    _lib.isl_basic_set_is_equal,
    Keep(BasicSet),
    Keep(BasicSet),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_basic_set_n_constraint = ISLFunction.create(
    _lib.isl_basic_set_n_constraint,
    Keep(BasicSet),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_basic_set_add_constraint = ISLFunction.create(
    _lib.isl_basic_set_add_constraint,
    Take(BasicSet),
    Take(Constraint),
    return_=Give(BasicSet),
    lib=_lib,
)

_isl_basic_set_project_out = ISLFunction.create(
    _lib.isl_basic_set_project_out,
    Take(BasicSet),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(BasicSet),
    lib=_lib,
)

_isl_basic_set_intersect = ISLFunction.create(
    _lib.isl_basic_set_intersect,
    Take(BasicSet),
    Take(BasicSet),
    return_=Give(BasicSet),
    lib=_lib,
)

_isl_basic_set_remove_divs = ISLFunction.create(
    _lib.isl_basic_set_remove_divs,
    Take(BasicSet),
    return_=Give(BasicSet),
    lib=_lib,
)

_isl_basic_set_remove_unknown_divs = ISLFunction.create(
    _lib.isl_basic_set_remove_unknown_divs,
    Take(BasicSet),
    return_=Give(BasicSet),
    lib=_lib,
)

_isl_basic_set_empty = ISLFunction.create(
    _lib.isl_basic_set_empty,
    Take(Space),
    return_=Give(BasicSet),
    lib=_lib,
)

_isl_basic_set_universe = ISLFunction.create(
    _lib.isl_basic_set_universe,
    Take(Space),
    return_=Give(BasicSet),
    lib=_lib,
)

_isl_basic_set_to_set = ISLFunction.create(
    _lib.isl_basic_set_to_set,
    Take(BasicSet),
    return_=Give(Set),
    lib=_lib,
)

_isl_basic_set_to_str = ISLFunction.create(
    _lib.isl_basic_set_to_str,
    Keep(BasicSet),
    return_=Param(str),
    lib=_lib,
)

__all__ = ["BasicSet"]
