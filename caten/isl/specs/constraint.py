from __future__ import annotations

from ctypes import c_int, c_void_p

from ..ffi import FfiPointer, load_libisl
from ..func import ISLFunction
from ..obj import ISLObject
from ..qualifier import Give, Keep, Null, Param, Take
from .local_types import ISLDimType
from .space import LocalSpace, Space

_lib = load_libisl()


class Constraint(ISLObject):
    """Wrapper around ``isl_constraint``."""

    __slots__ = ()

    def copy_handle(self) -> FfiPointer:
        return _isl_constraint_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_constraint_free(handle)

    @classmethod
    def equality(cls, ls: LocalSpace) -> "Constraint":
        return _isl_constraint_alloc_equality(ls)

    @classmethod
    def inequality(cls, ls: LocalSpace) -> "Constraint":
        return _isl_constraint_alloc_inequality(ls)

    def get_space(self) -> Space:
        return _isl_constraint_get_space(self)

    def get_local_space(self) -> LocalSpace:
        return _isl_constraint_get_local_space(self)

    def get_dim_name(self, dim_type: int, pos: int) -> str | None:
        return _isl_constraint_get_dim_name(self, dim_type, pos)

    def set_constant_si(self, v: int) -> "Constraint":
        return _isl_constraint_set_constant_si(self, v)

    def set_coefficient_si(self, dim_type: int, pos: int, v: int) -> "Constraint":
        return _isl_constraint_set_coefficient_si(self, dim_type, pos, v)

    def __str__(self) -> str:  # pragma: no cover
        return _isl_constraint_to_str(self)

    def __repr__(self) -> str:  # pragma: no cover
        return f"Constraint({self.__str__()})"


class EqualityConstraint(Constraint):
    """Logical alias for equality constraint."""


class InequalityConstraint(Constraint):
    """Logical alias for inequality constraint."""


_isl_constraint_alloc_equality = ISLFunction.create(
    _lib.isl_constraint_alloc_equality,
    Take(LocalSpace),
    return_=Give(EqualityConstraint),
    lib=_lib,
)

_isl_constraint_alloc_inequality = ISLFunction.create(
    _lib.isl_constraint_alloc_inequality,
    Take(LocalSpace),
    return_=Give(InequalityConstraint),
    lib=_lib,
)

_isl_constraint_copy = ISLFunction.create(
    _lib.isl_constraint_copy,
    Keep(Constraint),
    return_=Give(Constraint),
    lib=_lib,
)

_isl_constraint_free = ISLFunction.create(
    _lib.isl_constraint_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

_isl_constraint_get_space = ISLFunction.create(
    _lib.isl_constraint_get_space,
    Keep(Constraint),
    return_=Give(Space),
    lib=_lib,
)

_isl_constraint_get_local_space = ISLFunction.create(
    _lib.isl_constraint_get_local_space,
    Keep(Constraint),
    return_=Give(LocalSpace),
    lib=_lib,
)

_isl_constraint_get_dim_name = ISLFunction.create(
    _lib.isl_constraint_get_dim_name,
    Keep(Constraint),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Param(str),
    lib=_lib,
)

_isl_constraint_set_constant_si = ISLFunction.create(
    _lib.isl_constraint_set_constant_si,
    Take(Constraint),
    Param(int, ctype=c_int),
    return_=Give(Constraint),
    lib=_lib,
)

_isl_constraint_set_coefficient_si = ISLFunction.create(
    _lib.isl_constraint_set_coefficient_si,
    Take(Constraint),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Constraint),
    lib=_lib,
)

_isl_constraint_to_str = ISLFunction.create(
    _lib.isl_constraint_to_str,
    Keep(Constraint),
    return_=Param(str),
    lib=_lib,
)


__all__ = [
    "Constraint",
    "EqualityConstraint",
    "InequalityConstraint",
    "ISLDimType",
]
