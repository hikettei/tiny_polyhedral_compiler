from __future__ import annotations

from ctypes import (
    c_char_p,
    c_int,
    c_uint,
)
from typing import TYPE_CHECKING, Any

from ..ffi import load_libisl
from ..func import ISLFunction
from ..mixin import ISLObjectMixin
from ..obj import ISLObject
from ..qualifier import Give, Keep, Param, Take
from ..registry import register_type
from .context import Context

if TYPE_CHECKING:
    from .context import Context

_lib = load_libisl()

class Constraint(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_constraint_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_constraint_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_constraint_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_constraint_free(handle)

    def get_space(self) -> "Space":
        return _isl_constraint_get_space(self)

    def get_dim_name(self, type: int, pos: int) -> str:
        return _isl_constraint_get_dim_name(self, type, pos)

    def get_local_space(self) -> "LocalSpace":
        return _isl_constraint_get_local_space(self)

    @classmethod
    def equality(cls, ls: "LocalSpace") -> "Constraint":
        return _isl_constraint_alloc_equality(ls)

    @classmethod
    def inequality(cls, ls: "LocalSpace") -> "Constraint":
        return _isl_constraint_alloc_inequality(ls)

    def set_constant_si(self, v: int) -> "Constraint":
        return _isl_constraint_set_constant_si(self, v)

    def set_constant_val(self, v: "Val") -> "Constraint":
        return _isl_constraint_set_constant_val(self, v)

    def set_coefficient_si(self, type: int, pos: int, v: int) -> "Constraint":
        return _isl_constraint_set_coefficient_si(self, type, pos, v)

    def set_coefficient_val(self, type: int, pos: int, v: "Val") -> "Constraint":
        return _isl_constraint_set_coefficient_val(self, type, pos, v)

    def is_equality(self) -> bool:
        return _isl_constraint_is_equality(self)

    def is_lower_bound(self, type: int, pos: int) -> bool:
        return _isl_constraint_is_lower_bound(self, type, pos)

    def is_upper_bound(self, type: int, pos: int) -> bool:
        return _isl_constraint_is_upper_bound(self, type, pos)

    def get_constant_val(self) -> "Val":
        return _isl_constraint_get_constant_val(self)

    def get_coefficient_val(self, type: int, pos: int) -> "Val":
        return _isl_constraint_get_coefficient_val(self, type, pos)

    def get_div(self, pos: int) -> "Aff":
        return _isl_constraint_get_div(self, pos)

    def get_bound(self, type: int, pos: int) -> "Aff":
        return _isl_constraint_get_bound(self, type, pos)

    def get_aff(self) -> "Aff":
        return _isl_constraint_get_aff(self)

    def involves_dims(self, type: int, first: int, n: int) -> bool:
        return _isl_constraint_involves_dims(self, type, first, n)

    def plain_cmp(self, c2: "Constraint") -> int:
        return _isl_constraint_plain_cmp(self, c2)

    def cmp_last_non_zero(self, c2: "Constraint") -> int:
        return _isl_constraint_cmp_last_non_zero(self, c2)


register_type("Constraint", Constraint)

_isl_constraint_get_space = ISLFunction.create(
    "isl_constraint_get_space",
    Keep("Constraint"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_constraint_get_dim_name = ISLFunction.create(
    "isl_constraint_get_dim_name",
    Keep("Constraint"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_constraint_get_local_space = ISLFunction.create(
    "isl_constraint_get_local_space",
    Keep("Constraint"),
    return_=Give("LocalSpace"),
    lib=_lib,
)

_isl_constraint_alloc_equality = ISLFunction.create(
    "isl_constraint_alloc_equality",
    Take("LocalSpace"),
    return_=Give("Constraint"),
    lib=_lib,
)

_isl_constraint_alloc_inequality = ISLFunction.create(
    "isl_constraint_alloc_inequality",
    Take("LocalSpace"),
    return_=Give("Constraint"),
    lib=_lib,
)

_isl_constraint_set_constant_si = ISLFunction.create(
    "isl_constraint_set_constant_si",
    Take("Constraint"),
    Param(int, ctype=c_int),
    return_=Give("Constraint"),
    lib=_lib,
)

_isl_constraint_set_constant_val = ISLFunction.create(
    "isl_constraint_set_constant_val",
    Take("Constraint"),
    Take("Val"),
    return_=Give("Constraint"),
    lib=_lib,
)

_isl_constraint_set_coefficient_si = ISLFunction.create(
    "isl_constraint_set_coefficient_si",
    Take("Constraint"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give("Constraint"),
    lib=_lib,
)

_isl_constraint_set_coefficient_val = ISLFunction.create(
    "isl_constraint_set_coefficient_val",
    Take("Constraint"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Take("Val"),
    return_=Give("Constraint"),
    lib=_lib,
)

_isl_constraint_free = ISLFunction.create(
    "isl_constraint_free",
    Take("Constraint"),
    return_=Give("Constraint"),
    lib=_lib,
)

_isl_constraint_is_equality = ISLFunction.create(
    "isl_constraint_is_equality",
    Keep("Constraint"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_constraint_is_lower_bound = ISLFunction.create(
    "isl_constraint_is_lower_bound",
    Keep("Constraint"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_constraint_is_upper_bound = ISLFunction.create(
    "isl_constraint_is_upper_bound",
    Keep("Constraint"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_constraint_get_constant_val = ISLFunction.create(
    "isl_constraint_get_constant_val",
    Keep("Constraint"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_constraint_get_coefficient_val = ISLFunction.create(
    "isl_constraint_get_coefficient_val",
    Keep("Constraint"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give("Val"),
    lib=_lib,
)

_isl_constraint_get_div = ISLFunction.create(
    "isl_constraint_get_div",
    Keep("Constraint"),
    Param(int, ctype=c_int),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_constraint_get_bound = ISLFunction.create(
    "isl_constraint_get_bound",
    Keep("Constraint"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_constraint_get_aff = ISLFunction.create(
    "isl_constraint_get_aff",
    Keep("Constraint"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_constraint_involves_dims = ISLFunction.create(
    "isl_constraint_involves_dims",
    Keep("Constraint"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_constraint_plain_cmp = ISLFunction.create(
    "isl_constraint_plain_cmp",
    Keep("Constraint"),
    Keep("Constraint"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_constraint_cmp_last_non_zero = ISLFunction.create(
    "isl_constraint_cmp_last_non_zero",
    Keep("Constraint"),
    Keep("Constraint"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_constraint_read_from_str = ISLFunction.create(
    "isl_constraint_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("Constraint"),
    lib=_lib,
)

_isl_constraint_copy = ISLFunction.create(
    "isl_constraint_copy",
    Keep("Constraint"),
    return_=Give("Constraint"),
    lib=_lib,
)
