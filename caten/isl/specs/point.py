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

class Point(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_point_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_point_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_point_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_point_free(handle)

    def __str__(self) -> str:
        return _isl_point_to_str(self)

    def __repr__(self) -> str:
        return f"Point({self.__str__()})"

    def get_ctx(self) -> "Ctx":
        return _isl_point_get_ctx(self)

    def get_space(self) -> "Space":
        return _isl_point_get_space(self)

    @classmethod
    def zero(cls, space: "Space") -> "Point":
        return _isl_point_zero(space)

    def get_multi_val(self) -> "MultiVal":
        return _isl_point_get_multi_val(self)

    def get_coordinate_val(self, type: int, pos: int) -> "Val":
        return _isl_point_get_coordinate_val(self, type, pos)

    def set_coordinate_val(self, type: int, pos: int, v: "Val") -> "Point":
        return _isl_point_set_coordinate_val(self, type, pos, v)

    def add_ui(self, type: int, pos: int, val: int) -> "Point":
        return _isl_point_add_ui(self, type, pos, val)

    def sub_ui(self, type: int, pos: int, val: int) -> "Point":
        return _isl_point_sub_ui(self, type, pos, val)

    def to_set(self) -> "Set":
        return _isl_point_to_set(self)

    def is_void(self) -> bool:
        return _isl_point_is_void(self)


register_type("Point", Point)

_isl_point_get_ctx = ISLFunction.create(
    "isl_point_get_ctx",
    Keep("Point"),
    return_=Give("Ctx"),
    lib=_lib,
)

_isl_point_get_space = ISLFunction.create(
    "isl_point_get_space",
    Keep("Point"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_point_zero = ISLFunction.create(
    "isl_point_zero",
    Take("Space"),
    return_=Give("Point"),
    lib=_lib,
)

_isl_point_get_multi_val = ISLFunction.create(
    "isl_point_get_multi_val",
    Keep("Point"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_point_get_coordinate_val = ISLFunction.create(
    "isl_point_get_coordinate_val",
    Keep("Point"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give("Val"),
    lib=_lib,
)

_isl_point_set_coordinate_val = ISLFunction.create(
    "isl_point_set_coordinate_val",
    Take("Point"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Take("Val"),
    return_=Give("Point"),
    lib=_lib,
)

_isl_point_add_ui = ISLFunction.create(
    "isl_point_add_ui",
    Take("Point"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Give("Point"),
    lib=_lib,
)

_isl_point_sub_ui = ISLFunction.create(
    "isl_point_sub_ui",
    Take("Point"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Give("Point"),
    lib=_lib,
)

_isl_point_copy = ISLFunction.create(
    "isl_point_copy",
    Keep("Point"),
    return_=Give("Point"),
    lib=_lib,
)

_isl_point_free = ISLFunction.create(
    "isl_point_free",
    Take("Point"),
    return_=Give("Point"),
    lib=_lib,
)

_isl_point_to_set = ISLFunction.create(
    "isl_point_to_set",
    Take("Point"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_point_is_void = ISLFunction.create(
    "isl_point_is_void",
    Keep("Point"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_point_to_str = ISLFunction.create(
    "isl_point_to_str",
    Keep("Point"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_point_read_from_str = ISLFunction.create(
    "isl_point_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("Point"),
    lib=_lib,
)
