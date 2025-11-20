from __future__ import annotations

from ctypes import c_char_p, c_int, c_uint
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
    from .val import Val

_lib = load_libisl()

class Vec(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_vec_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_vec_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_vec_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_vec_free(handle)

    def get_ctx(self) -> "Context":
        return _isl_vec_get_ctx(self)

    @classmethod
    def alloc(cls, size: int) -> "Vec":
        return _isl_vec_alloc(size)

    @classmethod
    def zero(cls, size: int) -> "Vec":
        return _isl_vec_zero(size)

    def size(self) -> int:
        return _isl_vec_size(self)

    def get_element_val(self, pos: int) -> "Val":
        return _isl_vec_get_element_val(self, pos)

    def set_element_si(self, pos: int, v: int) -> "Vec":
        return _isl_vec_set_element_si(self, pos, v)

    def set_element_val(self, pos: int, v: "Val") -> "Vec":
        return _isl_vec_set_element_val(self, pos, v)

    def set_si(self, v: int) -> "Vec":
        return _isl_vec_set_si(self, v)

    def set_val(self, v: "Val") -> "Vec":
        return _isl_vec_set_val(self, v)

    def cmp_element(self, vec2: "Vec", pos: int) -> int:
        return _isl_vec_cmp_element(self, vec2, pos)

    def concat(self, vec2: "Vec") -> "Vec":
        return _isl_vec_concat(self, vec2)


register_type("Vec", Vec)

_isl_vec_get_ctx = ISLFunction.create(
    "isl_vec_get_ctx",
    Keep("Vec"),
    return_=Give("Context"),
    lib=_lib,
)

_isl_vec_alloc = ISLFunction.create(
    "isl_vec_alloc",
    Context(),
    Param(int, ctype=c_uint),
    return_=Give("Vec"),
    lib=_lib,
)

_isl_vec_zero = ISLFunction.create(
    "isl_vec_zero",
    Context(),
    Param(int, ctype=c_uint),
    return_=Give("Vec"),
    lib=_lib,
)

_isl_vec_copy = ISLFunction.create(
    "isl_vec_copy",
    Keep("Vec"),
    return_=Give("Vec"),
    lib=_lib,
)

_isl_vec_free = ISLFunction.create(
    "isl_vec_free",
    Take("Vec"),
    return_=Give("Vec"),
    lib=_lib,
)

_isl_vec_size = ISLFunction.create(
    "isl_vec_size",
    Keep("Vec"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_vec_get_element_val = ISLFunction.create(
    "isl_vec_get_element_val",
    Keep("Vec"),
    Param(int, ctype=c_int),
    return_=Give("Val"),
    lib=_lib,
)

_isl_vec_set_element_si = ISLFunction.create(
    "isl_vec_set_element_si",
    Take("Vec"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give("Vec"),
    lib=_lib,
)

_isl_vec_set_element_val = ISLFunction.create(
    "isl_vec_set_element_val",
    Take("Vec"),
    Param(int, ctype=c_int),
    Take("Val"),
    return_=Give("Vec"),
    lib=_lib,
)

_isl_vec_set_si = ISLFunction.create(
    "isl_vec_set_si",
    Take("Vec"),
    Param(int, ctype=c_int),
    return_=Give("Vec"),
    lib=_lib,
)

_isl_vec_set_val = ISLFunction.create(
    "isl_vec_set_val",
    Take("Vec"),
    Take("Val"),
    return_=Give("Vec"),
    lib=_lib,
)

_isl_vec_cmp_element = ISLFunction.create(
    "isl_vec_cmp_element",
    Keep("Vec"),
    Keep("Vec"),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_vec_concat = ISLFunction.create(
    "isl_vec_concat",
    Take("Vec"),
    Take("Vec"),
    return_=Give("Vec"),
    lib=_lib,
)

_isl_vec_read_from_str = ISLFunction.create(
    "isl_vec_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("Vec"),
    lib=_lib,
)
