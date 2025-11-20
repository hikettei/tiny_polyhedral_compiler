from __future__ import annotations

from ctypes import (
    c_char_p,
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

class StrideInfo(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_stride_info_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_stride_info_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_stride_info_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_stride_info_free(handle)

    def get_ctx(self) -> "Ctx":
        return _isl_stride_info_get_ctx(self)

    def get_stride(self) -> "Val":
        return _isl_stride_info_get_stride(self)

    def get_offset(self) -> "Aff":
        return _isl_stride_info_get_offset(self)


register_type("StrideInfo", StrideInfo)

_isl_stride_info_get_ctx = ISLFunction.create(
    "isl_stride_info_get_ctx",
    Keep("StrideInfo"),
    return_=Give("Ctx"),
    lib=_lib,
)

_isl_stride_info_get_stride = ISLFunction.create(
    "isl_stride_info_get_stride",
    Keep("StrideInfo"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_stride_info_get_offset = ISLFunction.create(
    "isl_stride_info_get_offset",
    Keep("StrideInfo"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_stride_info_copy = ISLFunction.create(
    "isl_stride_info_copy",
    Keep("StrideInfo"),
    return_=Give("StrideInfo"),
    lib=_lib,
)

_isl_stride_info_free = ISLFunction.create(
    "isl_stride_info_free",
    Take("StrideInfo"),
    return_=Give("StrideInfo"),
    lib=_lib,
)

_isl_stride_info_read_from_str = ISLFunction.create(
    "isl_stride_info_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("StrideInfo"),
    lib=_lib,
)
