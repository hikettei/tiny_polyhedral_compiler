from __future__ import annotations

from ctypes import c_long, c_void_p

from ..ffi import FfiPointer, load_libisl
from ..func import ISLFunction
from ..obj import ISLObject
from ..qualifier import Give, Keep, Null, Param
from .context import Context

_lib = load_libisl()


class Val(ISLObject):
    """Wrapper for ``isl_val`` supporting integers only."""

    __slots__ = ()

    @classmethod
    def int_from_si(cls, value: int) -> "Val":
        return _isl_val_int_from_si(value)

    def copy_handle(self) -> FfiPointer:
        return _isl_val_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_val_free(handle)

    def num_si(self) -> int:
        return _isl_val_get_num_si(self)


_isl_val_int_from_si = ISLFunction.create(
    _lib.isl_val_int_from_si,
    Context(),
    Param(int, ctype=c_long),
    return_=Give(Val),
    lib=_lib,
)

_isl_val_copy = ISLFunction.create(
    _lib.isl_val_copy,
    Keep(Val),
    return_=Give(Val),
    lib=_lib,
)

_isl_val_free = ISLFunction.create(
    _lib.isl_val_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

_isl_val_get_num_si = ISLFunction.create(
    _lib.isl_val_get_num_si,
    Keep(Val),
    return_=Param(int, ctype=c_long),
    lib=_lib,
)

__all__ = ["Val"]
