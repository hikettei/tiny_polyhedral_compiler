from __future__ import annotations

from ctypes import c_int, c_void_p

from ..ffi import FfiPointer, load_libisl
from ..func import ISLFunction
from ..obj import ISLObject
from ..qualifier import Give, Keep, Null, Param, Take
from .context import Context
from .space import Space

_lib = load_libisl()


class MultiVal(ISLObject):
    """Wrapper for ``isl_multi_val``."""

    __slots__ = ()

    def __init__(self, handle_or_spec: str | FfiPointer) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_multi_val_read_from_str(handle_or_spec, return_raw_pointer=True)
        else:
            handle = handle_or_spec
        super().__init__(handle)

    @classmethod
    def from_str(cls, spec: str) -> "MultiVal":
        return _isl_multi_val_read_from_str(spec)

    @classmethod
    def zero(cls, space: Space) -> "MultiVal":
        return _isl_multi_val_zero(space)

    def copy_handle(self) -> FfiPointer:
        return _isl_multi_val_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_multi_val_free(handle)

    def dim(self, dim_type: int) -> int:
        return _isl_multi_val_dim(self, dim_type)

    def get_space(self) -> Space:
        return _isl_multi_val_get_space(self)

    def __str__(self) -> str:  # pragma: no cover
        return _isl_multi_val_to_str(self)

    def __repr__(self) -> str:  # pragma: no cover
        return f"MultiVal({self.__str__()})"


_isl_multi_val_read_from_str = ISLFunction.create(
    _lib.isl_multi_val_read_from_str,
    Context(),
    Param(str),
    return_=Give(MultiVal),
    lib=_lib,
)

_isl_multi_val_copy = ISLFunction.create(
    _lib.isl_multi_val_copy,
    Keep(MultiVal),
    return_=Give(MultiVal),
    lib=_lib,
)

_isl_multi_val_free = ISLFunction.create(
    _lib.isl_multi_val_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

_isl_multi_val_dim = ISLFunction.create(
    _lib.isl_multi_val_dim,
    Keep(MultiVal),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_multi_val_get_space = ISLFunction.create(
    _lib.isl_multi_val_get_space,
    Keep(MultiVal),
    return_=Give(Space),
    lib=_lib,
)

_isl_multi_val_zero = ISLFunction.create(
    _lib.isl_multi_val_zero,
    Take(Space),
    return_=Give(MultiVal),
    lib=_lib,
)

_isl_multi_val_to_str = ISLFunction.create(
    _lib.isl_multi_val_to_str,
    Keep(MultiVal),
    return_=Param(str),
    lib=_lib,
)

__all__ = ["MultiVal"]

