from __future__ import annotations

from ctypes import c_void_p

from ..ffi import FfiPointer, load_libisl
from ..func import ISLFunction
from ..obj import ISLObject
from ..qualifier import Give, Keep, Null, Param
from .context import Context

_lib = load_libisl()


class Id(ISLObject):
    """Lightweight wrapper around ``isl_id`` handles."""

    __slots__ = ()

    def __init__(self, handle_or_spec: str | FfiPointer) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_id_read_from_str(handle_or_spec, return_raw_pointer=True)
        else:
            handle = handle_or_spec
        super().__init__(handle)

    @classmethod
    def alloc(cls, name: str, user: object | None = None) -> "Id":
        return _isl_id_alloc(name, user)

    @classmethod
    def from_str(cls, spec: str) -> "Id":
        return _isl_id_read_from_str(spec)

    def copy_handle(self) -> FfiPointer:
        return _isl_id_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_id_free(handle)

    def name(self) -> str:
        return _isl_id_get_name(self)

    def user(self) -> object | None:
        return _isl_id_get_user(self)

    def __str__(self) -> str:  # pragma: no cover
        return _isl_id_to_str(self)

    def __repr__(self) -> str:  # pragma: no cover
        return f"Id({self.__str__()})"


_isl_id_alloc = ISLFunction.create(
    _lib.isl_id_alloc,
    Context(),
    Param(str),
    Param(None, ctype=c_void_p),
    return_=Give(Id),
    lib=_lib,
)

_isl_id_read_from_str = ISLFunction.create(
    _lib.isl_id_read_from_str,
    Context(),
    Param(str),
    return_=Give(Id),
    lib=_lib,
)

_isl_id_copy = ISLFunction.create(
    _lib.isl_id_copy,
    Keep(Id),
    return_=Give(Id),
    lib=_lib,
)

_isl_id_free = ISLFunction.create(
    _lib.isl_id_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

_isl_id_get_name = ISLFunction.create(
    _lib.isl_id_get_name,
    Keep(Id),
    return_=Param(str),
    lib=_lib,
)

_isl_id_get_user = ISLFunction.create(
    _lib.isl_id_get_user,
    Keep(Id),
    return_=Param(None, ctype=c_void_p),
    lib=_lib,
)

_isl_id_to_str = ISLFunction.create(
    _lib.isl_id_to_str,
    Keep(Id),
    return_=Param(str),
    lib=_lib,
)

__all__ = ["Id"]
