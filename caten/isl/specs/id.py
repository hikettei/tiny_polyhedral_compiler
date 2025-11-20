from __future__ import annotations

from ctypes import c_char_p, c_void_p
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

class Id(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_id_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_id_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_id_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_id_free(handle)

    def __str__(self) -> str:
        return _isl_id_to_str(self)

    def __repr__(self) -> str:
        return f"Id({self.__str__()})"

    def get_ctx(self) -> "Context":
        return _isl_id_get_ctx(self)

    @classmethod
    def alloc(cls, name: str, user: Any = None) -> "Id":
        return _isl_id_alloc(name, user)

    def set_free_user(self, free_user: Any) -> "Id":
        return _isl_id_set_free_user(self, free_user)

    def user(self) -> Any:
        return _isl_id_get_user(self)

    def name(self) -> str:
        return _isl_id_get_name(self)


register_type("Id", Id)

_isl_id_get_ctx = ISLFunction.create(
    "isl_id_get_ctx",
    Keep("Id"),
    return_=Give("Context"),
    lib=_lib,
)

_isl_id_alloc = ISLFunction.create(
    "isl_id_alloc",
    Context(),
    Param(str, ctype=c_char_p),
    Param(None, ctype=c_void_p),
    return_=Give("Id"),
    lib=_lib,
)

_isl_id_set_free_user = ISLFunction.create(
    "isl_id_set_free_user",
    Take("Id"),
    Param(None, ctype=c_void_p),
    return_=Give("Id"),
    lib=_lib,
)

_isl_id_copy = ISLFunction.create(
    "isl_id_copy",
    Keep("Id"),
    return_=Give("Id"),
    lib=_lib,
)

_isl_id_free = ISLFunction.create(
    "isl_id_free",
    Take("Id"),
    return_=Give("Id"),
    lib=_lib,
)

_isl_id_get_user = ISLFunction.create(
    "isl_id_get_user",
    Keep("Id"),
    return_=Param(Any, ctype=c_void_p),
    lib=_lib,
)

_isl_id_get_name = ISLFunction.create(
    "isl_id_get_name",
    Keep("Id"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_id_read_from_str = ISLFunction.create(
    "isl_id_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("Id"),
    lib=_lib,
)

_isl_id_to_str = ISLFunction.create(
    "isl_id_to_str",
    Keep("Id"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)
