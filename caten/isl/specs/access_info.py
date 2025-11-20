from __future__ import annotations

from ctypes import c_char_p, c_int, c_void_p
from typing import TYPE_CHECKING, Any

from ..ffi import load_libisl
from ..func import ISLFunction
from ..mixin import ISLObjectMixin
from ..obj import ISLObject
from ..qualifier import Give, Param, Take
from ..registry import register_type
from .context import Context

if TYPE_CHECKING:
    from .context import Context
    from .flow import Flow
    from .map import Map

_lib = load_libisl()

class AccessInfo(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_access_info_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_access_info_read_from_str(spec)

    def copy_handle(self) -> Any:
        raise NotImplementedError(f"{type(self).__name__} does not support copy.")

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_access_info_free(handle)

    def add_source(self, source: "Map", must: int, source_user: Any) -> "AccessInfo":
        return _isl_access_info_add_source(self, source, must, source_user)

    def compute_flow(self) -> "Flow":
        return _isl_access_info_compute_flow(self)


register_type("AccessInfo", AccessInfo)

_isl_access_info_add_source = ISLFunction.create(
    "isl_access_info_add_source",
    Take("AccessInfo"),
    Take("Map"),
    Param(int, ctype=c_int),
    Param(Any, ctype=c_void_p),
    return_=Give("AccessInfo"),
    lib=_lib,
)

_isl_access_info_free = ISLFunction.create(
    "isl_access_info_free",
    Take("AccessInfo"),
    return_=Give("AccessInfo"),
    lib=_lib,
)

_isl_access_info_compute_flow = ISLFunction.create(
    "isl_access_info_compute_flow",
    Take("AccessInfo"),
    return_=Give("Flow"),
    lib=_lib,
)

_isl_access_info_read_from_str = ISLFunction.create(
    "isl_access_info_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("AccessInfo"),
    lib=_lib,
)
