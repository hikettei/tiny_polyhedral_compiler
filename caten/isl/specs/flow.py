from __future__ import annotations

from ctypes import c_char_p, c_int, c_void_p
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
    from .map import Map

_lib = load_libisl()

class Flow(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_flow_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_flow_read_from_str(spec)

    def copy_handle(self) -> Any:
        raise NotImplementedError(f"{type(self).__name__} does not support copy.")

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_flow_free(handle)

    def foreach(self, fn: Any, must: int, dep_user: Any, user: Any, user_: Any = None) -> int:
        return _isl_flow_foreach(self, fn, must, dep_user, user, user_)

    def get_no_source(self, must: int) -> "Map":
        return _isl_flow_get_no_source(self, must)


register_type("Flow", Flow)

_isl_flow_foreach = ISLFunction.create(
    "isl_flow_foreach",
    Keep("Flow"),
    Param(None, ctype=c_void_p),
    Param(int, ctype=c_int),
    Param(Any, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_flow_get_no_source = ISLFunction.create(
    "isl_flow_get_no_source",
    Keep("Flow"),
    Param(int, ctype=c_int),
    return_=Give("Map"),
    lib=_lib,
)

_isl_flow_free = ISLFunction.create(
    "isl_flow_free",
    Take("Flow"),
    return_=Give("Flow"),
    lib=_lib,
)

_isl_flow_read_from_str = ISLFunction.create(
    "isl_flow_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("Flow"),
    lib=_lib,
)
