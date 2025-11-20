from __future__ import annotations

from ctypes import (
    c_char_p,
)
from typing import TYPE_CHECKING, Any

from ..ffi import load_libisl
from ..func import ISLFunction
from ..mixin import ISLObjectMixin
from ..obj import ISLObject
from ..qualifier import Give, Param
from ..registry import register_type
from .context import Context

if TYPE_CHECKING:
    from .context import Context

_lib = load_libisl()

class MapList(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_map_list_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_map_list_read_from_str(spec)




register_type("MapList", MapList)

_isl_map_list_read_from_str = ISLFunction.create(
    "isl_map_list_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("MapList"),
    lib=_lib,
)
