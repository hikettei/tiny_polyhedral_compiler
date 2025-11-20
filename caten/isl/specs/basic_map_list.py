from __future__ import annotations

from ctypes import c_char_p
from typing import TYPE_CHECKING, Any

from ..ffi import load_libisl
from ..func import ISLFunction
from ..mixin import ISLObjectMixin
from ..obj import ISLObject
from ..qualifier import Give, Param, Take
from ..registry import register_type
from .context import Context

if TYPE_CHECKING:
    from .basic_map import BasicMap
    from .context import Context

_lib = load_libisl()

class BasicMapList(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_basic_map_list_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_basic_map_list_read_from_str(spec)



    def intersect(self) -> "BasicMap":
        return _isl_basic_map_list_intersect(self)


register_type("BasicMapList", BasicMapList)

_isl_basic_map_list_intersect = ISLFunction.create(
    "isl_basic_map_list_intersect",
    Take("BasicMapList"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_list_read_from_str = ISLFunction.create(
    "isl_basic_map_list_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("BasicMapList"),
    lib=_lib,
)
