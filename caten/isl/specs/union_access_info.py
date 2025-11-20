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
    from .schedule import Schedule
    from .union_flow import UnionFlow
    from .union_map import UnionMap

_lib = load_libisl()

class UnionAccessInfo(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_union_access_info_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_union_access_info_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_union_access_info_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_union_access_info_free(handle)

    def __str__(self) -> str:
        return _isl_union_access_info_to_str(self)

    def __repr__(self) -> str:
        return f"UnionAccessInfo({self.__str__()})"

    def get_ctx(self) -> "Context":
        return _isl_union_access_info_get_ctx(self)

    def compute_flow(self) -> "UnionFlow":
        return _isl_union_access_info_compute_flow(self)

    @classmethod
    def from_sink(cls, sink: "UnionMap") -> "UnionAccessInfo":
        return _isl_union_access_info_from_sink(sink)

    def set_kill(self, kill: "UnionMap") -> "UnionAccessInfo":
        return _isl_union_access_info_set_kill(self, kill)

    def set_may_source(self, may_source: "UnionMap") -> "UnionAccessInfo":
        return _isl_union_access_info_set_may_source(self, may_source)

    def set_must_source(self, must_source: "UnionMap") -> "UnionAccessInfo":
        return _isl_union_access_info_set_must_source(self, must_source)

    def set_schedule(self, schedule: "Schedule") -> "UnionAccessInfo":
        return _isl_union_access_info_set_schedule(self, schedule)

    def set_schedule_map(self, schedule_map: "UnionMap") -> "UnionAccessInfo":
        return _isl_union_access_info_set_schedule_map(self, schedule_map)

    @classmethod
    def read_from_file(cls, input: None) -> "UnionAccessInfo":
        return _isl_union_access_info_read_from_file(input)


register_type("UnionAccessInfo", UnionAccessInfo)

_isl_union_access_info_get_ctx = ISLFunction.create(
    "isl_union_access_info_get_ctx",
    Keep("UnionAccessInfo"),
    return_=Give("Context"),
    lib=_lib,
)

_isl_union_access_info_compute_flow = ISLFunction.create(
    "isl_union_access_info_compute_flow",
    Take("UnionAccessInfo"),
    return_=Give("UnionFlow"),
    lib=_lib,
)

_isl_union_access_info_from_sink = ISLFunction.create(
    "isl_union_access_info_from_sink",
    Take("UnionMap"),
    return_=Give("UnionAccessInfo"),
    lib=_lib,
)

_isl_union_access_info_set_kill = ISLFunction.create(
    "isl_union_access_info_set_kill",
    Take("UnionAccessInfo"),
    Take("UnionMap"),
    return_=Give("UnionAccessInfo"),
    lib=_lib,
)

_isl_union_access_info_set_may_source = ISLFunction.create(
    "isl_union_access_info_set_may_source",
    Take("UnionAccessInfo"),
    Take("UnionMap"),
    return_=Give("UnionAccessInfo"),
    lib=_lib,
)

_isl_union_access_info_set_must_source = ISLFunction.create(
    "isl_union_access_info_set_must_source",
    Take("UnionAccessInfo"),
    Take("UnionMap"),
    return_=Give("UnionAccessInfo"),
    lib=_lib,
)

_isl_union_access_info_set_schedule = ISLFunction.create(
    "isl_union_access_info_set_schedule",
    Take("UnionAccessInfo"),
    Take("Schedule"),
    return_=Give("UnionAccessInfo"),
    lib=_lib,
)

_isl_union_access_info_set_schedule_map = ISLFunction.create(
    "isl_union_access_info_set_schedule_map",
    Take("UnionAccessInfo"),
    Take("UnionMap"),
    return_=Give("UnionAccessInfo"),
    lib=_lib,
)

_isl_union_access_info_copy = ISLFunction.create(
    "isl_union_access_info_copy",
    Keep("UnionAccessInfo"),
    return_=Give("UnionAccessInfo"),
    lib=_lib,
)

_isl_union_access_info_free = ISLFunction.create(
    "isl_union_access_info_free",
    Take("UnionAccessInfo"),
    return_=Give("UnionAccessInfo"),
    lib=_lib,
)

_isl_union_access_info_read_from_file = ISLFunction.create(
    "isl_union_access_info_read_from_file",
    Context(),
    Param(None, ctype=c_void_p),
    return_=Give("UnionAccessInfo"),
    lib=_lib,
)

_isl_union_access_info_to_str = ISLFunction.create(
    "isl_union_access_info_to_str",
    Keep("UnionAccessInfo"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_union_access_info_read_from_str = ISLFunction.create(
    "isl_union_access_info_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("UnionAccessInfo"),
    lib=_lib,
)
