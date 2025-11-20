from __future__ import annotations

from ctypes import c_char_p
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
    from .set import Set

_lib = load_libisl()

class Restriction(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_restriction_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_restriction_read_from_str(spec)


    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_restriction_free(handle)

    def get_ctx(self) -> "Context":
        return _isl_restriction_get_ctx(self)

    @classmethod
    def output(cls, source_restr: "Set") -> "Restriction":
        return _isl_restriction_output(source_restr)

    @classmethod
    def none(cls, source_map: "Map") -> "Restriction":
        return _isl_restriction_none(source_map)

    @classmethod
    def empty(cls, source_map: "Map") -> "Restriction":
        return _isl_restriction_empty(source_map)


register_type("Restriction", Restriction)

_isl_restriction_get_ctx = ISLFunction.create(
    "isl_restriction_get_ctx",
    Keep("Restriction"),
    return_=Give("Context"),
    lib=_lib,
)

_isl_restriction_output = ISLFunction.create(
    "isl_restriction_output",
    Take("Set"),
    return_=Give("Restriction"),
    lib=_lib,
)

_isl_restriction_none = ISLFunction.create(
    "isl_restriction_none",
    Take("Map"),
    return_=Give("Restriction"),
    lib=_lib,
)

_isl_restriction_empty = ISLFunction.create(
    "isl_restriction_empty",
    Take("Map"),
    return_=Give("Restriction"),
    lib=_lib,
)

_isl_restriction_free = ISLFunction.create(
    "isl_restriction_free",
    Take("Restriction"),
    return_=Give("Restriction"),
    lib=_lib,
)

_isl_restriction_read_from_str = ISLFunction.create(
    "isl_restriction_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("Restriction"),
    lib=_lib,
)
