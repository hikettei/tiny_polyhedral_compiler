from __future__ import annotations

from ctypes import (
    c_char_p,
)
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

class UnionFlow(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_union_flow_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_union_flow_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_union_flow_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_union_flow_free(handle)

    def __str__(self) -> str:
        return _isl_union_flow_to_str(self)

    def __repr__(self) -> str:
        return f"UnionFlow({self.__str__()})"

    def get_ctx(self) -> "Ctx":
        return _isl_union_flow_get_ctx(self)

    def get_must_dependence(self) -> "UnionMap":
        return _isl_union_flow_get_must_dependence(self)

    def get_may_dependence(self) -> "UnionMap":
        return _isl_union_flow_get_may_dependence(self)

    def get_full_must_dependence(self) -> "UnionMap":
        return _isl_union_flow_get_full_must_dependence(self)

    def get_full_may_dependence(self) -> "UnionMap":
        return _isl_union_flow_get_full_may_dependence(self)

    def get_must_no_source(self) -> "UnionMap":
        return _isl_union_flow_get_must_no_source(self)

    def get_may_no_source(self) -> "UnionMap":
        return _isl_union_flow_get_may_no_source(self)


register_type("UnionFlow", UnionFlow)

_isl_union_flow_get_ctx = ISLFunction.create(
    "isl_union_flow_get_ctx",
    Keep("UnionFlow"),
    return_=Give("Ctx"),
    lib=_lib,
)

_isl_union_flow_get_must_dependence = ISLFunction.create(
    "isl_union_flow_get_must_dependence",
    Keep("UnionFlow"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_flow_get_may_dependence = ISLFunction.create(
    "isl_union_flow_get_may_dependence",
    Keep("UnionFlow"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_flow_get_full_must_dependence = ISLFunction.create(
    "isl_union_flow_get_full_must_dependence",
    Keep("UnionFlow"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_flow_get_full_may_dependence = ISLFunction.create(
    "isl_union_flow_get_full_may_dependence",
    Keep("UnionFlow"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_flow_get_must_no_source = ISLFunction.create(
    "isl_union_flow_get_must_no_source",
    Keep("UnionFlow"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_flow_get_may_no_source = ISLFunction.create(
    "isl_union_flow_get_may_no_source",
    Keep("UnionFlow"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_flow_copy = ISLFunction.create(
    "isl_union_flow_copy",
    Keep("UnionFlow"),
    return_=Give("UnionFlow"),
    lib=_lib,
)

_isl_union_flow_free = ISLFunction.create(
    "isl_union_flow_free",
    Take("UnionFlow"),
    return_=Give("UnionFlow"),
    lib=_lib,
)

_isl_union_flow_to_str = ISLFunction.create(
    "isl_union_flow_to_str",
    Keep("UnionFlow"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_union_flow_read_from_str = ISLFunction.create(
    "isl_union_flow_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("UnionFlow"),
    lib=_lib,
)
