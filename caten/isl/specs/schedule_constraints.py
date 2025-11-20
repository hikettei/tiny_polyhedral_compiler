from __future__ import annotations

from ctypes import (
    c_char_p,
    c_void_p,
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

class ScheduleConstraints(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_schedule_constraints_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_schedule_constraints_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_schedule_constraints_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_schedule_constraints_free(handle)

    def __str__(self) -> str:
        return _isl_schedule_constraints_to_str(self)

    def __repr__(self) -> str:
        return f"ScheduleConstraints({self.__str__()})"

    def get_ctx(self) -> "Ctx":
        return _isl_schedule_constraints_get_ctx(self)

    def compute_schedule(self) -> "Schedule":
        return _isl_schedule_constraints_compute_schedule(self)

    @classmethod
    def on_domain(cls, domain: "UnionSet") -> "ScheduleConstraints":
        return _isl_schedule_constraints_on_domain(domain)

    def set_context(self, context: "Set") -> "ScheduleConstraints":
        return _isl_schedule_constraints_set_context(self, context)

    def set_validity(self, validity: "UnionMap") -> "ScheduleConstraints":
        return _isl_schedule_constraints_set_validity(self, validity)

    def set_coincidence(self, coincidence: "UnionMap") -> "ScheduleConstraints":
        return _isl_schedule_constraints_set_coincidence(self, coincidence)

    def set_proximity(self, proximity: "UnionMap") -> "ScheduleConstraints":
        return _isl_schedule_constraints_set_proximity(self, proximity)

    def set_conditional_validity(self, condition: "UnionMap", validity: "UnionMap") -> "ScheduleConstraints":
        return _isl_schedule_constraints_set_conditional_validity(self, condition, validity)

    def apply(self, umap: "UnionMap") -> "ScheduleConstraints":
        return _isl_schedule_constraints_apply(self, umap)

    def get_domain(self) -> "UnionSet":
        return _isl_schedule_constraints_get_domain(self)

    def get_context(self) -> "Set":
        return _isl_schedule_constraints_get_context(self)

    def get_validity(self) -> "UnionMap":
        return _isl_schedule_constraints_get_validity(self)

    def get_coincidence(self) -> "UnionMap":
        return _isl_schedule_constraints_get_coincidence(self)

    def get_proximity(self) -> "UnionMap":
        return _isl_schedule_constraints_get_proximity(self)

    def get_conditional_validity(self) -> "UnionMap":
        return _isl_schedule_constraints_get_conditional_validity(self)

    def get_conditional_validity_condition(self) -> "UnionMap":
        return _isl_schedule_constraints_get_conditional_validity_condition(self)

    @classmethod
    def read_from_file(cls, input: None) -> "ScheduleConstraints":
        return _isl_schedule_constraints_read_from_file(input)


register_type("ScheduleConstraints", ScheduleConstraints)

_isl_schedule_constraints_get_ctx = ISLFunction.create(
    "isl_schedule_constraints_get_ctx",
    Keep("ScheduleConstraints"),
    return_=Give("Ctx"),
    lib=_lib,
)

_isl_schedule_constraints_compute_schedule = ISLFunction.create(
    "isl_schedule_constraints_compute_schedule",
    Take("ScheduleConstraints"),
    return_=Give("Schedule"),
    lib=_lib,
)

_isl_schedule_constraints_copy = ISLFunction.create(
    "isl_schedule_constraints_copy",
    Keep("ScheduleConstraints"),
    return_=Give("ScheduleConstraints"),
    lib=_lib,
)

_isl_schedule_constraints_on_domain = ISLFunction.create(
    "isl_schedule_constraints_on_domain",
    Take("UnionSet"),
    return_=Give("ScheduleConstraints"),
    lib=_lib,
)

_isl_schedule_constraints_set_context = ISLFunction.create(
    "isl_schedule_constraints_set_context",
    Take("ScheduleConstraints"),
    Take("Set"),
    return_=Give("ScheduleConstraints"),
    lib=_lib,
)

_isl_schedule_constraints_set_validity = ISLFunction.create(
    "isl_schedule_constraints_set_validity",
    Take("ScheduleConstraints"),
    Take("UnionMap"),
    return_=Give("ScheduleConstraints"),
    lib=_lib,
)

_isl_schedule_constraints_set_coincidence = ISLFunction.create(
    "isl_schedule_constraints_set_coincidence",
    Take("ScheduleConstraints"),
    Take("UnionMap"),
    return_=Give("ScheduleConstraints"),
    lib=_lib,
)

_isl_schedule_constraints_set_proximity = ISLFunction.create(
    "isl_schedule_constraints_set_proximity",
    Take("ScheduleConstraints"),
    Take("UnionMap"),
    return_=Give("ScheduleConstraints"),
    lib=_lib,
)

_isl_schedule_constraints_set_conditional_validity = ISLFunction.create(
    "isl_schedule_constraints_set_conditional_validity",
    Take("ScheduleConstraints"),
    Take("UnionMap"),
    Take("UnionMap"),
    return_=Give("ScheduleConstraints"),
    lib=_lib,
)

_isl_schedule_constraints_apply = ISLFunction.create(
    "isl_schedule_constraints_apply",
    Take("ScheduleConstraints"),
    Take("UnionMap"),
    return_=Give("ScheduleConstraints"),
    lib=_lib,
)

_isl_schedule_constraints_free = ISLFunction.create(
    "isl_schedule_constraints_free",
    Take("ScheduleConstraints"),
    return_=Give("ScheduleConstraints"),
    lib=_lib,
)

_isl_schedule_constraints_get_domain = ISLFunction.create(
    "isl_schedule_constraints_get_domain",
    Keep("ScheduleConstraints"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_schedule_constraints_get_context = ISLFunction.create(
    "isl_schedule_constraints_get_context",
    Keep("ScheduleConstraints"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_schedule_constraints_get_validity = ISLFunction.create(
    "isl_schedule_constraints_get_validity",
    Keep("ScheduleConstraints"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_schedule_constraints_get_coincidence = ISLFunction.create(
    "isl_schedule_constraints_get_coincidence",
    Keep("ScheduleConstraints"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_schedule_constraints_get_proximity = ISLFunction.create(
    "isl_schedule_constraints_get_proximity",
    Keep("ScheduleConstraints"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_schedule_constraints_get_conditional_validity = ISLFunction.create(
    "isl_schedule_constraints_get_conditional_validity",
    Keep("ScheduleConstraints"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_schedule_constraints_get_conditional_validity_condition = ISLFunction.create(
    "isl_schedule_constraints_get_conditional_validity_condition",
    Keep("ScheduleConstraints"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_schedule_constraints_read_from_str = ISLFunction.create(
    "isl_schedule_constraints_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("ScheduleConstraints"),
    lib=_lib,
)

_isl_schedule_constraints_read_from_file = ISLFunction.create(
    "isl_schedule_constraints_read_from_file",
    Context(),
    Param(None, ctype=c_void_p),
    return_=Give("ScheduleConstraints"),
    lib=_lib,
)

_isl_schedule_constraints_to_str = ISLFunction.create(
    "isl_schedule_constraints_to_str",
    Keep("ScheduleConstraints"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)
