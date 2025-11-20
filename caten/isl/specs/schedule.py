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
    from .multi_union_pw_aff import MultiUnionPwAff
    from .schedule_node import ScheduleNode
    from .set import Set
    from .space import Space
    from .union_map import UnionMap
    from .union_pw_multi_aff import UnionPwMultiAff
    from .union_set import UnionSet

_lib = load_libisl()

class Schedule(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_schedule_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_schedule_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_schedule_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_schedule_free(handle)

    def __str__(self) -> str:
        return _isl_schedule_to_str(self)

    def __repr__(self) -> str:
        return f"Schedule({self.__str__()})"

    def get_ctx(self) -> "Context":
        return _isl_schedule_get_ctx(self)

    @classmethod
    def empty(cls, space: "Space") -> "Schedule":
        return _isl_schedule_empty(space)

    @classmethod
    def from_domain(cls, domain: "UnionSet") -> "Schedule":
        return _isl_schedule_from_domain(domain)

    def plain_is_equal(self, schedule2: "Schedule") -> bool:
        return _isl_schedule_plain_is_equal(self, schedule2)

    def get_domain(self) -> "UnionSet":
        return _isl_schedule_get_domain(self)

    def insert_partial_schedule(self, partial: "MultiUnionPwAff") -> "Schedule":
        return _isl_schedule_insert_partial_schedule(self, partial)

    def insert_context(self, context: "Set") -> "Schedule":
        return _isl_schedule_insert_context(self, context)

    def insert_guard(self, guard: "Set") -> "Schedule":
        return _isl_schedule_insert_guard(self, guard)

    def sequence(self, schedule2: "Schedule") -> "Schedule":
        return _isl_schedule_sequence(self, schedule2)

    def set(self, schedule2: "Schedule") -> "Schedule":
        return _isl_schedule_set(self, schedule2)

    def intersect_domain(self, domain: "UnionSet") -> "Schedule":
        return _isl_schedule_intersect_domain(self, domain)

    def gist_domain_params(self, context: "Set") -> "Schedule":
        return _isl_schedule_gist_domain_params(self, context)

    def reset_user(self) -> "Schedule":
        return _isl_schedule_reset_user(self)

    def align_params(self, space: "Space") -> "Schedule":
        return _isl_schedule_align_params(self, space)

    def pullback_union_pw_multi_aff(self, upma: "UnionPwMultiAff") -> "Schedule":
        return _isl_schedule_pullback_union_pw_multi_aff(self, upma)

    def expand(self, contraction: "UnionPwMultiAff", expansion: "Schedule") -> "Schedule":
        return _isl_schedule_expand(self, contraction, expansion)

    def get_map(self) -> "UnionMap":
        return _isl_schedule_get_map(self)

    @classmethod
    def read_from_file(cls, input: None) -> "Schedule":
        return _isl_schedule_read_from_file(input)

    def get_root(self) -> "ScheduleNode":
        return _isl_schedule_get_root(self)

    def foreach_schedule_node_top_down(self, fn: Any, user: Any = None) -> int:
        return _isl_schedule_foreach_schedule_node_top_down(self, fn, user)

    def map_schedule_node_bottom_up(self, fn: Any, user: Any = None) -> "Schedule":
        return _isl_schedule_map_schedule_node_bottom_up(self, fn, user)


register_type("Schedule", Schedule)

_isl_schedule_get_ctx = ISLFunction.create(
    "isl_schedule_get_ctx",
    Keep("Schedule"),
    return_=Give("Context"),
    lib=_lib,
)

_isl_schedule_empty = ISLFunction.create(
    "isl_schedule_empty",
    Take("Space"),
    return_=Give("Schedule"),
    lib=_lib,
)

_isl_schedule_from_domain = ISLFunction.create(
    "isl_schedule_from_domain",
    Take("UnionSet"),
    return_=Give("Schedule"),
    lib=_lib,
)

_isl_schedule_copy = ISLFunction.create(
    "isl_schedule_copy",
    Keep("Schedule"),
    return_=Give("Schedule"),
    lib=_lib,
)

_isl_schedule_free = ISLFunction.create(
    "isl_schedule_free",
    Take("Schedule"),
    return_=Give("Schedule"),
    lib=_lib,
)

_isl_schedule_plain_is_equal = ISLFunction.create(
    "isl_schedule_plain_is_equal",
    Keep("Schedule"),
    Keep("Schedule"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_schedule_get_domain = ISLFunction.create(
    "isl_schedule_get_domain",
    Keep("Schedule"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_schedule_insert_partial_schedule = ISLFunction.create(
    "isl_schedule_insert_partial_schedule",
    Take("Schedule"),
    Take("MultiUnionPwAff"),
    return_=Give("Schedule"),
    lib=_lib,
)

_isl_schedule_insert_context = ISLFunction.create(
    "isl_schedule_insert_context",
    Take("Schedule"),
    Take("Set"),
    return_=Give("Schedule"),
    lib=_lib,
)

_isl_schedule_insert_guard = ISLFunction.create(
    "isl_schedule_insert_guard",
    Take("Schedule"),
    Take("Set"),
    return_=Give("Schedule"),
    lib=_lib,
)

_isl_schedule_sequence = ISLFunction.create(
    "isl_schedule_sequence",
    Take("Schedule"),
    Take("Schedule"),
    return_=Give("Schedule"),
    lib=_lib,
)

_isl_schedule_set = ISLFunction.create(
    "isl_schedule_set",
    Take("Schedule"),
    Take("Schedule"),
    return_=Give("Schedule"),
    lib=_lib,
)

_isl_schedule_intersect_domain = ISLFunction.create(
    "isl_schedule_intersect_domain",
    Take("Schedule"),
    Take("UnionSet"),
    return_=Give("Schedule"),
    lib=_lib,
)

_isl_schedule_gist_domain_params = ISLFunction.create(
    "isl_schedule_gist_domain_params",
    Take("Schedule"),
    Take("Set"),
    return_=Give("Schedule"),
    lib=_lib,
)

_isl_schedule_reset_user = ISLFunction.create(
    "isl_schedule_reset_user",
    Take("Schedule"),
    return_=Give("Schedule"),
    lib=_lib,
)

_isl_schedule_align_params = ISLFunction.create(
    "isl_schedule_align_params",
    Take("Schedule"),
    Take("Space"),
    return_=Give("Schedule"),
    lib=_lib,
)

_isl_schedule_pullback_union_pw_multi_aff = ISLFunction.create(
    "isl_schedule_pullback_union_pw_multi_aff",
    Take("Schedule"),
    Take("UnionPwMultiAff"),
    return_=Give("Schedule"),
    lib=_lib,
)

_isl_schedule_expand = ISLFunction.create(
    "isl_schedule_expand",
    Take("Schedule"),
    Take("UnionPwMultiAff"),
    Take("Schedule"),
    return_=Give("Schedule"),
    lib=_lib,
)

_isl_schedule_get_map = ISLFunction.create(
    "isl_schedule_get_map",
    Keep("Schedule"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_schedule_read_from_file = ISLFunction.create(
    "isl_schedule_read_from_file",
    Context(),
    Param(None, ctype=c_void_p),
    return_=Give("Schedule"),
    lib=_lib,
)

_isl_schedule_read_from_str = ISLFunction.create(
    "isl_schedule_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("Schedule"),
    lib=_lib,
)

_isl_schedule_to_str = ISLFunction.create(
    "isl_schedule_to_str",
    Keep("Schedule"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_schedule_get_root = ISLFunction.create(
    "isl_schedule_get_root",
    Keep("Schedule"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_foreach_schedule_node_top_down = ISLFunction.create(
    "isl_schedule_foreach_schedule_node_top_down",
    Keep("Schedule"),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_schedule_map_schedule_node_bottom_up = ISLFunction.create(
    "isl_schedule_map_schedule_node_bottom_up",
    Take("Schedule"),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Give("Schedule"),
    lib=_lib,
)
