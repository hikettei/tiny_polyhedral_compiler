from __future__ import annotations

from ctypes import c_int, c_void_p

from ..ffi import FfiPointer, load_libisl
from ..func import ISLFunction
from ..obj import ISLObject
from ..qualifier import Give, Keep, Null, Param, Take
from .space import Space
from .union_set import UnionSet

_lib = load_libisl()


class Schedule(ISLObject):
    __slots__ = ()

    def __init__(self, handle: FfiPointer) -> None:
        super().__init__(handle)

    @classmethod
    def empty(cls, space: Space) -> "Schedule":
        return _isl_schedule_empty(space)

    @classmethod
    def from_domain(cls, domain: UnionSet) -> "Schedule":
        return _isl_schedule_from_domain(domain)

    def copy_handle(self) -> FfiPointer:
        return _isl_schedule_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_schedule_free(handle)

    def is_equal(self, other: "Schedule") -> bool:
        return _isl_schedule_plain_is_equal(self, other)

    def get_domain(self) -> UnionSet:
        return _isl_schedule_get_domain(self)

    def __str__(self) -> str:  # pragma: no cover
        return _isl_schedule_to_str(self)

    def __repr__(self) -> str:  # pragma: no cover
        return f"Schedule({self.__str__()})"


class ScheduleNode(ISLObject):
    __slots__ = ()

    def copy_handle(self) -> FfiPointer:
        return _isl_schedule_node_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_schedule_node_free(handle)

    def __str__(self) -> str:  # pragma: no cover
        return _isl_schedule_node_to_str(self)

    def __repr__(self) -> str:  # pragma: no cover
        return f"ScheduleNode({self.__str__()})"


class ScheduleNodeBand(ScheduleNode):
    __slots__ = ()


# schedule primitives
_isl_schedule_empty = ISLFunction.create(
    _lib.isl_schedule_empty,
    Take(Space),
    return_=Give(Schedule),
    lib=_lib,
)

_isl_schedule_from_domain = ISLFunction.create(
    _lib.isl_schedule_from_domain,
    Take(UnionSet),
    return_=Give(Schedule),
    lib=_lib,
)

_isl_schedule_copy = ISLFunction.create(
    _lib.isl_schedule_copy,
    Keep(Schedule),
    return_=Give(Schedule),
    lib=_lib,
)

_isl_schedule_free = ISLFunction.create(
    _lib.isl_schedule_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

_isl_schedule_plain_is_equal = ISLFunction.create(
    _lib.isl_schedule_plain_is_equal,
    Keep(Schedule),
    Keep(Schedule),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_schedule_get_domain = ISLFunction.create(
    _lib.isl_schedule_get_domain,
    Keep(Schedule),
    return_=Give(UnionSet),
    lib=_lib,
)

_isl_schedule_to_str = ISLFunction.create(
    _lib.isl_schedule_to_str,
    Keep(Schedule),
    return_=Param(str),
    lib=_lib,
)

_isl_schedule_node_copy = ISLFunction.create(
    _lib.isl_schedule_node_copy,
    Keep(ScheduleNode),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_free = ISLFunction.create(
    _lib.isl_schedule_node_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

_isl_schedule_node_to_str = ISLFunction.create(
    _lib.isl_schedule_node_to_str,
    Keep(ScheduleNode),
    return_=Param(str),
    lib=_lib,
)

__all__ = ["Schedule", "ScheduleNode", "ScheduleNodeBand"]
