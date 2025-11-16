from __future__ import annotations

from ctypes import c_int, c_void_p

from ..ffi import FfiPointer, load_libisl
from ..func import ISLFunction
from ..obj import ISLObject
from ..qualifier import Give, Keep, Null, Param, Take
from .context import Context
from .id import Id
from .space import Space
from .union_map import UnionMap
from .union_set import UnionSet, UnionSetList

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

    def context(self) -> Context:
        return _isl_schedule_node_get_ctx(self)

    def copy_handle(self) -> FfiPointer:
        return _isl_schedule_node_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_schedule_node_free(handle)

    def __str__(self) -> str:  # pragma: no cover
        return _isl_schedule_node_to_str(self)

    def __repr__(self) -> str:  # pragma: no cover
        return f"ScheduleNode({self.__str__()})"

    def is_equal(self, other: "ScheduleNode") -> bool:
        return _isl_schedule_node_is_equal(self, other)

    def tree_depth(self) -> int:
        return _isl_schedule_node_get_tree_depth(self)

    def child_position(self) -> int:
        return _isl_schedule_node_get_child_position(self)

    def child(self, pos: int) -> "ScheduleNode":
        return _isl_schedule_node_child(self, pos)

    def n_children(self) -> int:
        return _isl_schedule_node_n_children(self)

    def has_children(self) -> bool:
        return _isl_schedule_node_has_children(self)

    def first_child(self) -> "ScheduleNode":
        return _isl_schedule_node_first_child(self)

    def has_previous_sibling(self) -> bool:
        return _isl_schedule_node_has_previous_sibling(self)

    def previous_sibling(self) -> "ScheduleNode":
        return _isl_schedule_node_previous_sibling(self)

    def has_next_sibling(self) -> bool:
        return _isl_schedule_node_has_next_sibling(self)

    def next_sibling(self) -> "ScheduleNode":
        return _isl_schedule_node_next_sibling(self)

    def has_parent(self) -> bool:
        return _isl_schedule_node_has_parent(self)

    def parent(self) -> "ScheduleNode":
        return _isl_schedule_node_parent(self)

    def root(self) -> "ScheduleNode":
        return _isl_schedule_node_root(self)

    def domain(self) -> UnionSet:
        return _isl_schedule_node_get_domain(self)

    def universe_domain(self) -> UnionSet:
        return _isl_schedule_node_get_universe_domain(self)

    def schedule_depth(self) -> int:
        return _isl_schedule_node_get_schedule_depth(self)

    def schedule(self) -> Schedule:
        return _isl_schedule_node_get_schedule(self)

    def prefix_schedule(self) -> UnionMap:
        return _isl_schedule_node_get_prefix_schedule_union_map(self)

    def align_params(self, space: Space) -> "ScheduleNode":
        return _isl_schedule_node_align_params(self, space)

    def band_space(self) -> Space:
        return _isl_schedule_node_band_get_space(self)

    def band_member_count(self) -> int:
        return _isl_schedule_node_band_n_member(self)

    def band_member_coincident(self, pos: int) -> bool:
        return _isl_schedule_node_band_member_get_coincident(self, pos)

    def set_band_member_coincident(self, pos: int, coincident: bool) -> "ScheduleNode":
        return _isl_schedule_node_band_member_set_coincident(self, pos, int(coincident))

    def band_permutable(self) -> bool:
        return _isl_schedule_node_band_get_permutable(self)

    def set_band_permutable(self, permutable: bool) -> "ScheduleNode":
        return _isl_schedule_node_band_set_permutable(self, int(permutable))

    def insert_filter(self, filter_set: UnionSet) -> "ScheduleNode":
        return _isl_schedule_node_insert_filter(self, filter_set)

    def insert_mark(self, mark: Id) -> "ScheduleNode":
        return _isl_schedule_node_insert_mark(self, mark)

    def insert_sequence(self, filters: UnionSetList) -> "ScheduleNode":
        return _isl_schedule_node_insert_sequence(self, filters)

    def insert_set(self, filters: UnionSetList) -> "ScheduleNode":
        return _isl_schedule_node_insert_set(self, filters)


class ScheduleNodeBand(ScheduleNode):
    __slots__ = ()


class ScheduleNodeLeaf(ScheduleNode):
    __slots__ = ()


class ScheduleNodeFilter(ScheduleNode):
    __slots__ = ()


class ScheduleNodeSequence(ScheduleNode):
    __slots__ = ()


class ScheduleNodeDomain(ScheduleNode):
    __slots__ = ()


class ScheduleNodeExpansion(ScheduleNode):
    __slots__ = ()


class ScheduleNodeExtension(ScheduleNode):
    __slots__ = ()


class ScheduleNodeMark(ScheduleNode):
    __slots__ = ()


class ScheduleNodeSet(ScheduleNode):
    __slots__ = ()


class ScheduleNodeContext(ScheduleNode):
    __slots__ = ()


class ScheduleNodeGuard(ScheduleNode):
    __slots__ = ()


class ScheduleNodeError(ScheduleNode):
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

_isl_schedule_node_get_ctx = ISLFunction.create(
    _lib.isl_schedule_node_get_ctx,
    Keep(ScheduleNode),
    return_=Keep(Context),
    lib=_lib,
)

_isl_schedule_node_free = ISLFunction.create(
    _lib.isl_schedule_node_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

_isl_schedule_node_get_type = ISLFunction.create(
    _lib.isl_schedule_node_get_type,
    Keep(ScheduleNode),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_get_parent_type = ISLFunction.create(
    _lib.isl_schedule_node_get_parent_type,
    Keep(ScheduleNode),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_has_parent = ISLFunction.create(
    _lib.isl_schedule_node_has_parent,
    Keep(ScheduleNode),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_parent = ISLFunction.create(
    _lib.isl_schedule_node_parent,
    Keep(ScheduleNode),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_root = ISLFunction.create(
    _lib.isl_schedule_node_root,
    Keep(ScheduleNode),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_n_children = ISLFunction.create(
    _lib.isl_schedule_node_n_children,
    Keep(ScheduleNode),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_child = ISLFunction.create(
    _lib.isl_schedule_node_child,
    Keep(ScheduleNode),
    Param(int, ctype=c_int),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_has_children = ISLFunction.create(
    _lib.isl_schedule_node_has_children,
    Keep(ScheduleNode),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_first_child = ISLFunction.create(
    _lib.isl_schedule_node_first_child,
    Keep(ScheduleNode),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_has_previous_sibling = ISLFunction.create(
    _lib.isl_schedule_node_has_previous_sibling,
    Keep(ScheduleNode),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_previous_sibling = ISLFunction.create(
    _lib.isl_schedule_node_previous_sibling,
    Keep(ScheduleNode),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_has_next_sibling = ISLFunction.create(
    _lib.isl_schedule_node_has_next_sibling,
    Keep(ScheduleNode),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_next_sibling = ISLFunction.create(
    _lib.isl_schedule_node_next_sibling,
    Keep(ScheduleNode),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_get_schedule = ISLFunction.create(
    _lib.isl_schedule_node_get_schedule,
    Keep(ScheduleNode),
    return_=Give(Schedule),
    lib=_lib,
)

_isl_schedule_node_is_equal = ISLFunction.create(
    _lib.isl_schedule_node_is_equal,
    Keep(ScheduleNode),
    Keep(ScheduleNode),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_get_tree_depth = ISLFunction.create(
    _lib.isl_schedule_node_get_tree_depth,
    Keep(ScheduleNode),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_get_child_position = ISLFunction.create(
    _lib.isl_schedule_node_get_child_position,
    Keep(ScheduleNode),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_get_child = ISLFunction.create(
    _lib.isl_schedule_node_get_child,
    Keep(ScheduleNode),
    Param(int, ctype=c_int),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_get_domain = ISLFunction.create(
    _lib.isl_schedule_node_get_domain,
    Keep(ScheduleNode),
    return_=Give(UnionSet),
    lib=_lib,
)

_isl_schedule_node_get_universe_domain = ISLFunction.create(
    _lib.isl_schedule_node_get_universe_domain,
    Keep(ScheduleNode),
    return_=Give(UnionSet),
    lib=_lib,
)

_isl_schedule_node_get_schedule_depth = ISLFunction.create(
    _lib.isl_schedule_node_get_schedule_depth,
    Keep(ScheduleNode),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_get_prefix_schedule_union_map = ISLFunction.create(
    _lib.isl_schedule_node_get_prefix_schedule_union_map,
    Keep(ScheduleNode),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_schedule_node_align_params = ISLFunction.create(
    _lib.isl_schedule_node_align_params,
    Take(ScheduleNode),
    Take(Space),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_band_get_space = ISLFunction.create(
    _lib.isl_schedule_node_band_get_space,
    Keep(ScheduleNode),
    return_=Give(Space),
    lib=_lib,
)

_isl_schedule_node_band_n_member = ISLFunction.create(
    _lib.isl_schedule_node_band_n_member,
    Keep(ScheduleNode),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_band_member_get_coincident = ISLFunction.create(
    _lib.isl_schedule_node_band_member_get_coincident,
    Keep(ScheduleNode),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_band_member_set_coincident = ISLFunction.create(
    _lib.isl_schedule_node_band_member_set_coincident,
    Take(ScheduleNode),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_band_get_permutable = ISLFunction.create(
    _lib.isl_schedule_node_band_get_permutable,
    Keep(ScheduleNode),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_band_set_permutable = ISLFunction.create(
    _lib.isl_schedule_node_band_set_permutable,
    Take(ScheduleNode),
    Param(int, ctype=c_int),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_insert_filter = ISLFunction.create(
    _lib.isl_schedule_node_insert_filter,
    Take(ScheduleNode),
    Take(UnionSet),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_insert_mark = ISLFunction.create(
    _lib.isl_schedule_node_insert_mark,
    Take(ScheduleNode),
    Take(Id),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_insert_sequence = ISLFunction.create(
    _lib.isl_schedule_node_insert_sequence,
    Take(ScheduleNode),
    Take(UnionSetList),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_insert_set = ISLFunction.create(
    _lib.isl_schedule_node_insert_set,
    Take(ScheduleNode),
    Take(UnionSetList),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_to_str = ISLFunction.create(
    _lib.isl_schedule_node_to_str,
    Keep(ScheduleNode),
    return_=Param(str),
    lib=_lib,
)

__all__ = [
    "Schedule",
    "ScheduleNode",
    "ScheduleNodeBand",
    "ScheduleNodeLeaf",
    "ScheduleNodeFilter",
    "ScheduleNodeSequence",
    "ScheduleNodeDomain",
    "ScheduleNodeExpansion",
    "ScheduleNodeExtension",
    "ScheduleNodeMark",
    "ScheduleNodeSet",
    "ScheduleNodeContext",
    "ScheduleNodeGuard",
    "ScheduleNodeError",
]
