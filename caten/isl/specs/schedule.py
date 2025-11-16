from __future__ import annotations

from ctypes import c_int, c_void_p

from ..ffi import FfiPointer, load_libisl
from ..func import ISLFunction
from ..obj import ISLObject
from ..qualifier import Give, Keep, Null, Param, Take
from .context import Context
from .id import Id
from .multi_val import MultiVal
from .space import Space
from .union_map import UnionMap
from .union_pw_aff import MultiUnionPwAff, UnionPwMultiAff
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

    def ancestor(self, generation: int) -> "ScheduleNode":
        return _isl_schedule_node_ancestor(self, generation)

    def graft_before(self, node: "ScheduleNode") -> "ScheduleNode":
        return _isl_schedule_node_graft_before(self, node)

    def graft_after(self, node: "ScheduleNode") -> "ScheduleNode":
        return _isl_schedule_node_graft_after(self, node)

    def grandchild(self, pos: int) -> "ScheduleNode":
        return _isl_schedule_node_grandchild(self, pos)

    def grandparent(self) -> "ScheduleNode":
        return _isl_schedule_node_grandparent(self)

    def group(self, ctx: UnionSet) -> "ScheduleNode":
        return _isl_schedule_node_group(self, ctx)

    def guard_get_guard(self) -> UnionSet:
        return _isl_schedule_node_guard_get_guard(self)

    def insert_context(self, ctx: UnionSet) -> "ScheduleNode":
        return _isl_schedule_node_insert_context(self, ctx)

    def insert_guard(self, guard: UnionSet) -> "ScheduleNode":
        return _isl_schedule_node_insert_guard(self, guard)

    def insert_partial_schedule(self, partial: MultiUnionPwAff) -> "ScheduleNode":
        return _isl_schedule_node_insert_partial_schedule(self, partial)

    def insert_sequence_splice_child(self, pos: int, child: "ScheduleNode") -> "ScheduleNode":
        return _isl_schedule_node_sequence_splice_child(self, pos, child)

    def insert_sequence_splice_children(
        self, start: int, end: int, children: "ScheduleNode"
    ) -> "ScheduleNode":
        return _isl_schedule_node_sequence_splice_children(
            self, start, end, children
        )

    def order_before(self, other: "ScheduleNode") -> "ScheduleNode":
        return _isl_schedule_node_order_before(self, other)

    def order_after(self, other: "ScheduleNode") -> "ScheduleNode":
        return _isl_schedule_node_order_after(self, other)

    def reset_user(self) -> "ScheduleNode":
        return _isl_schedule_node_reset_user(self)

    def is_subtree_anchored(self) -> bool:
        return _isl_schedule_node_is_subtree_anchored(self)

    def map_descendant_bottom_up(self, fn: object) -> "ScheduleNode":
        raise NotImplementedError("callback bridge not implemented")

    def band_get_ast_build_options(self) -> UnionSet:
        return _isl_schedule_node_band_get_ast_build_options(self)

    def band_get_partial_schedule(self) -> MultiUnionPwAff:
        return _isl_schedule_node_band_get_partial_schedule(self)

    def band_get_partial_schedule_union_map(self) -> UnionMap:
        return _isl_schedule_node_band_get_partial_schedule_union_map(self)

    def band_member_get_ast_loop_type(self, pos: int) -> int:
        return _isl_schedule_node_band_member_get_ast_loop_type(self, pos)

    def band_member_set_ast_loop_type(self, pos: int, loop_type: int) -> "ScheduleNode":
        return _isl_schedule_node_band_member_set_ast_loop_type(self, pos, loop_type)

    def band_member_get_isolate_ast_loop_type(self, pos: int) -> int:
        return _isl_schedule_node_band_member_get_isolate_ast_loop_type(self, pos)

    def band_member_set_isolate_ast_loop_type(self, pos: int, loop_type: int) -> "ScheduleNode":
        return _isl_schedule_node_band_member_set_isolate_ast_loop_type(self, pos, loop_type)

    def band_mod(self, mv: MultiVal) -> "ScheduleNode":
        return _isl_schedule_node_band_mod(self, mv)

    def band_scale(self, mv: MultiVal) -> "ScheduleNode":
        return _isl_schedule_node_band_scale(self, mv)

    def band_scale_down(self, mv: MultiVal) -> "ScheduleNode":
        return _isl_schedule_node_band_scale_down(self, mv)

    def band_set_ast_build_options(self, options: UnionSet) -> "ScheduleNode":
        return _isl_schedule_node_band_set_ast_build_options(self, options)

    def band_shift(self, shift: MultiUnionPwAff) -> "ScheduleNode":
        return _isl_schedule_node_band_shift(self, shift)

    def band_sink(self) -> "ScheduleNode":
        return _isl_schedule_node_band_sink(self)

    def band_split(self, pos: int) -> "ScheduleNode":
        return _isl_schedule_node_band_split(self, pos)

    def band_tile(self, sizes: MultiVal) -> "ScheduleNode":
        return _isl_schedule_node_band_tile(self, sizes)

    def band_get_prefix_schedule_multi_union_pw_aff(self) -> MultiUnionPwAff:
        return _isl_schedule_node_get_prefix_schedule_multi_union_pw_aff(self)

    def band_get_prefix_schedule_union_pw_multi_aff(self) -> UnionPwMultiAff:
        return _isl_schedule_node_get_prefix_schedule_union_pw_multi_aff(self)

    def band_get_prefix_schedule_relation(self) -> UnionMap:
        return _isl_schedule_node_get_prefix_schedule_relation(self)

    def band_get_subtree_expansion(self) -> UnionMap:
        return _isl_schedule_node_get_subtree_expansion(self)

    def band_get_subtree_contraction(self) -> UnionMap:
        return _isl_schedule_node_get_subtree_contraction(self)

    def band_get_subtree_schedule_union_map(self) -> UnionMap:
        return _isl_schedule_node_get_subtree_schedule_union_map(self)


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

_isl_schedule_node_ancestor = ISLFunction.create(
    _lib.isl_schedule_node_ancestor,
    Take(ScheduleNode),
    Param(int, ctype=c_int),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_graft_before = ISLFunction.create(
    _lib.isl_schedule_node_graft_before,
    Take(ScheduleNode),
    Take(ScheduleNode),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_graft_after = ISLFunction.create(
    _lib.isl_schedule_node_graft_after,
    Take(ScheduleNode),
    Take(ScheduleNode),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_grandchild = ISLFunction.create(
    _lib.isl_schedule_node_grandchild,
    Keep(ScheduleNode),
    Param(int, ctype=c_int),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_grandparent = ISLFunction.create(
    _lib.isl_schedule_node_grandparent,
    Keep(ScheduleNode),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_group = ISLFunction.create(
    _lib.isl_schedule_node_group,
    Take(ScheduleNode),
    Take(UnionSet),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_guard_get_guard = ISLFunction.create(
    _lib.isl_schedule_node_guard_get_guard,
    Keep(ScheduleNode),
    return_=Give(UnionSet),
    lib=_lib,
)

_isl_schedule_node_insert_context = ISLFunction.create(
    _lib.isl_schedule_node_insert_context,
    Take(ScheduleNode),
    Take(UnionSet),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_insert_guard = ISLFunction.create(
    _lib.isl_schedule_node_insert_guard,
    Take(ScheduleNode),
    Take(UnionSet),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_insert_partial_schedule = ISLFunction.create(
    _lib.isl_schedule_node_insert_partial_schedule,
    Take(ScheduleNode),
    Take(MultiUnionPwAff),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_sequence_splice_child = ISLFunction.create(
    _lib.isl_schedule_node_sequence_splice_child,
    Take(ScheduleNode),
    Param(int, ctype=c_int),
    Take(ScheduleNode),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_sequence_splice_children = ISLFunction.create(
    _lib.isl_schedule_node_sequence_splice_children,
    Take(ScheduleNode),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Take(ScheduleNode),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_order_before = ISLFunction.create(
    _lib.isl_schedule_node_order_before,
    Take(ScheduleNode),
    Take(ScheduleNode),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_order_after = ISLFunction.create(
    _lib.isl_schedule_node_order_after,
    Take(ScheduleNode),
    Take(ScheduleNode),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_reset_user = ISLFunction.create(
    _lib.isl_schedule_node_reset_user,
    Take(ScheduleNode),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_is_subtree_anchored = ISLFunction.create(
    _lib.isl_schedule_node_is_subtree_anchored,
    Keep(ScheduleNode),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_mark_get_id = ISLFunction.create(
    _lib.isl_schedule_node_mark_get_id,
    Keep(ScheduleNode),
    return_=Give(Id),
    lib=_lib,
)

_isl_schedule_node_band_get_ast_build_options = ISLFunction.create(
    _lib.isl_schedule_node_band_get_ast_build_options,
    Keep(ScheduleNode),
    return_=Give(UnionSet),
    lib=_lib,
)

_isl_schedule_node_band_get_ast_isolate_option = ISLFunction.create(
    _lib.isl_schedule_node_band_get_ast_isolate_option,
    Keep(ScheduleNode),
    return_=Give(UnionSet),
    lib=_lib,
)

_isl_schedule_node_band_get_partial_schedule = ISLFunction.create(
    _lib.isl_schedule_node_band_get_partial_schedule,
    Keep(ScheduleNode),
    return_=Give(MultiUnionPwAff),
    lib=_lib,
)

_isl_schedule_node_band_get_partial_schedule_union_map = ISLFunction.create(
    _lib.isl_schedule_node_band_get_partial_schedule_union_map,
    Keep(ScheduleNode),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_schedule_node_band_member_get_ast_loop_type = ISLFunction.create(
    _lib.isl_schedule_node_band_member_get_ast_loop_type,
    Keep(ScheduleNode),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_band_member_set_ast_loop_type = ISLFunction.create(
    _lib.isl_schedule_node_band_member_set_ast_loop_type,
    Take(ScheduleNode),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_band_member_get_isolate_ast_loop_type = ISLFunction.create(
    _lib.isl_schedule_node_band_member_get_isolate_ast_loop_type,
    Keep(ScheduleNode),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_band_member_set_isolate_ast_loop_type = ISLFunction.create(
    _lib.isl_schedule_node_band_member_set_isolate_ast_loop_type,
    Take(ScheduleNode),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_band_mod = ISLFunction.create(
    _lib.isl_schedule_node_band_mod,
    Take(ScheduleNode),
    Take(MultiVal),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_band_scale = ISLFunction.create(
    _lib.isl_schedule_node_band_scale,
    Take(ScheduleNode),
    Take(MultiVal),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_band_scale_down = ISLFunction.create(
    _lib.isl_schedule_node_band_scale_down,
    Take(ScheduleNode),
    Take(MultiVal),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_band_set_ast_build_options = ISLFunction.create(
    _lib.isl_schedule_node_band_set_ast_build_options,
    Take(ScheduleNode),
    Take(UnionSet),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_band_shift = ISLFunction.create(
    _lib.isl_schedule_node_band_shift,
    Take(ScheduleNode),
    Take(MultiUnionPwAff),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_band_sink = ISLFunction.create(
    _lib.isl_schedule_node_band_sink,
    Take(ScheduleNode),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_band_split = ISLFunction.create(
    _lib.isl_schedule_node_band_split,
    Take(ScheduleNode),
    Param(int, ctype=c_int),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_band_tile = ISLFunction.create(
    _lib.isl_schedule_node_band_tile,
    Take(ScheduleNode),
    Take(MultiVal),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_context_get_context = ISLFunction.create(
    _lib.isl_schedule_node_context_get_context,
    Keep(ScheduleNode),
    return_=Give(UnionSet),
    lib=_lib,
)

_isl_schedule_node_cut = ISLFunction.create(
    _lib.isl_schedule_node_cut,
    Take(ScheduleNode),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_delete = ISLFunction.create(
    _lib.isl_schedule_node_delete,
    Take(ScheduleNode),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_domain_get_domain = ISLFunction.create(
    _lib.isl_schedule_node_domain_get_domain,
    Keep(ScheduleNode),
    return_=Give(UnionSet),
    lib=_lib,
)

_isl_schedule_node_every_descendant = ISLFunction.create(
    _lib.isl_schedule_node_every_descendant,
    Keep(ScheduleNode),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_expansion_get_contraction = ISLFunction.create(
    _lib.isl_schedule_node_expansion_get_contraction,
    Keep(ScheduleNode),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_schedule_node_expansion_get_expansion = ISLFunction.create(
    _lib.isl_schedule_node_expansion_get_expansion,
    Keep(ScheduleNode),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_schedule_node_extension_get_extension = ISLFunction.create(
    _lib.isl_schedule_node_extension_get_extension,
    Keep(ScheduleNode),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_schedule_node_filter_get_filter = ISLFunction.create(
    _lib.isl_schedule_node_filter_get_filter,
    Keep(ScheduleNode),
    return_=Give(UnionSet),
    lib=_lib,
)

_isl_schedule_node_foreach_ancestor_top_down = ISLFunction.create(
    _lib.isl_schedule_node_foreach_ancestor_top_down,
    Keep(ScheduleNode),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_foreach_descendant_top_down = ISLFunction.create(
    _lib.isl_schedule_node_foreach_descendant_top_down,
    Keep(ScheduleNode),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_from_domain = ISLFunction.create(
    _lib.isl_schedule_node_from_domain,
    Take(UnionSet),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_from_extension = ISLFunction.create(
    _lib.isl_schedule_node_from_extension,
    Take(UnionMap),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_get_ancestor_child_position = ISLFunction.create(
    _lib.isl_schedule_node_get_ancestor_child_position,
    Keep(ScheduleNode),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_get_prefix_schedule_multi_union_pw_aff = ISLFunction.create(
    _lib.isl_schedule_node_get_prefix_schedule_multi_union_pw_aff,
    Keep(ScheduleNode),
    return_=Give(MultiUnionPwAff),
    lib=_lib,
)

_isl_schedule_node_get_prefix_schedule_relation = ISLFunction.create(
    _lib.isl_schedule_node_get_prefix_schedule_relation,
    Keep(ScheduleNode),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_schedule_node_get_prefix_schedule_union_pw_multi_aff = ISLFunction.create(
    _lib.isl_schedule_node_get_prefix_schedule_union_pw_multi_aff,
    Keep(ScheduleNode),
    return_=Give(UnionPwMultiAff),
    lib=_lib,
)

_isl_schedule_node_get_shared_ancestor = ISLFunction.create(
    _lib.isl_schedule_node_get_shared_ancestor,
    Keep(ScheduleNode),
    Keep(ScheduleNode),
    return_=Give(ScheduleNode),
    lib=_lib,
)

_isl_schedule_node_get_subtree_contraction = ISLFunction.create(
    _lib.isl_schedule_node_get_subtree_contraction,
    Keep(ScheduleNode),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_schedule_node_get_subtree_expansion = ISLFunction.create(
    _lib.isl_schedule_node_get_subtree_expansion,
    Keep(ScheduleNode),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_schedule_node_get_subtree_schedule_union_map = ISLFunction.create(
    _lib.isl_schedule_node_get_subtree_schedule_union_map,
    Keep(ScheduleNode),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_schedule_node_map_descendant_bottom_up = ISLFunction.create(
    _lib.isl_schedule_node_map_descendant_bottom_up,
    Take(ScheduleNode),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
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
