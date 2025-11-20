from __future__ import annotations

from ctypes import (
    c_char_p,
    c_int,
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

class ScheduleNode(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_schedule_node_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_schedule_node_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_schedule_node_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_schedule_node_free(handle)

    def __str__(self) -> str:
        return _isl_schedule_node_to_str(self)

    def __repr__(self) -> str:
        return f"ScheduleNode({self.__str__()})"

    def get_ctx(self) -> "Ctx":
        return _isl_schedule_node_get_ctx(self)

    @classmethod
    def from_domain(cls, domain: "UnionSet") -> "ScheduleNode":
        return _isl_schedule_node_from_domain(domain)

    @classmethod
    def from_extension(cls, extension: "UnionMap") -> "ScheduleNode":
        return _isl_schedule_node_from_extension(extension)

    def is_equal(self, node2: "ScheduleNode") -> bool:
        return _isl_schedule_node_is_equal(self, node2)

    def get_type(self) -> int:
        return _isl_schedule_node_get_type(self)

    def get_parent_type(self) -> int:
        return _isl_schedule_node_get_parent_type(self)

    def get_schedule(self) -> "Schedule":
        return _isl_schedule_node_get_schedule(self)

    def has_parent(self) -> bool:
        return _isl_schedule_node_has_parent(self)

    def parent(self) -> "ScheduleNode":
        return _isl_schedule_node_parent(self)

    def grandparent(self) -> "ScheduleNode":
        return _isl_schedule_node_grandparent(self)

    def root(self) -> "ScheduleNode":
        return _isl_schedule_node_root(self)

    def ancestor(self, generation: int) -> "ScheduleNode":
        return _isl_schedule_node_ancestor(self, generation)

    def n_children(self) -> int:
        return _isl_schedule_node_n_children(self)

    def child(self, pos: int) -> "ScheduleNode":
        return _isl_schedule_node_child(self, pos)

    def has_children(self) -> bool:
        return _isl_schedule_node_has_children(self)

    def grandchild(self, pos1: int, pos2: int) -> "ScheduleNode":
        return _isl_schedule_node_grandchild(self, pos1, pos2)

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

    def get_tree_depth(self) -> int:
        return _isl_schedule_node_get_tree_depth(self)

    def get_child_position(self) -> int:
        return _isl_schedule_node_get_child_position(self)

    def get_ancestor_child_position(self, ancestor: "ScheduleNode") -> int:
        return _isl_schedule_node_get_ancestor_child_position(self, ancestor)

    def get_child(self, pos: int) -> "ScheduleNode":
        return _isl_schedule_node_get_child(self, pos)

    def get_shared_ancestor(self, node2: "ScheduleNode") -> "ScheduleNode":
        return _isl_schedule_node_get_shared_ancestor(self, node2)

    def foreach_descendant_top_down(self, fn: Any, user: Any = None) -> int:
        return _isl_schedule_node_foreach_descendant_top_down(self, fn, user)

    def every_descendant(self, test: Any, user: Any = None) -> bool:
        return _isl_schedule_node_every_descendant(self, test, user)

    def foreach_ancestor_top_down(self, fn: Any, user: Any = None) -> int:
        return _isl_schedule_node_foreach_ancestor_top_down(self, fn, user)

    def map_descendant_bottom_up(self, fn: Any, user: Any = None) -> "ScheduleNode":
        return _isl_schedule_node_map_descendant_bottom_up(self, fn, user)

    def cut(self) -> "ScheduleNode":
        return _isl_schedule_node_cut(self)

    def delete(self) -> "ScheduleNode":
        return _isl_schedule_node_delete(self)

    def is_subtree_anchored(self) -> bool:
        return _isl_schedule_node_is_subtree_anchored(self)

    def reset_user(self) -> "ScheduleNode":
        return _isl_schedule_node_reset_user(self)

    def align_params(self, space: "Space") -> "ScheduleNode":
        return _isl_schedule_node_align_params(self, space)

    def band_get_space(self) -> "Space":
        return _isl_schedule_node_band_get_space(self)

    def band_get_partial_schedule(self) -> "MultiUnionPwAff":
        return _isl_schedule_node_band_get_partial_schedule(self)

    def band_get_partial_schedule_union_map(self) -> "UnionMap":
        return _isl_schedule_node_band_get_partial_schedule_union_map(self)

    def band_n_member(self) -> int:
        return _isl_schedule_node_band_n_member(self)

    def band_member_get_coincident(self, pos: int) -> bool:
        return _isl_schedule_node_band_member_get_coincident(self, pos)

    def band_member_set_coincident(self, pos: int, coincident: int) -> "ScheduleNode":
        return _isl_schedule_node_band_member_set_coincident(self, pos, coincident)

    def band_get_permutable(self) -> bool:
        return _isl_schedule_node_band_get_permutable(self)

    def band_set_permutable(self, permutable: int) -> "ScheduleNode":
        return _isl_schedule_node_band_set_permutable(self, permutable)

    def band_member_get_ast_loop_type(self, pos: int) -> int:
        return _isl_schedule_node_band_member_get_ast_loop_type(self, pos)

    def band_member_set_ast_loop_type(self, pos: int, type: int) -> "ScheduleNode":
        return _isl_schedule_node_band_member_set_ast_loop_type(self, pos, type)

    def band_member_get_isolate_ast_loop_type(self, pos: int) -> int:
        return _isl_schedule_node_band_member_get_isolate_ast_loop_type(self, pos)

    def band_member_set_isolate_ast_loop_type(self, pos: int, type: int) -> "ScheduleNode":
        return _isl_schedule_node_band_member_set_isolate_ast_loop_type(self, pos, type)

    def band_get_ast_build_options(self) -> "UnionSet":
        return _isl_schedule_node_band_get_ast_build_options(self)

    def band_set_ast_build_options(self, options: "UnionSet") -> "ScheduleNode":
        return _isl_schedule_node_band_set_ast_build_options(self, options)

    def band_get_ast_isolate_option(self) -> "Set":
        return _isl_schedule_node_band_get_ast_isolate_option(self)

    def context_get_context(self) -> "Set":
        return _isl_schedule_node_context_get_context(self)

    def domain_get_domain(self) -> "UnionSet":
        return _isl_schedule_node_domain_get_domain(self)

    def expansion_get_expansion(self) -> "UnionMap":
        return _isl_schedule_node_expansion_get_expansion(self)

    def expansion_get_contraction(self) -> "UnionPwMultiAff":
        return _isl_schedule_node_expansion_get_contraction(self)

    def extension_get_extension(self) -> "UnionMap":
        return _isl_schedule_node_extension_get_extension(self)

    def filter_get_filter(self) -> "UnionSet":
        return _isl_schedule_node_filter_get_filter(self)

    def guard_get_guard(self) -> "Set":
        return _isl_schedule_node_guard_get_guard(self)

    def mark_get_id(self) -> "Id":
        return _isl_schedule_node_mark_get_id(self)

    def get_prefix_schedule_multi_union_pw_aff(self) -> "MultiUnionPwAff":
        return _isl_schedule_node_get_prefix_schedule_multi_union_pw_aff(self)

    def get_prefix_schedule_union_pw_multi_aff(self) -> "UnionPwMultiAff":
        return _isl_schedule_node_get_prefix_schedule_union_pw_multi_aff(self)

    def get_prefix_schedule_union_map(self) -> "UnionMap":
        return _isl_schedule_node_get_prefix_schedule_union_map(self)

    def get_prefix_schedule_relation(self) -> "UnionMap":
        return _isl_schedule_node_get_prefix_schedule_relation(self)

    def get_subtree_schedule_union_map(self) -> "UnionMap":
        return _isl_schedule_node_get_subtree_schedule_union_map(self)

    def get_subtree_expansion(self) -> "UnionMap":
        return _isl_schedule_node_get_subtree_expansion(self)

    def get_subtree_contraction(self) -> "UnionPwMultiAff":
        return _isl_schedule_node_get_subtree_contraction(self)

    def get_schedule_depth(self) -> int:
        return _isl_schedule_node_get_schedule_depth(self)

    def get_domain(self) -> "UnionSet":
        return _isl_schedule_node_get_domain(self)

    def get_universe_domain(self) -> "UnionSet":
        return _isl_schedule_node_get_universe_domain(self)

    def insert_partial_schedule(self, schedule: "MultiUnionPwAff") -> "ScheduleNode":
        return _isl_schedule_node_insert_partial_schedule(self, schedule)

    def insert_context(self, context: "Set") -> "ScheduleNode":
        return _isl_schedule_node_insert_context(self, context)

    def insert_filter(self, filter: "UnionSet") -> "ScheduleNode":
        return _isl_schedule_node_insert_filter(self, filter)

    def insert_guard(self, guard: "Set") -> "ScheduleNode":
        return _isl_schedule_node_insert_guard(self, guard)

    def insert_mark(self, mark: "Id") -> "ScheduleNode":
        return _isl_schedule_node_insert_mark(self, mark)

    def insert_sequence(self, filters: "UnionSetList") -> "ScheduleNode":
        return _isl_schedule_node_insert_sequence(self, filters)

    def insert_set(self, filters: "UnionSetList") -> "ScheduleNode":
        return _isl_schedule_node_insert_set(self, filters)

    def group(self, group_id: "Id") -> "ScheduleNode":
        return _isl_schedule_node_group(self, group_id)

    def sequence_splice_child(self, pos: int) -> "ScheduleNode":
        return _isl_schedule_node_sequence_splice_child(self, pos)

    def sequence_splice_children(self) -> "ScheduleNode":
        return _isl_schedule_node_sequence_splice_children(self)

    def band_scale(self, mv: "MultiVal") -> "ScheduleNode":
        return _isl_schedule_node_band_scale(self, mv)

    def band_scale_down(self, mv: "MultiVal") -> "ScheduleNode":
        return _isl_schedule_node_band_scale_down(self, mv)

    def band_mod(self, mv: "MultiVal") -> "ScheduleNode":
        return _isl_schedule_node_band_mod(self, mv)

    def band_shift(self, shift: "MultiUnionPwAff") -> "ScheduleNode":
        return _isl_schedule_node_band_shift(self, shift)

    def band_tile(self, sizes: "MultiVal") -> "ScheduleNode":
        return _isl_schedule_node_band_tile(self, sizes)

    def band_split(self, pos: int) -> "ScheduleNode":
        return _isl_schedule_node_band_split(self, pos)

    def band_sink(self) -> "ScheduleNode":
        return _isl_schedule_node_band_sink(self)

    def order_before(self, filter: "UnionSet") -> "ScheduleNode":
        return _isl_schedule_node_order_before(self, filter)

    def order_after(self, filter: "UnionSet") -> "ScheduleNode":
        return _isl_schedule_node_order_after(self, filter)

    def graft_before(self, graft: "ScheduleNode") -> "ScheduleNode":
        return _isl_schedule_node_graft_before(self, graft)

    def graft_after(self, graft: "ScheduleNode") -> "ScheduleNode":
        return _isl_schedule_node_graft_after(self, graft)


register_type("ScheduleNode", ScheduleNode)

_isl_schedule_node_get_ctx = ISLFunction.create(
    "isl_schedule_node_get_ctx",
    Keep("ScheduleNode"),
    return_=Give("Ctx"),
    lib=_lib,
)

_isl_schedule_node_from_domain = ISLFunction.create(
    "isl_schedule_node_from_domain",
    Take("UnionSet"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_from_extension = ISLFunction.create(
    "isl_schedule_node_from_extension",
    Take("UnionMap"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_copy = ISLFunction.create(
    "isl_schedule_node_copy",
    Keep("ScheduleNode"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_free = ISLFunction.create(
    "isl_schedule_node_free",
    Take("ScheduleNode"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_is_equal = ISLFunction.create(
    "isl_schedule_node_is_equal",
    Keep("ScheduleNode"),
    Keep("ScheduleNode"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_get_type = ISLFunction.create(
    "isl_schedule_node_get_type",
    Keep("ScheduleNode"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_get_parent_type = ISLFunction.create(
    "isl_schedule_node_get_parent_type",
    Keep("ScheduleNode"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_get_schedule = ISLFunction.create(
    "isl_schedule_node_get_schedule",
    Keep("ScheduleNode"),
    return_=Give("Schedule"),
    lib=_lib,
)

_isl_schedule_node_has_parent = ISLFunction.create(
    "isl_schedule_node_has_parent",
    Keep("ScheduleNode"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_parent = ISLFunction.create(
    "isl_schedule_node_parent",
    Take("ScheduleNode"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_grandparent = ISLFunction.create(
    "isl_schedule_node_grandparent",
    Take("ScheduleNode"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_root = ISLFunction.create(
    "isl_schedule_node_root",
    Take("ScheduleNode"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_ancestor = ISLFunction.create(
    "isl_schedule_node_ancestor",
    Take("ScheduleNode"),
    Param(int, ctype=c_int),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_n_children = ISLFunction.create(
    "isl_schedule_node_n_children",
    Keep("ScheduleNode"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_child = ISLFunction.create(
    "isl_schedule_node_child",
    Take("ScheduleNode"),
    Param(int, ctype=c_int),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_has_children = ISLFunction.create(
    "isl_schedule_node_has_children",
    Keep("ScheduleNode"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_grandchild = ISLFunction.create(
    "isl_schedule_node_grandchild",
    Take("ScheduleNode"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_first_child = ISLFunction.create(
    "isl_schedule_node_first_child",
    Take("ScheduleNode"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_has_previous_sibling = ISLFunction.create(
    "isl_schedule_node_has_previous_sibling",
    Keep("ScheduleNode"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_previous_sibling = ISLFunction.create(
    "isl_schedule_node_previous_sibling",
    Take("ScheduleNode"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_has_next_sibling = ISLFunction.create(
    "isl_schedule_node_has_next_sibling",
    Keep("ScheduleNode"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_next_sibling = ISLFunction.create(
    "isl_schedule_node_next_sibling",
    Take("ScheduleNode"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_get_tree_depth = ISLFunction.create(
    "isl_schedule_node_get_tree_depth",
    Keep("ScheduleNode"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_get_child_position = ISLFunction.create(
    "isl_schedule_node_get_child_position",
    Keep("ScheduleNode"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_get_ancestor_child_position = ISLFunction.create(
    "isl_schedule_node_get_ancestor_child_position",
    Keep("ScheduleNode"),
    Keep("ScheduleNode"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_get_child = ISLFunction.create(
    "isl_schedule_node_get_child",
    Keep("ScheduleNode"),
    Param(int, ctype=c_int),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_get_shared_ancestor = ISLFunction.create(
    "isl_schedule_node_get_shared_ancestor",
    Keep("ScheduleNode"),
    Keep("ScheduleNode"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_foreach_descendant_top_down = ISLFunction.create(
    "isl_schedule_node_foreach_descendant_top_down",
    Keep("ScheduleNode"),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_every_descendant = ISLFunction.create(
    "isl_schedule_node_every_descendant",
    Keep("ScheduleNode"),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_foreach_ancestor_top_down = ISLFunction.create(
    "isl_schedule_node_foreach_ancestor_top_down",
    Keep("ScheduleNode"),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_map_descendant_bottom_up = ISLFunction.create(
    "isl_schedule_node_map_descendant_bottom_up",
    Take("ScheduleNode"),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_cut = ISLFunction.create(
    "isl_schedule_node_cut",
    Take("ScheduleNode"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_delete = ISLFunction.create(
    "isl_schedule_node_delete",
    Take("ScheduleNode"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_is_subtree_anchored = ISLFunction.create(
    "isl_schedule_node_is_subtree_anchored",
    Keep("ScheduleNode"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_reset_user = ISLFunction.create(
    "isl_schedule_node_reset_user",
    Take("ScheduleNode"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_align_params = ISLFunction.create(
    "isl_schedule_node_align_params",
    Take("ScheduleNode"),
    Take("Space"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_band_get_space = ISLFunction.create(
    "isl_schedule_node_band_get_space",
    Keep("ScheduleNode"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_schedule_node_band_get_partial_schedule = ISLFunction.create(
    "isl_schedule_node_band_get_partial_schedule",
    Keep("ScheduleNode"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_schedule_node_band_get_partial_schedule_union_map = ISLFunction.create(
    "isl_schedule_node_band_get_partial_schedule_union_map",
    Keep("ScheduleNode"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_schedule_node_band_n_member = ISLFunction.create(
    "isl_schedule_node_band_n_member",
    Keep("ScheduleNode"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_band_member_get_coincident = ISLFunction.create(
    "isl_schedule_node_band_member_get_coincident",
    Keep("ScheduleNode"),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_band_member_set_coincident = ISLFunction.create(
    "isl_schedule_node_band_member_set_coincident",
    Take("ScheduleNode"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_band_get_permutable = ISLFunction.create(
    "isl_schedule_node_band_get_permutable",
    Keep("ScheduleNode"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_band_set_permutable = ISLFunction.create(
    "isl_schedule_node_band_set_permutable",
    Take("ScheduleNode"),
    Param(int, ctype=c_int),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_band_member_get_ast_loop_type = ISLFunction.create(
    "isl_schedule_node_band_member_get_ast_loop_type",
    Keep("ScheduleNode"),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_band_member_set_ast_loop_type = ISLFunction.create(
    "isl_schedule_node_band_member_set_ast_loop_type",
    Take("ScheduleNode"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_band_member_get_isolate_ast_loop_type = ISLFunction.create(
    "isl_schedule_node_band_member_get_isolate_ast_loop_type",
    Keep("ScheduleNode"),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_band_member_set_isolate_ast_loop_type = ISLFunction.create(
    "isl_schedule_node_band_member_set_isolate_ast_loop_type",
    Take("ScheduleNode"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_band_get_ast_build_options = ISLFunction.create(
    "isl_schedule_node_band_get_ast_build_options",
    Keep("ScheduleNode"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_schedule_node_band_set_ast_build_options = ISLFunction.create(
    "isl_schedule_node_band_set_ast_build_options",
    Take("ScheduleNode"),
    Take("UnionSet"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_band_get_ast_isolate_option = ISLFunction.create(
    "isl_schedule_node_band_get_ast_isolate_option",
    Keep("ScheduleNode"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_schedule_node_context_get_context = ISLFunction.create(
    "isl_schedule_node_context_get_context",
    Keep("ScheduleNode"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_schedule_node_domain_get_domain = ISLFunction.create(
    "isl_schedule_node_domain_get_domain",
    Keep("ScheduleNode"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_schedule_node_expansion_get_expansion = ISLFunction.create(
    "isl_schedule_node_expansion_get_expansion",
    Keep("ScheduleNode"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_schedule_node_expansion_get_contraction = ISLFunction.create(
    "isl_schedule_node_expansion_get_contraction",
    Keep("ScheduleNode"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_schedule_node_extension_get_extension = ISLFunction.create(
    "isl_schedule_node_extension_get_extension",
    Keep("ScheduleNode"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_schedule_node_filter_get_filter = ISLFunction.create(
    "isl_schedule_node_filter_get_filter",
    Keep("ScheduleNode"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_schedule_node_guard_get_guard = ISLFunction.create(
    "isl_schedule_node_guard_get_guard",
    Keep("ScheduleNode"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_schedule_node_mark_get_id = ISLFunction.create(
    "isl_schedule_node_mark_get_id",
    Keep("ScheduleNode"),
    return_=Give("Id"),
    lib=_lib,
)

_isl_schedule_node_get_prefix_schedule_multi_union_pw_aff = ISLFunction.create(
    "isl_schedule_node_get_prefix_schedule_multi_union_pw_aff",
    Keep("ScheduleNode"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_schedule_node_get_prefix_schedule_union_pw_multi_aff = ISLFunction.create(
    "isl_schedule_node_get_prefix_schedule_union_pw_multi_aff",
    Keep("ScheduleNode"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_schedule_node_get_prefix_schedule_union_map = ISLFunction.create(
    "isl_schedule_node_get_prefix_schedule_union_map",
    Keep("ScheduleNode"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_schedule_node_get_prefix_schedule_relation = ISLFunction.create(
    "isl_schedule_node_get_prefix_schedule_relation",
    Keep("ScheduleNode"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_schedule_node_get_subtree_schedule_union_map = ISLFunction.create(
    "isl_schedule_node_get_subtree_schedule_union_map",
    Keep("ScheduleNode"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_schedule_node_get_subtree_expansion = ISLFunction.create(
    "isl_schedule_node_get_subtree_expansion",
    Keep("ScheduleNode"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_schedule_node_get_subtree_contraction = ISLFunction.create(
    "isl_schedule_node_get_subtree_contraction",
    Keep("ScheduleNode"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_schedule_node_get_schedule_depth = ISLFunction.create(
    "isl_schedule_node_get_schedule_depth",
    Keep("ScheduleNode"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_schedule_node_get_domain = ISLFunction.create(
    "isl_schedule_node_get_domain",
    Keep("ScheduleNode"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_schedule_node_get_universe_domain = ISLFunction.create(
    "isl_schedule_node_get_universe_domain",
    Keep("ScheduleNode"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_schedule_node_insert_partial_schedule = ISLFunction.create(
    "isl_schedule_node_insert_partial_schedule",
    Take("ScheduleNode"),
    Take("MultiUnionPwAff"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_insert_context = ISLFunction.create(
    "isl_schedule_node_insert_context",
    Take("ScheduleNode"),
    Take("Set"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_insert_filter = ISLFunction.create(
    "isl_schedule_node_insert_filter",
    Take("ScheduleNode"),
    Take("UnionSet"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_insert_guard = ISLFunction.create(
    "isl_schedule_node_insert_guard",
    Take("ScheduleNode"),
    Take("Set"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_insert_mark = ISLFunction.create(
    "isl_schedule_node_insert_mark",
    Take("ScheduleNode"),
    Take("Id"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_insert_sequence = ISLFunction.create(
    "isl_schedule_node_insert_sequence",
    Take("ScheduleNode"),
    Take("UnionSetList"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_insert_set = ISLFunction.create(
    "isl_schedule_node_insert_set",
    Take("ScheduleNode"),
    Take("UnionSetList"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_group = ISLFunction.create(
    "isl_schedule_node_group",
    Take("ScheduleNode"),
    Take("Id"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_sequence_splice_child = ISLFunction.create(
    "isl_schedule_node_sequence_splice_child",
    Take("ScheduleNode"),
    Param(int, ctype=c_int),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_sequence_splice_children = ISLFunction.create(
    "isl_schedule_node_sequence_splice_children",
    Take("ScheduleNode"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_band_scale = ISLFunction.create(
    "isl_schedule_node_band_scale",
    Take("ScheduleNode"),
    Take("MultiVal"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_band_scale_down = ISLFunction.create(
    "isl_schedule_node_band_scale_down",
    Take("ScheduleNode"),
    Take("MultiVal"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_band_mod = ISLFunction.create(
    "isl_schedule_node_band_mod",
    Take("ScheduleNode"),
    Take("MultiVal"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_band_shift = ISLFunction.create(
    "isl_schedule_node_band_shift",
    Take("ScheduleNode"),
    Take("MultiUnionPwAff"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_band_tile = ISLFunction.create(
    "isl_schedule_node_band_tile",
    Take("ScheduleNode"),
    Take("MultiVal"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_band_split = ISLFunction.create(
    "isl_schedule_node_band_split",
    Take("ScheduleNode"),
    Param(int, ctype=c_int),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_band_sink = ISLFunction.create(
    "isl_schedule_node_band_sink",
    Take("ScheduleNode"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_order_before = ISLFunction.create(
    "isl_schedule_node_order_before",
    Take("ScheduleNode"),
    Take("UnionSet"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_order_after = ISLFunction.create(
    "isl_schedule_node_order_after",
    Take("ScheduleNode"),
    Take("UnionSet"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_graft_before = ISLFunction.create(
    "isl_schedule_node_graft_before",
    Take("ScheduleNode"),
    Take("ScheduleNode"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_graft_after = ISLFunction.create(
    "isl_schedule_node_graft_after",
    Take("ScheduleNode"),
    Take("ScheduleNode"),
    return_=Give("ScheduleNode"),
    lib=_lib,
)

_isl_schedule_node_to_str = ISLFunction.create(
    "isl_schedule_node_to_str",
    Keep("ScheduleNode"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_schedule_node_read_from_str = ISLFunction.create(
    "isl_schedule_node_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("ScheduleNode"),
    lib=_lib,
)
