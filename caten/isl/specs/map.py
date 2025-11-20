from __future__ import annotations

from ctypes import (
    c_char_p,
    c_int,
    c_uint,
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

class Map(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_map_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_map_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_map_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_map_free(handle)

    def __str__(self) -> str:
        return _isl_map_to_str(self)

    def __repr__(self) -> str:
        return f"Map({self.__str__()})"

    def get_space(self) -> "Space":
        return _isl_map_get_space(self)

    def domain_tuple_dim(self) -> int:
        return _isl_map_domain_tuple_dim(self)

    def range_tuple_dim(self) -> int:
        return _isl_map_range_tuple_dim(self)

    def dim(self, type: int) -> int:
        return _isl_map_dim(self, type)

    def set_dim_id(self, type: int, pos: int, id: "Id") -> "Map":
        return _isl_map_set_dim_id(self, type, pos, id)

    def has_dim_id(self, type: int, pos: int) -> bool:
        return _isl_map_has_dim_id(self, type, pos)

    def get_dim_id(self, type: int, pos: int) -> "Id":
        return _isl_map_get_dim_id(self, type, pos)

    def has_dim_name(self, type: int, pos: int) -> bool:
        return _isl_map_has_dim_name(self, type, pos)

    def get_dim_name(self, type: int, pos: int) -> str:
        return _isl_map_get_dim_name(self, type, pos)

    def find_dim_by_id(self, type: int, id: "Id") -> int:
        return _isl_map_find_dim_by_id(self, type, id)

    def find_dim_by_name(self, type: int, name: str) -> int:
        return _isl_map_find_dim_by_name(self, type, name)

    def set_domain_tuple_id(self, id: "Id") -> "Map":
        return _isl_map_set_domain_tuple_id(self, id)

    def set_range_tuple_id(self, id: "Id") -> "Map":
        return _isl_map_set_range_tuple_id(self, id)

    def set_tuple_id(self, type: int, id: "Id") -> "Map":
        return _isl_map_set_tuple_id(self, type, id)

    def reset_tuple_id(self, type: int) -> "Map":
        return _isl_map_reset_tuple_id(self, type)

    def has_domain_tuple_id(self) -> bool:
        return _isl_map_has_domain_tuple_id(self)

    def has_range_tuple_id(self) -> bool:
        return _isl_map_has_range_tuple_id(self)

    def has_tuple_id(self, type: int) -> bool:
        return _isl_map_has_tuple_id(self, type)

    def get_domain_tuple_id(self) -> "Id":
        return _isl_map_get_domain_tuple_id(self)

    def get_range_tuple_id(self) -> "Id":
        return _isl_map_get_range_tuple_id(self)

    def get_tuple_id(self, type: int) -> "Id":
        return _isl_map_get_tuple_id(self, type)

    def set_tuple_name(self, type: int, s: str) -> "Map":
        return _isl_map_set_tuple_name(self, type, s)

    def has_tuple_name(self, type: int) -> bool:
        return _isl_map_has_tuple_name(self, type)

    def get_tuple_name(self, type: int) -> str:
        return _isl_map_get_tuple_name(self, type)

    def reset_user(self) -> "Map":
        return _isl_map_reset_user(self)

    @classmethod
    def empty(cls, space: "Space") -> "Map":
        return _isl_map_empty(space)

    @classmethod
    def universe(cls, space: "Space") -> "Map":
        return _isl_map_universe(space)

    @classmethod
    def nat_universe(cls, space: "Space") -> "Map":
        return _isl_map_nat_universe(space)

    @classmethod
    def identity(cls, space: "Space") -> "Map":
        return _isl_map_identity(space)

    @classmethod
    def lex_lt(cls, set_space: "Space") -> "Map":
        return _isl_map_lex_lt(set_space)

    @classmethod
    def lex_le(cls, set_space: "Space") -> "Map":
        return _isl_map_lex_le(set_space)

    @classmethod
    def lex_gt(cls, set_space: "Space") -> "Map":
        return _isl_map_lex_gt(set_space)

    @classmethod
    def lex_ge(cls, set_space: "Space") -> "Map":
        return _isl_map_lex_ge(set_space)

    @classmethod
    def lex_lt_first(cls, space: "Space", n: int) -> "Map":
        return _isl_map_lex_lt_first(space, n)

    @classmethod
    def lex_le_first(cls, space: "Space", n: int) -> "Map":
        return _isl_map_lex_le_first(space, n)

    @classmethod
    def lex_gt_first(cls, space: "Space", n: int) -> "Map":
        return _isl_map_lex_gt_first(space, n)

    @classmethod
    def lex_ge_first(cls, space: "Space", n: int) -> "Map":
        return _isl_map_lex_ge_first(space, n)

    @classmethod
    def from_basic_map(cls, bmap: "BasicMap") -> "Map":
        return _isl_map_from_basic_map(bmap)

    def to_union_map(self) -> "UnionMap":
        return _isl_map_to_union_map(self)

    @classmethod
    def from_union_map(cls, umap: "UnionMap") -> "Map":
        return _isl_map_from_union_map(umap)

    def add_constraint(self, constraint: "Constraint") -> "Map":
        return _isl_map_add_constraint(self, constraint)

    @classmethod
    def from_aff(cls, aff: "Aff") -> "Map":
        return _isl_map_from_aff(aff)

    @classmethod
    def from_multi_aff(cls, maff: "MultiAff") -> "Map":
        return _isl_map_from_multi_aff(maff)

    @classmethod
    def from_pw_aff(cls, pwaff: "PwAff") -> "Map":
        return _isl_map_from_pw_aff(pwaff)

    @classmethod
    def from_pw_multi_aff(cls, pma: "PwMultiAff") -> "Map":
        return _isl_map_from_pw_multi_aff(pma)

    @classmethod
    def from_multi_pw_aff(cls, mpa: "MultiPwAff") -> "Map":
        return _isl_map_from_multi_pw_aff(mpa)

    def compute_divs(self) -> "Map":
        return _isl_map_compute_divs(self)

    def remove_divs(self) -> "Map":
        return _isl_map_remove_divs(self)

    def remove_divs_involving_dims(self, type: int, first: int, n: int) -> "Map":
        return _isl_map_remove_divs_involving_dims(self, type, first, n)

    def remove_unknown_divs(self) -> "Map":
        return _isl_map_remove_unknown_divs(self)

    def foreach_basic_map(self, fn: Any, user: Any = None) -> int:
        return _isl_map_foreach_basic_map(self, fn, user)

    def make_disjoint(self) -> "Map":
        return _isl_map_make_disjoint(self)

    def n_basic_map(self) -> int:
        return _isl_map_n_basic_map(self)

    def get_basic_map_list(self) -> "BasicMapList":
        return _isl_map_get_basic_map_list(self)

    @classmethod
    def read_from_file(cls, input: None) -> "Map":
        return _isl_map_read_from_file(input)

    def plain_is_empty(self) -> bool:
        return _isl_map_plain_is_empty(self)

    def is_empty(self) -> bool:
        return _isl_map_is_empty(self)

    def plain_is_universe(self) -> bool:
        return _isl_map_plain_is_universe(self)

    def plain_is_single_valued(self) -> bool:
        return _isl_map_plain_is_single_valued(self)

    def is_single_valued(self) -> bool:
        return _isl_map_is_single_valued(self)

    def plain_is_injective(self) -> bool:
        return _isl_map_plain_is_injective(self)

    def is_injective(self) -> bool:
        return _isl_map_is_injective(self)

    def is_bijective(self) -> bool:
        return _isl_map_is_bijective(self)

    def is_identity(self) -> bool:
        return _isl_map_is_identity(self)

    def plain_get_val_if_fixed(self, type: int, pos: int) -> "Val":
        return _isl_map_plain_get_val_if_fixed(self, type, pos)

    def get_range_stride_info(self, pos: int) -> "StrideInfo":
        return _isl_map_get_range_stride_info(self, pos)

    def get_range_lattice_tile(self) -> "FixedBox":
        return _isl_map_get_range_lattice_tile(self)

    def involves_dims(self, type: int, first: int, n: int) -> bool:
        return _isl_map_involves_dims(self, type, first, n)

    def domain_is_wrapping(self) -> bool:
        return _isl_map_domain_is_wrapping(self)

    def range_is_wrapping(self) -> bool:
        return _isl_map_range_is_wrapping(self)

    def is_product(self) -> bool:
        return _isl_map_is_product(self)

    def can_zip(self) -> bool:
        return _isl_map_can_zip(self)

    def can_curry(self) -> bool:
        return _isl_map_can_curry(self)

    def can_uncurry(self) -> bool:
        return _isl_map_can_uncurry(self)

    def can_range_curry(self) -> bool:
        return _isl_map_can_range_curry(self)

    def is_equal(self, map2: "Map") -> bool:
        return _isl_map_is_equal(self, map2)

    def is_equal(self, map2: "Map") -> bool:
        return _isl_map_plain_is_equal(self, map2)

    def is_disjoint(self, map2: "Map") -> bool:
        return _isl_map_is_disjoint(self, map2)

    def is_subset(self, map2: "Map") -> bool:
        return _isl_map_is_subset(self, map2)

    def is_strict_subset(self, map2: "Map") -> bool:
        return _isl_map_is_strict_subset(self, map2)

    def complement(self) -> "Map":
        return _isl_map_complement(self)

    def reverse(self) -> "Map":
        return _isl_map_reverse(self)

    def domain_reverse(self) -> "Map":
        return _isl_map_domain_reverse(self)

    def range_reverse(self) -> "Map":
        return _isl_map_range_reverse(self)

    def bind_domain(self, tuple: "MultiId") -> "Set":
        return _isl_map_bind_domain(self, tuple)

    def bind_range(self, tuple: "MultiId") -> "Set":
        return _isl_map_bind_range(self, tuple)

    def project_out_param_id(self, id: "Id") -> "Map":
        return _isl_map_project_out_param_id(self, id)

    def project_out_param_id_list(self, list: "IdList") -> "Map":
        return _isl_map_project_out_param_id_list(self, list)

    def project_out(self, type: int, first: int, n: int) -> "Map":
        return _isl_map_project_out(self, type, first, n)

    def project_out_all_params(self) -> "Map":
        return _isl_map_project_out_all_params(self)

    def params(self) -> "Set":
        return _isl_map_params(self)

    def domain(self) -> "Set":
        return _isl_map_domain(self)

    def range(self) -> "Set":
        return _isl_map_range(self)

    def domain_map(self) -> "Map":
        return _isl_map_domain_map(self)

    def range_map(self) -> "Map":
        return _isl_map_range_map(self)

    def eliminate(self, type: int, first: int, n: int) -> "Map":
        return _isl_map_eliminate(self, type, first, n)

    @classmethod
    def from_domain(cls, set: "Set") -> "Map":
        return _isl_map_from_domain(set)

    @classmethod
    def from_range(cls, set: "Set") -> "Map":
        return _isl_map_from_range(set)

    def fix_si(self, type: int, pos: int, value: int) -> "Map":
        return _isl_map_fix_si(self, type, pos, value)

    def fix_val(self, type: int, pos: int, v: "Val") -> "Map":
        return _isl_map_fix_val(self, type, pos, v)

    def lower_bound_si(self, type: int, pos: int, value: int) -> "Map":
        return _isl_map_lower_bound_si(self, type, pos, value)

    def upper_bound_si(self, type: int, pos: int, value: int) -> "Map":
        return _isl_map_upper_bound_si(self, type, pos, value)

    def lower_bound_val(self, type: int, pos: int, value: "Val") -> "Map":
        return _isl_map_lower_bound_val(self, type, pos, value)

    def upper_bound_val(self, type: int, pos: int, value: "Val") -> "Map":
        return _isl_map_upper_bound_val(self, type, pos, value)

    def lower_bound_multi_pw_aff(self, lower: "MultiPwAff") -> "Map":
        return _isl_map_lower_bound_multi_pw_aff(self, lower)

    def upper_bound_multi_pw_aff(self, upper: "MultiPwAff") -> "Map":
        return _isl_map_upper_bound_multi_pw_aff(self, upper)

    def equate(self, type1: int, pos1: int, type2: int, pos2: int) -> "Map":
        return _isl_map_equate(self, type1, pos1, type2, pos2)

    def oppose(self, type1: int, pos1: int, type2: int, pos2: int) -> "Map":
        return _isl_map_oppose(self, type1, pos1, type2, pos2)

    def order_le(self, type1: int, pos1: int, type2: int, pos2: int) -> "Map":
        return _isl_map_order_le(self, type1, pos1, type2, pos2)

    def order_ge(self, type1: int, pos1: int, type2: int, pos2: int) -> "Map":
        return _isl_map_order_ge(self, type1, pos1, type2, pos2)

    def order_lt(self, type1: int, pos1: int, type2: int, pos2: int) -> "Map":
        return _isl_map_order_lt(self, type1, pos1, type2, pos2)

    def order_gt(self, type1: int, pos1: int, type2: int, pos2: int) -> "Map":
        return _isl_map_order_gt(self, type1, pos1, type2, pos2)

    def as_pw_multi_aff(self) -> "PwMultiAff":
        return _isl_map_as_pw_multi_aff(self)

    def deltas(self) -> "Set":
        return _isl_map_deltas(self)

    def deltas_map(self) -> "Map":
        return _isl_map_deltas_map(self)

    def coalesce(self) -> "Map":
        return _isl_map_coalesce(self)

    def detect_equalities(self) -> "Map":
        return _isl_map_detect_equalities(self)

    def remove_redundancies(self) -> "Map":
        return _isl_map_remove_redundancies(self)

    def convex_hull(self) -> "BasicMap":
        return _isl_map_convex_hull(self)

    def unshifted_simple_hull(self) -> "BasicMap":
        return _isl_map_unshifted_simple_hull(self)

    def simple_hull(self) -> "BasicMap":
        return _isl_map_simple_hull(self)

    def plain_unshifted_simple_hull(self) -> "BasicMap":
        return _isl_map_plain_unshifted_simple_hull(self)

    def unshifted_simple_hull_from_map_list(self, list: "MapList") -> "BasicMap":
        return _isl_map_unshifted_simple_hull_from_map_list(self, list)

    def affine_hull(self) -> "BasicMap":
        return _isl_map_affine_hull(self)

    def polyhedral_hull(self) -> "BasicMap":
        return _isl_map_polyhedral_hull(self)

    def drop_constraints_involving_dims(self, type: int, first: int, n: int) -> "Map":
        return _isl_map_drop_constraints_involving_dims(self, type, first, n)

    def drop_constraints_not_involving_dims(self, type: int, first: int, n: int) -> "Map":
        return _isl_map_drop_constraints_not_involving_dims(self, type, first, n)

    def sample(self) -> "BasicMap":
        return _isl_map_sample(self)

    def dim_min(self, pos: int) -> "PwAff":
        return _isl_map_dim_min(self, pos)

    def dim_max(self, pos: int) -> "PwAff":
        return _isl_map_dim_max(self, pos)

    def min_multi_pw_aff(self) -> "MultiPwAff":
        return _isl_map_min_multi_pw_aff(self)

    def max_multi_pw_aff(self) -> "MultiPwAff":
        return _isl_map_max_multi_pw_aff(self)

    def fixed_power_val(self, exp: "Val") -> "Map":
        return _isl_map_fixed_power_val(self, exp)

    def wrap(self) -> "Set":
        return _isl_map_wrap(self)

    def flatten_range(self) -> "Map":
        return _isl_map_flatten_range(self)

    def flatten_domain(self) -> "Map":
        return _isl_map_flatten_domain(self)

    def flatten(self) -> "Map":
        return _isl_map_flatten(self)

    def zip(self) -> "Map":
        return _isl_map_zip(self)

    def curry(self) -> "Map":
        return _isl_map_curry(self)

    def uncurry(self) -> "Map":
        return _isl_map_uncurry(self)

    def range_curry(self) -> "Map":
        return _isl_map_range_curry(self)

    def align_params(self, model: "Space") -> "Map":
        return _isl_map_align_params(self, model)

    def drop_unused_params(self) -> "Map":
        return _isl_map_drop_unused_params(self)

    def neg(self) -> "Map":
        return _isl_map_neg(self)

    def add_dims(self, type: int, n: int) -> "Map":
        return _isl_map_add_dims(self, type, n)

    def insert_dims(self, type: int, pos: int, n: int) -> "Map":
        return _isl_map_insert_dims(self, type, pos, n)

    def move_dims(self, dst_type: int, dst_pos: int, src_type: int, src_pos: int, n: int) -> "Map":
        return _isl_map_move_dims(self, dst_type, dst_pos, src_type, src_pos, n)

    def intersect_params(self, params: "Set") -> "Map":
        return _isl_map_intersect_params(self, params)

    def intersect_domain(self, set: "Set") -> "Map":
        return _isl_map_intersect_domain(self, set)

    def intersect_range(self, set: "Set") -> "Map":
        return _isl_map_intersect_range(self, set)

    def intersect(self, map2: "Map") -> "Map":
        return _isl_map_intersect(self, map2)

    def intersect_domain_factor_domain(self, factor: "Map") -> "Map":
        return _isl_map_intersect_domain_factor_domain(self, factor)

    def intersect_domain_factor_range(self, factor: "Map") -> "Map":
        return _isl_map_intersect_domain_factor_range(self, factor)

    def intersect_range_factor_domain(self, factor: "Map") -> "Map":
        return _isl_map_intersect_range_factor_domain(self, factor)

    def intersect_range_factor_range(self, factor: "Map") -> "Map":
        return _isl_map_intersect_range_factor_range(self, factor)

    def intersect_domain_wrapped_domain(self, domain: "Set") -> "Map":
        return _isl_map_intersect_domain_wrapped_domain(self, domain)

    def intersect_range_wrapped_domain(self, domain: "Set") -> "Map":
        return _isl_map_intersect_range_wrapped_domain(self, domain)

    def union(self, map2: "Map") -> "Map":
        return _isl_map_union(self, map2)

    def subtract(self, map2: "Map") -> "Map":
        return _isl_map_subtract(self, map2)

    def subtract_domain(self, dom: "Set") -> "Map":
        return _isl_map_subtract_domain(self, dom)

    def subtract_range(self, dom: "Set") -> "Map":
        return _isl_map_subtract_range(self, dom)

    def apply_domain(self, map2: "Map") -> "Map":
        return _isl_map_apply_domain(self, map2)

    def apply_range(self, map2: "Map") -> "Map":
        return _isl_map_apply_range(self, map2)

    def preimage_domain_multi_aff(self, ma: "MultiAff") -> "Map":
        return _isl_map_preimage_domain_multi_aff(self, ma)

    def preimage_range_multi_aff(self, ma: "MultiAff") -> "Map":
        return _isl_map_preimage_range_multi_aff(self, ma)

    def preimage_domain_pw_multi_aff(self, pma: "PwMultiAff") -> "Map":
        return _isl_map_preimage_domain_pw_multi_aff(self, pma)

    def preimage_range_pw_multi_aff(self, pma: "PwMultiAff") -> "Map":
        return _isl_map_preimage_range_pw_multi_aff(self, pma)

    def preimage_domain_multi_pw_aff(self, mpa: "MultiPwAff") -> "Map":
        return _isl_map_preimage_domain_multi_pw_aff(self, mpa)

    def eq_at_multi_pw_aff(self, mpa: "MultiPwAff") -> "Map":
        return _isl_map_eq_at_multi_pw_aff(self, mpa)

    def lex_lt_at_multi_pw_aff(self, mpa: "MultiPwAff") -> "Map":
        return _isl_map_lex_lt_at_multi_pw_aff(self, mpa)

    def lex_le_at_multi_pw_aff(self, mpa: "MultiPwAff") -> "Map":
        return _isl_map_lex_le_at_multi_pw_aff(self, mpa)

    def lex_gt_at_multi_pw_aff(self, mpa: "MultiPwAff") -> "Map":
        return _isl_map_lex_gt_at_multi_pw_aff(self, mpa)

    def lex_ge_at_multi_pw_aff(self, mpa: "MultiPwAff") -> "Map":
        return _isl_map_lex_ge_at_multi_pw_aff(self, mpa)

    def domain_product(self, map2: "Map") -> "Map":
        return _isl_map_domain_product(self, map2)

    def range_product(self, map2: "Map") -> "Map":
        return _isl_map_range_product(self, map2)

    def product(self, map2: "Map") -> "Map":
        return _isl_map_product(self, map2)

    def flat_domain_product(self, map2: "Map") -> "Map":
        return _isl_map_flat_domain_product(self, map2)

    def flat_range_product(self, map2: "Map") -> "Map":
        return _isl_map_flat_range_product(self, map2)

    def flat_product(self, map2: "Map") -> "Map":
        return _isl_map_flat_product(self, map2)

    def factor_domain(self) -> "Map":
        return _isl_map_factor_domain(self)

    def factor_range(self) -> "Map":
        return _isl_map_factor_range(self)

    def domain_factor_domain(self) -> "Map":
        return _isl_map_domain_factor_domain(self)

    def domain_factor_range(self) -> "Map":
        return _isl_map_domain_factor_range(self)

    def range_factor_domain(self) -> "Map":
        return _isl_map_range_factor_domain(self)

    def range_factor_range(self) -> "Map":
        return _isl_map_range_factor_range(self)

    def gist(self, context: "Map") -> "Map":
        return _isl_map_gist(self, context)

    def gist_params(self, context: "Set") -> "Map":
        return _isl_map_gist_params(self, context)

    def gist_domain(self, context: "Set") -> "Map":
        return _isl_map_gist_domain(self, context)

    def gist_range(self, context: "Set") -> "Map":
        return _isl_map_gist_range(self, context)

    def sum(self, map2: "Map") -> "Map":
        return _isl_map_sum(self, map2)

    def partial_lexmax(self, dom: "Set", empty: "Set") -> "Map":
        return _isl_map_partial_lexmax(self, dom, empty)

    def partial_lexmin(self, dom: "Set", empty: "Set") -> "Map":
        return _isl_map_partial_lexmin(self, dom, empty)

    def lexmin(self) -> "Map":
        return _isl_map_lexmin(self)

    def lexmax(self) -> "Map":
        return _isl_map_lexmax(self)

    def lexmin_pw_multi_aff(self) -> "PwMultiAff":
        return _isl_map_lexmin_pw_multi_aff(self)

    def lexmax_pw_multi_aff(self) -> "PwMultiAff":
        return _isl_map_lexmax_pw_multi_aff(self)


register_type("Map", Map)

_isl_map_get_space = ISLFunction.create(
    "isl_map_get_space",
    Keep("Map"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_map_domain_tuple_dim = ISLFunction.create(
    "isl_map_domain_tuple_dim",
    Keep("Map"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_map_range_tuple_dim = ISLFunction.create(
    "isl_map_range_tuple_dim",
    Keep("Map"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_map_dim = ISLFunction.create(
    "isl_map_dim",
    Keep("Map"),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_map_set_dim_id = ISLFunction.create(
    "isl_map_set_dim_id",
    Take("Map"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Take("Id"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_has_dim_id = ISLFunction.create(
    "isl_map_has_dim_id",
    Keep("Map"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_get_dim_id = ISLFunction.create(
    "isl_map_get_dim_id",
    Keep("Map"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Give("Id"),
    lib=_lib,
)

_isl_map_has_dim_name = ISLFunction.create(
    "isl_map_has_dim_name",
    Keep("Map"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_get_dim_name = ISLFunction.create(
    "isl_map_get_dim_name",
    Keep("Map"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_map_find_dim_by_id = ISLFunction.create(
    "isl_map_find_dim_by_id",
    Keep("Map"),
    Param(int, ctype=c_int),
    Keep("Id"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_map_find_dim_by_name = ISLFunction.create(
    "isl_map_find_dim_by_name",
    Keep("Map"),
    Param(int, ctype=c_int),
    Param(str, ctype=c_char_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_map_set_domain_tuple_id = ISLFunction.create(
    "isl_map_set_domain_tuple_id",
    Take("Map"),
    Take("Id"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_set_range_tuple_id = ISLFunction.create(
    "isl_map_set_range_tuple_id",
    Take("Map"),
    Take("Id"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_set_tuple_id = ISLFunction.create(
    "isl_map_set_tuple_id",
    Take("Map"),
    Param(int, ctype=c_int),
    Take("Id"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_reset_tuple_id = ISLFunction.create(
    "isl_map_reset_tuple_id",
    Take("Map"),
    Param(int, ctype=c_int),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_has_domain_tuple_id = ISLFunction.create(
    "isl_map_has_domain_tuple_id",
    Keep("Map"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_has_range_tuple_id = ISLFunction.create(
    "isl_map_has_range_tuple_id",
    Keep("Map"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_has_tuple_id = ISLFunction.create(
    "isl_map_has_tuple_id",
    Keep("Map"),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_get_domain_tuple_id = ISLFunction.create(
    "isl_map_get_domain_tuple_id",
    Keep("Map"),
    return_=Give("Id"),
    lib=_lib,
)

_isl_map_get_range_tuple_id = ISLFunction.create(
    "isl_map_get_range_tuple_id",
    Keep("Map"),
    return_=Give("Id"),
    lib=_lib,
)

_isl_map_get_tuple_id = ISLFunction.create(
    "isl_map_get_tuple_id",
    Keep("Map"),
    Param(int, ctype=c_int),
    return_=Give("Id"),
    lib=_lib,
)

_isl_map_set_tuple_name = ISLFunction.create(
    "isl_map_set_tuple_name",
    Take("Map"),
    Param(int, ctype=c_int),
    Param(str, ctype=c_char_p),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_has_tuple_name = ISLFunction.create(
    "isl_map_has_tuple_name",
    Keep("Map"),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_get_tuple_name = ISLFunction.create(
    "isl_map_get_tuple_name",
    Keep("Map"),
    Param(int, ctype=c_int),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_map_reset_user = ISLFunction.create(
    "isl_map_reset_user",
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_empty = ISLFunction.create(
    "isl_map_empty",
    Take("Space"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_universe = ISLFunction.create(
    "isl_map_universe",
    Take("Space"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_nat_universe = ISLFunction.create(
    "isl_map_nat_universe",
    Take("Space"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_identity = ISLFunction.create(
    "isl_map_identity",
    Take("Space"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_lex_lt = ISLFunction.create(
    "isl_map_lex_lt",
    Take("Space"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_lex_le = ISLFunction.create(
    "isl_map_lex_le",
    Take("Space"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_lex_gt = ISLFunction.create(
    "isl_map_lex_gt",
    Take("Space"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_lex_ge = ISLFunction.create(
    "isl_map_lex_ge",
    Take("Space"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_lex_lt_first = ISLFunction.create(
    "isl_map_lex_lt_first",
    Take("Space"),
    Param(int, ctype=c_uint),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_lex_le_first = ISLFunction.create(
    "isl_map_lex_le_first",
    Take("Space"),
    Param(int, ctype=c_uint),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_lex_gt_first = ISLFunction.create(
    "isl_map_lex_gt_first",
    Take("Space"),
    Param(int, ctype=c_uint),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_lex_ge_first = ISLFunction.create(
    "isl_map_lex_ge_first",
    Take("Space"),
    Param(int, ctype=c_uint),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_from_basic_map = ISLFunction.create(
    "isl_map_from_basic_map",
    Take("BasicMap"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_to_union_map = ISLFunction.create(
    "isl_map_to_union_map",
    Take("Map"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_map_from_union_map = ISLFunction.create(
    "isl_map_from_union_map",
    Take("UnionMap"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_copy = ISLFunction.create(
    "isl_map_copy",
    Keep("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_free = ISLFunction.create(
    "isl_map_free",
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_add_constraint = ISLFunction.create(
    "isl_map_add_constraint",
    Take("Map"),
    Take("Constraint"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_from_aff = ISLFunction.create(
    "isl_map_from_aff",
    Take("Aff"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_from_multi_aff = ISLFunction.create(
    "isl_map_from_multi_aff",
    Take("MultiAff"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_from_pw_aff = ISLFunction.create(
    "isl_map_from_pw_aff",
    Take("PwAff"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_from_pw_multi_aff = ISLFunction.create(
    "isl_map_from_pw_multi_aff",
    Take("PwMultiAff"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_from_multi_pw_aff = ISLFunction.create(
    "isl_map_from_multi_pw_aff",
    Take("MultiPwAff"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_compute_divs = ISLFunction.create(
    "isl_map_compute_divs",
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_remove_divs = ISLFunction.create(
    "isl_map_remove_divs",
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_remove_divs_involving_dims = ISLFunction.create(
    "isl_map_remove_divs_involving_dims",
    Take("Map"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_remove_unknown_divs = ISLFunction.create(
    "isl_map_remove_unknown_divs",
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_foreach_basic_map = ISLFunction.create(
    "isl_map_foreach_basic_map",
    Keep("Map"),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_map_make_disjoint = ISLFunction.create(
    "isl_map_make_disjoint",
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_n_basic_map = ISLFunction.create(
    "isl_map_n_basic_map",
    Keep("Map"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_map_get_basic_map_list = ISLFunction.create(
    "isl_map_get_basic_map_list",
    Keep("Map"),
    return_=Give("BasicMapList"),
    lib=_lib,
)

_isl_map_read_from_file = ISLFunction.create(
    "isl_map_read_from_file",
    Context(),
    Param(None, ctype=c_void_p),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_read_from_str = ISLFunction.create(
    "isl_map_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_to_str = ISLFunction.create(
    "isl_map_to_str",
    Keep("Map"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_map_plain_is_empty = ISLFunction.create(
    "isl_map_plain_is_empty",
    Keep("Map"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_empty = ISLFunction.create(
    "isl_map_is_empty",
    Keep("Map"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_plain_is_universe = ISLFunction.create(
    "isl_map_plain_is_universe",
    Keep("Map"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_plain_is_single_valued = ISLFunction.create(
    "isl_map_plain_is_single_valued",
    Keep("Map"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_single_valued = ISLFunction.create(
    "isl_map_is_single_valued",
    Keep("Map"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_plain_is_injective = ISLFunction.create(
    "isl_map_plain_is_injective",
    Keep("Map"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_injective = ISLFunction.create(
    "isl_map_is_injective",
    Keep("Map"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_bijective = ISLFunction.create(
    "isl_map_is_bijective",
    Keep("Map"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_identity = ISLFunction.create(
    "isl_map_is_identity",
    Keep("Map"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_plain_get_val_if_fixed = ISLFunction.create(
    "isl_map_plain_get_val_if_fixed",
    Keep("Map"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Give("Val"),
    lib=_lib,
)

_isl_map_get_range_stride_info = ISLFunction.create(
    "isl_map_get_range_stride_info",
    Keep("Map"),
    Param(int, ctype=c_int),
    return_=Give("StrideInfo"),
    lib=_lib,
)

_isl_map_get_range_lattice_tile = ISLFunction.create(
    "isl_map_get_range_lattice_tile",
    Keep("Map"),
    return_=Give("FixedBox"),
    lib=_lib,
)

_isl_map_involves_dims = ISLFunction.create(
    "isl_map_involves_dims",
    Keep("Map"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_domain_is_wrapping = ISLFunction.create(
    "isl_map_domain_is_wrapping",
    Keep("Map"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_range_is_wrapping = ISLFunction.create(
    "isl_map_range_is_wrapping",
    Keep("Map"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_product = ISLFunction.create(
    "isl_map_is_product",
    Keep("Map"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_can_zip = ISLFunction.create(
    "isl_map_can_zip",
    Keep("Map"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_can_curry = ISLFunction.create(
    "isl_map_can_curry",
    Keep("Map"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_can_uncurry = ISLFunction.create(
    "isl_map_can_uncurry",
    Keep("Map"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_can_range_curry = ISLFunction.create(
    "isl_map_can_range_curry",
    Keep("Map"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_equal = ISLFunction.create(
    "isl_map_is_equal",
    Keep("Map"),
    Keep("Map"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_plain_is_equal = ISLFunction.create(
    "isl_map_plain_is_equal",
    Keep("Map"),
    Keep("Map"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_disjoint = ISLFunction.create(
    "isl_map_is_disjoint",
    Keep("Map"),
    Keep("Map"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_subset = ISLFunction.create(
    "isl_map_is_subset",
    Keep("Map"),
    Keep("Map"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_strict_subset = ISLFunction.create(
    "isl_map_is_strict_subset",
    Keep("Map"),
    Keep("Map"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_complement = ISLFunction.create(
    "isl_map_complement",
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_reverse = ISLFunction.create(
    "isl_map_reverse",
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_domain_reverse = ISLFunction.create(
    "isl_map_domain_reverse",
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_range_reverse = ISLFunction.create(
    "isl_map_range_reverse",
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_bind_domain = ISLFunction.create(
    "isl_map_bind_domain",
    Take("Map"),
    Take("MultiId"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_map_bind_range = ISLFunction.create(
    "isl_map_bind_range",
    Take("Map"),
    Take("MultiId"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_map_project_out_param_id = ISLFunction.create(
    "isl_map_project_out_param_id",
    Take("Map"),
    Take("Id"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_project_out_param_id_list = ISLFunction.create(
    "isl_map_project_out_param_id_list",
    Take("Map"),
    Take("IdList"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_project_out = ISLFunction.create(
    "isl_map_project_out",
    Take("Map"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_project_out_all_params = ISLFunction.create(
    "isl_map_project_out_all_params",
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_params = ISLFunction.create(
    "isl_map_params",
    Take("Map"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_map_domain = ISLFunction.create(
    "isl_map_domain",
    Take("Map"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_map_range = ISLFunction.create(
    "isl_map_range",
    Take("Map"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_map_domain_map = ISLFunction.create(
    "isl_map_domain_map",
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_range_map = ISLFunction.create(
    "isl_map_range_map",
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_eliminate = ISLFunction.create(
    "isl_map_eliminate",
    Take("Map"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_from_domain = ISLFunction.create(
    "isl_map_from_domain",
    Take("Set"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_from_range = ISLFunction.create(
    "isl_map_from_range",
    Take("Set"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_fix_si = ISLFunction.create(
    "isl_map_fix_si",
    Take("Map"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_int),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_fix_val = ISLFunction.create(
    "isl_map_fix_val",
    Take("Map"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Take("Val"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_lower_bound_si = ISLFunction.create(
    "isl_map_lower_bound_si",
    Take("Map"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_int),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_upper_bound_si = ISLFunction.create(
    "isl_map_upper_bound_si",
    Take("Map"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_int),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_lower_bound_val = ISLFunction.create(
    "isl_map_lower_bound_val",
    Take("Map"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Take("Val"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_upper_bound_val = ISLFunction.create(
    "isl_map_upper_bound_val",
    Take("Map"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Take("Val"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_lower_bound_multi_pw_aff = ISLFunction.create(
    "isl_map_lower_bound_multi_pw_aff",
    Take("Map"),
    Take("MultiPwAff"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_upper_bound_multi_pw_aff = ISLFunction.create(
    "isl_map_upper_bound_multi_pw_aff",
    Take("Map"),
    Take("MultiPwAff"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_equate = ISLFunction.create(
    "isl_map_equate",
    Take("Map"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_oppose = ISLFunction.create(
    "isl_map_oppose",
    Take("Map"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_order_le = ISLFunction.create(
    "isl_map_order_le",
    Take("Map"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_order_ge = ISLFunction.create(
    "isl_map_order_ge",
    Take("Map"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_order_lt = ISLFunction.create(
    "isl_map_order_lt",
    Take("Map"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_order_gt = ISLFunction.create(
    "isl_map_order_gt",
    Take("Map"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_as_pw_multi_aff = ISLFunction.create(
    "isl_map_as_pw_multi_aff",
    Take("Map"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_map_deltas = ISLFunction.create(
    "isl_map_deltas",
    Take("Map"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_map_deltas_map = ISLFunction.create(
    "isl_map_deltas_map",
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_coalesce = ISLFunction.create(
    "isl_map_coalesce",
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_detect_equalities = ISLFunction.create(
    "isl_map_detect_equalities",
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_remove_redundancies = ISLFunction.create(
    "isl_map_remove_redundancies",
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_convex_hull = ISLFunction.create(
    "isl_map_convex_hull",
    Take("Map"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_map_unshifted_simple_hull = ISLFunction.create(
    "isl_map_unshifted_simple_hull",
    Take("Map"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_map_simple_hull = ISLFunction.create(
    "isl_map_simple_hull",
    Take("Map"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_map_plain_unshifted_simple_hull = ISLFunction.create(
    "isl_map_plain_unshifted_simple_hull",
    Take("Map"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_map_unshifted_simple_hull_from_map_list = ISLFunction.create(
    "isl_map_unshifted_simple_hull_from_map_list",
    Take("Map"),
    Take("MapList"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_map_affine_hull = ISLFunction.create(
    "isl_map_affine_hull",
    Take("Map"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_map_polyhedral_hull = ISLFunction.create(
    "isl_map_polyhedral_hull",
    Take("Map"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_map_drop_constraints_involving_dims = ISLFunction.create(
    "isl_map_drop_constraints_involving_dims",
    Take("Map"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_drop_constraints_not_involving_dims = ISLFunction.create(
    "isl_map_drop_constraints_not_involving_dims",
    Take("Map"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_sample = ISLFunction.create(
    "isl_map_sample",
    Take("Map"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_map_dim_min = ISLFunction.create(
    "isl_map_dim_min",
    Take("Map"),
    Param(int, ctype=c_int),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_map_dim_max = ISLFunction.create(
    "isl_map_dim_max",
    Take("Map"),
    Param(int, ctype=c_int),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_map_min_multi_pw_aff = ISLFunction.create(
    "isl_map_min_multi_pw_aff",
    Take("Map"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_map_max_multi_pw_aff = ISLFunction.create(
    "isl_map_max_multi_pw_aff",
    Take("Map"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_map_fixed_power_val = ISLFunction.create(
    "isl_map_fixed_power_val",
    Take("Map"),
    Take("Val"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_wrap = ISLFunction.create(
    "isl_map_wrap",
    Take("Map"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_map_flatten_range = ISLFunction.create(
    "isl_map_flatten_range",
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_flatten_domain = ISLFunction.create(
    "isl_map_flatten_domain",
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_flatten = ISLFunction.create(
    "isl_map_flatten",
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_zip = ISLFunction.create(
    "isl_map_zip",
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_curry = ISLFunction.create(
    "isl_map_curry",
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_uncurry = ISLFunction.create(
    "isl_map_uncurry",
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_range_curry = ISLFunction.create(
    "isl_map_range_curry",
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_align_params = ISLFunction.create(
    "isl_map_align_params",
    Take("Map"),
    Take("Space"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_drop_unused_params = ISLFunction.create(
    "isl_map_drop_unused_params",
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_neg = ISLFunction.create(
    "isl_map_neg",
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_add_dims = ISLFunction.create(
    "isl_map_add_dims",
    Take("Map"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_insert_dims = ISLFunction.create(
    "isl_map_insert_dims",
    Take("Map"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_move_dims = ISLFunction.create(
    "isl_map_move_dims",
    Take("Map"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_intersect_params = ISLFunction.create(
    "isl_map_intersect_params",
    Take("Map"),
    Take("Set"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_intersect_domain = ISLFunction.create(
    "isl_map_intersect_domain",
    Take("Map"),
    Take("Set"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_intersect_range = ISLFunction.create(
    "isl_map_intersect_range",
    Take("Map"),
    Take("Set"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_intersect = ISLFunction.create(
    "isl_map_intersect",
    Take("Map"),
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_intersect_domain_factor_domain = ISLFunction.create(
    "isl_map_intersect_domain_factor_domain",
    Take("Map"),
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_intersect_domain_factor_range = ISLFunction.create(
    "isl_map_intersect_domain_factor_range",
    Take("Map"),
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_intersect_range_factor_domain = ISLFunction.create(
    "isl_map_intersect_range_factor_domain",
    Take("Map"),
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_intersect_range_factor_range = ISLFunction.create(
    "isl_map_intersect_range_factor_range",
    Take("Map"),
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_intersect_domain_wrapped_domain = ISLFunction.create(
    "isl_map_intersect_domain_wrapped_domain",
    Take("Map"),
    Take("Set"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_intersect_range_wrapped_domain = ISLFunction.create(
    "isl_map_intersect_range_wrapped_domain",
    Take("Map"),
    Take("Set"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_union = ISLFunction.create(
    "isl_map_union",
    Take("Map"),
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_subtract = ISLFunction.create(
    "isl_map_subtract",
    Take("Map"),
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_subtract_domain = ISLFunction.create(
    "isl_map_subtract_domain",
    Take("Map"),
    Take("Set"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_subtract_range = ISLFunction.create(
    "isl_map_subtract_range",
    Take("Map"),
    Take("Set"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_apply_domain = ISLFunction.create(
    "isl_map_apply_domain",
    Take("Map"),
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_apply_range = ISLFunction.create(
    "isl_map_apply_range",
    Take("Map"),
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_preimage_domain_multi_aff = ISLFunction.create(
    "isl_map_preimage_domain_multi_aff",
    Take("Map"),
    Take("MultiAff"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_preimage_range_multi_aff = ISLFunction.create(
    "isl_map_preimage_range_multi_aff",
    Take("Map"),
    Take("MultiAff"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_preimage_domain_pw_multi_aff = ISLFunction.create(
    "isl_map_preimage_domain_pw_multi_aff",
    Take("Map"),
    Take("PwMultiAff"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_preimage_range_pw_multi_aff = ISLFunction.create(
    "isl_map_preimage_range_pw_multi_aff",
    Take("Map"),
    Take("PwMultiAff"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_preimage_domain_multi_pw_aff = ISLFunction.create(
    "isl_map_preimage_domain_multi_pw_aff",
    Take("Map"),
    Take("MultiPwAff"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_eq_at_multi_pw_aff = ISLFunction.create(
    "isl_map_eq_at_multi_pw_aff",
    Take("Map"),
    Take("MultiPwAff"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_lex_lt_at_multi_pw_aff = ISLFunction.create(
    "isl_map_lex_lt_at_multi_pw_aff",
    Take("Map"),
    Take("MultiPwAff"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_lex_le_at_multi_pw_aff = ISLFunction.create(
    "isl_map_lex_le_at_multi_pw_aff",
    Take("Map"),
    Take("MultiPwAff"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_lex_gt_at_multi_pw_aff = ISLFunction.create(
    "isl_map_lex_gt_at_multi_pw_aff",
    Take("Map"),
    Take("MultiPwAff"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_lex_ge_at_multi_pw_aff = ISLFunction.create(
    "isl_map_lex_ge_at_multi_pw_aff",
    Take("Map"),
    Take("MultiPwAff"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_domain_product = ISLFunction.create(
    "isl_map_domain_product",
    Take("Map"),
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_range_product = ISLFunction.create(
    "isl_map_range_product",
    Take("Map"),
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_product = ISLFunction.create(
    "isl_map_product",
    Take("Map"),
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_flat_domain_product = ISLFunction.create(
    "isl_map_flat_domain_product",
    Take("Map"),
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_flat_range_product = ISLFunction.create(
    "isl_map_flat_range_product",
    Take("Map"),
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_flat_product = ISLFunction.create(
    "isl_map_flat_product",
    Take("Map"),
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_factor_domain = ISLFunction.create(
    "isl_map_factor_domain",
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_factor_range = ISLFunction.create(
    "isl_map_factor_range",
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_domain_factor_domain = ISLFunction.create(
    "isl_map_domain_factor_domain",
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_domain_factor_range = ISLFunction.create(
    "isl_map_domain_factor_range",
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_range_factor_domain = ISLFunction.create(
    "isl_map_range_factor_domain",
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_range_factor_range = ISLFunction.create(
    "isl_map_range_factor_range",
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_gist = ISLFunction.create(
    "isl_map_gist",
    Take("Map"),
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_gist_params = ISLFunction.create(
    "isl_map_gist_params",
    Take("Map"),
    Take("Set"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_gist_domain = ISLFunction.create(
    "isl_map_gist_domain",
    Take("Map"),
    Take("Set"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_gist_range = ISLFunction.create(
    "isl_map_gist_range",
    Take("Map"),
    Take("Set"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_sum = ISLFunction.create(
    "isl_map_sum",
    Take("Map"),
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_partial_lexmax = ISLFunction.create(
    "isl_map_partial_lexmax",
    Take("Map"),
    Take("Set"),
    Give("Set"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_partial_lexmin = ISLFunction.create(
    "isl_map_partial_lexmin",
    Take("Map"),
    Take("Set"),
    Give("Set"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_lexmin = ISLFunction.create(
    "isl_map_lexmin",
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_lexmax = ISLFunction.create(
    "isl_map_lexmax",
    Take("Map"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_map_lexmin_pw_multi_aff = ISLFunction.create(
    "isl_map_lexmin_pw_multi_aff",
    Take("Map"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_map_lexmax_pw_multi_aff = ISLFunction.create(
    "isl_map_lexmax_pw_multi_aff",
    Take("Map"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)
