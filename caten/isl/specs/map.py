from __future__ import annotations

from ctypes import c_int, c_void_p
from typing import TYPE_CHECKING

from ..ffi import FfiPointer, load_libisl
from ..func import ISLFunction
from ..obj import ISLObject
from ..qualifier import Give, Keep, Null, Param, Take
from .aff import Aff, MultiAff, PwAff, PwMultiAff
from .basic_map import BasicMap
from .constraint import Constraint
from .context import Context
from .id import Id
from .map_list import MapList
from .set import Set
from .space import Space
from .val import Val

if TYPE_CHECKING:
    from .union_map import UnionMap

_lib = load_libisl()


class Map(ISLObject):
    """Wrapper around ``isl_map``."""

    __slots__ = ()

    def __init__(self, handle_or_spec: str | FfiPointer) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_map_read_from_str(handle_or_spec, return_raw_pointer=True)
        else:
            handle = handle_or_spec
        super().__init__(handle)

    @classmethod
    def from_str(cls, spec: str) -> "Map":
        return _isl_map_read_from_str(spec)

    @classmethod
    def empty(cls, space: Space) -> "Map":
        return _isl_map_empty(space)

    @classmethod
    def universe(cls, space: Space) -> "Map":
        return _isl_map_universe(space)

    @classmethod
    def from_basic_map(cls, bmap: BasicMap) -> "Map":
        return _isl_map_from_basic_map(bmap)

    def copy_handle(self) -> FfiPointer:
        return _isl_map_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_map_free(handle)

    # queries
    def dim(self, dim_type: int) -> int:
        return _isl_map_dim(self, dim_type)

    def is_empty(self) -> bool:
        return _isl_map_is_empty(self)

    def is_equal(self, other: "Map") -> bool:
        return _isl_map_is_equal(self, other)

    # transforms
    def add_constraint(self, constraint: Constraint) -> "Map":
        return _isl_map_add_constraint(self, constraint)

    def project_out(self, dim_type: int, first: int, n: int) -> "Map":
        return _isl_map_project_out(self, dim_type, first, n)

    def remove_divs(self) -> "Map":
        return _isl_map_remove_divs(self)

    def remove_unknown_divs(self) -> "Map":
        return _isl_map_remove_unknown_divs(self)

    def add_dims(self, dim_type: int, n: int) -> "Map":
        return _isl_map_add_dims(self, dim_type, n)

    def insert_dims(self, dim_type: int, first: int, n: int) -> "Map":
        return _isl_map_insert_dims(self, dim_type, first, n)

    def move_dims(self, dst_type: int, dst_pos: int, src_type: int, src_pos: int, n: int) -> "Map":
        return _isl_map_move_dims(self, dst_type, dst_pos, src_type, src_pos, n)

    def align_params(self, space: Space) -> "Map":
        return _isl_map_align_params(self, space)

    def drop_unused_params(self) -> "Map":
        return _isl_map_drop_unused_params(self)

    def __str__(self) -> str:  # pragma: no cover
        return _isl_map_to_str(self)

    def __repr__(self) -> str:  # pragma: no cover
        return f"Map({self.__str__()})"

    # basic queries
    def get_space(self) -> Space:
        return _isl_map_get_space(self)

    def domain_tuple_dim(self) -> int:
        return _isl_map_domain_tuple_dim(self)

    def range_tuple_dim(self) -> int:
        return _isl_map_range_tuple_dim(self)

    # set/has/get tuple/dim ids
    def set_dim_id(self, dim_type: int, pos: int, id: "Id") -> "Map":
        return _isl_map_set_dim_id(self, dim_type, pos, id)

    def has_dim_id(self, dim_type: int, pos: int) -> bool:
        return _isl_map_has_dim_id(self, dim_type, pos)

    def get_dim_id(self, dim_type: int, pos: int) -> Id:
        return _isl_map_get_dim_id(self, dim_type, pos)

    def has_dim_name(self, dim_type: int, pos: int) -> bool:
        return _isl_map_has_dim_name(self, dim_type, pos)

    def get_dim_name(self, dim_type: int, pos: int) -> str:
        return _isl_map_get_dim_name(self, dim_type, pos)

    def find_dim_by_id(self, id: "Id") -> int:
        return _isl_map_find_dim_by_id(self, id)

    def find_dim_by_name(self, dim_type: int, name: str) -> int:
        return _isl_map_find_dim_by_name(self, dim_type, name)

    def set_domain_tuple_id(self, id: "Id") -> "Map":
        return _isl_map_set_domain_tuple_id(self, id)

    def set_range_tuple_id(self, id: "Id") -> "Map":
        return _isl_map_set_range_tuple_id(self, id)

    def set_tuple_id(self, dim_type: int, id: "Id") -> "Map":
        return _isl_map_set_tuple_id(self, dim_type, id)

    def reset_tuple_id(self, dim_type: int) -> "Map":
        return _isl_map_reset_tuple_id(self, dim_type)

    def has_domain_tuple_id(self) -> bool:
        return _isl_map_has_domain_tuple_id(self)

    def has_range_tuple_id(self) -> bool:
        return _isl_map_has_range_tuple_id(self)

    def has_tuple_id(self, dim_type: int) -> bool:
        return _isl_map_has_tuple_id(self, dim_type)

    def get_domain_tuple_id(self) -> "Id":
        return _isl_map_get_domain_tuple_id(self)

    def get_range_tuple_id(self) -> "Id":
        return _isl_map_get_range_tuple_id(self)

    def get_tuple_id(self, dim_type: int) -> "Id":
        return _isl_map_get_tuple_id(self, dim_type)

    # set/has tuple names
    def set_domain_tuple_name(self, name: str) -> "Map":
        raise NotImplementedError("isl_map_set_domain_tuple_name not available in linked libisl")

    def set_range_tuple_name(self, name: str) -> "Map":
        raise NotImplementedError("isl_map_set_range_tuple_name not available in linked libisl")

    def set_tuple_name(self, dim_type: int, name: str) -> "Map":
        return _isl_map_set_tuple_name(self, dim_type, name)

    def get_domain_tuple_name(self) -> str:
        raise NotImplementedError("isl_map_get_domain_tuple_name not available in linked libisl")

    def get_range_tuple_name(self) -> str:
        raise NotImplementedError("isl_map_get_range_tuple_name not available in linked libisl")

    def get_tuple_name(self, dim_type: int) -> str:
        return _isl_map_get_tuple_name(self, dim_type)

    # domain / range
    def domain(self) -> Set:
        return _isl_map_domain(self)

    def range(self) -> Set:
        return _isl_map_range(self)

    def intersect(self, other: "Map") -> "Map":
        return _isl_map_intersect(self, other)

    def union(self, other: "Map") -> "Map":
        return _isl_map_union(self, other)

    def subtract(self, other: "Map") -> "Map":
        return _isl_map_subtract(self, other)

    def subtract_domain(self, dom: Set) -> "Map":
        return _isl_map_subtract_domain(self, dom)

    def subtract_range(self, rng: Set) -> "Map":
        return _isl_map_subtract_range(self, rng)

    def intersect_domain(self, set_: Set) -> "Map":
        return _isl_map_intersect_domain(self, set_)

    def intersect_range(self, set_: Set) -> "Map":
        return _isl_map_intersect_range(self, set_)

    def intersect_params(self, set_: Set) -> "Map":
        return _isl_map_intersect_params(self, set_)

    def intersect_domain_factor_domain(self) -> "Map":
        return _isl_map_intersect_domain_factor_domain(self)

    def intersect_domain_factor_range(self) -> "Map":
        return _isl_map_intersect_domain_factor_range(self)

    def intersect_range_factor_domain(self) -> "Map":
        return _isl_map_intersect_range_factor_domain(self)

    def intersect_range_factor_range(self) -> "Map":
        return _isl_map_intersect_range_factor_range(self)

    def intersect_domain_wrapped_domain(self, set_: Set) -> "Map":
        return _isl_map_intersect_domain_wrapped_domain(self, set_)

    def intersect_range_wrapped_domain(self, set_: Set) -> "Map":
        return _isl_map_intersect_range_wrapped_domain(self, set_)

    # extras
    def has_tuple_name(self) -> bool:
        return _isl_map_has_tuple_name(self)

    def reset_user(self) -> "Map":
        return _isl_map_reset_user(self)

    def nat_universe(self) -> "Map":
        return _isl_map_nat_universe(self)

    def identity(self) -> "Map":
        return _isl_map_identity(self)

    def negate(self) -> "Map":
        return _isl_map_neg(self)

    def oppose(self, type1: int, pos1: int, type2: int, pos2: int) -> "Map":
        return _isl_map_oppose(self, type1, pos1, type2, pos2)

    def lex_lt(self, other: "Map") -> "Map":
        return _isl_map_lex_lt(self, other)

    def lex_le(self, other: "Map") -> "Map":
        return _isl_map_lex_le(self, other)

    def lex_gt(self, other: "Map") -> "Map":
        return _isl_map_lex_gt(self, other)

    def lex_ge(self, other: "Map") -> "Map":
        return _isl_map_lex_ge(self, other)

    def lex_lt_first(self, other: "Map") -> "Map":
        return _isl_map_lex_lt_first(self, other)

    def lex_le_first(self, other: "Map") -> "Map":
        return _isl_map_lex_le_first(self, other)

    def lex_gt_first(self, other: "Map") -> "Map":
        return _isl_map_lex_gt_first(self, other)

    def lex_ge_first(self, other: "Map") -> "Map":
        return _isl_map_lex_ge_first(self, other)

    def lexmin(self) -> "Map":
        return _isl_map_lexmin(self)

    def lexmax(self) -> "Map":
        return _isl_map_lexmax(self)

    def lexmin_pw_multi_aff(self) -> PwMultiAff:
        return _isl_map_lexmin_pw_multi_aff(self)

    def lexmax_pw_multi_aff(self) -> PwMultiAff:
        return _isl_map_lexmax_pw_multi_aff(self)

    @classmethod
    def from_aff(cls, aff: "Aff") -> "Map":
        return _isl_map_from_aff(aff)

    @classmethod
    def from_multi_aff(cls, maff: "MultiAff") -> "Map":
        return _isl_map_from_multi_aff(maff)

    @classmethod
    def from_pw_aff(cls, paff: "PwAff") -> "Map":
        return _isl_map_from_pw_aff(paff)

    @classmethod
    def from_pw_multi_aff(cls, pmaff: "PwMultiAff") -> "Map":
        return _isl_map_from_pw_multi_aff(pmaff)

    @classmethod
    # MultiPwAff not wrapped; keep placeholder

    def compute_divs(self) -> "Map":
        return _isl_map_compute_divs(self)

    def coalesce(self) -> "Map":
        return _isl_map_coalesce(self)

    def detect_equalities(self) -> "Map":
        return _isl_map_detect_equalities(self)

    def remove_redundancies(self) -> "Map":
        return _isl_map_remove_redundancies(self)

    def affine_hull(self) -> BasicMap:
        return _isl_map_affine_hull(self)

    def convex_hull(self) -> BasicMap:
        return _isl_map_convex_hull(self)

    def polyhedral_hull(self) -> BasicMap:
        return _isl_map_polyhedral_hull(self)

    def plain_unshifted_simple_hull(self) -> BasicMap:
        return _isl_map_plain_unshifted_simple_hull(self)

    def simple_hull(self) -> BasicMap:
        return _isl_map_simple_hull(self)

    def unshifted_simple_hull(self) -> BasicMap:
        return _isl_map_unshifted_simple_hull(self)

    def unshifted_simple_hull_from_map_list(self, map_list: MapList) -> BasicMap:
        return _isl_map_unshifted_simple_hull_from_map_list(self, map_list)

    def remove_divs_involving_dims(self, dim_type: int, first: int, n: int) -> "Map":
        return _isl_map_remove_divs_involving_dims(self, dim_type, first, n)

    def drop_constraints_involving_dims(self, dim_type: int, first: int, n: int) -> "Map":
        return _isl_map_drop_constraints_involving_dims(self, dim_type, first, n)

    def drop_constraints_not_involving_dims(self, dim_type: int, first: int, n: int) -> "Map":
        return _isl_map_drop_constraints_not_involving_dims(self, dim_type, first, n)

    def make_disjoint(self) -> "Map":
        return _isl_map_make_disjoint(self)

    def n_basic_map(self) -> int:
        return _isl_map_n_basic_map(self)

    def get_basic_map_list(self) -> "BasicMap":
        return _isl_map_get_basic_map_list(self)

    @classmethod
    def read_from_file(cls, ctx: "Context", file_ptr: int) -> "Map":
        return _isl_map_read_from_file(ctx, file_ptr)

    # predicates
    def plain_is_empty(self) -> bool:
        return _isl_map_plain_is_empty(self)

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

    def plain_get_val_if_fixed(self, dim_type: int, pos: int) -> "Set":
        return _isl_map_plain_get_val_if_fixed(self, dim_type, pos)

    def get_range_stride_info(self, pos: int) -> object:
        return _isl_map_get_range_stride_info(self, pos)

    def get_range_lattice_tile(self) -> object:
        return _isl_map_get_range_lattice_tile(self)

    def get_range_simple_fixed_box_hull(self) -> object:
        return _isl_map_get_range_simple_fixed_box_hull(self)

    def involves_dims(self, dim_type: int, first: int, n: int) -> bool:
        return _isl_map_involves_dims(self, dim_type, first, n)

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

    def plain_is_equal(self, other: "Map") -> bool:
        return _isl_map_plain_is_equal(self, other)

    def is_disjoint(self, other: "Map") -> bool:
        return _isl_map_is_disjoint(self, other)

    def is_subset(self, other: "Map") -> bool:
        return _isl_map_is_subset(self, other)

    def is_strict_subset(self, other: "Map") -> bool:
        return _isl_map_is_strict_subset(self, other)

    def complement(self) -> "Map":
        return _isl_map_complement(self)

    def reverse(self) -> "Map":
        return _isl_map_reverse(self)

    def domain_reverse(self) -> "Map":
        return _isl_map_domain_reverse(self)

    def range_reverse(self) -> "Map":
        return _isl_map_range_reverse(self)

    def zip(self) -> "Map":
        return _isl_map_zip(self)

    def wrap(self) -> "Map":
        return _isl_map_wrap(self)

    def flatten(self) -> "Map":
        return _isl_map_flatten(self)

    def flatten_domain(self) -> "Map":
        return _isl_map_flatten_domain(self)

    def flatten_range(self) -> "Map":
        return _isl_map_flatten_range(self)

    def project_out_param_id(self, id: Id) -> "Map":
        return _isl_map_project_out_param_id(self, id)

    def project_out_all_params(self) -> "Map":
        return _isl_map_project_out_all_params(self)

    def params(self) -> Set:
        return _isl_map_params(self)

    def domain_map(self) -> "Map":
        return _isl_map_domain_map(self)

    def range_map(self) -> "Map":
        return _isl_map_range_map(self)

    def range_curry(self) -> "Map":
        return _isl_map_range_curry(self)

    def curry(self) -> "Map":
        return _isl_map_curry(self)

    def uncurry(self) -> "Map":
        return _isl_map_uncurry(self)

    def transitive_closure(self) -> "Map":
        return _isl_map_transitive_closure(self, None)

    def reaching_path_lengths(self) -> "Map":
        return _isl_map_reaching_path_lengths(self, None)

    def eliminate(self, dim_type: int, first: int, n: int) -> "Map":
        return _isl_map_eliminate(self, dim_type, first, n)

    @classmethod
    def from_domain(cls, set_: Set) -> "Map":
        return _isl_map_from_domain(set_)

    @classmethod
    def from_range(cls, set_: Set) -> "Map":
        return _isl_map_from_range(set_)

    def domain_product(self, other: "Map") -> "Map":
        return _isl_map_domain_product(self, other)

    def range_product(self, other: "Map") -> "Map":
        return _isl_map_range_product(self, other)

    def product(self, other: "Map") -> "Map":
        return _isl_map_product(self, other)

    def flat_domain_product(self, other: "Map") -> "Map":
        return _isl_map_flat_domain_product(self, other)

    def flat_range_product(self, other: "Map") -> "Map":
        return _isl_map_flat_range_product(self, other)

    def flat_product(self, other: "Map") -> "Map":
        return _isl_map_flat_product(self, other)

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

    def fix_si(self, dim_type: int, pos: int, value: int) -> "Map":
        return _isl_map_fix_si(self, dim_type, pos, value)

    def fix_val(self, dim_type: int, pos: int, value: Val) -> "Map":
        return _isl_map_fix_val(self, dim_type, pos, value)

    def fixed_power_val(self, exp: Val) -> "Map":
        return _isl_map_fixed_power_val(self, exp)

    def power(self) -> "Map":
        return _isl_map_power(self, None)

    def lower_bound_si(self, dim_type: int, pos: int, value: int) -> "Map":
        return _isl_map_lower_bound_si(self, dim_type, pos, value)

    def upper_bound_si(self, dim_type: int, pos: int, value: int) -> "Map":
        return _isl_map_upper_bound_si(self, dim_type, pos, value)

    def lower_bound_val(self, dim_type: int, pos: int, value: Val) -> "Map":
        return _isl_map_lower_bound_val(self, dim_type, pos, value)

    def upper_bound_val(self, dim_type: int, pos: int, value: Val) -> "Map":
        return _isl_map_upper_bound_val(self, dim_type, pos, value)

    def dim_min(self, pos: int) -> PwAff:
        return _isl_map_dim_min(self, pos)

    def dim_max(self, pos: int) -> PwAff:
        return _isl_map_dim_max(self, pos)

    def lower_bound_multi_pw_aff(self, _func: object) -> "Map":
        raise NotImplementedError("isl_map_lower_bound_multi_pw_aff requires MultiPwAff binding")

    def upper_bound_multi_pw_aff(self, _func: object) -> "Map":
        raise NotImplementedError("isl_map_upper_bound_multi_pw_aff requires MultiPwAff binding")

    def min_multi_pw_aff(self) -> object:
        raise NotImplementedError("isl_map_min_multi_pw_aff requires MultiPwAff binding")

    def max_multi_pw_aff(self) -> object:
        raise NotImplementedError("isl_map_max_multi_pw_aff requires MultiPwAff binding")

    def partial_lexmax(self, dom: Set) -> "Map":
        return _isl_map_partial_lexmax(self, dom, None)

    def partial_lexmin(self, dom: Set) -> "Map":
        return _isl_map_partial_lexmin(self, dom, None)

    def to_union_map(self) -> "UnionMap":  # local import avoids circular
        from .union_map import UnionMap  # local import to avoid circular at module load
        ptr = _isl_map_to_union_map(self, return_raw_pointer=True)
        return UnionMap(ptr)

    @classmethod
    def from_union_map(cls, umap: object) -> "Map":  # NotImplemented placeholder
        return _isl_map_from_union_map(umap)

    @classmethod
    def from_multi_pw_aff(cls, _mpa: object) -> "Map":
        return _isl_map_from_multi_pw_aff(_mpa)

    @classmethod
    def list_read_from_str(cls, ctx: Context, spec: str) -> MapList:
        return _isl_map_list_read_from_str(ctx, spec)

    def apply_pw_qpolynomial_fold(self, _pwf: object) -> object:
        return _isl_map_apply_pw_qpolynomial_fold(self, _pwf, None)

    def preimage_domain_multi_aff(self, ma: MultiAff) -> "Map":
        return _isl_map_preimage_domain_multi_aff(self, ma)

    def preimage_range_multi_aff(self, ma: MultiAff) -> "Map":
        return _isl_map_preimage_range_multi_aff(self, ma)

    def preimage_domain_pw_multi_aff(self, pma: PwMultiAff) -> "Map":
        return _isl_map_preimage_domain_pw_multi_aff(self, pma)

    def preimage_range_pw_multi_aff(self, pma: PwMultiAff) -> "Map":
        return _isl_map_preimage_range_pw_multi_aff(self, pma)

    def preimage_domain_multi_pw_aff(self, _mpa: object) -> "Map":
        return _isl_map_preimage_domain_multi_pw_aff(self, _mpa)

    def apply_domain(self, other: "Map") -> "Map":
        return _isl_map_apply_domain(self, other)

    def apply_range(self, other: "Map") -> "Map":
        return _isl_map_apply_range(self, other)

    def foreach_basic_map(self, fn: object) -> None:
        raise NotImplementedError("callback bridge not implemented")

    def bind_domain(self, multi_id: object) -> "Set":
        raise NotImplementedError("multi_id wrapper not implemented")

    def bind_range(self, multi_id: object) -> "Set":
        raise NotImplementedError("multi_id wrapper not implemented")

    def project_out_param_id_list(self, id_list: object) -> "Map":
        raise NotImplementedError("id_list wrapper not implemented")

    def equate(self, t1: int, pos1: int, t2: int, pos2: int) -> "Map":
        return _isl_map_equate(self, t1, pos1, t2, pos2)

    def order_le(self, t1: int, pos1: int, t2: int, pos2: int) -> "Map":
        return _isl_map_order_le(self, t1, pos1, t2, pos2)

    def order_ge(self, t1: int, pos1: int, t2: int, pos2: int) -> "Map":
        return _isl_map_order_ge(self, t1, pos1, t2, pos2)

    def order_lt(self, t1: int, pos1: int, t2: int, pos2: int) -> "Map":
        return _isl_map_order_lt(self, t1, pos1, t2, pos2)

    def order_gt(self, t1: int, pos1: int, t2: int, pos2: int) -> "Map":
        return _isl_map_order_gt(self, t1, pos1, t2, pos2)

    def deltas(self) -> Set:
        return _isl_map_deltas(self)

    def deltas_map(self) -> "Map":
        return _isl_map_deltas_map(self)

    def gist(self, context: "Map") -> "Map":
        return _isl_map_gist(self, context)

    def gist_params(self, context: Set) -> "Map":
        return _isl_map_gist_params(self, context)

    def gist_domain(self, context: Set) -> "Map":
        return _isl_map_gist_domain(self, context)

    def gist_range(self, context: Set) -> "Map":
        return _isl_map_gist_range(self, context)

    def sum(self, other: "Map") -> "Map":
        return _isl_map_sum(self, other)

    def sample(self) -> BasicMap:
        return _isl_map_sample(self)

    def as_pw_multi_aff(self) -> PwMultiAff:
        return _isl_map_as_pw_multi_aff(self)


_isl_map_read_from_str = ISLFunction.create(
    _lib.isl_map_read_from_str,
    Context(),
    Param(str),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_copy = ISLFunction.create(
    _lib.isl_map_copy,
    Keep(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_free = ISLFunction.create(
    _lib.isl_map_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

_isl_map_empty = ISLFunction.create(
    _lib.isl_map_empty,
    Take(Space),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_universe = ISLFunction.create(
    _lib.isl_map_universe,
    Take(Space),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_from_basic_map = ISLFunction.create(
    _lib.isl_map_from_basic_map,
    Take(BasicMap),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_dim = ISLFunction.create(
    _lib.isl_map_dim,
    Keep(Map),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_map_is_empty = ISLFunction.create(
    _lib.isl_map_is_empty,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_equal = ISLFunction.create(
    _lib.isl_map_is_equal,
    Keep(Map),
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_add_constraint = ISLFunction.create(
    _lib.isl_map_add_constraint,
    Take(Map),
    Take(Constraint),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_intersect = ISLFunction.create(
    _lib.isl_map_intersect,
    Take(Map),
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_intersect_domain = ISLFunction.create(
    _lib.isl_map_intersect_domain,
    Take(Map),
    Take(Set),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_intersect_range = ISLFunction.create(
    _lib.isl_map_intersect_range,
    Take(Map),
    Take(Set),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_intersect_params = ISLFunction.create(
    _lib.isl_map_intersect_params,
    Take(Map),
    Take(Set),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_intersect_domain_factor_domain = ISLFunction.create(
    _lib.isl_map_intersect_domain_factor_domain,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_intersect_domain_factor_range = ISLFunction.create(
    _lib.isl_map_intersect_domain_factor_range,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_intersect_range_factor_domain = ISLFunction.create(
    _lib.isl_map_intersect_range_factor_domain,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_intersect_range_factor_range = ISLFunction.create(
    _lib.isl_map_intersect_range_factor_range,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_intersect_domain_wrapped_domain = ISLFunction.create(
    _lib.isl_map_intersect_domain_wrapped_domain,
    Take(Map),
    Take(Set),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_intersect_range_wrapped_domain = ISLFunction.create(
    _lib.isl_map_intersect_range_wrapped_domain,
    Take(Map),
    Take(Set),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_project_out = ISLFunction.create(
    _lib.isl_map_project_out,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_remove_divs = ISLFunction.create(
    _lib.isl_map_remove_divs,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_remove_unknown_divs = ISLFunction.create(
    _lib.isl_map_remove_unknown_divs,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_add_dims = ISLFunction.create(
    _lib.isl_map_add_dims,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_insert_dims = ISLFunction.create(
    _lib.isl_map_insert_dims,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_move_dims = ISLFunction.create(
    _lib.isl_map_move_dims,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_align_params = ISLFunction.create(
    _lib.isl_map_align_params,
    Take(Map),
    Take(Space),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_drop_unused_params = ISLFunction.create(
    _lib.isl_map_drop_unused_params,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_to_str = ISLFunction.create(
    _lib.isl_map_to_str,
    Keep(Map),
    return_=Param(str),
    lib=_lib,
)

_isl_map_has_tuple_name = ISLFunction.create(
    _lib.isl_map_has_tuple_name,
    Keep(Map),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_set_tuple_name = ISLFunction.create(
    _lib.isl_map_set_tuple_name,
    Take(Map),
    Param(int, ctype=c_int),
    Param(str),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_get_tuple_name = ISLFunction.create(
    _lib.isl_map_get_tuple_name,
    Keep(Map),
    Param(int, ctype=c_int),
    return_=Param(str),
    lib=_lib,
)

_isl_map_reset_user = ISLFunction.create(
    _lib.isl_map_reset_user,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_nat_universe = ISLFunction.create(
    _lib.isl_map_nat_universe,
    Take(Space),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_identity = ISLFunction.create(
    _lib.isl_map_identity,
    Take(Space),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_neg = ISLFunction.create(
    _lib.isl_map_neg,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_oppose = ISLFunction.create(
    _lib.isl_map_oppose,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_lex_lt = ISLFunction.create(
    _lib.isl_map_lex_lt,
    Take(Space),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_lex_le = ISLFunction.create(
    _lib.isl_map_lex_le,
    Take(Space),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_lex_gt = ISLFunction.create(
    _lib.isl_map_lex_gt,
    Take(Space),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_lex_ge = ISLFunction.create(
    _lib.isl_map_lex_ge,
    Take(Space),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_lex_lt_first = ISLFunction.create(
    _lib.isl_map_lex_lt_first,
    Take(Space),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_lex_le_first = ISLFunction.create(
    _lib.isl_map_lex_le_first,
    Take(Space),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_lex_gt_first = ISLFunction.create(
    _lib.isl_map_lex_gt_first,
    Take(Space),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_lex_ge_first = ISLFunction.create(
    _lib.isl_map_lex_ge_first,
    Take(Space),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_lexmin = ISLFunction.create(
    _lib.isl_map_lexmin,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_lexmax = ISLFunction.create(
    _lib.isl_map_lexmax,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_lexmin_pw_multi_aff = ISLFunction.create(
    _lib.isl_map_lexmin_pw_multi_aff,
    Take(Map),
    return_=Give(PwMultiAff),
    lib=_lib,
)

_isl_map_lexmax_pw_multi_aff = ISLFunction.create(
    _lib.isl_map_lexmax_pw_multi_aff,
    Take(Map),
    return_=Give(PwMultiAff),
    lib=_lib,
)

_isl_map_from_aff = ISLFunction.create(
    _lib.isl_map_from_aff,
    Take(Aff),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_from_multi_aff = ISLFunction.create(
    _lib.isl_map_from_multi_aff,
    Take(MultiAff),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_from_pw_aff = ISLFunction.create(
    _lib.isl_map_from_pw_aff,
    Take(PwAff),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_from_pw_multi_aff = ISLFunction.create(
    _lib.isl_map_from_pw_multi_aff,
    Take(PwMultiAff),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_compute_divs = ISLFunction.create(
    _lib.isl_map_compute_divs,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_coalesce = ISLFunction.create(
    _lib.isl_map_coalesce,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_detect_equalities = ISLFunction.create(
    _lib.isl_map_detect_equalities,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_remove_redundancies = ISLFunction.create(
    _lib.isl_map_remove_redundancies,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_affine_hull = ISLFunction.create(
    _lib.isl_map_affine_hull,
    Take(Map),
    return_=Give(BasicMap),
    lib=_lib,
)

_isl_map_convex_hull = ISLFunction.create(
    _lib.isl_map_convex_hull,
    Take(Map),
    return_=Give(BasicMap),
    lib=_lib,
)

_isl_map_polyhedral_hull = ISLFunction.create(
    _lib.isl_map_polyhedral_hull,
    Take(Map),
    return_=Give(BasicMap),
    lib=_lib,
)

_isl_map_plain_unshifted_simple_hull = ISLFunction.create(
    _lib.isl_map_plain_unshifted_simple_hull,
    Take(Map),
    return_=Give(BasicMap),
    lib=_lib,
)

_isl_map_simple_hull = ISLFunction.create(
    _lib.isl_map_simple_hull,
    Take(Map),
    return_=Give(BasicMap),
    lib=_lib,
)

_isl_map_unshifted_simple_hull = ISLFunction.create(
    _lib.isl_map_unshifted_simple_hull,
    Take(Map),
    return_=Give(BasicMap),
    lib=_lib,
)

_isl_map_unshifted_simple_hull_from_map_list = ISLFunction.create(
    _lib.isl_map_unshifted_simple_hull_from_map_list,
    Take(Map),
    Take(MapList),
    return_=Give(BasicMap),
    lib=_lib,
)

_isl_map_remove_divs_involving_dims = ISLFunction.create(
    _lib.isl_map_remove_divs_involving_dims,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_drop_constraints_involving_dims = ISLFunction.create(
    _lib.isl_map_drop_constraints_involving_dims,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_drop_constraints_not_involving_dims = ISLFunction.create(
    _lib.isl_map_drop_constraints_not_involving_dims,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_make_disjoint = ISLFunction.create(
    _lib.isl_map_make_disjoint,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_n_basic_map = ISLFunction.create(
    _lib.isl_map_n_basic_map,
    Keep(Map),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_map_get_basic_map_list = ISLFunction.create(
    _lib.isl_map_get_basic_map_list,
    Keep(Map),
    return_=Give(BasicMap),
    lib=_lib,
)

_isl_map_read_from_file = ISLFunction.create(
    _lib.isl_map_read_from_file,
    Context(),
    Param(None, ctype=c_void_p),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_plain_is_empty = ISLFunction.create(
    _lib.isl_map_plain_is_empty,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_plain_is_universe = ISLFunction.create(
    _lib.isl_map_plain_is_universe,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_plain_is_single_valued = ISLFunction.create(
    _lib.isl_map_plain_is_single_valued,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_single_valued = ISLFunction.create(
    _lib.isl_map_is_single_valued,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_plain_is_injective = ISLFunction.create(
    _lib.isl_map_plain_is_injective,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_injective = ISLFunction.create(
    _lib.isl_map_is_injective,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_bijective = ISLFunction.create(
    _lib.isl_map_is_bijective,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_identity = ISLFunction.create(
    _lib.isl_map_is_identity,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_plain_get_val_if_fixed = ISLFunction.create(
    _lib.isl_map_plain_get_val_if_fixed,
    Keep(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Param(None, ctype=c_void_p),
    lib=_lib,
)

_isl_map_get_range_stride_info = ISLFunction.create(
    _lib.isl_map_get_range_stride_info,
    Keep(Map),
    Param(int, ctype=c_int),
    return_=Param(None, ctype=c_void_p),
    lib=_lib,
)

_isl_map_get_range_lattice_tile = ISLFunction.create(
    _lib.isl_map_get_range_lattice_tile,
    Keep(Map),
    return_=Param(None, ctype=c_void_p),
    lib=_lib,
)

_isl_map_get_range_simple_fixed_box_hull = ISLFunction.create(
    _lib.isl_map_get_range_simple_fixed_box_hull,
    Keep(Map),
    return_=Param(None, ctype=c_void_p),
    lib=_lib,
)

_isl_map_involves_dims = ISLFunction.create(
    _lib.isl_map_involves_dims,
    Keep(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_domain_is_wrapping = ISLFunction.create(
    _lib.isl_map_domain_is_wrapping,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_range_is_wrapping = ISLFunction.create(
    _lib.isl_map_range_is_wrapping,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_product = ISLFunction.create(
    _lib.isl_map_is_product,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_can_zip = ISLFunction.create(
    _lib.isl_map_can_zip,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_can_curry = ISLFunction.create(
    _lib.isl_map_can_curry,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_can_uncurry = ISLFunction.create(
    _lib.isl_map_can_uncurry,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_can_range_curry = ISLFunction.create(
    _lib.isl_map_can_range_curry,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_plain_is_equal = ISLFunction.create(
    _lib.isl_map_plain_is_equal,
    Keep(Map),
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_disjoint = ISLFunction.create(
    _lib.isl_map_is_disjoint,
    Keep(Map),
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_subset = ISLFunction.create(
    _lib.isl_map_is_subset,
    Keep(Map),
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_strict_subset = ISLFunction.create(
    _lib.isl_map_is_strict_subset,
    Keep(Map),
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_complement = ISLFunction.create(
    _lib.isl_map_complement,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_reverse = ISLFunction.create(
    _lib.isl_map_reverse,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_domain_reverse = ISLFunction.create(
    _lib.isl_map_domain_reverse,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_range_reverse = ISLFunction.create(
    _lib.isl_map_range_reverse,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_zip = ISLFunction.create(
    _lib.isl_map_zip,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_wrap = ISLFunction.create(
    _lib.isl_map_wrap,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_flatten = ISLFunction.create(
    _lib.isl_map_flatten,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_flatten_domain = ISLFunction.create(
    _lib.isl_map_flatten_domain,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_flatten_range = ISLFunction.create(
    _lib.isl_map_flatten_range,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_to_union_map = ISLFunction.create(
    _lib.isl_map_to_union_map,
    Take(Map),
    return_=Param(None, ctype=c_void_p),
    lib=_lib,
)

_isl_map_from_union_map = ISLFunction.create(
    _lib.isl_map_from_union_map,
    Param(None, ctype=c_void_p),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_project_out_param_id = ISLFunction.create(
    _lib.isl_map_project_out_param_id,
    Take(Map),
    Take(Id),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_project_out_all_params = ISLFunction.create(
    _lib.isl_map_project_out_all_params,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_params = ISLFunction.create(
    _lib.isl_map_params,
    Take(Map),
    return_=Give(Set),
    lib=_lib,
)

_isl_map_domain_map = ISLFunction.create(
    _lib.isl_map_domain_map,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_range_map = ISLFunction.create(
    _lib.isl_map_range_map,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_apply_domain = ISLFunction.create(
    _lib.isl_map_apply_domain,
    Take(Map),
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_apply_range = ISLFunction.create(
    _lib.isl_map_apply_range,
    Take(Map),
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_range_curry = ISLFunction.create(
    _lib.isl_map_range_curry,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_curry = ISLFunction.create(
    _lib.isl_map_curry,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_uncurry = ISLFunction.create(
    _lib.isl_map_uncurry,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_transitive_closure = ISLFunction.create(
    _lib.isl_map_transitive_closure,
    Take(Map),
    Param(None, ctype=c_void_p),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_reaching_path_lengths = ISLFunction.create(
    _lib.isl_map_reaching_path_lengths,
    Take(Map),
    Param(None, ctype=c_void_p),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_eliminate = ISLFunction.create(
    _lib.isl_map_eliminate,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_from_domain = ISLFunction.create(
    _lib.isl_map_from_domain,
    Take(Set),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_from_range = ISLFunction.create(
    _lib.isl_map_from_range,
    Take(Set),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_domain_product = ISLFunction.create(
    _lib.isl_map_domain_product,
    Take(Map),
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_range_product = ISLFunction.create(
    _lib.isl_map_range_product,
    Take(Map),
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_product = ISLFunction.create(
    _lib.isl_map_product,
    Take(Map),
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_flat_domain_product = ISLFunction.create(
    _lib.isl_map_flat_domain_product,
    Take(Map),
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_flat_range_product = ISLFunction.create(
    _lib.isl_map_flat_range_product,
    Take(Map),
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_flat_product = ISLFunction.create(
    _lib.isl_map_flat_product,
    Take(Map),
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_factor_domain = ISLFunction.create(
    _lib.isl_map_factor_domain,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_factor_range = ISLFunction.create(
    _lib.isl_map_factor_range,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_domain_factor_domain = ISLFunction.create(
    _lib.isl_map_domain_factor_domain,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_domain_factor_range = ISLFunction.create(
    _lib.isl_map_domain_factor_range,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_range_factor_domain = ISLFunction.create(
    _lib.isl_map_range_factor_domain,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_range_factor_range = ISLFunction.create(
    _lib.isl_map_range_factor_range,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_fix_si = ISLFunction.create(
    _lib.isl_map_fix_si,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_fix_val = ISLFunction.create(
    _lib.isl_map_fix_val,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Take(Val),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_fixed_power_val = ISLFunction.create(
    _lib.isl_map_fixed_power_val,
    Take(Map),
    Take(Val),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_power = ISLFunction.create(
    _lib.isl_map_power,
    Take(Map),
    Param(None, ctype=c_void_p),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_lower_bound_si = ISLFunction.create(
    _lib.isl_map_lower_bound_si,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_upper_bound_si = ISLFunction.create(
    _lib.isl_map_upper_bound_si,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_lower_bound_val = ISLFunction.create(
    _lib.isl_map_lower_bound_val,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Take(Val),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_lower_bound_multi_pw_aff = ISLFunction.create(
    _lib.isl_map_lower_bound_multi_pw_aff,
    Take(Map),
    Param(None, ctype=c_void_p),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_upper_bound_multi_pw_aff = ISLFunction.create(
    _lib.isl_map_upper_bound_multi_pw_aff,
    Take(Map),
    Param(None, ctype=c_void_p),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_upper_bound_val = ISLFunction.create(
    _lib.isl_map_upper_bound_val,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Take(Val),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_dim_min = ISLFunction.create(
    _lib.isl_map_dim_min,
    Take(Map),
    Param(int, ctype=c_int),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_map_dim_max = ISLFunction.create(
    _lib.isl_map_dim_max,
    Take(Map),
    Param(int, ctype=c_int),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_map_foreach_basic_map = ISLFunction.create(
    _lib.isl_map_foreach_basic_map,
    Keep(Map),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_map_bind_domain = ISLFunction.create(
    _lib.isl_map_bind_domain,
    Take(Map),
    Param(None, ctype=c_void_p),
    return_=Give(Set),
    lib=_lib,
)

_isl_map_bind_range = ISLFunction.create(
    _lib.isl_map_bind_range,
    Take(Map),
    Param(None, ctype=c_void_p),
    return_=Give(Set),
    lib=_lib,
)

_isl_map_project_out_param_id_list = ISLFunction.create(
    _lib.isl_map_project_out_param_id_list,
    Take(Map),
    Param(None, ctype=c_void_p),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_equate = ISLFunction.create(
    _lib.isl_map_equate,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_order_le = ISLFunction.create(
    _lib.isl_map_order_le,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_order_ge = ISLFunction.create(
    _lib.isl_map_order_ge,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_order_lt = ISLFunction.create(
    _lib.isl_map_order_lt,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_order_gt = ISLFunction.create(
    _lib.isl_map_order_gt,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_deltas = ISLFunction.create(
    _lib.isl_map_deltas,
    Take(Map),
    return_=Give(Set),
    lib=_lib,
)

_isl_map_deltas_map = ISLFunction.create(
    _lib.isl_map_deltas_map,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)
_isl_map_get_space = ISLFunction.create(
    _lib.isl_map_get_space,
    Keep(Map),
    return_=Give(Space),
    lib=_lib,
)

_isl_map_domain_tuple_dim = ISLFunction.create(
    _lib.isl_map_domain_tuple_dim,
    Keep(Map),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_map_range_tuple_dim = ISLFunction.create(
    _lib.isl_map_range_tuple_dim,
    Keep(Map),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_map_set_dim_id = ISLFunction.create(
    _lib.isl_map_set_dim_id,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Take(Id),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_has_dim_id = ISLFunction.create(
    _lib.isl_map_has_dim_id,
    Keep(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_get_dim_id = ISLFunction.create(
    _lib.isl_map_get_dim_id,
    Keep(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Id),
    lib=_lib,
)

_isl_map_has_dim_name = ISLFunction.create(
    _lib.isl_map_has_dim_name,
    Keep(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_get_dim_name = ISLFunction.create(
    _lib.isl_map_get_dim_name,
    Keep(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Param(str),
    lib=_lib,
)

_isl_map_find_dim_by_id = ISLFunction.create(
    _lib.isl_map_find_dim_by_id,
    Keep(Map),
    Keep(Id),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_map_find_dim_by_name = ISLFunction.create(
    _lib.isl_map_find_dim_by_name,
    Keep(Map),
    Param(int, ctype=c_int),
    Param(str),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_map_set_domain_tuple_id = ISLFunction.create(
    _lib.isl_map_set_domain_tuple_id,
    Take(Map),
    Take(Id),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_set_range_tuple_id = ISLFunction.create(
    _lib.isl_map_set_range_tuple_id,
    Take(Map),
    Take(Id),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_set_tuple_id = ISLFunction.create(
    _lib.isl_map_set_tuple_id,
    Take(Map),
    Param(int, ctype=c_int),
    Take(Id),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_reset_tuple_id = ISLFunction.create(
    _lib.isl_map_reset_tuple_id,
    Take(Map),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_has_domain_tuple_id = ISLFunction.create(
    _lib.isl_map_has_domain_tuple_id,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_has_range_tuple_id = ISLFunction.create(
    _lib.isl_map_has_range_tuple_id,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_has_tuple_id = ISLFunction.create(
    _lib.isl_map_has_tuple_id,
    Keep(Map),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_get_domain_tuple_id = ISLFunction.create(
    _lib.isl_map_get_domain_tuple_id,
    Keep(Map),
    return_=Give(Id),
    lib=_lib,
)

_isl_map_get_range_tuple_id = ISLFunction.create(
    _lib.isl_map_get_range_tuple_id,
    Keep(Map),
    return_=Give(Id),
    lib=_lib,
)

_isl_map_get_tuple_id = ISLFunction.create(
    _lib.isl_map_get_tuple_id,
    Keep(Map),
    Param(int, ctype=c_int),
    return_=Give(Id),
    lib=_lib,
)

_isl_map_domain = ISLFunction.create(
    _lib.isl_map_domain,
    Take(Map),
    return_=Give(Set),
    lib=_lib,
)

_isl_map_range = ISLFunction.create(
    _lib.isl_map_range,
    Take(Map),
    return_=Give(Set),
    lib=_lib,
)

_isl_map_union = ISLFunction.create(
    _lib.isl_map_union,
    Take(Map),
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_subtract = ISLFunction.create(
    _lib.isl_map_subtract,
    Take(Map),
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_subtract_domain = ISLFunction.create(
    _lib.isl_map_subtract_domain,
    Take(Map),
    Take(Set),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_subtract_range = ISLFunction.create(
    _lib.isl_map_subtract_range,
    Take(Map),
    Take(Set),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_remove_divs_involving_dims = ISLFunction.create(
    _lib.isl_map_remove_divs_involving_dims,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_make_disjoint = ISLFunction.create(
    _lib.isl_map_make_disjoint,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_n_basic_map = ISLFunction.create(
    _lib.isl_map_n_basic_map,
    Keep(Map),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_map_get_basic_map_list = ISLFunction.create(
    _lib.isl_map_get_basic_map_list,
    Keep(Map),
    return_=Give(BasicMap),
    lib=_lib,
)

_isl_map_plain_is_empty = ISLFunction.create(
    _lib.isl_map_plain_is_empty,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_plain_is_universe = ISLFunction.create(
    _lib.isl_map_plain_is_universe,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_plain_is_single_valued = ISLFunction.create(
    _lib.isl_map_plain_is_single_valued,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_single_valued = ISLFunction.create(
    _lib.isl_map_is_single_valued,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_plain_is_injective = ISLFunction.create(
    _lib.isl_map_plain_is_injective,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_injective = ISLFunction.create(
    _lib.isl_map_is_injective,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_bijective = ISLFunction.create(
    _lib.isl_map_is_bijective,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_identity = ISLFunction.create(
    _lib.isl_map_is_identity,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_plain_get_val_if_fixed = ISLFunction.create(
    _lib.isl_map_plain_get_val_if_fixed,
    Keep(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Set),  # placeholder; actual type is isl_val*
    lib=_lib,
)

_isl_map_get_range_stride_info = ISLFunction.create(
    _lib.isl_map_get_range_stride_info,
    Keep(Map),
    Param(int, ctype=c_int),
    return_=Param(None, ctype=c_void_p),
    lib=_lib,
)

_isl_map_get_range_lattice_tile = ISLFunction.create(
    _lib.isl_map_get_range_lattice_tile,
    Keep(Map),
    return_=Param(None, ctype=c_void_p),
    lib=_lib,
)

_isl_map_involves_dims = ISLFunction.create(
    _lib.isl_map_involves_dims,
    Keep(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_domain_is_wrapping = ISLFunction.create(
    _lib.isl_map_domain_is_wrapping,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_range_is_wrapping = ISLFunction.create(
    _lib.isl_map_range_is_wrapping,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_product = ISLFunction.create(
    _lib.isl_map_is_product,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_can_zip = ISLFunction.create(
    _lib.isl_map_can_zip,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_can_curry = ISLFunction.create(
    _lib.isl_map_can_curry,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_can_uncurry = ISLFunction.create(
    _lib.isl_map_can_uncurry,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_can_range_curry = ISLFunction.create(
    _lib.isl_map_can_range_curry,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_gist = ISLFunction.create(
    _lib.isl_map_gist,
    Take(Map),
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_gist_params = ISLFunction.create(
    _lib.isl_map_gist_params,
    Take(Map),
    Take(Set),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_gist_domain = ISLFunction.create(
    _lib.isl_map_gist_domain,
    Take(Map),
    Take(Set),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_gist_range = ISLFunction.create(
    _lib.isl_map_gist_range,
    Take(Map),
    Take(Set),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_sum = ISLFunction.create(
    _lib.isl_map_sum,
    Take(Map),
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_sample = ISLFunction.create(
    _lib.isl_map_sample,
    Take(Map),
    return_=Give(BasicMap),
    lib=_lib,
)

_isl_map_as_pw_multi_aff = ISLFunction.create(
    _lib.isl_map_as_pw_multi_aff,
    Take(Map),
    return_=Give(PwMultiAff),
    lib=_lib,
)

# Additional bindings (some rely on yet-to-be-wrapped helper types)

_isl_map_from_multi_pw_aff = ISLFunction.create(
    _lib.isl_map_from_multi_pw_aff,
    Param(None, ctype=c_void_p),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_apply_pw_qpolynomial_fold = ISLFunction.create(
    _lib.isl_map_apply_pw_qpolynomial_fold,
    Take(Map),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(None, ctype=c_void_p),
    lib=_lib,
)

_isl_map_preimage_domain_multi_aff = ISLFunction.create(
    _lib.isl_map_preimage_domain_multi_aff,
    Take(Map),
    Take(MultiAff),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_preimage_range_multi_aff = ISLFunction.create(
    _lib.isl_map_preimage_range_multi_aff,
    Take(Map),
    Take(MultiAff),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_preimage_domain_pw_multi_aff = ISLFunction.create(
    _lib.isl_map_preimage_domain_pw_multi_aff,
    Take(Map),
    Take(PwMultiAff),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_preimage_range_pw_multi_aff = ISLFunction.create(
    _lib.isl_map_preimage_range_pw_multi_aff,
    Take(Map),
    Take(PwMultiAff),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_preimage_domain_multi_pw_aff = ISLFunction.create(
    _lib.isl_map_preimage_domain_multi_pw_aff,
    Take(Map),
    Param(None, ctype=c_void_p),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_min_multi_pw_aff = ISLFunction.create(
    _lib.isl_map_min_multi_pw_aff,
    Take(Map),
    return_=Param(None, ctype=c_void_p),
    lib=_lib,
)

_isl_map_max_multi_pw_aff = ISLFunction.create(
    _lib.isl_map_max_multi_pw_aff,
    Take(Map),
    return_=Param(None, ctype=c_void_p),
    lib=_lib,
)

_isl_map_eq_at_multi_pw_aff = ISLFunction.create(
    _lib.isl_map_eq_at_multi_pw_aff,
    Take(Map),
    Param(None, ctype=c_void_p),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_lex_lt_at_multi_pw_aff = ISLFunction.create(
    _lib.isl_map_lex_lt_at_multi_pw_aff,
    Take(Map),
    Param(None, ctype=c_void_p),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_lex_le_at_multi_pw_aff = ISLFunction.create(
    _lib.isl_map_lex_le_at_multi_pw_aff,
    Take(Map),
    Param(None, ctype=c_void_p),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_lex_gt_at_multi_pw_aff = ISLFunction.create(
    _lib.isl_map_lex_gt_at_multi_pw_aff,
    Take(Map),
    Param(None, ctype=c_void_p),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_lex_ge_at_multi_pw_aff = ISLFunction.create(
    _lib.isl_map_lex_ge_at_multi_pw_aff,
    Take(Map),
    Param(None, ctype=c_void_p),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_partial_lexmax = ISLFunction.create(
    _lib.isl_map_partial_lexmax,
    Take(Map),
    Take(Set),
    Param(None, ctype=c_void_p),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_partial_lexmin = ISLFunction.create(
    _lib.isl_map_partial_lexmin,
    Take(Map),
    Take(Set),
    Param(None, ctype=c_void_p),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_list_read_from_str = ISLFunction.create(
    _lib.isl_map_list_read_from_str,
    Context(),
    Param(str),
    return_=Give(MapList),
    lib=_lib,
)
__all__ = ["Map"]
