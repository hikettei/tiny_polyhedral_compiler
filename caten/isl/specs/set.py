from __future__ import annotations

from ctypes import c_char_p, c_int, c_uint, c_void_p
from typing import TYPE_CHECKING, Any

from ..ffi import load_libisl
from ..func import ISLFunction
from ..mixin import ISLObjectMixin
from ..obj import ISLObject
from ..qualifier import Give, Keep, Param, Take
from ..registry import register_type
from .context import Context

if TYPE_CHECKING:
    from .aff import Aff
    from .basic_set import BasicSet
    from .basic_set_list import BasicSetList
    from .constraint import Constraint
    from .context import Context
    from .fixed_box import FixedBox
    from .id import Id
    from .id_list import IdList
    from .map import Map
    from .multi_aff import MultiAff
    from .multi_id import MultiId
    from .multi_pw_aff import MultiPwAff
    from .multi_val import MultiVal
    from .point import Point
    from .pw_aff import PwAff
    from .pw_multi_aff import PwMultiAff
    from .set_list import SetList
    from .space import Space
    from .stride_info import StrideInfo
    from .union_set import UnionSet
    from .val import Val

_lib = load_libisl()

class Set(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_set_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_set_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_set_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_set_free(handle)

    def __str__(self) -> str:
        return _isl_set_to_str(self)

    def __repr__(self) -> str:
        return f"Set({self.__str__()})"

    def get_space(self) -> "Space":
        return _isl_set_get_space(self)

    def tuple_dim(self) -> int:
        return _isl_set_tuple_dim(self)

    def dim(self, type: int) -> int:
        return _isl_set_dim(self, type)

    def set_dim_id(self, type: int, pos: int, id: "Id") -> "Set":
        return _isl_set_set_dim_id(self, type, pos, id)

    def has_dim_id(self, type: int, pos: int) -> bool:
        return _isl_set_has_dim_id(self, type, pos)

    def get_dim_id(self, type: int, pos: int) -> "Id":
        return _isl_set_get_dim_id(self, type, pos)

    def has_dim_name(self, type: int, pos: int) -> bool:
        return _isl_set_has_dim_name(self, type, pos)

    def get_dim_name(self, type: int, pos: int) -> str:
        return _isl_set_get_dim_name(self, type, pos)

    def find_dim_by_id(self, type: int, id: "Id") -> int:
        return _isl_set_find_dim_by_id(self, type, id)

    def find_dim_by_name(self, type: int, name: str) -> int:
        return _isl_set_find_dim_by_name(self, type, name)

    def set_tuple_id(self, id: "Id") -> "Set":
        return _isl_set_set_tuple_id(self, id)

    def reset_tuple_id(self) -> "Set":
        return _isl_set_reset_tuple_id(self)

    def has_tuple_id(self) -> bool:
        return _isl_set_has_tuple_id(self)

    def get_tuple_id(self) -> "Id":
        return _isl_set_get_tuple_id(self)

    def set_tuple_name(self, s: str) -> "Set":
        return _isl_set_set_tuple_name(self, s)

    def has_tuple_name(self) -> bool:
        return _isl_set_has_tuple_name(self)

    def get_tuple_name(self) -> str:
        return _isl_set_get_tuple_name(self)

    def reset_user(self) -> "Set":
        return _isl_set_reset_user(self)

    @classmethod
    def empty(cls, space: "Space") -> "Set":
        return _isl_set_empty(space)

    @classmethod
    def universe(cls, space: "Space") -> "Set":
        return _isl_set_universe(space)

    @classmethod
    def nat_universe(cls, space: "Space") -> "Set":
        return _isl_set_nat_universe(space)

    @classmethod
    def from_basic_set(cls, bset: "BasicSet") -> "Set":
        return _isl_set_from_basic_set(bset)

    def to_union_set(self) -> "UnionSet":
        return _isl_set_to_union_set(self)

    @classmethod
    def from_union_set(cls, uset: "UnionSet") -> "Set":
        return _isl_set_from_union_set(uset)

    def add_constraint(self, constraint: "Constraint") -> "Set":
        return _isl_set_add_constraint(self, constraint)

    @classmethod
    def from_multi_aff(cls, ma: "MultiAff") -> "Set":
        return _isl_set_from_multi_aff(ma)

    @classmethod
    def from_pw_aff(cls, pwaff: "PwAff") -> "Set":
        return _isl_set_from_pw_aff(pwaff)

    @classmethod
    def from_pw_multi_aff(cls, pma: "PwMultiAff") -> "Set":
        return _isl_set_from_pw_multi_aff(pma)

    @classmethod
    def from_multi_pw_aff(cls, mpa: "MultiPwAff") -> "Set":
        return _isl_set_from_multi_pw_aff(mpa)

    def compute_divs(self) -> "Set":
        return _isl_set_compute_divs(self)

    def remove_divs(self) -> "Set":
        return _isl_set_remove_divs(self)

    def remove_divs_involving_dims(self, type: int, first: int, n: int) -> "Set":
        return _isl_set_remove_divs_involving_dims(self, type, first, n)

    def remove_unknown_divs(self) -> "Set":
        return _isl_set_remove_unknown_divs(self)

    def foreach_basic_set(self, fn: Any, user: Any = None) -> int:
        return _isl_set_foreach_basic_set(self, fn, user)

    def make_disjoint(self) -> "Set":
        return _isl_set_make_disjoint(self)

    def n_basic_set(self) -> int:
        return _isl_set_n_basic_set(self)

    def get_basic_set_list(self) -> "BasicSetList":
        return _isl_set_get_basic_set_list(self)

    @classmethod
    def from_point(cls, pnt: "Point") -> "Set":
        return _isl_set_from_point(pnt)

    @classmethod
    def box_from_points(cls, pnt1: "Point", pnt2: "Point") -> "Set":
        return _isl_set_box_from_points(pnt1, pnt2)

    def foreach_point(self, fn: Any, user: Any = None) -> int:
        return _isl_set_foreach_point(self, fn, user)

    def sample_point(self) -> "Point":
        return _isl_set_sample_point(self)

    def pw_aff_on_domain_val(self, v: "Val") -> "PwAff":
        return _isl_set_pw_aff_on_domain_val(self, v)

    def pw_multi_aff_on_domain_multi_val(self, mv: "MultiVal") -> "PwMultiAff":
        return _isl_set_pw_multi_aff_on_domain_multi_val(self, mv)

    def param_pw_aff_on_domain_id(self, id: "Id") -> "PwAff":
        return _isl_set_param_pw_aff_on_domain_id(self, id)

    @classmethod
    def read_from_file(cls, input: None) -> "Set":
        return _isl_set_read_from_file(input)

    def plain_is_empty(self) -> bool:
        return _isl_set_plain_is_empty(self)

    def is_empty(self) -> bool:
        return _isl_set_is_empty(self)

    def plain_is_universe(self) -> bool:
        return _isl_set_plain_is_universe(self)

    def is_singleton(self) -> bool:
        return _isl_set_is_singleton(self)

    def plain_get_val_if_fixed(self, type: int, pos: int) -> "Val":
        return _isl_set_plain_get_val_if_fixed(self, type, pos)

    def get_plain_multi_val_if_fixed(self) -> "MultiVal":
        return _isl_set_get_plain_multi_val_if_fixed(self)

    def dim_residue_class_val(self, pos: int, modulo: "Val", residue: "Val") -> int:
        return _isl_set_dim_residue_class_val(self, pos, modulo, residue)

    def get_stride_info(self, pos: int) -> "StrideInfo":
        return _isl_set_get_stride_info(self, pos)

    def get_stride(self, pos: int) -> "Val":
        return _isl_set_get_stride(self, pos)

    def get_lattice_tile(self) -> "FixedBox":
        return _isl_set_get_lattice_tile(self)

    def involves_locals(self) -> bool:
        return _isl_set_involves_locals(self)

    def involves_dims(self, type: int, first: int, n: int) -> bool:
        return _isl_set_involves_dims(self, type, first, n)

    def dim_has_any_lower_bound(self, type: int, pos: int) -> bool:
        return _isl_set_dim_has_any_lower_bound(self, type, pos)

    def dim_has_any_upper_bound(self, type: int, pos: int) -> bool:
        return _isl_set_dim_has_any_upper_bound(self, type, pos)

    def dim_has_lower_bound(self, type: int, pos: int) -> bool:
        return _isl_set_dim_has_lower_bound(self, type, pos)

    def dim_has_upper_bound(self, type: int, pos: int) -> bool:
        return _isl_set_dim_has_upper_bound(self, type, pos)

    def is_params(self) -> bool:
        return _isl_set_is_params(self)

    def is_wrapping(self) -> bool:
        return _isl_set_is_wrapping(self)

    def plain_is_equal(self, set2: "Set") -> bool:
        return _isl_set_plain_is_equal(self, set2)

    def is_equal(self, set2: "Set") -> bool:
        return _isl_set_is_equal(self, set2)

    def plain_is_disjoint(self, set2: "Set") -> bool:
        return _isl_set_plain_is_disjoint(self, set2)

    def is_disjoint(self, set2: "Set") -> bool:
        return _isl_set_is_disjoint(self, set2)

    def is_subset(self, set2: "Set") -> bool:
        return _isl_set_is_subset(self, set2)

    def is_strict_subset(self, set2: "Set") -> bool:
        return _isl_set_is_strict_subset(self, set2)

    def plain_cmp(self, set2: "Set") -> int:
        return _isl_set_plain_cmp(self, set2)

    def complement(self) -> "Set":
        return _isl_set_complement(self)

    def wrapped_reverse(self) -> "Set":
        return _isl_set_wrapped_reverse(self)

    def bind(self, tuple: "MultiId") -> "Set":
        return _isl_set_bind(self, tuple)

    def project_out_param_id(self, id: "Id") -> "Set":
        return _isl_set_project_out_param_id(self, id)

    def project_out_param_id_list(self, list: "IdList") -> "Set":
        return _isl_set_project_out_param_id_list(self, list)

    def project_out(self, type: int, first: int, n: int) -> "Set":
        return _isl_set_project_out(self, type, first, n)

    def project_out_all_params(self) -> "Set":
        return _isl_set_project_out_all_params(self)

    def project_onto_map(self, type: int, first: int, n: int) -> "Map":
        return _isl_set_project_onto_map(self, type, first, n)

    def params(self) -> "Set":
        return _isl_set_params(self)

    def wrapped_domain_map(self) -> "Map":
        return _isl_set_wrapped_domain_map(self)

    def eliminate(self, type: int, first: int, n: int) -> "Set":
        return _isl_set_eliminate(self, type, first, n)

    def unbind_params(self, tuple: "MultiId") -> "Set":
        return _isl_set_unbind_params(self, tuple)

    def from_params(self) -> "Set":
        return _isl_set_from_params(self)

    def unbind_params_insert_domain(self, domain: "MultiId") -> "Map":
        return _isl_set_unbind_params_insert_domain(self, domain)

    def insert_domain(self, domain: "Space") -> "Map":
        return _isl_set_insert_domain(self, domain)

    def fix_si(self, type: int, pos: int, value: int) -> "Set":
        return _isl_set_fix_si(self, type, pos, value)

    def fix_val(self, type: int, pos: int, v: "Val") -> "Set":
        return _isl_set_fix_val(self, type, pos, v)

    def lower_bound_si(self, type: int, pos: int, value: int) -> "Set":
        return _isl_set_lower_bound_si(self, type, pos, value)

    def lower_bound_val(self, type: int, pos: int, value: "Val") -> "Set":
        return _isl_set_lower_bound_val(self, type, pos, value)

    def upper_bound_si(self, type: int, pos: int, value: int) -> "Set":
        return _isl_set_upper_bound_si(self, type, pos, value)

    def upper_bound_val(self, type: int, pos: int, value: "Val") -> "Set":
        return _isl_set_upper_bound_val(self, type, pos, value)

    def lower_bound_multi_val(self, lower: "MultiVal") -> "Set":
        return _isl_set_lower_bound_multi_val(self, lower)

    def upper_bound_multi_val(self, upper: "MultiVal") -> "Set":
        return _isl_set_upper_bound_multi_val(self, upper)

    def lower_bound_multi_pw_aff(self, lower: "MultiPwAff") -> "Set":
        return _isl_set_lower_bound_multi_pw_aff(self, lower)

    def upper_bound_multi_pw_aff(self, upper: "MultiPwAff") -> "Set":
        return _isl_set_upper_bound_multi_pw_aff(self, upper)

    def equate(self, type1: int, pos1: int, type2: int, pos2: int) -> "Set":
        return _isl_set_equate(self, type1, pos1, type2, pos2)

    def identity(self) -> "Map":
        return _isl_set_identity(self)

    def indicator_function(self) -> "PwAff":
        return _isl_set_indicator_function(self)

    def as_pw_multi_aff(self) -> "PwMultiAff":
        return _isl_set_as_pw_multi_aff(self)

    def translation(self) -> "Map":
        return _isl_set_translation(self)

    def coalesce(self) -> "Set":
        return _isl_set_coalesce(self)

    def detect_equalities(self) -> "Set":
        return _isl_set_detect_equalities(self)

    def remove_redundancies(self) -> "Set":
        return _isl_set_remove_redundancies(self)

    def convex_hull(self) -> "BasicSet":
        return _isl_set_convex_hull(self)

    def unshifted_simple_hull(self) -> "BasicSet":
        return _isl_set_unshifted_simple_hull(self)

    def simple_hull(self) -> "BasicSet":
        return _isl_set_simple_hull(self)

    def plain_unshifted_simple_hull(self) -> "BasicSet":
        return _isl_set_plain_unshifted_simple_hull(self)

    def unshifted_simple_hull_from_set_list(self, list: "SetList") -> "BasicSet":
        return _isl_set_unshifted_simple_hull_from_set_list(self, list)

    def affine_hull(self) -> "BasicSet":
        return _isl_set_affine_hull(self)

    def polyhedral_hull(self) -> "BasicSet":
        return _isl_set_polyhedral_hull(self)

    def get_simple_fixed_box_hull(self) -> "FixedBox":
        return _isl_set_get_simple_fixed_box_hull(self)

    def drop_constraints_involving_dims(self, type: int, first: int, n: int) -> "Set":
        return _isl_set_drop_constraints_involving_dims(self, type, first, n)

    def drop_constraints_not_involving_dims(self, type: int, first: int, n: int) -> "Set":
        return _isl_set_drop_constraints_not_involving_dims(self, type, first, n)

    def sample(self) -> "BasicSet":
        return _isl_set_sample(self)

    def min_val(self, obj: "Aff") -> "Val":
        return _isl_set_min_val(self, obj)

    def max_val(self, obj: "Aff") -> "Val":
        return _isl_set_max_val(self, obj)

    def dim_min_val(self, pos: int) -> "Val":
        return _isl_set_dim_min_val(self, pos)

    def dim_max_val(self, pos: int) -> "Val":
        return _isl_set_dim_max_val(self, pos)

    def dim_min(self, pos: int) -> "PwAff":
        return _isl_set_dim_min(self, pos)

    def dim_max(self, pos: int) -> "PwAff":
        return _isl_set_dim_max(self, pos)

    def min_multi_pw_aff(self) -> "MultiPwAff":
        return _isl_set_min_multi_pw_aff(self)

    def max_multi_pw_aff(self) -> "MultiPwAff":
        return _isl_set_max_multi_pw_aff(self)

    def coefficients(self) -> "BasicSet":
        return _isl_set_coefficients(self)

    def solutions(self) -> "BasicSet":
        return _isl_set_solutions(self)

    def unwrap(self) -> "Map":
        return _isl_set_unwrap(self)

    def flatten(self) -> "Set":
        return _isl_set_flatten(self)

    def flatten_map(self) -> "Map":
        return _isl_set_flatten_map(self)

    def lift(self) -> "Set":
        return _isl_set_lift(self)

    def align_params(self, model: "Space") -> "Set":
        return _isl_set_align_params(self, model)

    def drop_unused_params(self) -> "Set":
        return _isl_set_drop_unused_params(self)

    def neg(self) -> "Set":
        return _isl_set_neg(self)

    def add_dims(self, type: int, n: int) -> "Set":
        return _isl_set_add_dims(self, type, n)

    def insert_dims(self, type: int, pos: int, n: int) -> "Set":
        return _isl_set_insert_dims(self, type, pos, n)

    def move_dims(self, dst_type: int, dst_pos: int, src_type: int, src_pos: int, n: int) -> "Set":
        return _isl_set_move_dims(self, dst_type, dst_pos, src_type, src_pos, n)

    def intersect_params(self, params: "Set") -> "Set":
        return _isl_set_intersect_params(self, params)

    def intersect(self, set2: "Set") -> "Set":
        return _isl_set_intersect(self, set2)

    def intersect_factor_domain(self, domain: "Set") -> "Set":
        return _isl_set_intersect_factor_domain(self, domain)

    def intersect_factor_range(self, range: "Set") -> "Set":
        return _isl_set_intersect_factor_range(self, range)

    def union(self, set2: "Set") -> "Set":
        return _isl_set_union(self, set2)

    def subtract(self, set2: "Set") -> "Set":
        return _isl_set_subtract(self, set2)

    def apply(self, map: "Map") -> "Set":
        return _isl_set_apply(self, map)

    def preimage_multi_aff(self, ma: "MultiAff") -> "Set":
        return _isl_set_preimage_multi_aff(self, ma)

    def preimage_pw_multi_aff(self, pma: "PwMultiAff") -> "Set":
        return _isl_set_preimage_pw_multi_aff(self, pma)

    def preimage_multi_pw_aff(self, mpa: "MultiPwAff") -> "Set":
        return _isl_set_preimage_multi_pw_aff(self, mpa)

    def product(self, set2: "Set") -> "Set":
        return _isl_set_product(self, set2)

    def flat_product(self, set2: "Set") -> "Set":
        return _isl_set_flat_product(self, set2)

    def gist(self, context: "Set") -> "Set":
        return _isl_set_gist(self, context)

    def gist_params(self, context: "Set") -> "Set":
        return _isl_set_gist_params(self, context)

    def sum(self, set2: "Set") -> "Set":
        return _isl_set_sum(self, set2)

    def partial_lexmin(self, dom: "Set", empty: "Set") -> "Set":
        return _isl_set_partial_lexmin(self, dom, empty)

    def partial_lexmax(self, dom: "Set", empty: "Set") -> "Set":
        return _isl_set_partial_lexmax(self, dom, empty)

    def lexmin(self) -> "Set":
        return _isl_set_lexmin(self)

    def lexmax(self) -> "Set":
        return _isl_set_lexmax(self)

    def lexmin_pw_multi_aff(self) -> "PwMultiAff":
        return _isl_set_lexmin_pw_multi_aff(self)

    def lexmax_pw_multi_aff(self) -> "PwMultiAff":
        return _isl_set_lexmax_pw_multi_aff(self)

    def to_list(self) -> "SetList":
        return _isl_set_to_list(self)


register_type("Set", Set)

_isl_set_get_space = ISLFunction.create(
    "isl_set_get_space",
    Keep("Set"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_set_tuple_dim = ISLFunction.create(
    "isl_set_tuple_dim",
    Keep("Set"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_set_dim = ISLFunction.create(
    "isl_set_dim",
    Keep("Set"),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_set_set_dim_id = ISLFunction.create(
    "isl_set_set_dim_id",
    Take("Set"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Take("Id"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_has_dim_id = ISLFunction.create(
    "isl_set_has_dim_id",
    Keep("Set"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_get_dim_id = ISLFunction.create(
    "isl_set_get_dim_id",
    Keep("Set"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Give("Id"),
    lib=_lib,
)

_isl_set_has_dim_name = ISLFunction.create(
    "isl_set_has_dim_name",
    Keep("Set"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_get_dim_name = ISLFunction.create(
    "isl_set_get_dim_name",
    Keep("Set"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_set_find_dim_by_id = ISLFunction.create(
    "isl_set_find_dim_by_id",
    Keep("Set"),
    Param(int, ctype=c_int),
    Keep("Id"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_set_find_dim_by_name = ISLFunction.create(
    "isl_set_find_dim_by_name",
    Keep("Set"),
    Param(int, ctype=c_int),
    Param(str, ctype=c_char_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_set_set_tuple_id = ISLFunction.create(
    "isl_set_set_tuple_id",
    Take("Set"),
    Take("Id"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_reset_tuple_id = ISLFunction.create(
    "isl_set_reset_tuple_id",
    Take("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_has_tuple_id = ISLFunction.create(
    "isl_set_has_tuple_id",
    Keep("Set"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_get_tuple_id = ISLFunction.create(
    "isl_set_get_tuple_id",
    Keep("Set"),
    return_=Give("Id"),
    lib=_lib,
)

_isl_set_set_tuple_name = ISLFunction.create(
    "isl_set_set_tuple_name",
    Take("Set"),
    Param(str, ctype=c_char_p),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_has_tuple_name = ISLFunction.create(
    "isl_set_has_tuple_name",
    Keep("Set"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_get_tuple_name = ISLFunction.create(
    "isl_set_get_tuple_name",
    Keep("Set"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_set_reset_user = ISLFunction.create(
    "isl_set_reset_user",
    Take("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_empty = ISLFunction.create(
    "isl_set_empty",
    Take("Space"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_universe = ISLFunction.create(
    "isl_set_universe",
    Take("Space"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_nat_universe = ISLFunction.create(
    "isl_set_nat_universe",
    Take("Space"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_from_basic_set = ISLFunction.create(
    "isl_set_from_basic_set",
    Take("BasicSet"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_to_union_set = ISLFunction.create(
    "isl_set_to_union_set",
    Take("Set"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_set_from_union_set = ISLFunction.create(
    "isl_set_from_union_set",
    Take("UnionSet"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_copy = ISLFunction.create(
    "isl_set_copy",
    Keep("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_free = ISLFunction.create(
    "isl_set_free",
    Take("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_add_constraint = ISLFunction.create(
    "isl_set_add_constraint",
    Take("Set"),
    Take("Constraint"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_from_multi_aff = ISLFunction.create(
    "isl_set_from_multi_aff",
    Take("MultiAff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_from_pw_aff = ISLFunction.create(
    "isl_set_from_pw_aff",
    Take("PwAff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_from_pw_multi_aff = ISLFunction.create(
    "isl_set_from_pw_multi_aff",
    Take("PwMultiAff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_from_multi_pw_aff = ISLFunction.create(
    "isl_set_from_multi_pw_aff",
    Take("MultiPwAff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_compute_divs = ISLFunction.create(
    "isl_set_compute_divs",
    Take("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_remove_divs = ISLFunction.create(
    "isl_set_remove_divs",
    Take("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_remove_divs_involving_dims = ISLFunction.create(
    "isl_set_remove_divs_involving_dims",
    Take("Set"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_remove_unknown_divs = ISLFunction.create(
    "isl_set_remove_unknown_divs",
    Take("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_foreach_basic_set = ISLFunction.create(
    "isl_set_foreach_basic_set",
    Keep("Set"),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_set_make_disjoint = ISLFunction.create(
    "isl_set_make_disjoint",
    Take("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_n_basic_set = ISLFunction.create(
    "isl_set_n_basic_set",
    Keep("Set"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_set_get_basic_set_list = ISLFunction.create(
    "isl_set_get_basic_set_list",
    Keep("Set"),
    return_=Give("BasicSetList"),
    lib=_lib,
)

_isl_set_from_point = ISLFunction.create(
    "isl_set_from_point",
    Take("Point"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_box_from_points = ISLFunction.create(
    "isl_set_box_from_points",
    Take("Point"),
    Take("Point"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_foreach_point = ISLFunction.create(
    "isl_set_foreach_point",
    Keep("Set"),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_set_sample_point = ISLFunction.create(
    "isl_set_sample_point",
    Take("Set"),
    return_=Give("Point"),
    lib=_lib,
)

_isl_set_pw_aff_on_domain_val = ISLFunction.create(
    "isl_set_pw_aff_on_domain_val",
    Take("Set"),
    Take("Val"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_set_pw_multi_aff_on_domain_multi_val = ISLFunction.create(
    "isl_set_pw_multi_aff_on_domain_multi_val",
    Take("Set"),
    Take("MultiVal"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_set_param_pw_aff_on_domain_id = ISLFunction.create(
    "isl_set_param_pw_aff_on_domain_id",
    Take("Set"),
    Take("Id"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_set_read_from_file = ISLFunction.create(
    "isl_set_read_from_file",
    Context(),
    Param(None, ctype=c_void_p),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_read_from_str = ISLFunction.create(
    "isl_set_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_to_str = ISLFunction.create(
    "isl_set_to_str",
    Keep("Set"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_set_plain_is_empty = ISLFunction.create(
    "isl_set_plain_is_empty",
    Keep("Set"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_is_empty = ISLFunction.create(
    "isl_set_is_empty",
    Keep("Set"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_plain_is_universe = ISLFunction.create(
    "isl_set_plain_is_universe",
    Keep("Set"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_is_singleton = ISLFunction.create(
    "isl_set_is_singleton",
    Keep("Set"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_plain_get_val_if_fixed = ISLFunction.create(
    "isl_set_plain_get_val_if_fixed",
    Keep("Set"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Give("Val"),
    lib=_lib,
)

_isl_set_get_plain_multi_val_if_fixed = ISLFunction.create(
    "isl_set_get_plain_multi_val_if_fixed",
    Keep("Set"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_set_dim_residue_class_val = ISLFunction.create(
    "isl_set_dim_residue_class_val",
    Keep("Set"),
    Param(int, ctype=c_int),
    Give("Val"),
    Give("Val"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_set_get_stride_info = ISLFunction.create(
    "isl_set_get_stride_info",
    Keep("Set"),
    Param(int, ctype=c_int),
    return_=Give("StrideInfo"),
    lib=_lib,
)

_isl_set_get_stride = ISLFunction.create(
    "isl_set_get_stride",
    Keep("Set"),
    Param(int, ctype=c_int),
    return_=Give("Val"),
    lib=_lib,
)

_isl_set_get_lattice_tile = ISLFunction.create(
    "isl_set_get_lattice_tile",
    Keep("Set"),
    return_=Give("FixedBox"),
    lib=_lib,
)

_isl_set_involves_locals = ISLFunction.create(
    "isl_set_involves_locals",
    Keep("Set"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_involves_dims = ISLFunction.create(
    "isl_set_involves_dims",
    Keep("Set"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_dim_has_any_lower_bound = ISLFunction.create(
    "isl_set_dim_has_any_lower_bound",
    Keep("Set"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_dim_has_any_upper_bound = ISLFunction.create(
    "isl_set_dim_has_any_upper_bound",
    Keep("Set"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_dim_has_lower_bound = ISLFunction.create(
    "isl_set_dim_has_lower_bound",
    Keep("Set"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_dim_has_upper_bound = ISLFunction.create(
    "isl_set_dim_has_upper_bound",
    Keep("Set"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_is_params = ISLFunction.create(
    "isl_set_is_params",
    Keep("Set"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_is_wrapping = ISLFunction.create(
    "isl_set_is_wrapping",
    Keep("Set"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_plain_is_equal = ISLFunction.create(
    "isl_set_plain_is_equal",
    Keep("Set"),
    Keep("Set"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_is_equal = ISLFunction.create(
    "isl_set_is_equal",
    Keep("Set"),
    Keep("Set"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_plain_is_disjoint = ISLFunction.create(
    "isl_set_plain_is_disjoint",
    Keep("Set"),
    Keep("Set"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_is_disjoint = ISLFunction.create(
    "isl_set_is_disjoint",
    Keep("Set"),
    Keep("Set"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_is_subset = ISLFunction.create(
    "isl_set_is_subset",
    Keep("Set"),
    Keep("Set"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_is_strict_subset = ISLFunction.create(
    "isl_set_is_strict_subset",
    Keep("Set"),
    Keep("Set"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_plain_cmp = ISLFunction.create(
    "isl_set_plain_cmp",
    Keep("Set"),
    Keep("Set"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_set_complement = ISLFunction.create(
    "isl_set_complement",
    Take("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_wrapped_reverse = ISLFunction.create(
    "isl_set_wrapped_reverse",
    Take("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_bind = ISLFunction.create(
    "isl_set_bind",
    Take("Set"),
    Take("MultiId"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_project_out_param_id = ISLFunction.create(
    "isl_set_project_out_param_id",
    Take("Set"),
    Take("Id"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_project_out_param_id_list = ISLFunction.create(
    "isl_set_project_out_param_id_list",
    Take("Set"),
    Take("IdList"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_project_out = ISLFunction.create(
    "isl_set_project_out",
    Take("Set"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_project_out_all_params = ISLFunction.create(
    "isl_set_project_out_all_params",
    Take("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_project_onto_map = ISLFunction.create(
    "isl_set_project_onto_map",
    Take("Set"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("Map"),
    lib=_lib,
)

_isl_set_params = ISLFunction.create(
    "isl_set_params",
    Take("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_wrapped_domain_map = ISLFunction.create(
    "isl_set_wrapped_domain_map",
    Take("Set"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_set_eliminate = ISLFunction.create(
    "isl_set_eliminate",
    Take("Set"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_unbind_params = ISLFunction.create(
    "isl_set_unbind_params",
    Take("Set"),
    Take("MultiId"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_from_params = ISLFunction.create(
    "isl_set_from_params",
    Take("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_unbind_params_insert_domain = ISLFunction.create(
    "isl_set_unbind_params_insert_domain",
    Take("Set"),
    Take("MultiId"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_set_insert_domain = ISLFunction.create(
    "isl_set_insert_domain",
    Take("Set"),
    Take("Space"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_set_fix_si = ISLFunction.create(
    "isl_set_fix_si",
    Take("Set"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_int),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_fix_val = ISLFunction.create(
    "isl_set_fix_val",
    Take("Set"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Take("Val"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_lower_bound_si = ISLFunction.create(
    "isl_set_lower_bound_si",
    Take("Set"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_int),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_lower_bound_val = ISLFunction.create(
    "isl_set_lower_bound_val",
    Take("Set"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Take("Val"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_upper_bound_si = ISLFunction.create(
    "isl_set_upper_bound_si",
    Take("Set"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_int),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_upper_bound_val = ISLFunction.create(
    "isl_set_upper_bound_val",
    Take("Set"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Take("Val"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_lower_bound_multi_val = ISLFunction.create(
    "isl_set_lower_bound_multi_val",
    Take("Set"),
    Take("MultiVal"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_upper_bound_multi_val = ISLFunction.create(
    "isl_set_upper_bound_multi_val",
    Take("Set"),
    Take("MultiVal"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_lower_bound_multi_pw_aff = ISLFunction.create(
    "isl_set_lower_bound_multi_pw_aff",
    Take("Set"),
    Take("MultiPwAff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_upper_bound_multi_pw_aff = ISLFunction.create(
    "isl_set_upper_bound_multi_pw_aff",
    Take("Set"),
    Take("MultiPwAff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_equate = ISLFunction.create(
    "isl_set_equate",
    Take("Set"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_identity = ISLFunction.create(
    "isl_set_identity",
    Take("Set"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_set_indicator_function = ISLFunction.create(
    "isl_set_indicator_function",
    Take("Set"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_set_as_pw_multi_aff = ISLFunction.create(
    "isl_set_as_pw_multi_aff",
    Take("Set"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_set_translation = ISLFunction.create(
    "isl_set_translation",
    Take("Set"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_set_coalesce = ISLFunction.create(
    "isl_set_coalesce",
    Take("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_detect_equalities = ISLFunction.create(
    "isl_set_detect_equalities",
    Take("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_remove_redundancies = ISLFunction.create(
    "isl_set_remove_redundancies",
    Take("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_convex_hull = ISLFunction.create(
    "isl_set_convex_hull",
    Take("Set"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_set_unshifted_simple_hull = ISLFunction.create(
    "isl_set_unshifted_simple_hull",
    Take("Set"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_set_simple_hull = ISLFunction.create(
    "isl_set_simple_hull",
    Take("Set"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_set_plain_unshifted_simple_hull = ISLFunction.create(
    "isl_set_plain_unshifted_simple_hull",
    Take("Set"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_set_unshifted_simple_hull_from_set_list = ISLFunction.create(
    "isl_set_unshifted_simple_hull_from_set_list",
    Take("Set"),
    Take("SetList"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_set_affine_hull = ISLFunction.create(
    "isl_set_affine_hull",
    Take("Set"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_set_polyhedral_hull = ISLFunction.create(
    "isl_set_polyhedral_hull",
    Take("Set"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_set_get_simple_fixed_box_hull = ISLFunction.create(
    "isl_set_get_simple_fixed_box_hull",
    Keep("Set"),
    return_=Give("FixedBox"),
    lib=_lib,
)

_isl_set_drop_constraints_involving_dims = ISLFunction.create(
    "isl_set_drop_constraints_involving_dims",
    Take("Set"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_drop_constraints_not_involving_dims = ISLFunction.create(
    "isl_set_drop_constraints_not_involving_dims",
    Take("Set"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_sample = ISLFunction.create(
    "isl_set_sample",
    Take("Set"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_set_min_val = ISLFunction.create(
    "isl_set_min_val",
    Keep("Set"),
    Keep("Aff"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_set_max_val = ISLFunction.create(
    "isl_set_max_val",
    Keep("Set"),
    Keep("Aff"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_set_dim_min_val = ISLFunction.create(
    "isl_set_dim_min_val",
    Take("Set"),
    Param(int, ctype=c_int),
    return_=Give("Val"),
    lib=_lib,
)

_isl_set_dim_max_val = ISLFunction.create(
    "isl_set_dim_max_val",
    Take("Set"),
    Param(int, ctype=c_int),
    return_=Give("Val"),
    lib=_lib,
)

_isl_set_dim_min = ISLFunction.create(
    "isl_set_dim_min",
    Take("Set"),
    Param(int, ctype=c_int),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_set_dim_max = ISLFunction.create(
    "isl_set_dim_max",
    Take("Set"),
    Param(int, ctype=c_int),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_set_min_multi_pw_aff = ISLFunction.create(
    "isl_set_min_multi_pw_aff",
    Take("Set"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_set_max_multi_pw_aff = ISLFunction.create(
    "isl_set_max_multi_pw_aff",
    Take("Set"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_set_coefficients = ISLFunction.create(
    "isl_set_coefficients",
    Take("Set"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_set_solutions = ISLFunction.create(
    "isl_set_solutions",
    Take("Set"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_set_unwrap = ISLFunction.create(
    "isl_set_unwrap",
    Take("Set"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_set_flatten = ISLFunction.create(
    "isl_set_flatten",
    Take("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_flatten_map = ISLFunction.create(
    "isl_set_flatten_map",
    Take("Set"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_set_lift = ISLFunction.create(
    "isl_set_lift",
    Take("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_align_params = ISLFunction.create(
    "isl_set_align_params",
    Take("Set"),
    Take("Space"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_drop_unused_params = ISLFunction.create(
    "isl_set_drop_unused_params",
    Take("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_neg = ISLFunction.create(
    "isl_set_neg",
    Take("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_add_dims = ISLFunction.create(
    "isl_set_add_dims",
    Take("Set"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_insert_dims = ISLFunction.create(
    "isl_set_insert_dims",
    Take("Set"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_move_dims = ISLFunction.create(
    "isl_set_move_dims",
    Take("Set"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_intersect_params = ISLFunction.create(
    "isl_set_intersect_params",
    Take("Set"),
    Take("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_intersect = ISLFunction.create(
    "isl_set_intersect",
    Take("Set"),
    Take("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_intersect_factor_domain = ISLFunction.create(
    "isl_set_intersect_factor_domain",
    Take("Set"),
    Take("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_intersect_factor_range = ISLFunction.create(
    "isl_set_intersect_factor_range",
    Take("Set"),
    Take("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_union = ISLFunction.create(
    "isl_set_union",
    Take("Set"),
    Take("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_subtract = ISLFunction.create(
    "isl_set_subtract",
    Take("Set"),
    Take("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_apply = ISLFunction.create(
    "isl_set_apply",
    Take("Set"),
    Take("Map"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_preimage_multi_aff = ISLFunction.create(
    "isl_set_preimage_multi_aff",
    Take("Set"),
    Take("MultiAff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_preimage_pw_multi_aff = ISLFunction.create(
    "isl_set_preimage_pw_multi_aff",
    Take("Set"),
    Take("PwMultiAff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_preimage_multi_pw_aff = ISLFunction.create(
    "isl_set_preimage_multi_pw_aff",
    Take("Set"),
    Take("MultiPwAff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_product = ISLFunction.create(
    "isl_set_product",
    Take("Set"),
    Take("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_flat_product = ISLFunction.create(
    "isl_set_flat_product",
    Take("Set"),
    Take("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_gist = ISLFunction.create(
    "isl_set_gist",
    Take("Set"),
    Take("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_gist_params = ISLFunction.create(
    "isl_set_gist_params",
    Take("Set"),
    Take("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_sum = ISLFunction.create(
    "isl_set_sum",
    Take("Set"),
    Take("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_partial_lexmin = ISLFunction.create(
    "isl_set_partial_lexmin",
    Take("Set"),
    Take("Set"),
    Give("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_partial_lexmax = ISLFunction.create(
    "isl_set_partial_lexmax",
    Take("Set"),
    Take("Set"),
    Give("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_lexmin = ISLFunction.create(
    "isl_set_lexmin",
    Take("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_lexmax = ISLFunction.create(
    "isl_set_lexmax",
    Take("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_lexmin_pw_multi_aff = ISLFunction.create(
    "isl_set_lexmin_pw_multi_aff",
    Take("Set"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_set_lexmax_pw_multi_aff = ISLFunction.create(
    "isl_set_lexmax_pw_multi_aff",
    Take("Set"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_set_to_list = ISLFunction.create(
    "isl_set_to_list",
    Take("Set"),
    return_=Give("SetList"),
    lib=_lib,
)
