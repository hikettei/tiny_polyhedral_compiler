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
    from .aff_list import AffList
    from .basic_set import BasicSet
    from .constraint import Constraint
    from .context import Context
    from .id import Id
    from .local_space import LocalSpace
    from .map import Map
    from .mat import Mat
    from .multi_aff import MultiAff
    from .pw_multi_aff import PwMultiAff
    from .set import Set
    from .space import Space
    from .val import Val

_lib = load_libisl()

class BasicMap(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_basic_map_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_basic_map_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_basic_map_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_basic_map_free(handle)

    def __str__(self) -> str:
        return _isl_basic_map_to_str(self)

    def __repr__(self) -> str:
        return f"BasicMap({self.__str__()})"

    def get_space(self) -> "Space":
        return _isl_basic_map_get_space(self)

    def dim(self, type: int) -> int:
        return _isl_basic_map_dim(self, type)

    def has_dim_id(self, type: int, pos: int) -> bool:
        return _isl_basic_map_has_dim_id(self, type, pos)

    def get_dim_name(self, type: int, pos: int) -> str:
        return _isl_basic_map_get_dim_name(self, type, pos)

    def find_dim_by_name(self, type: int, name: str) -> int:
        return _isl_basic_map_find_dim_by_name(self, type, name)

    def set_tuple_id(self, type: int, id: "Id") -> "BasicMap":
        return _isl_basic_map_set_tuple_id(self, type, id)

    def get_tuple_name(self, type: int) -> str:
        return _isl_basic_map_get_tuple_name(self, type)

    def set_tuple_name(self, type: int, s: str) -> "BasicMap":
        return _isl_basic_map_set_tuple_name(self, type, s)

    def get_local_space(self) -> "LocalSpace":
        return _isl_basic_map_get_local_space(self)

    @classmethod
    def empty(cls, space: "Space") -> "BasicMap":
        return _isl_basic_map_empty(space)

    @classmethod
    def universe(cls, space: "Space") -> "BasicMap":
        return _isl_basic_map_universe(space)

    @classmethod
    def nat_universe(cls, space: "Space") -> "BasicMap":
        return _isl_basic_map_nat_universe(space)

    @classmethod
    def identity(cls, space: "Space") -> "BasicMap":
        return _isl_basic_map_identity(space)

    def add_constraint(self, constraint: "Constraint") -> "BasicMap":
        return _isl_basic_map_add_constraint(self, constraint)

    @classmethod
    def from_constraint_matrices(cls, space: "Space", eq: "Mat", ineq: "Mat", c1: int, c2: int, c3: int, c4: int, c5: int) -> "BasicMap":
        return _isl_basic_map_from_constraint_matrices(space, eq, ineq, c1, c2, c3, c4, c5)

    @classmethod
    def from_aff(cls, aff: "Aff") -> "BasicMap":
        return _isl_basic_map_from_aff(aff)

    @classmethod
    def from_aff_list(cls, domain_space: "Space", list: "AffList") -> "BasicMap":
        return _isl_basic_map_from_aff_list(domain_space, list)

    @classmethod
    def from_multi_aff(cls, maff: "MultiAff") -> "BasicMap":
        return _isl_basic_map_from_multi_aff(maff)

    def remove_divs(self) -> "BasicMap":
        return _isl_basic_map_remove_divs(self)

    def remove_divs_involving_dims(self, type: int, first: int, n: int) -> "BasicMap":
        return _isl_basic_map_remove_divs_involving_dims(self, type, first, n)

    def n_constraint(self) -> int:
        return _isl_basic_map_n_constraint(self)

    def foreach_constraint(self, fn: Any, user: Any, user_: Any = None) -> int:
        return _isl_basic_map_foreach_constraint(self, fn, user, user_)

    def equalities_matrix(self, c1: int, c2: int, c3: int, c4: int, c5: int) -> "Mat":
        return _isl_basic_map_equalities_matrix(self, c1, c2, c3, c4, c5)

    def inequalities_matrix(self, c1: int, c2: int, c3: int, c4: int, c5: int) -> "Mat":
        return _isl_basic_map_inequalities_matrix(self, c1, c2, c3, c4, c5)

    @classmethod
    def read_from_file(cls, input: None) -> "BasicMap":
        return _isl_basic_map_read_from_file(input)

    def plain_is_empty(self) -> bool:
        return _isl_basic_map_plain_is_empty(self)

    def is_empty(self) -> bool:
        return _isl_basic_map_is_empty(self)

    def plain_is_universe(self) -> bool:
        return _isl_basic_map_plain_is_universe(self)

    def is_universe(self) -> bool:
        return _isl_basic_map_is_universe(self)

    def is_single_valued(self) -> bool:
        return _isl_basic_map_is_single_valued(self)

    def plain_get_val_if_fixed(self, type: int, pos: int) -> "Val":
        return _isl_basic_map_plain_get_val_if_fixed(self, type, pos)

    def involves_dims(self, type: int, first: int, n: int) -> bool:
        return _isl_basic_map_involves_dims(self, type, first, n)

    def can_zip(self) -> bool:
        return _isl_basic_map_can_zip(self)

    def can_curry(self) -> bool:
        return _isl_basic_map_can_curry(self)

    def can_uncurry(self) -> bool:
        return _isl_basic_map_can_uncurry(self)

    def is_equal(self, bmap2: "BasicMap") -> bool:
        return _isl_basic_map_is_equal(self, bmap2)

    def is_disjoint(self, bmap2: "BasicMap") -> bool:
        return _isl_basic_map_is_disjoint(self, bmap2)

    def is_subset(self, bmap2: "BasicMap") -> bool:
        return _isl_basic_map_is_subset(self, bmap2)

    def is_strict_subset(self, bmap2: "BasicMap") -> bool:
        return _isl_basic_map_is_strict_subset(self, bmap2)

    def reverse(self) -> "BasicMap":
        return _isl_basic_map_reverse(self)

    def project_out(self, type: int, first: int, n: int) -> "BasicMap":
        return _isl_basic_map_project_out(self, type, first, n)

    def domain(self) -> "BasicSet":
        return _isl_basic_map_domain(self)

    def range(self) -> "BasicSet":
        return _isl_basic_map_range(self)

    def domain_map(self) -> "BasicMap":
        return _isl_basic_map_domain_map(self)

    def range_map(self) -> "BasicMap":
        return _isl_basic_map_range_map(self)

    def eliminate(self, type: int, first: int, n: int) -> "BasicMap":
        return _isl_basic_map_eliminate(self, type, first, n)

    def fix_si(self, type: int, pos: int, value: int) -> "BasicMap":
        return _isl_basic_map_fix_si(self, type, pos, value)

    def fix_val(self, type: int, pos: int, v: "Val") -> "BasicMap":
        return _isl_basic_map_fix_val(self, type, pos, v)

    def lower_bound_si(self, type: int, pos: int, value: int) -> "BasicMap":
        return _isl_basic_map_lower_bound_si(self, type, pos, value)

    def upper_bound_si(self, type: int, pos: int, value: int) -> "BasicMap":
        return _isl_basic_map_upper_bound_si(self, type, pos, value)

    def equate(self, type1: int, pos1: int, type2: int, pos2: int) -> "BasicMap":
        return _isl_basic_map_equate(self, type1, pos1, type2, pos2)

    def order_ge(self, type1: int, pos1: int, type2: int, pos2: int) -> "BasicMap":
        return _isl_basic_map_order_ge(self, type1, pos1, type2, pos2)

    def order_gt(self, type1: int, pos1: int, type2: int, pos2: int) -> "BasicMap":
        return _isl_basic_map_order_gt(self, type1, pos1, type2, pos2)

    def deltas(self) -> "BasicSet":
        return _isl_basic_map_deltas(self)

    def deltas_map(self) -> "BasicMap":
        return _isl_basic_map_deltas_map(self)

    def detect_equalities(self) -> "BasicMap":
        return _isl_basic_map_detect_equalities(self)

    def remove_redundancies(self) -> "BasicMap":
        return _isl_basic_map_remove_redundancies(self)

    def affine_hull(self) -> "BasicMap":
        return _isl_basic_map_affine_hull(self)

    def drop_constraints_involving_dims(self, type: int, first: int, n: int) -> "BasicMap":
        return _isl_basic_map_drop_constraints_involving_dims(self, type, first, n)

    def drop_constraints_not_involving_dims(self, type: int, first: int, n: int) -> "BasicMap":
        return _isl_basic_map_drop_constraints_not_involving_dims(self, type, first, n)

    def sample(self) -> "BasicMap":
        return _isl_basic_map_sample(self)

    def wrap(self) -> "BasicSet":
        return _isl_basic_map_wrap(self)

    def flatten_domain(self) -> "BasicMap":
        return _isl_basic_map_flatten_domain(self)

    def flatten_range(self) -> "BasicMap":
        return _isl_basic_map_flatten_range(self)

    def flatten(self) -> "BasicMap":
        return _isl_basic_map_flatten(self)

    def zip(self) -> "BasicMap":
        return _isl_basic_map_zip(self)

    def curry(self) -> "BasicMap":
        return _isl_basic_map_curry(self)

    def uncurry(self) -> "BasicMap":
        return _isl_basic_map_uncurry(self)

    def align_params(self, model: "Space") -> "BasicMap":
        return _isl_basic_map_align_params(self, model)

    def drop_unused_params(self) -> "BasicMap":
        return _isl_basic_map_drop_unused_params(self)

    def add_dims(self, type: int, n: int) -> "BasicMap":
        return _isl_basic_map_add_dims(self, type, n)

    def insert_dims(self, type: int, pos: int, n: int) -> "BasicMap":
        return _isl_basic_map_insert_dims(self, type, pos, n)

    def move_dims(self, dst_type: int, dst_pos: int, src_type: int, src_pos: int, n: int) -> "BasicMap":
        return _isl_basic_map_move_dims(self, dst_type, dst_pos, src_type, src_pos, n)

    def intersect_params(self, bset: "BasicSet") -> "BasicMap":
        return _isl_basic_map_intersect_params(self, bset)

    def intersect_domain(self, bset: "BasicSet") -> "BasicMap":
        return _isl_basic_map_intersect_domain(self, bset)

    def intersect_range(self, bset: "BasicSet") -> "BasicMap":
        return _isl_basic_map_intersect_range(self, bset)

    def intersect(self, bmap2: "BasicMap") -> "BasicMap":
        return _isl_basic_map_intersect(self, bmap2)

    def union(self, bmap2: "BasicMap") -> "Map":
        return _isl_basic_map_union(self, bmap2)

    def apply_domain(self, bmap2: "BasicMap") -> "BasicMap":
        return _isl_basic_map_apply_domain(self, bmap2)

    def apply_range(self, bmap2: "BasicMap") -> "BasicMap":
        return _isl_basic_map_apply_range(self, bmap2)

    def preimage_domain_multi_aff(self, ma: "MultiAff") -> "BasicMap":
        return _isl_basic_map_preimage_domain_multi_aff(self, ma)

    def preimage_range_multi_aff(self, ma: "MultiAff") -> "BasicMap":
        return _isl_basic_map_preimage_range_multi_aff(self, ma)

    def domain_product(self, bmap2: "BasicMap") -> "BasicMap":
        return _isl_basic_map_domain_product(self, bmap2)

    def range_product(self, bmap2: "BasicMap") -> "BasicMap":
        return _isl_basic_map_range_product(self, bmap2)

    def product(self, bmap2: "BasicMap") -> "BasicMap":
        return _isl_basic_map_product(self, bmap2)

    def flat_range_product(self, bmap2: "BasicMap") -> "BasicMap":
        return _isl_basic_map_flat_range_product(self, bmap2)

    def flat_product(self, bmap2: "BasicMap") -> "BasicMap":
        return _isl_basic_map_flat_product(self, bmap2)

    def gist(self, context: "BasicMap") -> "BasicMap":
        return _isl_basic_map_gist(self, context)

    def gist_domain(self, context: "BasicSet") -> "BasicMap":
        return _isl_basic_map_gist_domain(self, context)

    def partial_lexmax(self, dom: "BasicSet", empty: "Set") -> "Map":
        return _isl_basic_map_partial_lexmax(self, dom, empty)

    def partial_lexmin(self, dom: "BasicSet", empty: "Set") -> "Map":
        return _isl_basic_map_partial_lexmin(self, dom, empty)

    def lexmin(self) -> "Map":
        return _isl_basic_map_lexmin(self)

    def lexmax(self) -> "Map":
        return _isl_basic_map_lexmax(self)

    def lexmin_pw_multi_aff(self) -> "PwMultiAff":
        return _isl_basic_map_lexmin_pw_multi_aff(self)

    def partial_lexmin_pw_multi_aff(self, dom: "BasicSet", empty: "Set") -> "PwMultiAff":
        return _isl_basic_map_partial_lexmin_pw_multi_aff(self, dom, empty)

    def partial_lexmax_pw_multi_aff(self, dom: "BasicSet", empty: "Set") -> "PwMultiAff":
        return _isl_basic_map_partial_lexmax_pw_multi_aff(self, dom, empty)

    def remove_unknown_divs(self) -> "BasicMap":
        return _isl_basic_map_remove_unknown_divs(self)


register_type("BasicMap", BasicMap)

_isl_basic_map_get_space = ISLFunction.create(
    "isl_basic_map_get_space",
    Keep("BasicMap"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_basic_map_dim = ISLFunction.create(
    "isl_basic_map_dim",
    Keep("BasicMap"),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_basic_map_has_dim_id = ISLFunction.create(
    "isl_basic_map_has_dim_id",
    Keep("BasicMap"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_basic_map_get_dim_name = ISLFunction.create(
    "isl_basic_map_get_dim_name",
    Keep("BasicMap"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_basic_map_find_dim_by_name = ISLFunction.create(
    "isl_basic_map_find_dim_by_name",
    Keep("BasicMap"),
    Param(int, ctype=c_int),
    Param(str, ctype=c_char_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_basic_map_set_tuple_id = ISLFunction.create(
    "isl_basic_map_set_tuple_id",
    Take("BasicMap"),
    Param(int, ctype=c_int),
    Take("Id"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_get_tuple_name = ISLFunction.create(
    "isl_basic_map_get_tuple_name",
    Keep("BasicMap"),
    Param(int, ctype=c_int),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_basic_map_set_tuple_name = ISLFunction.create(
    "isl_basic_map_set_tuple_name",
    Take("BasicMap"),
    Param(int, ctype=c_int),
    Param(str, ctype=c_char_p),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_get_local_space = ISLFunction.create(
    "isl_basic_map_get_local_space",
    Keep("BasicMap"),
    return_=Give("LocalSpace"),
    lib=_lib,
)

_isl_basic_map_empty = ISLFunction.create(
    "isl_basic_map_empty",
    Take("Space"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_universe = ISLFunction.create(
    "isl_basic_map_universe",
    Take("Space"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_nat_universe = ISLFunction.create(
    "isl_basic_map_nat_universe",
    Take("Space"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_identity = ISLFunction.create(
    "isl_basic_map_identity",
    Take("Space"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_copy = ISLFunction.create(
    "isl_basic_map_copy",
    Keep("BasicMap"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_free = ISLFunction.create(
    "isl_basic_map_free",
    Take("BasicMap"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_add_constraint = ISLFunction.create(
    "isl_basic_map_add_constraint",
    Take("BasicMap"),
    Take("Constraint"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_from_constraint_matrices = ISLFunction.create(
    "isl_basic_map_from_constraint_matrices",
    Take("Space"),
    Take("Mat"),
    Take("Mat"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_from_aff = ISLFunction.create(
    "isl_basic_map_from_aff",
    Take("Aff"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_from_aff_list = ISLFunction.create(
    "isl_basic_map_from_aff_list",
    Take("Space"),
    Take("AffList"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_from_multi_aff = ISLFunction.create(
    "isl_basic_map_from_multi_aff",
    Take("MultiAff"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_remove_divs = ISLFunction.create(
    "isl_basic_map_remove_divs",
    Take("BasicMap"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_remove_divs_involving_dims = ISLFunction.create(
    "isl_basic_map_remove_divs_involving_dims",
    Take("BasicMap"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_n_constraint = ISLFunction.create(
    "isl_basic_map_n_constraint",
    Keep("BasicMap"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_basic_map_foreach_constraint = ISLFunction.create(
    "isl_basic_map_foreach_constraint",
    Keep("BasicMap"),
    Param(None, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_basic_map_equalities_matrix = ISLFunction.create(
    "isl_basic_map_equalities_matrix",
    Keep("BasicMap"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give("Mat"),
    lib=_lib,
)

_isl_basic_map_inequalities_matrix = ISLFunction.create(
    "isl_basic_map_inequalities_matrix",
    Keep("BasicMap"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give("Mat"),
    lib=_lib,
)

_isl_basic_map_read_from_file = ISLFunction.create(
    "isl_basic_map_read_from_file",
    Context(),
    Param(None, ctype=c_void_p),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_read_from_str = ISLFunction.create(
    "isl_basic_map_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_to_str = ISLFunction.create(
    "isl_basic_map_to_str",
    Keep("BasicMap"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_basic_map_plain_is_empty = ISLFunction.create(
    "isl_basic_map_plain_is_empty",
    Keep("BasicMap"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_basic_map_is_empty = ISLFunction.create(
    "isl_basic_map_is_empty",
    Keep("BasicMap"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_basic_map_plain_is_universe = ISLFunction.create(
    "isl_basic_map_plain_is_universe",
    Keep("BasicMap"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_basic_map_is_universe = ISLFunction.create(
    "isl_basic_map_is_universe",
    Keep("BasicMap"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_basic_map_is_single_valued = ISLFunction.create(
    "isl_basic_map_is_single_valued",
    Keep("BasicMap"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_basic_map_plain_get_val_if_fixed = ISLFunction.create(
    "isl_basic_map_plain_get_val_if_fixed",
    Keep("BasicMap"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Give("Val"),
    lib=_lib,
)

_isl_basic_map_involves_dims = ISLFunction.create(
    "isl_basic_map_involves_dims",
    Keep("BasicMap"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_basic_map_can_zip = ISLFunction.create(
    "isl_basic_map_can_zip",
    Keep("BasicMap"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_basic_map_can_curry = ISLFunction.create(
    "isl_basic_map_can_curry",
    Keep("BasicMap"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_basic_map_can_uncurry = ISLFunction.create(
    "isl_basic_map_can_uncurry",
    Keep("BasicMap"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_basic_map_is_equal = ISLFunction.create(
    "isl_basic_map_is_equal",
    Keep("BasicMap"),
    Keep("BasicMap"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_basic_map_is_disjoint = ISLFunction.create(
    "isl_basic_map_is_disjoint",
    Keep("BasicMap"),
    Keep("BasicMap"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_basic_map_is_subset = ISLFunction.create(
    "isl_basic_map_is_subset",
    Keep("BasicMap"),
    Keep("BasicMap"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_basic_map_is_strict_subset = ISLFunction.create(
    "isl_basic_map_is_strict_subset",
    Keep("BasicMap"),
    Keep("BasicMap"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_basic_map_reverse = ISLFunction.create(
    "isl_basic_map_reverse",
    Take("BasicMap"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_project_out = ISLFunction.create(
    "isl_basic_map_project_out",
    Take("BasicMap"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_domain = ISLFunction.create(
    "isl_basic_map_domain",
    Take("BasicMap"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_map_range = ISLFunction.create(
    "isl_basic_map_range",
    Take("BasicMap"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_map_domain_map = ISLFunction.create(
    "isl_basic_map_domain_map",
    Take("BasicMap"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_range_map = ISLFunction.create(
    "isl_basic_map_range_map",
    Take("BasicMap"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_eliminate = ISLFunction.create(
    "isl_basic_map_eliminate",
    Take("BasicMap"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_fix_si = ISLFunction.create(
    "isl_basic_map_fix_si",
    Take("BasicMap"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_int),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_fix_val = ISLFunction.create(
    "isl_basic_map_fix_val",
    Take("BasicMap"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Take("Val"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_lower_bound_si = ISLFunction.create(
    "isl_basic_map_lower_bound_si",
    Take("BasicMap"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_int),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_upper_bound_si = ISLFunction.create(
    "isl_basic_map_upper_bound_si",
    Take("BasicMap"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_int),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_equate = ISLFunction.create(
    "isl_basic_map_equate",
    Take("BasicMap"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_order_ge = ISLFunction.create(
    "isl_basic_map_order_ge",
    Take("BasicMap"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_order_gt = ISLFunction.create(
    "isl_basic_map_order_gt",
    Take("BasicMap"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_deltas = ISLFunction.create(
    "isl_basic_map_deltas",
    Take("BasicMap"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_map_deltas_map = ISLFunction.create(
    "isl_basic_map_deltas_map",
    Take("BasicMap"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_detect_equalities = ISLFunction.create(
    "isl_basic_map_detect_equalities",
    Take("BasicMap"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_remove_redundancies = ISLFunction.create(
    "isl_basic_map_remove_redundancies",
    Take("BasicMap"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_affine_hull = ISLFunction.create(
    "isl_basic_map_affine_hull",
    Take("BasicMap"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_drop_constraints_involving_dims = ISLFunction.create(
    "isl_basic_map_drop_constraints_involving_dims",
    Take("BasicMap"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_drop_constraints_not_involving_dims = ISLFunction.create(
    "isl_basic_map_drop_constraints_not_involving_dims",
    Take("BasicMap"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_sample = ISLFunction.create(
    "isl_basic_map_sample",
    Take("BasicMap"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_wrap = ISLFunction.create(
    "isl_basic_map_wrap",
    Take("BasicMap"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_map_flatten_domain = ISLFunction.create(
    "isl_basic_map_flatten_domain",
    Take("BasicMap"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_flatten_range = ISLFunction.create(
    "isl_basic_map_flatten_range",
    Take("BasicMap"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_flatten = ISLFunction.create(
    "isl_basic_map_flatten",
    Take("BasicMap"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_zip = ISLFunction.create(
    "isl_basic_map_zip",
    Take("BasicMap"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_curry = ISLFunction.create(
    "isl_basic_map_curry",
    Take("BasicMap"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_uncurry = ISLFunction.create(
    "isl_basic_map_uncurry",
    Take("BasicMap"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_align_params = ISLFunction.create(
    "isl_basic_map_align_params",
    Take("BasicMap"),
    Take("Space"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_drop_unused_params = ISLFunction.create(
    "isl_basic_map_drop_unused_params",
    Take("BasicMap"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_add_dims = ISLFunction.create(
    "isl_basic_map_add_dims",
    Take("BasicMap"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_insert_dims = ISLFunction.create(
    "isl_basic_map_insert_dims",
    Take("BasicMap"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_move_dims = ISLFunction.create(
    "isl_basic_map_move_dims",
    Take("BasicMap"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_intersect_params = ISLFunction.create(
    "isl_basic_map_intersect_params",
    Take("BasicMap"),
    Take("BasicSet"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_intersect_domain = ISLFunction.create(
    "isl_basic_map_intersect_domain",
    Take("BasicMap"),
    Take("BasicSet"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_intersect_range = ISLFunction.create(
    "isl_basic_map_intersect_range",
    Take("BasicMap"),
    Take("BasicSet"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_intersect = ISLFunction.create(
    "isl_basic_map_intersect",
    Take("BasicMap"),
    Take("BasicMap"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_union = ISLFunction.create(
    "isl_basic_map_union",
    Take("BasicMap"),
    Take("BasicMap"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_basic_map_apply_domain = ISLFunction.create(
    "isl_basic_map_apply_domain",
    Take("BasicMap"),
    Take("BasicMap"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_apply_range = ISLFunction.create(
    "isl_basic_map_apply_range",
    Take("BasicMap"),
    Take("BasicMap"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_preimage_domain_multi_aff = ISLFunction.create(
    "isl_basic_map_preimage_domain_multi_aff",
    Take("BasicMap"),
    Take("MultiAff"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_preimage_range_multi_aff = ISLFunction.create(
    "isl_basic_map_preimage_range_multi_aff",
    Take("BasicMap"),
    Take("MultiAff"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_domain_product = ISLFunction.create(
    "isl_basic_map_domain_product",
    Take("BasicMap"),
    Take("BasicMap"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_range_product = ISLFunction.create(
    "isl_basic_map_range_product",
    Take("BasicMap"),
    Take("BasicMap"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_product = ISLFunction.create(
    "isl_basic_map_product",
    Take("BasicMap"),
    Take("BasicMap"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_flat_range_product = ISLFunction.create(
    "isl_basic_map_flat_range_product",
    Take("BasicMap"),
    Take("BasicMap"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_flat_product = ISLFunction.create(
    "isl_basic_map_flat_product",
    Take("BasicMap"),
    Take("BasicMap"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_gist = ISLFunction.create(
    "isl_basic_map_gist",
    Take("BasicMap"),
    Take("BasicMap"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_gist_domain = ISLFunction.create(
    "isl_basic_map_gist_domain",
    Take("BasicMap"),
    Take("BasicSet"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_map_partial_lexmax = ISLFunction.create(
    "isl_basic_map_partial_lexmax",
    Take("BasicMap"),
    Take("BasicSet"),
    Give("Set"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_basic_map_partial_lexmin = ISLFunction.create(
    "isl_basic_map_partial_lexmin",
    Take("BasicMap"),
    Take("BasicSet"),
    Give("Set"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_basic_map_lexmin = ISLFunction.create(
    "isl_basic_map_lexmin",
    Take("BasicMap"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_basic_map_lexmax = ISLFunction.create(
    "isl_basic_map_lexmax",
    Take("BasicMap"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_basic_map_lexmin_pw_multi_aff = ISLFunction.create(
    "isl_basic_map_lexmin_pw_multi_aff",
    Take("BasicMap"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_basic_map_partial_lexmin_pw_multi_aff = ISLFunction.create(
    "isl_basic_map_partial_lexmin_pw_multi_aff",
    Take("BasicMap"),
    Take("BasicSet"),
    Give("Set"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_basic_map_partial_lexmax_pw_multi_aff = ISLFunction.create(
    "isl_basic_map_partial_lexmax_pw_multi_aff",
    Take("BasicMap"),
    Take("BasicSet"),
    Give("Set"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_basic_map_remove_unknown_divs = ISLFunction.create(
    "isl_basic_map_remove_unknown_divs",
    Take("BasicMap"),
    return_=Give("BasicMap"),
    lib=_lib,
)
