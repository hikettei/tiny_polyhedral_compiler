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

class BasicSet(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_basic_set_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_basic_set_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_basic_set_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_basic_set_free(handle)

    def __str__(self) -> str:
        return _isl_basic_set_to_str(self)

    def __repr__(self) -> str:
        return f"BasicSet({self.__str__()})"

    def get_space(self) -> "Space":
        return _isl_basic_set_get_space(self)

    def dim(self, type: int) -> int:
        return _isl_basic_set_dim(self, type)

    def get_dim_id(self, type: int, pos: int) -> "Id":
        return _isl_basic_set_get_dim_id(self, type, pos)

    def get_dim_name(self, type: int, pos: int) -> str:
        return _isl_basic_set_get_dim_name(self, type, pos)

    def set_tuple_id(self, id: "Id") -> "BasicSet":
        return _isl_basic_set_set_tuple_id(self, id)

    def set_tuple_name(self, s: str) -> "BasicSet":
        return _isl_basic_set_set_tuple_name(self, s)

    def get_tuple_name(self) -> str:
        return _isl_basic_set_get_tuple_name(self)

    def get_local_space(self) -> "LocalSpace":
        return _isl_basic_set_get_local_space(self)

    @classmethod
    def empty(cls, space: "Space") -> "BasicSet":
        return _isl_basic_set_empty(space)

    @classmethod
    def universe(cls, space: "Space") -> "BasicSet":
        return _isl_basic_set_universe(space)

    @classmethod
    def nat_universe(cls, space: "Space") -> "BasicSet":
        return _isl_basic_set_nat_universe(space)

    def to_set(self) -> "Set":
        return _isl_basic_set_to_set(self)

    def add_constraint(self, constraint: "Constraint") -> "BasicSet":
        return _isl_basic_set_add_constraint(self, constraint)

    @classmethod
    def from_constraint_matrices(cls, space: "Space", eq: "Mat", ineq: "Mat", c1: int, c2: int, c3: int, c4: int) -> "BasicSet":
        return _isl_basic_set_from_constraint_matrices(space, eq, ineq, c1, c2, c3, c4)

    @classmethod
    def from_multi_aff(cls, ma: "MultiAff") -> "BasicSet":
        return _isl_basic_set_from_multi_aff(ma)

    def remove_divs(self) -> "BasicSet":
        return _isl_basic_set_remove_divs(self)

    def remove_divs_involving_dims(self, type: int, first: int, n: int) -> "BasicSet":
        return _isl_basic_set_remove_divs_involving_dims(self, type, first, n)

    def remove_unknown_divs(self) -> "BasicSet":
        return _isl_basic_set_remove_unknown_divs(self)

    def n_constraint(self) -> int:
        return _isl_basic_set_n_constraint(self)

    def foreach_constraint(self, fn: Any, user: Any = None) -> int:
        return _isl_basic_set_foreach_constraint(self, fn, user)

    def get_constraint_list(self) -> "ConstraintList":
        return _isl_basic_set_get_constraint_list(self)

    def equalities_matrix(self, c1: int, c2: int, c3: int, c4: int) -> "Mat":
        return _isl_basic_set_equalities_matrix(self, c1, c2, c3, c4)

    def inequalities_matrix(self, c1: int, c2: int, c3: int, c4: int) -> "Mat":
        return _isl_basic_set_inequalities_matrix(self, c1, c2, c3, c4)

    @classmethod
    def from_point(cls, pnt: "Point") -> "BasicSet":
        return _isl_basic_set_from_point(pnt)

    @classmethod
    def box_from_points(cls, pnt1: "Point", pnt2: "Point") -> "BasicSet":
        return _isl_basic_set_box_from_points(pnt1, pnt2)

    def sample_point(self) -> "Point":
        return _isl_basic_set_sample_point(self)

    @classmethod
    def read_from_file(cls, input: None) -> "BasicSet":
        return _isl_basic_set_read_from_file(input)

    def plain_is_empty(self) -> bool:
        return _isl_basic_set_plain_is_empty(self)

    def is_empty(self) -> bool:
        return _isl_basic_set_is_empty(self)

    def plain_is_universe(self) -> bool:
        return _isl_basic_set_plain_is_universe(self)

    def is_universe(self) -> bool:
        return _isl_basic_set_is_universe(self)

    def involves_dims(self, type: int, first: int, n: int) -> bool:
        return _isl_basic_set_involves_dims(self, type, first, n)

    def is_wrapping(self) -> bool:
        return _isl_basic_set_is_wrapping(self)

    def is_equal(self, bset2: "BasicSet") -> bool:
        return _isl_basic_set_plain_is_equal(self, bset2)

    def is_equal(self, bset2: "BasicSet") -> bool:
        return _isl_basic_set_is_equal(self, bset2)

    def is_disjoint(self, bset2: "BasicSet") -> bool:
        return _isl_basic_set_is_disjoint(self, bset2)

    def is_subset(self, bset2: "BasicSet") -> bool:
        return _isl_basic_set_is_subset(self, bset2)

    def params(self) -> "BasicSet":
        return _isl_basic_set_params(self)

    def eliminate(self, type: int, first: int, n: int) -> "BasicSet":
        return _isl_basic_set_eliminate(self, type, first, n)

    def from_params(self) -> "BasicSet":
        return _isl_basic_set_from_params(self)

    def fix_si(self, type: int, pos: int, value: int) -> "BasicSet":
        return _isl_basic_set_fix_si(self, type, pos, value)

    def fix_val(self, type: int, pos: int, v: "Val") -> "BasicSet":
        return _isl_basic_set_fix_val(self, type, pos, v)

    def lower_bound_val(self, type: int, pos: int, value: "Val") -> "BasicSet":
        return _isl_basic_set_lower_bound_val(self, type, pos, value)

    def upper_bound_val(self, type: int, pos: int, value: "Val") -> "BasicSet":
        return _isl_basic_set_upper_bound_val(self, type, pos, value)

    def detect_equalities(self) -> "BasicSet":
        return _isl_basic_set_detect_equalities(self)

    def remove_redundancies(self) -> "BasicSet":
        return _isl_basic_set_remove_redundancies(self)

    def affine_hull(self) -> "BasicSet":
        return _isl_basic_set_affine_hull(self)

    def drop_constraints_involving_dims(self, type: int, first: int, n: int) -> "BasicSet":
        return _isl_basic_set_drop_constraints_involving_dims(self, type, first, n)

    def drop_constraints_not_involving_dims(self, type: int, first: int, n: int) -> "BasicSet":
        return _isl_basic_set_drop_constraints_not_involving_dims(self, type, first, n)

    def sample(self) -> "BasicSet":
        return _isl_basic_set_sample(self)

    def max_val(self, obj: "Aff") -> "Val":
        return _isl_basic_set_max_val(self, obj)

    def dim_max_val(self, pos: int) -> "Val":
        return _isl_basic_set_dim_max_val(self, pos)

    def coefficients(self) -> "BasicSet":
        return _isl_basic_set_coefficients(self)

    def solutions(self) -> "BasicSet":
        return _isl_basic_set_solutions(self)

    def unwrap(self) -> "BasicMap":
        return _isl_basic_set_unwrap(self)

    def flatten(self) -> "BasicSet":
        return _isl_basic_set_flatten(self)

    def lift(self) -> "BasicSet":
        return _isl_basic_set_lift(self)

    def align_params(self, model: "Space") -> "BasicSet":
        return _isl_basic_set_align_params(self, model)

    def drop_unused_params(self) -> "BasicSet":
        return _isl_basic_set_drop_unused_params(self)

    def add_dims(self, type: int, n: int) -> "BasicSet":
        return _isl_basic_set_add_dims(self, type, n)

    def insert_dims(self, type: int, pos: int, n: int) -> "BasicSet":
        return _isl_basic_set_insert_dims(self, type, pos, n)

    def move_dims(self, dst_type: int, dst_pos: int, src_type: int, src_pos: int, n: int) -> "BasicSet":
        return _isl_basic_set_move_dims(self, dst_type, dst_pos, src_type, src_pos, n)

    def intersect_params(self, bset2: "BasicSet") -> "BasicSet":
        return _isl_basic_set_intersect_params(self, bset2)

    def union(self, bset2: "BasicSet") -> "Set":
        return _isl_basic_set_union(self, bset2)

    def apply(self, bmap: "BasicMap") -> "BasicSet":
        return _isl_basic_set_apply(self, bmap)

    def flat_product(self, bset2: "BasicSet") -> "BasicSet":
        return _isl_basic_set_flat_product(self, bset2)

    def gist(self, context: "BasicSet") -> "BasicSet":
        return _isl_basic_set_gist(self, context)

    def partial_lexmin(self, dom: "BasicSet", empty: "Set") -> "Set":
        return _isl_basic_set_partial_lexmin(self, dom, empty)

    def partial_lexmax(self, dom: "BasicSet", empty: "Set") -> "Set":
        return _isl_basic_set_partial_lexmax(self, dom, empty)

    def lexmin(self) -> "Set":
        return _isl_basic_set_lexmin(self)

    def lexmax(self) -> "Set":
        return _isl_basic_set_lexmax(self)

    def partial_lexmin_pw_multi_aff(self, dom: "BasicSet", empty: "Set") -> "PwMultiAff":
        return _isl_basic_set_partial_lexmin_pw_multi_aff(self, dom, empty)

    def partial_lexmax_pw_multi_aff(self, dom: "BasicSet", empty: "Set") -> "PwMultiAff":
        return _isl_basic_set_partial_lexmax_pw_multi_aff(self, dom, empty)

    def compute_vertices(self) -> "Vertices":
        return _isl_basic_set_compute_vertices(self)

    def intersect(self, bset2: "BasicSet") -> "BasicSet":
        return _isl_basic_set_intersect(self, bset2)

    def project_out(self, type: int, first: int, n: int) -> "BasicSet":
        return _isl_basic_set_project_out(self, type, first, n)


register_type("BasicSet", BasicSet)

_isl_basic_set_get_space = ISLFunction.create(
    "isl_basic_set_get_space",
    Keep("BasicSet"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_basic_set_dim = ISLFunction.create(
    "isl_basic_set_dim",
    Keep("BasicSet"),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_basic_set_get_dim_id = ISLFunction.create(
    "isl_basic_set_get_dim_id",
    Keep("BasicSet"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Give("Id"),
    lib=_lib,
)

_isl_basic_set_get_dim_name = ISLFunction.create(
    "isl_basic_set_get_dim_name",
    Keep("BasicSet"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_basic_set_set_tuple_id = ISLFunction.create(
    "isl_basic_set_set_tuple_id",
    Take("BasicSet"),
    Take("Id"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_set_tuple_name = ISLFunction.create(
    "isl_basic_set_set_tuple_name",
    Take("BasicSet"),
    Param(str, ctype=c_char_p),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_get_tuple_name = ISLFunction.create(
    "isl_basic_set_get_tuple_name",
    Keep("BasicSet"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_basic_set_get_local_space = ISLFunction.create(
    "isl_basic_set_get_local_space",
    Keep("BasicSet"),
    return_=Give("LocalSpace"),
    lib=_lib,
)

_isl_basic_set_empty = ISLFunction.create(
    "isl_basic_set_empty",
    Take("Space"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_universe = ISLFunction.create(
    "isl_basic_set_universe",
    Take("Space"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_nat_universe = ISLFunction.create(
    "isl_basic_set_nat_universe",
    Take("Space"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_to_set = ISLFunction.create(
    "isl_basic_set_to_set",
    Take("BasicSet"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_basic_set_copy = ISLFunction.create(
    "isl_basic_set_copy",
    Keep("BasicSet"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_free = ISLFunction.create(
    "isl_basic_set_free",
    Take("BasicSet"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_add_constraint = ISLFunction.create(
    "isl_basic_set_add_constraint",
    Take("BasicSet"),
    Take("Constraint"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_from_constraint_matrices = ISLFunction.create(
    "isl_basic_set_from_constraint_matrices",
    Take("Space"),
    Take("Mat"),
    Take("Mat"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_from_multi_aff = ISLFunction.create(
    "isl_basic_set_from_multi_aff",
    Take("MultiAff"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_remove_divs = ISLFunction.create(
    "isl_basic_set_remove_divs",
    Take("BasicSet"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_remove_divs_involving_dims = ISLFunction.create(
    "isl_basic_set_remove_divs_involving_dims",
    Take("BasicSet"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_remove_unknown_divs = ISLFunction.create(
    "isl_basic_set_remove_unknown_divs",
    Take("BasicSet"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_n_constraint = ISLFunction.create(
    "isl_basic_set_n_constraint",
    Keep("BasicSet"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_basic_set_foreach_constraint = ISLFunction.create(
    "isl_basic_set_foreach_constraint",
    Keep("BasicSet"),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_basic_set_get_constraint_list = ISLFunction.create(
    "isl_basic_set_get_constraint_list",
    Keep("BasicSet"),
    return_=Give("ConstraintList"),
    lib=_lib,
)

_isl_basic_set_equalities_matrix = ISLFunction.create(
    "isl_basic_set_equalities_matrix",
    Keep("BasicSet"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give("Mat"),
    lib=_lib,
)

_isl_basic_set_inequalities_matrix = ISLFunction.create(
    "isl_basic_set_inequalities_matrix",
    Keep("BasicSet"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give("Mat"),
    lib=_lib,
)

_isl_basic_set_from_point = ISLFunction.create(
    "isl_basic_set_from_point",
    Take("Point"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_box_from_points = ISLFunction.create(
    "isl_basic_set_box_from_points",
    Take("Point"),
    Take("Point"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_sample_point = ISLFunction.create(
    "isl_basic_set_sample_point",
    Take("BasicSet"),
    return_=Give("Point"),
    lib=_lib,
)

_isl_basic_set_read_from_file = ISLFunction.create(
    "isl_basic_set_read_from_file",
    Context(),
    Param(None, ctype=c_void_p),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_to_str = ISLFunction.create(
    "isl_basic_set_to_str",
    Keep("BasicSet"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_basic_set_plain_is_empty = ISLFunction.create(
    "isl_basic_set_plain_is_empty",
    Keep("BasicSet"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_basic_set_is_empty = ISLFunction.create(
    "isl_basic_set_is_empty",
    Keep("BasicSet"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_basic_set_plain_is_universe = ISLFunction.create(
    "isl_basic_set_plain_is_universe",
    Keep("BasicSet"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_basic_set_is_universe = ISLFunction.create(
    "isl_basic_set_is_universe",
    Keep("BasicSet"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_basic_set_involves_dims = ISLFunction.create(
    "isl_basic_set_involves_dims",
    Keep("BasicSet"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_basic_set_is_wrapping = ISLFunction.create(
    "isl_basic_set_is_wrapping",
    Keep("BasicSet"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_basic_set_plain_is_equal = ISLFunction.create(
    "isl_basic_set_plain_is_equal",
    Keep("BasicSet"),
    Keep("BasicSet"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_basic_set_is_equal = ISLFunction.create(
    "isl_basic_set_is_equal",
    Keep("BasicSet"),
    Keep("BasicSet"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_basic_set_is_disjoint = ISLFunction.create(
    "isl_basic_set_is_disjoint",
    Keep("BasicSet"),
    Keep("BasicSet"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_basic_set_is_subset = ISLFunction.create(
    "isl_basic_set_is_subset",
    Keep("BasicSet"),
    Keep("BasicSet"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_basic_set_params = ISLFunction.create(
    "isl_basic_set_params",
    Take("BasicSet"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_eliminate = ISLFunction.create(
    "isl_basic_set_eliminate",
    Take("BasicSet"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_from_params = ISLFunction.create(
    "isl_basic_set_from_params",
    Take("BasicSet"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_fix_si = ISLFunction.create(
    "isl_basic_set_fix_si",
    Take("BasicSet"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_int),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_fix_val = ISLFunction.create(
    "isl_basic_set_fix_val",
    Take("BasicSet"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Take("Val"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_lower_bound_val = ISLFunction.create(
    "isl_basic_set_lower_bound_val",
    Take("BasicSet"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Take("Val"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_upper_bound_val = ISLFunction.create(
    "isl_basic_set_upper_bound_val",
    Take("BasicSet"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Take("Val"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_detect_equalities = ISLFunction.create(
    "isl_basic_set_detect_equalities",
    Take("BasicSet"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_remove_redundancies = ISLFunction.create(
    "isl_basic_set_remove_redundancies",
    Take("BasicSet"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_affine_hull = ISLFunction.create(
    "isl_basic_set_affine_hull",
    Take("BasicSet"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_drop_constraints_involving_dims = ISLFunction.create(
    "isl_basic_set_drop_constraints_involving_dims",
    Take("BasicSet"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_drop_constraints_not_involving_dims = ISLFunction.create(
    "isl_basic_set_drop_constraints_not_involving_dims",
    Take("BasicSet"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_sample = ISLFunction.create(
    "isl_basic_set_sample",
    Take("BasicSet"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_max_val = ISLFunction.create(
    "isl_basic_set_max_val",
    Keep("BasicSet"),
    Keep("Aff"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_basic_set_dim_max_val = ISLFunction.create(
    "isl_basic_set_dim_max_val",
    Take("BasicSet"),
    Param(int, ctype=c_int),
    return_=Give("Val"),
    lib=_lib,
)

_isl_basic_set_coefficients = ISLFunction.create(
    "isl_basic_set_coefficients",
    Take("BasicSet"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_solutions = ISLFunction.create(
    "isl_basic_set_solutions",
    Take("BasicSet"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_unwrap = ISLFunction.create(
    "isl_basic_set_unwrap",
    Take("BasicSet"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_basic_set_flatten = ISLFunction.create(
    "isl_basic_set_flatten",
    Take("BasicSet"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_lift = ISLFunction.create(
    "isl_basic_set_lift",
    Take("BasicSet"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_align_params = ISLFunction.create(
    "isl_basic_set_align_params",
    Take("BasicSet"),
    Take("Space"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_drop_unused_params = ISLFunction.create(
    "isl_basic_set_drop_unused_params",
    Take("BasicSet"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_add_dims = ISLFunction.create(
    "isl_basic_set_add_dims",
    Take("BasicSet"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_insert_dims = ISLFunction.create(
    "isl_basic_set_insert_dims",
    Take("BasicSet"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_move_dims = ISLFunction.create(
    "isl_basic_set_move_dims",
    Take("BasicSet"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_intersect_params = ISLFunction.create(
    "isl_basic_set_intersect_params",
    Take("BasicSet"),
    Take("BasicSet"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_union = ISLFunction.create(
    "isl_basic_set_union",
    Take("BasicSet"),
    Take("BasicSet"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_basic_set_apply = ISLFunction.create(
    "isl_basic_set_apply",
    Take("BasicSet"),
    Take("BasicMap"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_flat_product = ISLFunction.create(
    "isl_basic_set_flat_product",
    Take("BasicSet"),
    Take("BasicSet"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_gist = ISLFunction.create(
    "isl_basic_set_gist",
    Take("BasicSet"),
    Take("BasicSet"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_partial_lexmin = ISLFunction.create(
    "isl_basic_set_partial_lexmin",
    Take("BasicSet"),
    Take("BasicSet"),
    Give("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_basic_set_partial_lexmax = ISLFunction.create(
    "isl_basic_set_partial_lexmax",
    Take("BasicSet"),
    Take("BasicSet"),
    Give("Set"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_basic_set_lexmin = ISLFunction.create(
    "isl_basic_set_lexmin",
    Take("BasicSet"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_basic_set_lexmax = ISLFunction.create(
    "isl_basic_set_lexmax",
    Take("BasicSet"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_basic_set_partial_lexmin_pw_multi_aff = ISLFunction.create(
    "isl_basic_set_partial_lexmin_pw_multi_aff",
    Take("BasicSet"),
    Take("BasicSet"),
    Give("Set"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_basic_set_partial_lexmax_pw_multi_aff = ISLFunction.create(
    "isl_basic_set_partial_lexmax_pw_multi_aff",
    Take("BasicSet"),
    Take("BasicSet"),
    Give("Set"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_basic_set_compute_vertices = ISLFunction.create(
    "isl_basic_set_compute_vertices",
    Keep("BasicSet"),
    return_=Give("Vertices"),
    lib=_lib,
)

_isl_basic_set_read_from_str = ISLFunction.create(
    "isl_basic_set_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_intersect = ISLFunction.create(
    "isl_basic_set_intersect",
    Take("BasicSet"),
    Take("BasicSet"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_basic_set_project_out = ISLFunction.create(
    "isl_basic_set_project_out",
    Take("BasicSet"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("BasicSet"),
    lib=_lib,
)
