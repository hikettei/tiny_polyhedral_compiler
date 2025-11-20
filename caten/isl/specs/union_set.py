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
    from .basic_set import BasicSet
    from .basic_set_list import BasicSetList
    from .context import Context
    from .multi_aff import MultiAff
    from .multi_union_pw_aff import MultiUnionPwAff
    from .multi_val import MultiVal
    from .point import Point
    from .pw_multi_aff import PwMultiAff
    from .schedule import Schedule
    from .set import Set
    from .set_list import SetList
    from .space import Space
    from .union_map import UnionMap
    from .union_pw_multi_aff import UnionPwMultiAff

_lib = load_libisl()

class UnionSet(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_union_set_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_union_set_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_union_set_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_union_set_free(handle)

    def __str__(self) -> str:
        return _isl_union_set_to_str(self)

    def __repr__(self) -> str:
        return f"UnionSet({self.__str__()})"

    def get_space(self) -> "Space":
        return _isl_union_set_get_space(self)

    def dim(self, type: int) -> int:
        return _isl_union_set_dim(self, type)

    def reset_user(self) -> "UnionSet":
        return _isl_union_set_reset_user(self)

    @classmethod
    def empty_ctx(cls) -> "UnionSet":
        return _isl_union_set_empty_ctx()

    @classmethod
    def empty_space(cls, space: "Space") -> "UnionSet":
        return _isl_union_set_empty_space(space)

    @classmethod
    def empty(cls, space: "Space") -> "UnionSet":
        return _isl_union_set_empty(space)

    def universe(self) -> "UnionSet":
        return _isl_union_set_universe(self)

    @classmethod
    def from_basic_set(cls, bset: "BasicSet") -> "UnionSet":
        return _isl_union_set_from_basic_set(bset)

    @classmethod
    def from_set(cls, set: "Set") -> "UnionSet":
        return _isl_union_set_from_set(set)

    def isa_set(self) -> bool:
        return _isl_union_set_isa_set(self)

    def as_set(self) -> "Set":
        return _isl_union_set_as_set(self)

    def compute_divs(self) -> "UnionSet":
        return _isl_union_set_compute_divs(self)

    def remove_divs(self) -> "UnionSet":
        return _isl_union_set_remove_divs(self)

    def foreach_set(self, fn: Any, user: Any = None) -> int:
        return _isl_union_set_foreach_set(self, fn, user)

    def every_set(self, test: Any, user: Any = None) -> bool:
        return _isl_union_set_every_set(self, test, user)

    def n_set(self) -> int:
        return _isl_union_set_n_set(self)

    def extract_set(self, space: "Space") -> "Set":
        return _isl_union_set_extract_set(self, space)

    def get_basic_set_list(self) -> "BasicSetList":
        return _isl_union_set_get_basic_set_list(self)

    def get_set_list(self) -> "SetList":
        return _isl_union_set_get_set_list(self)

    @classmethod
    def from_point(cls, pnt: "Point") -> "UnionSet":
        return _isl_union_set_from_point(pnt)

    def foreach_point(self, fn: Any, user: Any = None) -> int:
        return _isl_union_set_foreach_point(self, fn, user)

    def sample_point(self) -> "Point":
        return _isl_union_set_sample_point(self)

    @classmethod
    def read_from_file(cls, input: None) -> "UnionSet":
        return _isl_union_set_read_from_file(input)

    def is_empty(self) -> bool:
        return _isl_union_set_is_empty(self)

    def is_params(self) -> bool:
        return _isl_union_set_is_params(self)

    def is_equal(self, uset2: "UnionSet") -> bool:
        return _isl_union_set_is_equal(self, uset2)

    def is_disjoint(self, uset2: "UnionSet") -> bool:
        return _isl_union_set_is_disjoint(self, uset2)

    def is_subset(self, uset2: "UnionSet") -> bool:
        return _isl_union_set_is_subset(self, uset2)

    def is_strict_subset(self, uset2: "UnionSet") -> bool:
        return _isl_union_set_is_strict_subset(self, uset2)

    def project_out(self, type: int, first: int, n: int) -> "UnionSet":
        return _isl_union_set_project_out(self, type, first, n)

    def project_out_all_params(self) -> "UnionSet":
        return _isl_union_set_project_out_all_params(self)

    def params(self) -> "Set":
        return _isl_union_set_params(self)

    def wrapped_domain_map(self) -> "UnionMap":
        return _isl_union_set_wrapped_domain_map(self)

    def identity(self) -> "UnionMap":
        return _isl_union_set_identity(self)

    def identity_union_pw_multi_aff(self) -> "UnionPwMultiAff":
        return _isl_union_set_identity_union_pw_multi_aff(self)

    def coalesce(self) -> "UnionSet":
        return _isl_union_set_coalesce(self)

    def detect_equalities(self) -> "UnionSet":
        return _isl_union_set_detect_equalities(self)

    def remove_redundancies(self) -> "UnionSet":
        return _isl_union_set_remove_redundancies(self)

    def affine_hull(self) -> "UnionSet":
        return _isl_union_set_affine_hull(self)

    def polyhedral_hull(self) -> "UnionSet":
        return _isl_union_set_polyhedral_hull(self)

    def min_multi_union_pw_aff(self, obj: "MultiUnionPwAff") -> "MultiVal":
        return _isl_union_set_min_multi_union_pw_aff(self, obj)

    def coefficients(self) -> "UnionSet":
        return _isl_union_set_coefficients(self)

    def solutions(self) -> "UnionSet":
        return _isl_union_set_solutions(self)

    def unwrap(self) -> "UnionMap":
        return _isl_union_set_unwrap(self)

    def lift(self) -> "UnionSet":
        return _isl_union_set_lift(self)

    def drop_unused_params(self) -> "UnionSet":
        return _isl_union_set_drop_unused_params(self)

    def intersect_params(self, set: "Set") -> "UnionSet":
        return _isl_union_set_intersect_params(self, set)

    def intersect(self, uset2: "UnionSet") -> "UnionSet":
        return _isl_union_set_intersect(self, uset2)

    def union(self, uset2: "UnionSet") -> "UnionSet":
        return _isl_union_set_union(self, uset2)

    def subtract(self, uset2: "UnionSet") -> "UnionSet":
        return _isl_union_set_subtract(self, uset2)

    def apply(self, umap: "UnionMap") -> "UnionSet":
        return _isl_union_set_apply(self, umap)

    def preimage_multi_aff(self, ma: "MultiAff") -> "UnionSet":
        return _isl_union_set_preimage_multi_aff(self, ma)

    def preimage_pw_multi_aff(self, pma: "PwMultiAff") -> "UnionSet":
        return _isl_union_set_preimage_pw_multi_aff(self, pma)

    def preimage_union_pw_multi_aff(self, upma: "UnionPwMultiAff") -> "UnionSet":
        return _isl_union_set_preimage_union_pw_multi_aff(self, upma)

    def product(self, uset2: "UnionSet") -> "UnionSet":
        return _isl_union_set_product(self, uset2)

    def gist(self, context: "UnionSet") -> "UnionSet":
        return _isl_union_set_gist(self, context)

    def gist_params(self, set: "Set") -> "UnionSet":
        return _isl_union_set_gist_params(self, set)

    def lexmin(self) -> "UnionSet":
        return _isl_union_set_lexmin(self)

    def lexmax(self) -> "UnionSet":
        return _isl_union_set_lexmax(self)

    def compute_schedule(self, validity: "UnionMap", proximity: "UnionMap") -> "Schedule":
        return _isl_union_set_compute_schedule(self, validity, proximity)


register_type("UnionSet", UnionSet)

_isl_union_set_get_space = ISLFunction.create(
    "isl_union_set_get_space",
    Keep("UnionSet"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_union_set_dim = ISLFunction.create(
    "isl_union_set_dim",
    Keep("UnionSet"),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_set_reset_user = ISLFunction.create(
    "isl_union_set_reset_user",
    Take("UnionSet"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_empty_ctx = ISLFunction.create(
    "isl_union_set_empty_ctx",
    Context(),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_empty_space = ISLFunction.create(
    "isl_union_set_empty_space",
    Take("Space"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_empty = ISLFunction.create(
    "isl_union_set_empty",
    Take("Space"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_universe = ISLFunction.create(
    "isl_union_set_universe",
    Take("UnionSet"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_from_basic_set = ISLFunction.create(
    "isl_union_set_from_basic_set",
    Take("BasicSet"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_from_set = ISLFunction.create(
    "isl_union_set_from_set",
    Take("Set"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_isa_set = ISLFunction.create(
    "isl_union_set_isa_set",
    Keep("UnionSet"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_set_as_set = ISLFunction.create(
    "isl_union_set_as_set",
    Take("UnionSet"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_union_set_copy = ISLFunction.create(
    "isl_union_set_copy",
    Keep("UnionSet"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_free = ISLFunction.create(
    "isl_union_set_free",
    Take("UnionSet"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_compute_divs = ISLFunction.create(
    "isl_union_set_compute_divs",
    Take("UnionSet"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_remove_divs = ISLFunction.create(
    "isl_union_set_remove_divs",
    Take("UnionSet"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_foreach_set = ISLFunction.create(
    "isl_union_set_foreach_set",
    Keep("UnionSet"),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_set_every_set = ISLFunction.create(
    "isl_union_set_every_set",
    Keep("UnionSet"),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_set_n_set = ISLFunction.create(
    "isl_union_set_n_set",
    Keep("UnionSet"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_set_extract_set = ISLFunction.create(
    "isl_union_set_extract_set",
    Keep("UnionSet"),
    Take("Space"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_union_set_get_basic_set_list = ISLFunction.create(
    "isl_union_set_get_basic_set_list",
    Keep("UnionSet"),
    return_=Give("BasicSetList"),
    lib=_lib,
)

_isl_union_set_get_set_list = ISLFunction.create(
    "isl_union_set_get_set_list",
    Keep("UnionSet"),
    return_=Give("SetList"),
    lib=_lib,
)

_isl_union_set_from_point = ISLFunction.create(
    "isl_union_set_from_point",
    Take("Point"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_foreach_point = ISLFunction.create(
    "isl_union_set_foreach_point",
    Keep("UnionSet"),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_set_sample_point = ISLFunction.create(
    "isl_union_set_sample_point",
    Take("UnionSet"),
    return_=Give("Point"),
    lib=_lib,
)

_isl_union_set_read_from_file = ISLFunction.create(
    "isl_union_set_read_from_file",
    Context(),
    Param(None, ctype=c_void_p),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_read_from_str = ISLFunction.create(
    "isl_union_set_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_to_str = ISLFunction.create(
    "isl_union_set_to_str",
    Keep("UnionSet"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_union_set_is_empty = ISLFunction.create(
    "isl_union_set_is_empty",
    Keep("UnionSet"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_set_is_params = ISLFunction.create(
    "isl_union_set_is_params",
    Keep("UnionSet"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_set_is_equal = ISLFunction.create(
    "isl_union_set_is_equal",
    Keep("UnionSet"),
    Keep("UnionSet"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_set_is_disjoint = ISLFunction.create(
    "isl_union_set_is_disjoint",
    Keep("UnionSet"),
    Keep("UnionSet"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_set_is_subset = ISLFunction.create(
    "isl_union_set_is_subset",
    Keep("UnionSet"),
    Keep("UnionSet"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_set_is_strict_subset = ISLFunction.create(
    "isl_union_set_is_strict_subset",
    Keep("UnionSet"),
    Keep("UnionSet"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_set_project_out = ISLFunction.create(
    "isl_union_set_project_out",
    Take("UnionSet"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_project_out_all_params = ISLFunction.create(
    "isl_union_set_project_out_all_params",
    Take("UnionSet"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_params = ISLFunction.create(
    "isl_union_set_params",
    Take("UnionSet"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_union_set_wrapped_domain_map = ISLFunction.create(
    "isl_union_set_wrapped_domain_map",
    Take("UnionSet"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_set_identity = ISLFunction.create(
    "isl_union_set_identity",
    Take("UnionSet"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_set_identity_union_pw_multi_aff = ISLFunction.create(
    "isl_union_set_identity_union_pw_multi_aff",
    Take("UnionSet"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_set_coalesce = ISLFunction.create(
    "isl_union_set_coalesce",
    Take("UnionSet"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_detect_equalities = ISLFunction.create(
    "isl_union_set_detect_equalities",
    Take("UnionSet"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_remove_redundancies = ISLFunction.create(
    "isl_union_set_remove_redundancies",
    Take("UnionSet"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_affine_hull = ISLFunction.create(
    "isl_union_set_affine_hull",
    Take("UnionSet"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_polyhedral_hull = ISLFunction.create(
    "isl_union_set_polyhedral_hull",
    Take("UnionSet"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_min_multi_union_pw_aff = ISLFunction.create(
    "isl_union_set_min_multi_union_pw_aff",
    Keep("UnionSet"),
    Keep("MultiUnionPwAff"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_union_set_coefficients = ISLFunction.create(
    "isl_union_set_coefficients",
    Take("UnionSet"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_solutions = ISLFunction.create(
    "isl_union_set_solutions",
    Take("UnionSet"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_unwrap = ISLFunction.create(
    "isl_union_set_unwrap",
    Take("UnionSet"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_set_lift = ISLFunction.create(
    "isl_union_set_lift",
    Take("UnionSet"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_drop_unused_params = ISLFunction.create(
    "isl_union_set_drop_unused_params",
    Take("UnionSet"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_intersect_params = ISLFunction.create(
    "isl_union_set_intersect_params",
    Take("UnionSet"),
    Take("Set"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_intersect = ISLFunction.create(
    "isl_union_set_intersect",
    Take("UnionSet"),
    Take("UnionSet"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_union = ISLFunction.create(
    "isl_union_set_union",
    Take("UnionSet"),
    Take("UnionSet"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_subtract = ISLFunction.create(
    "isl_union_set_subtract",
    Take("UnionSet"),
    Take("UnionSet"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_apply = ISLFunction.create(
    "isl_union_set_apply",
    Take("UnionSet"),
    Take("UnionMap"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_preimage_multi_aff = ISLFunction.create(
    "isl_union_set_preimage_multi_aff",
    Take("UnionSet"),
    Take("MultiAff"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_preimage_pw_multi_aff = ISLFunction.create(
    "isl_union_set_preimage_pw_multi_aff",
    Take("UnionSet"),
    Take("PwMultiAff"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_preimage_union_pw_multi_aff = ISLFunction.create(
    "isl_union_set_preimage_union_pw_multi_aff",
    Take("UnionSet"),
    Take("UnionPwMultiAff"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_product = ISLFunction.create(
    "isl_union_set_product",
    Take("UnionSet"),
    Take("UnionSet"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_gist = ISLFunction.create(
    "isl_union_set_gist",
    Take("UnionSet"),
    Take("UnionSet"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_gist_params = ISLFunction.create(
    "isl_union_set_gist_params",
    Take("UnionSet"),
    Take("Set"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_lexmin = ISLFunction.create(
    "isl_union_set_lexmin",
    Take("UnionSet"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_lexmax = ISLFunction.create(
    "isl_union_set_lexmax",
    Take("UnionSet"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_set_compute_schedule = ISLFunction.create(
    "isl_union_set_compute_schedule",
    Take("UnionSet"),
    Take("UnionMap"),
    Take("UnionMap"),
    return_=Give("Schedule"),
    lib=_lib,
)
