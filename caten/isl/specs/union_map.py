from __future__ import annotations

from ctypes import c_int, c_void_p

from ..ffi import FfiPointer, load_libisl
from ..func import ISLFunction
from ..obj import ISLObject
from ..qualifier import Give, Keep, Null, Param, Take
from .basic_map import BasicMap
from .context import Context
from .map import Map
from .map_list import MapList
from .set import Set
from .space import Space
from .union_pw_aff import MultiUnionPwAff, UnionPwAff, UnionPwMultiAff
from .union_set import UnionSet

_lib = load_libisl()


class UnionMap(ISLObject):
    """Wrapper around ``isl_union_map``."""

    __slots__ = ()

    def __init__(self, handle_or_spec: str | FfiPointer) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_union_map_read_from_str(handle_or_spec, return_raw_pointer=True)
        else:
            handle = handle_or_spec
        super().__init__(handle)

    @classmethod
    def from_str(cls, spec: str) -> "UnionMap":
        return _isl_union_map_read_from_str(spec)

    @classmethod
    def from_map(cls, m: Map) -> "UnionMap":
        return _isl_union_map_from_map(m)

    @classmethod
    def from_basic_map(cls, bm: BasicMap) -> "UnionMap":
        return _isl_union_map_from_basic_map(bm)

    def copy_handle(self) -> FfiPointer:
        return _isl_union_map_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_union_map_free(handle)

    # queries
    def is_empty(self) -> bool:
        return _isl_union_map_is_empty(self)

    def is_equal(self, other: "UnionMap") -> bool:
        return _isl_union_map_is_equal(self, other)

    def n_map(self) -> int:
        return _isl_union_map_n_map(self)

    def get_map_list(self) -> "MapList":
        return _isl_union_map_get_map_list(self)

    def get_space(self) -> Space:
        return _isl_union_map_get_space(self)

    def dim(self, dim_type: int) -> int:
        return _isl_union_map_dim(self, dim_type)

    def get_dim_id(self, dim_type: int, pos: int) -> object:
        return _isl_union_map_get_dim_id(self, dim_type, pos)

    def find_dim_by_name(self, dim_type: int, name: str) -> int:
        return _isl_union_map_find_dim_by_name(self, dim_type, name)

    def reset_user(self) -> "UnionMap":
        return _isl_union_map_reset_user(self)

    @classmethod
    def empty(cls, set_space: Space) -> "UnionMap":
        return _isl_union_map_empty_space(set_space)

    @classmethod
    def empty_ctx(cls, ctx: Context) -> "UnionMap":
        return _isl_union_map_empty_ctx(ctx)

    @classmethod
    def universe(cls, set_space: Space) -> "UnionMap":
        return _isl_union_map_universe(set_space)

    def isa_map(self) -> bool:
        return _isl_union_map_isa_map(self)

    def as_map(self) -> Map:
        return _isl_union_map_as_map(self)

    def as_union_pw_multi_aff(self) -> UnionPwMultiAff:
        return _isl_union_map_as_union_pw_multi_aff(self)

    def as_multi_union_pw_aff(self) -> MultiUnionPwAff:
        return _isl_union_map_as_multi_union_pw_aff(self)

    @classmethod
    def from_union_pw_aff(cls, upa: UnionPwAff) -> "UnionMap":
        return _isl_union_map_from_union_pw_aff(upa)

    @classmethod
    def from_union_pw_multi_aff(cls, upma: UnionPwMultiAff) -> "UnionMap":
        return _isl_union_map_from_union_pw_multi_aff(upma)

    @classmethod
    def from_multi_union_pw_aff(cls, mupa: MultiUnionPwAff) -> "UnionMap":
        return _isl_union_map_from_multi_union_pw_aff(mupa)

    # divs / foreach / extract
    def compute_divs(self) -> "UnionMap":
        return _isl_union_map_compute_divs(self)

    def remove_divs(self) -> "UnionMap":
        return _isl_union_map_remove_divs(self)

    def foreach_map(self, fn: object) -> int:
        return _isl_union_map_foreach_map(self, fn, None)

    def every_map(self, test_fn: object) -> bool:
        return bool(_isl_union_map_every_map(self, test_fn, None))

    def extract_map(self, space: Space) -> Map:
        return _isl_union_map_extract_map(self, space)

    @classmethod
    def read_from_file(cls, ctx: Context, file_ptr: int) -> "UnionMap":
        return _isl_union_map_read_from_file(ctx, file_ptr)

    def remove_map_if(self, pred_fn: object) -> "UnionMap":
        return _isl_union_map_remove_map_if(self, pred_fn, None)

    # domain / range
    def domain(self) -> UnionSet:
        return _isl_union_map_domain(self)

    def range(self) -> UnionSet:
        return _isl_union_map_range(self)

    def domain_map(self) -> "UnionMap":
        return _isl_union_map_domain_map(self)

    def range_map(self) -> "UnionMap":
        return _isl_union_map_range_map(self)

    def domain_map_union_pw_multi_aff(self) -> UnionPwMultiAff:
        return _isl_union_map_domain_map_union_pw_multi_aff(self)

    def from_domain(self, uset: UnionSet) -> "UnionMap":
        return _isl_union_map_from_domain(uset)

    def from_range(self, uset: UnionSet) -> "UnionMap":
        return _isl_union_map_from_range(uset)

    def from_domain_and_range(self, dom: UnionSet, ran: UnionSet) -> "UnionMap":
        return _isl_union_map_from_domain_and_range(dom, ran)

    # intersect / apply
    def intersect_params(self, params: Set) -> "UnionMap":
        return _isl_union_map_intersect_params(self, params)

    def intersect_domain(self, uset: UnionSet) -> "UnionMap":
        return _isl_union_map_intersect_domain(self, uset)

    def intersect_range(self, uset: UnionSet) -> "UnionMap":
        return _isl_union_map_intersect_range(self, uset)
    
    def intersect_domain_union_set(self, uset: UnionSet) -> "UnionMap":
        return _isl_union_map_intersect_domain_union_set(self, uset)

    def intersect_range_union_set(self, uset: UnionSet) -> "UnionMap":
        return _isl_union_map_intersect_range_union_set(self, uset)

    def intersect_domain_space(self, space: Space) -> "UnionMap":
        return _isl_union_map_intersect_domain_space(self, space)

    def intersect_range_space(self, space: Space) -> "UnionMap":
        return _isl_union_map_intersect_range_space(self, space)

    def intersect_domain_factor_domain(self) -> "UnionMap":
        return _isl_union_map_intersect_domain_factor_domain(self)

    def intersect_domain_factor_range(self) -> "UnionMap":
        return _isl_union_map_intersect_domain_factor_range(self)

    def intersect_range_factor_domain(self) -> "UnionMap":
        return _isl_union_map_intersect_range_factor_domain(self)

    def intersect_range_factor_range(self) -> "UnionMap":
        return _isl_union_map_intersect_range_factor_range(self)

    def intersect_domain_wrapped_domain_union_set(self, uset: UnionSet) -> "UnionMap":
        return _isl_union_map_intersect_domain_wrapped_domain_union_set(self, uset)

    def intersect_range_wrapped_domain_union_set(self, uset: UnionSet) -> "UnionMap":
        return _isl_union_map_intersect_range_wrapped_domain_union_set(self, uset)

    def apply_domain(self, other: "UnionMap") -> "UnionMap":
        return _isl_union_map_apply_domain(self, other)

    def apply_range(self, other: "UnionMap") -> "UnionMap":
        return _isl_union_map_apply_range(self, other)

    def subtract_domain(self, uset: UnionSet) -> "UnionMap":
        return _isl_union_map_subtract_domain(self, uset)

    def subtract_range(self, uset: UnionSet) -> "UnionMap":
        return _isl_union_map_subtract_range(self, uset)

    # gist / lexmin / lexmax
    def gist_params(self, params: Set) -> "UnionMap":
        return _isl_union_map_gist_params(self, params)

    def gist_domain(self, context: UnionSet) -> "UnionMap":
        return _isl_union_map_gist_domain(self, context)

    def gist_range(self, context: UnionSet) -> "UnionMap":
        return _isl_union_map_gist_range(self, context)

    def lexmin(self) -> "UnionMap":
        return _isl_union_map_lexmin(self)

    def lexmax(self) -> "UnionMap":
        return _isl_union_map_lexmax(self)

    def simple_hull(self) -> "UnionMap":
        return _isl_union_map_simple_hull(self)

    def drop_unused_params(self) -> "UnionMap":
        return _isl_union_map_drop_unused_params(self)

    # project
    def project_out(self, dim_type: int, first: int, n: int) -> "UnionMap":
        return _isl_union_map_project_out(self, dim_type, first, n)

    def project_out_all_params(self) -> "UnionMap":
        return _isl_union_map_project_out_all_params(self)

    def project_out_param_id(self, id_: object) -> "UnionMap":
        return _isl_union_map_project_out_param_id(self, id_)

    def project_out_param_id_list(self, id_list: object) -> "UnionMap":
        return _isl_union_map_project_out_param_id_list(self, id_list)

    def params(self) -> Set:
        return _isl_union_map_params(self)

    # reverse / wrap / curry
    def reverse(self) -> "UnionMap":
        return _isl_union_map_reverse(self)

    def domain_reverse(self) -> "UnionMap":
        return _isl_union_map_domain_reverse(self)

    def range_reverse(self) -> "UnionMap":
        return _isl_union_map_range_reverse(self)

    def bind_range(self, multi_id: object) -> "UnionMap":
        return _isl_union_map_bind_range(self, multi_id)

    def wrap(self) -> "UnionMap":
        if _isl_union_map_wrap is None:
            raise NotImplementedError("isl_union_map_wrap not available in linked libisl")
        return _isl_union_map_wrap(self)

    def zip(self) -> "UnionMap":
        if _isl_union_map_zip is None:
            raise NotImplementedError("isl_union_map_zip not available in linked libisl")
        return _isl_union_map_zip(self)

    def curry(self) -> "UnionMap":
        return _isl_union_map_curry(self)

    def uncurry(self) -> "UnionMap":
        return _isl_union_map_uncurry(self)

    def range_curry(self) -> "UnionMap":
        return _isl_union_map_range_curry(self)

    # deltas / coalesce / hulls
    def deltas(self) -> "UnionSet":
        return _isl_union_map_deltas(self)

    def deltas_map(self) -> "UnionMap":
        return _isl_union_map_deltas_map(self)

    def coalesce(self) -> "UnionMap":
        return _isl_union_map_coalesce(self)

    def detect_equalities(self) -> "UnionMap":
        return _isl_union_map_detect_equalities(self)

    def remove_redundancies(self) -> "UnionMap":
        return _isl_union_map_remove_redundancies(self)

    def affine_hull(self) -> "UnionMap":
        return _isl_union_map_affine_hull(self)

    def polyhedral_hull(self) -> "UnionMap":
        return _isl_union_map_polyhedral_hull(self)

    # factor / product
    def domain_product(self, other: "UnionMap") -> "UnionMap":
        return _isl_union_map_domain_product(self, other)

    def range_product(self, other: "UnionMap") -> "UnionMap":
        return _isl_union_map_range_product(self, other)

    def flat_domain_product(self, other: "UnionMap") -> "UnionMap":
        return _isl_union_map_flat_domain_product(self, other)

    def flat_range_product(self, other: "UnionMap") -> "UnionMap":
        return _isl_union_map_flat_range_product(self, other)

    def factor_domain(self) -> "UnionMap":
        return _isl_union_map_factor_domain(self)

    def factor_range(self) -> "UnionMap":
        return _isl_union_map_factor_range(self)

    def domain_factor_domain(self) -> "UnionMap":
        return _isl_union_map_domain_factor_domain(self)

    def domain_factor_range(self) -> "UnionMap":
        return _isl_union_map_domain_factor_range(self)

    def range_factor_domain(self) -> "UnionMap":
        return _isl_union_map_range_factor_domain(self)

    def range_factor_range(self) -> "UnionMap":
        return _isl_union_map_range_factor_range(self)

    # power / transitive
    def fixed_power_val(self, val: object) -> "UnionMap":
        return _isl_union_map_fixed_power_val(self, val)

    def power(self) -> "UnionMap":
        return _isl_union_map_power(self, None)

    def transitive_closure(self) -> "UnionMap":
        return _isl_union_map_transitive_closure(self, None)

    # predicates
    def plain_is_empty(self) -> bool:
        return _isl_union_map_plain_is_empty(self)

    def is_single_valued(self) -> bool:
        return _isl_union_map_is_single_valued(self)

    def plain_is_injective(self) -> bool:
        return _isl_union_map_plain_is_injective(self)

    def is_injective(self) -> bool:
        return _isl_union_map_is_injective(self)

    def is_bijective(self) -> bool:
        return _isl_union_map_is_bijective(self)

    def is_identity(self) -> bool:
        return _isl_union_map_is_identity(self)

    def involves_dims(self, dim_type: int, first: int, n: int) -> bool:
        return _isl_union_map_involves_dims(self, dim_type, first, n)

    def is_disjoint(self, other: "UnionMap") -> bool:
        return _isl_union_map_is_disjoint(self, other)

    def is_subset(self, other: "UnionMap") -> bool:
        return _isl_union_map_is_subset(self, other)

    def is_strict_subset(self, other: "UnionMap") -> bool:
        return _isl_union_map_is_strict_subset(self, other)

    # apply / preimage with qpolynomial and multi_aff variants
    def apply_union_pw_qpolynomial_fold(self, _pwf: object) -> object:
        return _isl_union_map_apply_union_pw_qpolynomial_fold(self, _pwf, None)

    def preimage_domain_multi_aff(self, ma: object) -> "UnionMap":
        return _isl_union_map_preimage_domain_multi_aff(self, ma)

    def preimage_range_multi_aff(self, ma: object) -> "UnionMap":
        return _isl_union_map_preimage_range_multi_aff(self, ma)

    def preimage_domain_pw_multi_aff(self, pma: object) -> "UnionMap":
        return _isl_union_map_preimage_domain_pw_multi_aff(self, pma)

    def preimage_range_pw_multi_aff(self, pma: object) -> "UnionMap":
        return _isl_union_map_preimage_range_pw_multi_aff(self, pma)

    def preimage_domain_union_pw_multi_aff(self, upma: UnionPwMultiAff) -> "UnionMap":
        return _isl_union_map_preimage_domain_union_pw_multi_aff(self, upma)

    def preimage_range_union_pw_multi_aff(self, upma: UnionPwMultiAff) -> "UnionMap":
        return _isl_union_map_preimage_range_union_pw_multi_aff(self, upma)

    # comparisons with multi_union_pw_aff
    def eq_at_multi_union_pw_aff(self, mupa: MultiUnionPwAff) -> "UnionMap":
        return _isl_union_map_eq_at_multi_union_pw_aff(self, mupa)

    def lex_lt_at_multi_union_pw_aff(self, mupa: MultiUnionPwAff) -> "UnionMap":
        return _isl_union_map_lex_lt_at_multi_union_pw_aff(self, mupa)

    def lex_le_at_multi_union_pw_aff(self, mupa: MultiUnionPwAff) -> "UnionMap":
        return _isl_union_map_lex_le_at_multi_union_pw_aff(self, mupa)

    def lex_gt_at_multi_union_pw_aff(self, mupa: MultiUnionPwAff) -> "UnionMap":
        return _isl_union_map_lex_gt_at_multi_union_pw_aff(self, mupa)

    def lex_ge_at_multi_union_pw_aff(self, mupa: MultiUnionPwAff) -> "UnionMap":
        return _isl_union_map_lex_ge_at_multi_union_pw_aff(self, mupa)

    # transforms
    def union(self, other: "UnionMap") -> "UnionMap":
        return _isl_union_map_union(self, other)

    def intersect(self, other: "UnionMap") -> "UnionMap":
        return _isl_union_map_intersect(self, other)

    def subtract(self, other: "UnionMap") -> "UnionMap":
        return _isl_union_map_subtract(self, other)

    def product(self, other: "UnionMap") -> "UnionMap":
        return _isl_union_map_product(self, other)

    def gist(self, context: "UnionMap") -> "UnionMap":
        return _isl_union_map_gist(self, context)

    # string
    def __str__(self) -> str:  # pragma: no cover
        return _isl_union_map_to_str(self)

    def __repr__(self) -> str:  # pragma: no cover
        return f"UnionMap({self.__str__()})"


_isl_union_map_read_from_str = ISLFunction.create(
    _lib.isl_union_map_read_from_str,
    Context(),
    Param(str),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_copy = ISLFunction.create(
    _lib.isl_union_map_copy,
    Keep(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_free = ISLFunction.create(
    _lib.isl_union_map_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

_isl_union_map_from_map = ISLFunction.create(
    _lib.isl_union_map_from_map,
    Take(Map),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_from_basic_map = ISLFunction.create(
    _lib.isl_union_map_from_basic_map,
    Take(BasicMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_is_empty = ISLFunction.create(
    _lib.isl_union_map_is_empty,
    Keep(UnionMap),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_map_is_equal = ISLFunction.create(
    _lib.isl_union_map_is_equal,
    Keep(UnionMap),
    Keep(UnionMap),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_map_n_map = ISLFunction.create(
    _lib.isl_union_map_n_map,
    Keep(UnionMap),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_map_get_map_list = ISLFunction.create(
    _lib.isl_union_map_get_map_list,
    Keep(UnionMap),
    return_=Give(MapList),
    lib=_lib,
)

_isl_union_map_union = ISLFunction.create(
    _lib.isl_union_map_union,
    Take(UnionMap),
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_intersect = ISLFunction.create(
    _lib.isl_union_map_intersect,
    Take(UnionMap),
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_subtract = ISLFunction.create(
    _lib.isl_union_map_subtract,
    Take(UnionMap),
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_product = ISLFunction.create(
    _lib.isl_union_map_product,
    Take(UnionMap),
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_gist = ISLFunction.create(
    _lib.isl_union_map_gist,
    Take(UnionMap),
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_to_str = ISLFunction.create(
    _lib.isl_union_map_to_str,
    Keep(UnionMap),
    return_=Param(str),
    lib=_lib,
)

_isl_union_map_get_space = ISLFunction.create(
    _lib.isl_union_map_get_space,
    Keep(UnionMap),
    return_=Give(Space),
    lib=_lib,
)

_isl_union_map_dim = ISLFunction.create(
    _lib.isl_union_map_dim,
    Keep(UnionMap),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_map_get_dim_id = ISLFunction.create(
    _lib.isl_union_map_get_dim_id,
    Keep(UnionMap),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Param(None, ctype=c_void_p),
    lib=_lib,
)

_isl_union_map_find_dim_by_name = ISLFunction.create(
    _lib.isl_union_map_find_dim_by_name,
    Keep(UnionMap),
    Param(int, ctype=c_int),
    Param(str),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_map_reset_user = ISLFunction.create(
    _lib.isl_union_map_reset_user,
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_empty_ctx = ISLFunction.create(
    _lib.isl_union_map_empty_ctx,
    Context(),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_empty_space = ISLFunction.create(
    _lib.isl_union_map_empty_space,
    Take(Space),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_empty = ISLFunction.create(
    _lib.isl_union_map_empty,
    Take(Space),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_universe = ISLFunction.create(
    _lib.isl_union_map_universe,
    Take(Space),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_isa_map = ISLFunction.create(
    _lib.isl_union_map_isa_map,
    Keep(UnionMap),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_map_as_map = ISLFunction.create(
    _lib.isl_union_map_as_map,
    Take(UnionMap),
    return_=Give(Map),
    lib=_lib,
)

_isl_union_map_as_union_pw_multi_aff = ISLFunction.create(
    _lib.isl_union_map_as_union_pw_multi_aff,
    Take(UnionMap),
    return_=Give(UnionPwMultiAff),
    lib=_lib,
)

_isl_union_map_as_multi_union_pw_aff = ISLFunction.create(
    _lib.isl_union_map_as_multi_union_pw_aff,
    Take(UnionMap),
    return_=Give(MultiUnionPwAff),
    lib=_lib,
)

_isl_union_map_from_union_pw_aff = ISLFunction.create(
    _lib.isl_union_map_from_union_pw_aff,
    Take(UnionPwAff),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_from_union_pw_multi_aff = ISLFunction.create(
    _lib.isl_union_map_from_union_pw_multi_aff,
    Take(UnionPwMultiAff),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_from_multi_union_pw_aff = ISLFunction.create(
    _lib.isl_union_map_from_multi_union_pw_aff,
    Take(MultiUnionPwAff),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_compute_divs = ISLFunction.create(
    _lib.isl_union_map_compute_divs,
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_remove_divs = ISLFunction.create(
    _lib.isl_union_map_remove_divs,
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_foreach_map = ISLFunction.create(
    _lib.isl_union_map_foreach_map,
    Keep(UnionMap),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_map_every_map = ISLFunction.create(
    _lib.isl_union_map_every_map,
    Keep(UnionMap),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_map_extract_map = ISLFunction.create(
    _lib.isl_union_map_extract_map,
    Keep(UnionMap),
    Take(Space),
    return_=Give(Map),
    lib=_lib,
)

_isl_union_map_read_from_file = ISLFunction.create(
    _lib.isl_union_map_read_from_file,
    Context(),
    Param(None, ctype=c_void_p),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_remove_map_if = ISLFunction.create(
    _lib.isl_union_map_remove_map_if,
    Take(UnionMap),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_domain = ISLFunction.create(
    _lib.isl_union_map_domain,
    Take(UnionMap),
    return_=Give(UnionSet),
    lib=_lib,
)

_isl_union_map_range = ISLFunction.create(
    _lib.isl_union_map_range,
    Take(UnionMap),
    return_=Give(UnionSet),
    lib=_lib,
)

_isl_union_map_domain_map = ISLFunction.create(
    _lib.isl_union_map_domain_map,
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_domain_map_union_pw_multi_aff = ISLFunction.create(
    _lib.isl_union_map_domain_map_union_pw_multi_aff,
    Take(UnionMap),
    return_=Give(UnionPwMultiAff),
    lib=_lib,
)

_isl_union_map_range_map = ISLFunction.create(
    _lib.isl_union_map_range_map,
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_from_domain = ISLFunction.create(
    _lib.isl_union_map_from_domain,
    Take(UnionSet),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_from_range = ISLFunction.create(
    _lib.isl_union_map_from_range,
    Take(UnionSet),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_from_domain_and_range = ISLFunction.create(
    _lib.isl_union_map_from_domain_and_range,
    Take(UnionSet),
    Take(UnionSet),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_intersect_params = ISLFunction.create(
    _lib.isl_union_map_intersect_params,
    Take(UnionMap),
    Take(Set),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_intersect_domain = ISLFunction.create(
    _lib.isl_union_map_intersect_domain,
    Take(UnionMap),
    Take(UnionSet),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_intersect_range = ISLFunction.create(
    _lib.isl_union_map_intersect_range,
    Take(UnionMap),
    Take(UnionSet),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_intersect_domain_union_set = ISLFunction.create(
    _lib.isl_union_map_intersect_domain_union_set,
    Take(UnionMap),
    Take(UnionSet),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_intersect_range_union_set = ISLFunction.create(
    _lib.isl_union_map_intersect_range_union_set,
    Take(UnionMap),
    Take(UnionSet),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_intersect_domain_space = ISLFunction.create(
    _lib.isl_union_map_intersect_domain_space,
    Take(UnionMap),
    Take(Space),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_intersect_range_space = ISLFunction.create(
    _lib.isl_union_map_intersect_range_space,
    Take(UnionMap),
    Take(Space),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_intersect_domain_factor_domain = ISLFunction.create(
    _lib.isl_union_map_intersect_domain_factor_domain,
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_intersect_domain_factor_range = ISLFunction.create(
    _lib.isl_union_map_intersect_domain_factor_range,
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_intersect_range_factor_domain = ISLFunction.create(
    _lib.isl_union_map_intersect_range_factor_domain,
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_intersect_range_factor_range = ISLFunction.create(
    _lib.isl_union_map_intersect_range_factor_range,
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_intersect_domain_wrapped_domain_union_set = ISLFunction.create(
    _lib.isl_union_map_intersect_domain_wrapped_domain_union_set,
    Take(UnionMap),
    Take(UnionSet),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_intersect_range_wrapped_domain_union_set = ISLFunction.create(
    _lib.isl_union_map_intersect_range_wrapped_domain_union_set,
    Take(UnionMap),
    Take(UnionSet),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_apply_domain = ISLFunction.create(
    _lib.isl_union_map_apply_domain,
    Take(UnionMap),
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_apply_range = ISLFunction.create(
    _lib.isl_union_map_apply_range,
    Take(UnionMap),
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_subtract_domain = ISLFunction.create(
    _lib.isl_union_map_subtract_domain,
    Take(UnionMap),
    Take(UnionSet),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_subtract_range = ISLFunction.create(
    _lib.isl_union_map_subtract_range,
    Take(UnionMap),
    Take(UnionSet),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_gist_params = ISLFunction.create(
    _lib.isl_union_map_gist_params,
    Take(UnionMap),
    Take(Set),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_gist_domain = ISLFunction.create(
    _lib.isl_union_map_gist_domain,
    Take(UnionMap),
    Param(None, ctype=c_void_p),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_gist_range = ISLFunction.create(
    _lib.isl_union_map_gist_range,
    Take(UnionMap),
    Param(None, ctype=c_void_p),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_project_out = ISLFunction.create(
    _lib.isl_union_map_project_out,
    Take(UnionMap),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_project_out_all_params = ISLFunction.create(
    _lib.isl_union_map_project_out_all_params,
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_project_out_param_id = ISLFunction.create(
    _lib.isl_union_map_project_out_param_id,
    Take(UnionMap),
    Param(None, ctype=c_void_p),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_project_out_param_id_list = ISLFunction.create(
    _lib.isl_union_map_project_out_param_id_list,
    Take(UnionMap),
    Param(None, ctype=c_void_p),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_params = ISLFunction.create(
    _lib.isl_union_map_params,
    Take(UnionMap),
    return_=Give(Set),
    lib=_lib,
)

_isl_union_map_bind_range = ISLFunction.create(
    _lib.isl_union_map_bind_range,
    Take(UnionMap),
    Param(None, ctype=c_void_p),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_lexmin = ISLFunction.create(
    _lib.isl_union_map_lexmin,
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_lexmax = ISLFunction.create(
    _lib.isl_union_map_lexmax,
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_simple_hull = ISLFunction.create(
    _lib.isl_union_map_simple_hull,
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_drop_unused_params = ISLFunction.create(
    _lib.isl_union_map_drop_unused_params,
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_reverse = ISLFunction.create(
    _lib.isl_union_map_reverse,
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_domain_reverse = ISLFunction.create(
    _lib.isl_union_map_domain_reverse,
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_range_reverse = ISLFunction.create(
    _lib.isl_union_map_range_reverse,
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

try:
    _isl_union_map_wrap = ISLFunction.create(
        _lib.isl_union_map_wrap,
        Take(UnionMap),
        return_=Give(UnionMap),
        lib=_lib,
    )
except AttributeError:
    _isl_union_map_wrap = None  # type: ignore[assignment]

try:
    _isl_union_map_zip = ISLFunction.create(
        _lib.isl_union_map_zip,
        Take(UnionMap),
        return_=Give(UnionMap),
        lib=_lib,
    )
except AttributeError:
    _isl_union_map_zip = None  # type: ignore[assignment]

_isl_union_map_curry = ISLFunction.create(
    _lib.isl_union_map_curry,
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_uncurry = ISLFunction.create(
    _lib.isl_union_map_uncurry,
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_range_curry = ISLFunction.create(
    _lib.isl_union_map_range_curry,
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_deltas = ISLFunction.create(
    _lib.isl_union_map_deltas,
    Take(UnionMap),
    return_=Param(None, ctype=c_void_p),
    lib=_lib,
)

_isl_union_map_deltas_map = ISLFunction.create(
    _lib.isl_union_map_deltas_map,
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_coalesce = ISLFunction.create(
    _lib.isl_union_map_coalesce,
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_detect_equalities = ISLFunction.create(
    _lib.isl_union_map_detect_equalities,
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_remove_redundancies = ISLFunction.create(
    _lib.isl_union_map_remove_redundancies,
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_affine_hull = ISLFunction.create(
    _lib.isl_union_map_affine_hull,
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_polyhedral_hull = ISLFunction.create(
    _lib.isl_union_map_polyhedral_hull,
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_domain_product = ISLFunction.create(
    _lib.isl_union_map_domain_product,
    Take(UnionMap),
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_range_product = ISLFunction.create(
    _lib.isl_union_map_range_product,
    Take(UnionMap),
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_flat_domain_product = ISLFunction.create(
    _lib.isl_union_map_flat_domain_product,
    Take(UnionMap),
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_flat_range_product = ISLFunction.create(
    _lib.isl_union_map_flat_range_product,
    Take(UnionMap),
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_factor_domain = ISLFunction.create(
    _lib.isl_union_map_factor_domain,
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_factor_range = ISLFunction.create(
    _lib.isl_union_map_factor_range,
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_domain_factor_domain = ISLFunction.create(
    _lib.isl_union_map_domain_factor_domain,
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_domain_factor_range = ISLFunction.create(
    _lib.isl_union_map_domain_factor_range,
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_range_factor_domain = ISLFunction.create(
    _lib.isl_union_map_range_factor_domain,
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_range_factor_range = ISLFunction.create(
    _lib.isl_union_map_range_factor_range,
    Take(UnionMap),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_fixed_power_val = ISLFunction.create(
    _lib.isl_union_map_fixed_power_val,
    Take(UnionMap),
    Param(None, ctype=c_void_p),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_power = ISLFunction.create(
    _lib.isl_union_map_power,
    Take(UnionMap),
    Param(None, ctype=c_void_p),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_transitive_closure = ISLFunction.create(
    _lib.isl_union_map_transitive_closure,
    Take(UnionMap),
    Param(None, ctype=c_void_p),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_apply_union_pw_qpolynomial_fold = ISLFunction.create(
    _lib.isl_union_map_apply_union_pw_qpolynomial_fold,
    Take(UnionMap),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(None, ctype=c_void_p),
    lib=_lib,
)

_isl_union_map_preimage_domain_multi_aff = ISLFunction.create(
    _lib.isl_union_map_preimage_domain_multi_aff,
    Take(UnionMap),
    Param(None, ctype=c_void_p),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_preimage_range_multi_aff = ISLFunction.create(
    _lib.isl_union_map_preimage_range_multi_aff,
    Take(UnionMap),
    Param(None, ctype=c_void_p),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_preimage_domain_pw_multi_aff = ISLFunction.create(
    _lib.isl_union_map_preimage_domain_pw_multi_aff,
    Take(UnionMap),
    Param(None, ctype=c_void_p),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_preimage_range_pw_multi_aff = ISLFunction.create(
    _lib.isl_union_map_preimage_range_pw_multi_aff,
    Take(UnionMap),
    Param(None, ctype=c_void_p),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_preimage_domain_union_pw_multi_aff = ISLFunction.create(
    _lib.isl_union_map_preimage_domain_union_pw_multi_aff,
    Take(UnionMap),
    Param(None, ctype=c_void_p),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_preimage_range_union_pw_multi_aff = ISLFunction.create(
    _lib.isl_union_map_preimage_range_union_pw_multi_aff,
    Take(UnionMap),
    Param(None, ctype=c_void_p),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_eq_at_multi_union_pw_aff = ISLFunction.create(
    _lib.isl_union_map_eq_at_multi_union_pw_aff,
    Take(UnionMap),
    Param(None, ctype=c_void_p),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_lex_lt_at_multi_union_pw_aff = ISLFunction.create(
    _lib.isl_union_map_lex_lt_at_multi_union_pw_aff,
    Take(UnionMap),
    Param(None, ctype=c_void_p),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_lex_le_at_multi_union_pw_aff = ISLFunction.create(
    _lib.isl_union_map_lex_le_at_multi_union_pw_aff,
    Take(UnionMap),
    Param(None, ctype=c_void_p),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_lex_gt_at_multi_union_pw_aff = ISLFunction.create(
    _lib.isl_union_map_lex_gt_at_multi_union_pw_aff,
    Take(UnionMap),
    Param(None, ctype=c_void_p),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_lex_ge_at_multi_union_pw_aff = ISLFunction.create(
    _lib.isl_union_map_lex_ge_at_multi_union_pw_aff,
    Take(UnionMap),
    Param(None, ctype=c_void_p),
    return_=Give(UnionMap),
    lib=_lib,
)

_isl_union_map_plain_is_empty = ISLFunction.create(
    _lib.isl_union_map_plain_is_empty,
    Keep(UnionMap),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_map_is_single_valued = ISLFunction.create(
    _lib.isl_union_map_is_single_valued,
    Keep(UnionMap),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_map_plain_is_injective = ISLFunction.create(
    _lib.isl_union_map_plain_is_injective,
    Keep(UnionMap),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_map_is_injective = ISLFunction.create(
    _lib.isl_union_map_is_injective,
    Keep(UnionMap),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_map_is_bijective = ISLFunction.create(
    _lib.isl_union_map_is_bijective,
    Keep(UnionMap),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_map_is_identity = ISLFunction.create(
    _lib.isl_union_map_is_identity,
    Keep(UnionMap),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_map_involves_dims = ISLFunction.create(
    _lib.isl_union_map_involves_dims,
    Keep(UnionMap),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_map_is_disjoint = ISLFunction.create(
    _lib.isl_union_map_is_disjoint,
    Keep(UnionMap),
    Keep(UnionMap),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_map_is_subset = ISLFunction.create(
    _lib.isl_union_map_is_subset,
    Keep(UnionMap),
    Keep(UnionMap),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_map_is_strict_subset = ISLFunction.create(
    _lib.isl_union_map_is_strict_subset,
    Keep(UnionMap),
    Keep(UnionMap),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)
__all__ = ["UnionMap"]
