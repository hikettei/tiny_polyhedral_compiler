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
    from .basic_map import BasicMap
    from .context import Context
    from .id import Id
    from .id_list import IdList
    from .map import Map
    from .map_list import MapList
    from .multi_aff import MultiAff
    from .multi_id import MultiId
    from .multi_union_pw_aff import MultiUnionPwAff
    from .pw_multi_aff import PwMultiAff
    from .set import Set
    from .space import Space
    from .union_pw_aff import UnionPwAff
    from .union_pw_multi_aff import UnionPwMultiAff
    from .union_set import UnionSet
    from .val import Val

_lib = load_libisl()

class UnionMap(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_union_map_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_union_map_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_union_map_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_union_map_free(handle)

    def __str__(self) -> str:
        return _isl_union_map_to_str(self)

    def __repr__(self) -> str:
        return f"UnionMap({self.__str__()})"

    def get_space(self) -> "Space":
        return _isl_union_map_get_space(self)

    def dim(self, type: int) -> int:
        return _isl_union_map_dim(self, type)

    def get_dim_id(self, type: int, pos: int) -> "Id":
        return _isl_union_map_get_dim_id(self, type, pos)

    def find_dim_by_name(self, type: int, name: str) -> int:
        return _isl_union_map_find_dim_by_name(self, type, name)

    def reset_user(self) -> "UnionMap":
        return _isl_union_map_reset_user(self)

    @classmethod
    def empty_ctx(cls) -> "UnionMap":
        return _isl_union_map_empty_ctx()

    @classmethod
    def empty_space(cls, space: "Space") -> "UnionMap":
        return _isl_union_map_empty_space(space)

    @classmethod
    def empty(cls, space: "Space") -> "UnionMap":
        return _isl_union_map_empty(space)

    def universe(self) -> "UnionMap":
        return _isl_union_map_universe(self)

    @classmethod
    def from_basic_map(cls, bmap: "BasicMap") -> "UnionMap":
        return _isl_union_map_from_basic_map(bmap)

    @classmethod
    def from_map(cls, map: "Map") -> "UnionMap":
        return _isl_union_map_from_map(map)

    def isa_map(self) -> bool:
        return _isl_union_map_isa_map(self)

    def as_map(self) -> "Map":
        return _isl_union_map_as_map(self)

    @classmethod
    def from_union_pw_aff(cls, upa: "UnionPwAff") -> "UnionMap":
        return _isl_union_map_from_union_pw_aff(upa)

    @classmethod
    def from_union_pw_multi_aff(cls, upma: "UnionPwMultiAff") -> "UnionMap":
        return _isl_union_map_from_union_pw_multi_aff(upma)

    @classmethod
    def from_multi_union_pw_aff(cls, mupa: "MultiUnionPwAff") -> "UnionMap":
        return _isl_union_map_from_multi_union_pw_aff(mupa)

    def compute_divs(self) -> "UnionMap":
        return _isl_union_map_compute_divs(self)

    def remove_divs(self) -> "UnionMap":
        return _isl_union_map_remove_divs(self)

    def foreach_map(self, fn: Any, user: Any, user_: Any = None) -> int:
        return _isl_union_map_foreach_map(self, fn, user, user_)

    def every_map(self, test: Any, user: Any, user_: Any = None) -> bool:
        return _isl_union_map_every_map(self, test, user, user_)

    def n_map(self) -> int:
        return _isl_union_map_n_map(self)

    def extract_map(self, space: "Space") -> "Map":
        return _isl_union_map_extract_map(self, space)

    def get_map_list(self) -> "MapList":
        return _isl_union_map_get_map_list(self)

    @classmethod
    def read_from_file(cls, input: None) -> "UnionMap":
        return _isl_union_map_read_from_file(input)

    def plain_is_empty(self) -> bool:
        return _isl_union_map_plain_is_empty(self)

    def is_empty(self) -> bool:
        return _isl_union_map_is_empty(self)

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

    def involves_dims(self, type: int, first: int, n: int) -> bool:
        return _isl_union_map_involves_dims(self, type, first, n)

    def is_equal(self, umap2: "UnionMap") -> bool:
        return _isl_union_map_is_equal(self, umap2)

    def is_disjoint(self, umap2: "UnionMap") -> bool:
        return _isl_union_map_is_disjoint(self, umap2)

    def is_subset(self, umap2: "UnionMap") -> bool:
        return _isl_union_map_is_subset(self, umap2)

    def is_strict_subset(self, umap2: "UnionMap") -> bool:
        return _isl_union_map_is_strict_subset(self, umap2)

    def reverse(self) -> "UnionMap":
        return _isl_union_map_reverse(self)

    def domain_reverse(self) -> "UnionMap":
        return _isl_union_map_domain_reverse(self)

    def range_reverse(self) -> "UnionMap":
        return _isl_union_map_range_reverse(self)

    def bind_range(self, tuple: "MultiId") -> "UnionSet":
        return _isl_union_map_bind_range(self, tuple)

    def project_out_param_id(self, id: "Id") -> "UnionMap":
        return _isl_union_map_project_out_param_id(self, id)

    def project_out_param_id_list(self, list: "IdList") -> "UnionMap":
        return _isl_union_map_project_out_param_id_list(self, list)

    def project_out(self, type: int, first: int, n: int) -> "UnionMap":
        return _isl_union_map_project_out(self, type, first, n)

    def project_out_all_params(self) -> "UnionMap":
        return _isl_union_map_project_out_all_params(self)

    def params(self) -> "Set":
        return _isl_union_map_params(self)

    def domain(self) -> "UnionSet":
        return _isl_union_map_domain(self)

    def range(self) -> "UnionSet":
        return _isl_union_map_range(self)

    def domain_map(self) -> "UnionMap":
        return _isl_union_map_domain_map(self)

    def domain_map_union_pw_multi_aff(self) -> "UnionPwMultiAff":
        return _isl_union_map_domain_map_union_pw_multi_aff(self)

    def range_map(self) -> "UnionMap":
        return _isl_union_map_range_map(self)

    @classmethod
    def from_domain(cls, uset: "UnionSet") -> "UnionMap":
        return _isl_union_map_from_domain(uset)

    @classmethod
    def from_range(cls, uset: "UnionSet") -> "UnionMap":
        return _isl_union_map_from_range(uset)

    @classmethod
    def from_domain_and_range(cls, domain: "UnionSet", range: "UnionSet") -> "UnionMap":
        return _isl_union_map_from_domain_and_range(domain, range)

    def remove_map_if(self, fn: Any, user: Any, user_: Any = None) -> "UnionMap":
        return _isl_union_map_remove_map_if(self, fn, user, user_)

    def as_union_pw_multi_aff(self) -> "UnionPwMultiAff":
        return _isl_union_map_as_union_pw_multi_aff(self)

    def as_multi_union_pw_aff(self) -> "MultiUnionPwAff":
        return _isl_union_map_as_multi_union_pw_aff(self)

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

    def simple_hull(self) -> "UnionMap":
        return _isl_union_map_simple_hull(self)

    def affine_hull(self) -> "UnionMap":
        return _isl_union_map_affine_hull(self)

    def polyhedral_hull(self) -> "UnionMap":
        return _isl_union_map_polyhedral_hull(self)

    def fixed_power_val(self, exp: "Val") -> "UnionMap":
        return _isl_union_map_fixed_power_val(self, exp)

    def wrap(self) -> "UnionSet":
        return _isl_union_map_wrap(self)

    def zip(self) -> "UnionMap":
        return _isl_union_map_zip(self)

    def curry(self) -> "UnionMap":
        return _isl_union_map_curry(self)

    def uncurry(self) -> "UnionMap":
        return _isl_union_map_uncurry(self)

    def range_curry(self) -> "UnionMap":
        return _isl_union_map_range_curry(self)

    def drop_unused_params(self) -> "UnionMap":
        return _isl_union_map_drop_unused_params(self)

    def intersect_params(self, set: "Set") -> "UnionMap":
        return _isl_union_map_intersect_params(self, set)

    def intersect_domain_union_set(self, uset: "UnionSet") -> "UnionMap":
        return _isl_union_map_intersect_domain_union_set(self, uset)

    def intersect_domain_space(self, space: "Space") -> "UnionMap":
        return _isl_union_map_intersect_domain_space(self, space)

    def intersect_domain(self, uset: "UnionSet") -> "UnionMap":
        return _isl_union_map_intersect_domain(self, uset)

    def intersect_range_union_set(self, uset: "UnionSet") -> "UnionMap":
        return _isl_union_map_intersect_range_union_set(self, uset)

    def intersect_range_space(self, space: "Space") -> "UnionMap":
        return _isl_union_map_intersect_range_space(self, space)

    def intersect_range(self, uset: "UnionSet") -> "UnionMap":
        return _isl_union_map_intersect_range(self, uset)

    def intersect(self, umap2: "UnionMap") -> "UnionMap":
        return _isl_union_map_intersect(self, umap2)

    def intersect_domain_factor_domain(self, factor: "UnionMap") -> "UnionMap":
        return _isl_union_map_intersect_domain_factor_domain(self, factor)

    def intersect_domain_factor_range(self, factor: "UnionMap") -> "UnionMap":
        return _isl_union_map_intersect_domain_factor_range(self, factor)

    def intersect_range_factor_domain(self, factor: "UnionMap") -> "UnionMap":
        return _isl_union_map_intersect_range_factor_domain(self, factor)

    def intersect_range_factor_range(self, factor: "UnionMap") -> "UnionMap":
        return _isl_union_map_intersect_range_factor_range(self, factor)

    def intersect_domain_wrapped_domain_union_set(self, domain: "UnionSet") -> "UnionMap":
        return _isl_union_map_intersect_domain_wrapped_domain_union_set(self, domain)

    def intersect_range_wrapped_domain_union_set(self, domain: "UnionSet") -> "UnionMap":
        return _isl_union_map_intersect_range_wrapped_domain_union_set(self, domain)

    def union(self, umap2: "UnionMap") -> "UnionMap":
        return _isl_union_map_union(self, umap2)

    def subtract(self, umap2: "UnionMap") -> "UnionMap":
        return _isl_union_map_subtract(self, umap2)

    def subtract_domain(self, dom: "UnionSet") -> "UnionMap":
        return _isl_union_map_subtract_domain(self, dom)

    def subtract_range(self, dom: "UnionSet") -> "UnionMap":
        return _isl_union_map_subtract_range(self, dom)

    def apply_domain(self, umap2: "UnionMap") -> "UnionMap":
        return _isl_union_map_apply_domain(self, umap2)

    def apply_range(self, umap2: "UnionMap") -> "UnionMap":
        return _isl_union_map_apply_range(self, umap2)

    def preimage_domain_multi_aff(self, ma: "MultiAff") -> "UnionMap":
        return _isl_union_map_preimage_domain_multi_aff(self, ma)

    def preimage_range_multi_aff(self, ma: "MultiAff") -> "UnionMap":
        return _isl_union_map_preimage_range_multi_aff(self, ma)

    def preimage_domain_pw_multi_aff(self, pma: "PwMultiAff") -> "UnionMap":
        return _isl_union_map_preimage_domain_pw_multi_aff(self, pma)

    def preimage_range_pw_multi_aff(self, pma: "PwMultiAff") -> "UnionMap":
        return _isl_union_map_preimage_range_pw_multi_aff(self, pma)

    def preimage_domain_union_pw_multi_aff(self, upma: "UnionPwMultiAff") -> "UnionMap":
        return _isl_union_map_preimage_domain_union_pw_multi_aff(self, upma)

    def preimage_range_union_pw_multi_aff(self, upma: "UnionPwMultiAff") -> "UnionMap":
        return _isl_union_map_preimage_range_union_pw_multi_aff(self, upma)

    def eq_at_multi_union_pw_aff(self, mupa: "MultiUnionPwAff") -> "UnionMap":
        return _isl_union_map_eq_at_multi_union_pw_aff(self, mupa)

    def lex_lt_at_multi_union_pw_aff(self, mupa: "MultiUnionPwAff") -> "UnionMap":
        return _isl_union_map_lex_lt_at_multi_union_pw_aff(self, mupa)

    def lex_le_at_multi_union_pw_aff(self, mupa: "MultiUnionPwAff") -> "UnionMap":
        return _isl_union_map_lex_le_at_multi_union_pw_aff(self, mupa)

    def lex_gt_at_multi_union_pw_aff(self, mupa: "MultiUnionPwAff") -> "UnionMap":
        return _isl_union_map_lex_gt_at_multi_union_pw_aff(self, mupa)

    def lex_ge_at_multi_union_pw_aff(self, mupa: "MultiUnionPwAff") -> "UnionMap":
        return _isl_union_map_lex_ge_at_multi_union_pw_aff(self, mupa)

    def domain_product(self, umap2: "UnionMap") -> "UnionMap":
        return _isl_union_map_domain_product(self, umap2)

    def range_product(self, umap2: "UnionMap") -> "UnionMap":
        return _isl_union_map_range_product(self, umap2)

    def product(self, umap2: "UnionMap") -> "UnionMap":
        return _isl_union_map_product(self, umap2)

    def flat_domain_product(self, umap2: "UnionMap") -> "UnionMap":
        return _isl_union_map_flat_domain_product(self, umap2)

    def flat_range_product(self, umap2: "UnionMap") -> "UnionMap":
        return _isl_union_map_flat_range_product(self, umap2)

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

    def gist(self, context: "UnionMap") -> "UnionMap":
        return _isl_union_map_gist(self, context)

    def gist_params(self, set: "Set") -> "UnionMap":
        return _isl_union_map_gist_params(self, set)

    def gist_domain(self, uset: "UnionSet") -> "UnionMap":
        return _isl_union_map_gist_domain(self, uset)

    def gist_range(self, uset: "UnionSet") -> "UnionMap":
        return _isl_union_map_gist_range(self, uset)

    def lexmin(self) -> "UnionMap":
        return _isl_union_map_lexmin(self)

    def lexmax(self) -> "UnionMap":
        return _isl_union_map_lexmax(self)


register_type("UnionMap", UnionMap)

_isl_union_map_get_space = ISLFunction.create(
    "isl_union_map_get_space",
    Keep("UnionMap"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_union_map_dim = ISLFunction.create(
    "isl_union_map_dim",
    Keep("UnionMap"),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_map_get_dim_id = ISLFunction.create(
    "isl_union_map_get_dim_id",
    Keep("UnionMap"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Give("Id"),
    lib=_lib,
)

_isl_union_map_find_dim_by_name = ISLFunction.create(
    "isl_union_map_find_dim_by_name",
    Keep("UnionMap"),
    Param(int, ctype=c_int),
    Param(str, ctype=c_char_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_map_reset_user = ISLFunction.create(
    "isl_union_map_reset_user",
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_empty_ctx = ISLFunction.create(
    "isl_union_map_empty_ctx",
    Context(),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_empty_space = ISLFunction.create(
    "isl_union_map_empty_space",
    Take("Space"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_empty = ISLFunction.create(
    "isl_union_map_empty",
    Take("Space"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_universe = ISLFunction.create(
    "isl_union_map_universe",
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_from_basic_map = ISLFunction.create(
    "isl_union_map_from_basic_map",
    Take("BasicMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_from_map = ISLFunction.create(
    "isl_union_map_from_map",
    Take("Map"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_isa_map = ISLFunction.create(
    "isl_union_map_isa_map",
    Keep("UnionMap"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_map_as_map = ISLFunction.create(
    "isl_union_map_as_map",
    Take("UnionMap"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_union_map_copy = ISLFunction.create(
    "isl_union_map_copy",
    Keep("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_free = ISLFunction.create(
    "isl_union_map_free",
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_from_union_pw_aff = ISLFunction.create(
    "isl_union_map_from_union_pw_aff",
    Take("UnionPwAff"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_from_union_pw_multi_aff = ISLFunction.create(
    "isl_union_map_from_union_pw_multi_aff",
    Take("UnionPwMultiAff"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_from_multi_union_pw_aff = ISLFunction.create(
    "isl_union_map_from_multi_union_pw_aff",
    Take("MultiUnionPwAff"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_compute_divs = ISLFunction.create(
    "isl_union_map_compute_divs",
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_remove_divs = ISLFunction.create(
    "isl_union_map_remove_divs",
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_foreach_map = ISLFunction.create(
    "isl_union_map_foreach_map",
    Keep("UnionMap"),
    Param(None, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_map_every_map = ISLFunction.create(
    "isl_union_map_every_map",
    Keep("UnionMap"),
    Param(None, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_map_n_map = ISLFunction.create(
    "isl_union_map_n_map",
    Keep("UnionMap"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_map_extract_map = ISLFunction.create(
    "isl_union_map_extract_map",
    Keep("UnionMap"),
    Take("Space"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_union_map_get_map_list = ISLFunction.create(
    "isl_union_map_get_map_list",
    Keep("UnionMap"),
    return_=Give("MapList"),
    lib=_lib,
)

_isl_union_map_read_from_file = ISLFunction.create(
    "isl_union_map_read_from_file",
    Context(),
    Param(None, ctype=c_void_p),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_read_from_str = ISLFunction.create(
    "isl_union_map_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_to_str = ISLFunction.create(
    "isl_union_map_to_str",
    Keep("UnionMap"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_union_map_plain_is_empty = ISLFunction.create(
    "isl_union_map_plain_is_empty",
    Keep("UnionMap"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_map_is_empty = ISLFunction.create(
    "isl_union_map_is_empty",
    Keep("UnionMap"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_map_is_single_valued = ISLFunction.create(
    "isl_union_map_is_single_valued",
    Keep("UnionMap"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_map_plain_is_injective = ISLFunction.create(
    "isl_union_map_plain_is_injective",
    Keep("UnionMap"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_map_is_injective = ISLFunction.create(
    "isl_union_map_is_injective",
    Keep("UnionMap"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_map_is_bijective = ISLFunction.create(
    "isl_union_map_is_bijective",
    Keep("UnionMap"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_map_is_identity = ISLFunction.create(
    "isl_union_map_is_identity",
    Keep("UnionMap"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_map_involves_dims = ISLFunction.create(
    "isl_union_map_involves_dims",
    Keep("UnionMap"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_map_is_equal = ISLFunction.create(
    "isl_union_map_is_equal",
    Keep("UnionMap"),
    Keep("UnionMap"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_map_is_disjoint = ISLFunction.create(
    "isl_union_map_is_disjoint",
    Keep("UnionMap"),
    Keep("UnionMap"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_map_is_subset = ISLFunction.create(
    "isl_union_map_is_subset",
    Keep("UnionMap"),
    Keep("UnionMap"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_map_is_strict_subset = ISLFunction.create(
    "isl_union_map_is_strict_subset",
    Keep("UnionMap"),
    Keep("UnionMap"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_map_reverse = ISLFunction.create(
    "isl_union_map_reverse",
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_domain_reverse = ISLFunction.create(
    "isl_union_map_domain_reverse",
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_range_reverse = ISLFunction.create(
    "isl_union_map_range_reverse",
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_bind_range = ISLFunction.create(
    "isl_union_map_bind_range",
    Take("UnionMap"),
    Take("MultiId"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_map_project_out_param_id = ISLFunction.create(
    "isl_union_map_project_out_param_id",
    Take("UnionMap"),
    Take("Id"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_project_out_param_id_list = ISLFunction.create(
    "isl_union_map_project_out_param_id_list",
    Take("UnionMap"),
    Take("IdList"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_project_out = ISLFunction.create(
    "isl_union_map_project_out",
    Take("UnionMap"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_project_out_all_params = ISLFunction.create(
    "isl_union_map_project_out_all_params",
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_params = ISLFunction.create(
    "isl_union_map_params",
    Take("UnionMap"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_union_map_domain = ISLFunction.create(
    "isl_union_map_domain",
    Take("UnionMap"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_map_range = ISLFunction.create(
    "isl_union_map_range",
    Take("UnionMap"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_map_domain_map = ISLFunction.create(
    "isl_union_map_domain_map",
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_domain_map_union_pw_multi_aff = ISLFunction.create(
    "isl_union_map_domain_map_union_pw_multi_aff",
    Take("UnionMap"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_map_range_map = ISLFunction.create(
    "isl_union_map_range_map",
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_from_domain = ISLFunction.create(
    "isl_union_map_from_domain",
    Take("UnionSet"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_from_range = ISLFunction.create(
    "isl_union_map_from_range",
    Take("UnionSet"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_from_domain_and_range = ISLFunction.create(
    "isl_union_map_from_domain_and_range",
    Take("UnionSet"),
    Take("UnionSet"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_remove_map_if = ISLFunction.create(
    "isl_union_map_remove_map_if",
    Take("UnionMap"),
    Param(None, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_as_union_pw_multi_aff = ISLFunction.create(
    "isl_union_map_as_union_pw_multi_aff",
    Take("UnionMap"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_map_as_multi_union_pw_aff = ISLFunction.create(
    "isl_union_map_as_multi_union_pw_aff",
    Take("UnionMap"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_union_map_deltas = ISLFunction.create(
    "isl_union_map_deltas",
    Take("UnionMap"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_map_deltas_map = ISLFunction.create(
    "isl_union_map_deltas_map",
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_coalesce = ISLFunction.create(
    "isl_union_map_coalesce",
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_detect_equalities = ISLFunction.create(
    "isl_union_map_detect_equalities",
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_remove_redundancies = ISLFunction.create(
    "isl_union_map_remove_redundancies",
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_simple_hull = ISLFunction.create(
    "isl_union_map_simple_hull",
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_affine_hull = ISLFunction.create(
    "isl_union_map_affine_hull",
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_polyhedral_hull = ISLFunction.create(
    "isl_union_map_polyhedral_hull",
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_fixed_power_val = ISLFunction.create(
    "isl_union_map_fixed_power_val",
    Take("UnionMap"),
    Take("Val"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_wrap = ISLFunction.create(
    "isl_union_map_wrap",
    Take("UnionMap"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_map_zip = ISLFunction.create(
    "isl_union_map_zip",
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_curry = ISLFunction.create(
    "isl_union_map_curry",
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_uncurry = ISLFunction.create(
    "isl_union_map_uncurry",
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_range_curry = ISLFunction.create(
    "isl_union_map_range_curry",
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_drop_unused_params = ISLFunction.create(
    "isl_union_map_drop_unused_params",
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_intersect_params = ISLFunction.create(
    "isl_union_map_intersect_params",
    Take("UnionMap"),
    Take("Set"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_intersect_domain_union_set = ISLFunction.create(
    "isl_union_map_intersect_domain_union_set",
    Take("UnionMap"),
    Take("UnionSet"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_intersect_domain_space = ISLFunction.create(
    "isl_union_map_intersect_domain_space",
    Take("UnionMap"),
    Take("Space"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_intersect_domain = ISLFunction.create(
    "isl_union_map_intersect_domain",
    Take("UnionMap"),
    Take("UnionSet"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_intersect_range_union_set = ISLFunction.create(
    "isl_union_map_intersect_range_union_set",
    Take("UnionMap"),
    Take("UnionSet"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_intersect_range_space = ISLFunction.create(
    "isl_union_map_intersect_range_space",
    Take("UnionMap"),
    Take("Space"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_intersect_range = ISLFunction.create(
    "isl_union_map_intersect_range",
    Take("UnionMap"),
    Take("UnionSet"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_intersect = ISLFunction.create(
    "isl_union_map_intersect",
    Take("UnionMap"),
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_intersect_domain_factor_domain = ISLFunction.create(
    "isl_union_map_intersect_domain_factor_domain",
    Take("UnionMap"),
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_intersect_domain_factor_range = ISLFunction.create(
    "isl_union_map_intersect_domain_factor_range",
    Take("UnionMap"),
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_intersect_range_factor_domain = ISLFunction.create(
    "isl_union_map_intersect_range_factor_domain",
    Take("UnionMap"),
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_intersect_range_factor_range = ISLFunction.create(
    "isl_union_map_intersect_range_factor_range",
    Take("UnionMap"),
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_intersect_domain_wrapped_domain_union_set = ISLFunction.create(
    "isl_union_map_intersect_domain_wrapped_domain_union_set",
    Take("UnionMap"),
    Take("UnionSet"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_intersect_range_wrapped_domain_union_set = ISLFunction.create(
    "isl_union_map_intersect_range_wrapped_domain_union_set",
    Take("UnionMap"),
    Take("UnionSet"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_union = ISLFunction.create(
    "isl_union_map_union",
    Take("UnionMap"),
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_subtract = ISLFunction.create(
    "isl_union_map_subtract",
    Take("UnionMap"),
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_subtract_domain = ISLFunction.create(
    "isl_union_map_subtract_domain",
    Take("UnionMap"),
    Take("UnionSet"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_subtract_range = ISLFunction.create(
    "isl_union_map_subtract_range",
    Take("UnionMap"),
    Take("UnionSet"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_apply_domain = ISLFunction.create(
    "isl_union_map_apply_domain",
    Take("UnionMap"),
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_apply_range = ISLFunction.create(
    "isl_union_map_apply_range",
    Take("UnionMap"),
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_preimage_domain_multi_aff = ISLFunction.create(
    "isl_union_map_preimage_domain_multi_aff",
    Take("UnionMap"),
    Take("MultiAff"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_preimage_range_multi_aff = ISLFunction.create(
    "isl_union_map_preimage_range_multi_aff",
    Take("UnionMap"),
    Take("MultiAff"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_preimage_domain_pw_multi_aff = ISLFunction.create(
    "isl_union_map_preimage_domain_pw_multi_aff",
    Take("UnionMap"),
    Take("PwMultiAff"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_preimage_range_pw_multi_aff = ISLFunction.create(
    "isl_union_map_preimage_range_pw_multi_aff",
    Take("UnionMap"),
    Take("PwMultiAff"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_preimage_domain_union_pw_multi_aff = ISLFunction.create(
    "isl_union_map_preimage_domain_union_pw_multi_aff",
    Take("UnionMap"),
    Take("UnionPwMultiAff"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_preimage_range_union_pw_multi_aff = ISLFunction.create(
    "isl_union_map_preimage_range_union_pw_multi_aff",
    Take("UnionMap"),
    Take("UnionPwMultiAff"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_eq_at_multi_union_pw_aff = ISLFunction.create(
    "isl_union_map_eq_at_multi_union_pw_aff",
    Take("UnionMap"),
    Take("MultiUnionPwAff"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_lex_lt_at_multi_union_pw_aff = ISLFunction.create(
    "isl_union_map_lex_lt_at_multi_union_pw_aff",
    Take("UnionMap"),
    Take("MultiUnionPwAff"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_lex_le_at_multi_union_pw_aff = ISLFunction.create(
    "isl_union_map_lex_le_at_multi_union_pw_aff",
    Take("UnionMap"),
    Take("MultiUnionPwAff"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_lex_gt_at_multi_union_pw_aff = ISLFunction.create(
    "isl_union_map_lex_gt_at_multi_union_pw_aff",
    Take("UnionMap"),
    Take("MultiUnionPwAff"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_lex_ge_at_multi_union_pw_aff = ISLFunction.create(
    "isl_union_map_lex_ge_at_multi_union_pw_aff",
    Take("UnionMap"),
    Take("MultiUnionPwAff"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_domain_product = ISLFunction.create(
    "isl_union_map_domain_product",
    Take("UnionMap"),
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_range_product = ISLFunction.create(
    "isl_union_map_range_product",
    Take("UnionMap"),
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_product = ISLFunction.create(
    "isl_union_map_product",
    Take("UnionMap"),
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_flat_domain_product = ISLFunction.create(
    "isl_union_map_flat_domain_product",
    Take("UnionMap"),
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_flat_range_product = ISLFunction.create(
    "isl_union_map_flat_range_product",
    Take("UnionMap"),
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_factor_domain = ISLFunction.create(
    "isl_union_map_factor_domain",
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_factor_range = ISLFunction.create(
    "isl_union_map_factor_range",
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_domain_factor_domain = ISLFunction.create(
    "isl_union_map_domain_factor_domain",
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_domain_factor_range = ISLFunction.create(
    "isl_union_map_domain_factor_range",
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_range_factor_domain = ISLFunction.create(
    "isl_union_map_range_factor_domain",
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_range_factor_range = ISLFunction.create(
    "isl_union_map_range_factor_range",
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_gist = ISLFunction.create(
    "isl_union_map_gist",
    Take("UnionMap"),
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_gist_params = ISLFunction.create(
    "isl_union_map_gist_params",
    Take("UnionMap"),
    Take("Set"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_gist_domain = ISLFunction.create(
    "isl_union_map_gist_domain",
    Take("UnionMap"),
    Take("UnionSet"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_gist_range = ISLFunction.create(
    "isl_union_map_gist_range",
    Take("UnionMap"),
    Take("UnionSet"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_lexmin = ISLFunction.create(
    "isl_union_map_lexmin",
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_map_lexmax = ISLFunction.create(
    "isl_union_map_lexmax",
    Take("UnionMap"),
    return_=Give("UnionMap"),
    lib=_lib,
)
