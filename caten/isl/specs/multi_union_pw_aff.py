from __future__ import annotations

from ctypes import c_char_p, c_int, c_uint
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
    from .context import Context
    from .id import Id
    from .multi_aff import MultiAff
    from .multi_id import MultiId
    from .multi_pw_aff import MultiPwAff
    from .multi_val import MultiVal
    from .pw_aff import PwAff
    from .pw_multi_aff import PwMultiAff
    from .set import Set
    from .space import Space
    from .union_map import UnionMap
    from .union_pw_aff import UnionPwAff
    from .union_pw_aff_list import UnionPwAffList
    from .union_pw_multi_aff import UnionPwMultiAff
    from .union_set import UnionSet
    from .val import Val

_lib = load_libisl()

class MultiUnionPwAff(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_multi_union_pw_aff_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_multi_union_pw_aff_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_multi_union_pw_aff_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_multi_union_pw_aff_free(handle)

    def __str__(self) -> str:
        return _isl_multi_union_pw_aff_to_str(self)

    def __repr__(self) -> str:
        return f"MultiUnionPwAff({self.__str__()})"

    def get_ctx(self) -> "Context":
        return _isl_multi_union_pw_aff_get_ctx(self)

    def get_domain_space(self) -> "Space":
        return _isl_multi_union_pw_aff_get_domain_space(self)

    def get_space(self) -> "Space":
        return _isl_multi_union_pw_aff_get_space(self)

    def dim(self, type: int) -> int:
        return _isl_multi_union_pw_aff_dim(self, type)

    def set_dim_id(self, type: int, pos: int, id: "Id") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_set_dim_id(self, type, pos, id)

    def get_dim_id(self, type: int, pos: int) -> "Id":
        return _isl_multi_union_pw_aff_get_dim_id(self, type, pos)

    def set_dim_name(self, type: int, pos: int, s: str) -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_set_dim_name(self, type, pos, s)

    def find_dim_by_id(self, type: int, id: "Id") -> int:
        return _isl_multi_union_pw_aff_find_dim_by_id(self, type, id)

    def find_dim_by_name(self, type: int, name: str) -> int:
        return _isl_multi_union_pw_aff_find_dim_by_name(self, type, name)

    def set_range_tuple_id(self, id: "Id") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_set_range_tuple_id(self, id)

    def set_tuple_id(self, type: int, id: "Id") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_set_tuple_id(self, type, id)

    def reset_range_tuple_id(self) -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_reset_range_tuple_id(self)

    def reset_tuple_id(self, type: int) -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_reset_tuple_id(self, type)

    def has_range_tuple_id(self) -> bool:
        return _isl_multi_union_pw_aff_has_range_tuple_id(self)

    def get_range_tuple_id(self) -> "Id":
        return _isl_multi_union_pw_aff_get_range_tuple_id(self)

    def has_tuple_id(self, type: int) -> bool:
        return _isl_multi_union_pw_aff_has_tuple_id(self, type)

    def get_tuple_id(self, type: int) -> "Id":
        return _isl_multi_union_pw_aff_get_tuple_id(self, type)

    def set_tuple_name(self, type: int, s: str) -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_set_tuple_name(self, type, s)

    def get_tuple_name(self, type: int) -> str:
        return _isl_multi_union_pw_aff_get_tuple_name(self, type)

    def reset_user(self) -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_reset_user(self)

    @classmethod
    def zero(cls, space: "Space") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_zero(space)

    @classmethod
    def from_union_pw_aff(cls, upa: "UnionPwAff") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_from_union_pw_aff(upa)

    @classmethod
    def from_union_pw_aff_list(cls, space: "Space", list: "UnionPwAffList") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_from_union_pw_aff_list(space, list)

    @classmethod
    def from_multi_aff(cls, ma: "MultiAff") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_from_multi_aff(ma)

    @classmethod
    def from_multi_pw_aff(cls, mpa: "MultiPwAff") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_from_multi_pw_aff(mpa)

    @classmethod
    def multi_val_on_domain(cls, domain: "UnionSet", mv: "MultiVal") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_multi_val_on_domain(domain, mv)

    @classmethod
    def multi_aff_on_domain(cls, domain: "UnionSet", ma: "MultiAff") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_multi_aff_on_domain(domain, ma)

    @classmethod
    def pw_multi_aff_on_domain(cls, domain: "UnionSet", pma: "PwMultiAff") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_pw_multi_aff_on_domain(domain, pma)

    def size(self) -> int:
        return _isl_multi_union_pw_aff_size(self)

    def get_at(self, pos: int) -> "UnionPwAff":
        return _isl_multi_union_pw_aff_get_at(self, pos)

    def get_union_pw_aff(self, pos: int) -> "UnionPwAff":
        return _isl_multi_union_pw_aff_get_union_pw_aff(self, pos)

    def set_at(self, pos: int, upa: "UnionPwAff") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_set_at(self, pos, upa)

    def set_union_pw_aff(self, pos: int, upa: "UnionPwAff") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_set_union_pw_aff(self, pos, upa)

    def list(self) -> "UnionPwAffList":
        return _isl_multi_union_pw_aff_list(self)

    def extract_multi_pw_aff(self, space: "Space") -> "MultiPwAff":
        return _isl_multi_union_pw_aff_extract_multi_pw_aff(self, space)

    @classmethod
    def from_union_pw_multi_aff(cls, upma: "UnionPwMultiAff") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_from_union_pw_multi_aff(upma)

    def range_is_wrapping(self) -> bool:
        return _isl_multi_union_pw_aff_range_is_wrapping(self)

    def involves_nan(self) -> bool:
        return _isl_multi_union_pw_aff_involves_nan(self)

    def plain_is_equal(self, mupa2: "MultiUnionPwAff") -> bool:
        return _isl_multi_union_pw_aff_plain_is_equal(self, mupa2)

    def bind(self, tuple: "MultiId") -> "UnionSet":
        return _isl_multi_union_pw_aff_bind(self, tuple)

    def domain(self) -> "UnionSet":
        return _isl_multi_union_pw_aff_domain(self)

    def from_range(self) -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_from_range(self)

    def zero_union_set(self) -> "UnionSet":
        return _isl_multi_union_pw_aff_zero_union_set(self)

    @classmethod
    def from_union_map(cls, umap: "UnionMap") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_from_union_map(umap)

    def coalesce(self) -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_coalesce(self)

    def min_multi_val(self) -> "MultiVal":
        return _isl_multi_union_pw_aff_min_multi_val(self)

    def max_multi_val(self) -> "MultiVal":
        return _isl_multi_union_pw_aff_max_multi_val(self)

    def flatten_range(self) -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_flatten_range(self)

    def align_params(self, model: "Space") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_align_params(self, model)

    def neg(self) -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_neg(self)

    def floor(self) -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_floor(self)

    def drop_dims(self, type: int, first: int, n: int) -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_drop_dims(self, type, first, n)

    def intersect_domain(self, uset: "UnionSet") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_intersect_domain(self, uset)

    def intersect_params(self, params: "Set") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_intersect_params(self, params)

    def intersect_range(self, set: "Set") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_intersect_range(self, set)

    def apply_aff(self, aff: "Aff") -> "UnionPwAff":
        return _isl_multi_union_pw_aff_apply_aff(self, aff)

    def apply_pw_aff(self, pa: "PwAff") -> "UnionPwAff":
        return _isl_multi_union_pw_aff_apply_pw_aff(self, pa)

    def apply_multi_aff(self, ma: "MultiAff") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_apply_multi_aff(self, ma)

    def apply_pw_multi_aff(self, pma: "PwMultiAff") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_apply_pw_multi_aff(self, pma)

    def pullback_union_pw_multi_aff(self, upma: "UnionPwMultiAff") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_pullback_union_pw_multi_aff(self, upma)

    def range_product(self, mupa2: "MultiUnionPwAff") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_range_product(self, mupa2)

    def flat_range_product(self, mupa2: "MultiUnionPwAff") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_flat_range_product(self, mupa2)

    def factor_range(self) -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_factor_range(self)

    def range_factor_domain(self) -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_range_factor_domain(self)

    def range_factor_range(self) -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_range_factor_range(self)

    def range_splice(self, pos: int, mupa2: "MultiUnionPwAff") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_range_splice(self, pos, mupa2)

    def gist_params(self, context: "Set") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_gist_params(self, context)

    def gist(self, context: "UnionSet") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_gist(self, context)

    def add(self, mupa2: "MultiUnionPwAff") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_add(self, mupa2)

    def sub(self, mupa2: "MultiUnionPwAff") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_sub(self, mupa2)

    def union_add(self, mupa2: "MultiUnionPwAff") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_union_add(self, mupa2)

    def scale_val(self, v: "Val") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_scale_val(self, v)

    def scale_down_val(self, v: "Val") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_scale_down_val(self, v)

    def mod_multi_val(self, mv: "MultiVal") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_mod_multi_val(self, mv)

    def scale_multi_val(self, mv: "MultiVal") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_scale_multi_val(self, mv)

    def scale_down_multi_val(self, mv: "MultiVal") -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_scale_down_multi_val(self, mv)


register_type("MultiUnionPwAff", MultiUnionPwAff)

_isl_multi_union_pw_aff_get_ctx = ISLFunction.create(
    "isl_multi_union_pw_aff_get_ctx",
    Keep("MultiUnionPwAff"),
    return_=Give("Context"),
    lib=_lib,
)

_isl_multi_union_pw_aff_get_domain_space = ISLFunction.create(
    "isl_multi_union_pw_aff_get_domain_space",
    Keep("MultiUnionPwAff"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_multi_union_pw_aff_get_space = ISLFunction.create(
    "isl_multi_union_pw_aff_get_space",
    Keep("MultiUnionPwAff"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_multi_union_pw_aff_dim = ISLFunction.create(
    "isl_multi_union_pw_aff_dim",
    Keep("MultiUnionPwAff"),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_multi_union_pw_aff_set_dim_id = ISLFunction.create(
    "isl_multi_union_pw_aff_set_dim_id",
    Take("MultiUnionPwAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Take("Id"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_get_dim_id = ISLFunction.create(
    "isl_multi_union_pw_aff_get_dim_id",
    Keep("MultiUnionPwAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Give("Id"),
    lib=_lib,
)

_isl_multi_union_pw_aff_set_dim_name = ISLFunction.create(
    "isl_multi_union_pw_aff_set_dim_name",
    Take("MultiUnionPwAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(str, ctype=c_char_p),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_find_dim_by_id = ISLFunction.create(
    "isl_multi_union_pw_aff_find_dim_by_id",
    Keep("MultiUnionPwAff"),
    Param(int, ctype=c_int),
    Keep("Id"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_multi_union_pw_aff_find_dim_by_name = ISLFunction.create(
    "isl_multi_union_pw_aff_find_dim_by_name",
    Keep("MultiUnionPwAff"),
    Param(int, ctype=c_int),
    Param(str, ctype=c_char_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_multi_union_pw_aff_set_range_tuple_id = ISLFunction.create(
    "isl_multi_union_pw_aff_set_range_tuple_id",
    Take("MultiUnionPwAff"),
    Take("Id"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_set_tuple_id = ISLFunction.create(
    "isl_multi_union_pw_aff_set_tuple_id",
    Take("MultiUnionPwAff"),
    Param(int, ctype=c_int),
    Take("Id"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_reset_range_tuple_id = ISLFunction.create(
    "isl_multi_union_pw_aff_reset_range_tuple_id",
    Take("MultiUnionPwAff"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_reset_tuple_id = ISLFunction.create(
    "isl_multi_union_pw_aff_reset_tuple_id",
    Take("MultiUnionPwAff"),
    Param(int, ctype=c_int),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_has_range_tuple_id = ISLFunction.create(
    "isl_multi_union_pw_aff_has_range_tuple_id",
    Keep("MultiUnionPwAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_union_pw_aff_get_range_tuple_id = ISLFunction.create(
    "isl_multi_union_pw_aff_get_range_tuple_id",
    Keep("MultiUnionPwAff"),
    return_=Give("Id"),
    lib=_lib,
)

_isl_multi_union_pw_aff_has_tuple_id = ISLFunction.create(
    "isl_multi_union_pw_aff_has_tuple_id",
    Keep("MultiUnionPwAff"),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_union_pw_aff_get_tuple_id = ISLFunction.create(
    "isl_multi_union_pw_aff_get_tuple_id",
    Keep("MultiUnionPwAff"),
    Param(int, ctype=c_int),
    return_=Give("Id"),
    lib=_lib,
)

_isl_multi_union_pw_aff_set_tuple_name = ISLFunction.create(
    "isl_multi_union_pw_aff_set_tuple_name",
    Take("MultiUnionPwAff"),
    Param(int, ctype=c_int),
    Param(str, ctype=c_char_p),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_get_tuple_name = ISLFunction.create(
    "isl_multi_union_pw_aff_get_tuple_name",
    Keep("MultiUnionPwAff"),
    Param(int, ctype=c_int),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_multi_union_pw_aff_reset_user = ISLFunction.create(
    "isl_multi_union_pw_aff_reset_user",
    Take("MultiUnionPwAff"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_zero = ISLFunction.create(
    "isl_multi_union_pw_aff_zero",
    Take("Space"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_from_union_pw_aff = ISLFunction.create(
    "isl_multi_union_pw_aff_from_union_pw_aff",
    Take("UnionPwAff"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_from_union_pw_aff_list = ISLFunction.create(
    "isl_multi_union_pw_aff_from_union_pw_aff_list",
    Take("Space"),
    Take("UnionPwAffList"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_from_multi_aff = ISLFunction.create(
    "isl_multi_union_pw_aff_from_multi_aff",
    Take("MultiAff"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_from_multi_pw_aff = ISLFunction.create(
    "isl_multi_union_pw_aff_from_multi_pw_aff",
    Take("MultiPwAff"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_multi_val_on_domain = ISLFunction.create(
    "isl_multi_union_pw_aff_multi_val_on_domain",
    Take("UnionSet"),
    Take("MultiVal"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_multi_aff_on_domain = ISLFunction.create(
    "isl_multi_union_pw_aff_multi_aff_on_domain",
    Take("UnionSet"),
    Take("MultiAff"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_pw_multi_aff_on_domain = ISLFunction.create(
    "isl_multi_union_pw_aff_pw_multi_aff_on_domain",
    Take("UnionSet"),
    Take("PwMultiAff"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_copy = ISLFunction.create(
    "isl_multi_union_pw_aff_copy",
    Keep("MultiUnionPwAff"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_free = ISLFunction.create(
    "isl_multi_union_pw_aff_free",
    Take("MultiUnionPwAff"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_size = ISLFunction.create(
    "isl_multi_union_pw_aff_size",
    Keep("MultiUnionPwAff"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_multi_union_pw_aff_get_at = ISLFunction.create(
    "isl_multi_union_pw_aff_get_at",
    Keep("MultiUnionPwAff"),
    Param(int, ctype=c_int),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_get_union_pw_aff = ISLFunction.create(
    "isl_multi_union_pw_aff_get_union_pw_aff",
    Keep("MultiUnionPwAff"),
    Param(int, ctype=c_int),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_set_at = ISLFunction.create(
    "isl_multi_union_pw_aff_set_at",
    Take("MultiUnionPwAff"),
    Param(int, ctype=c_int),
    Take("UnionPwAff"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_set_union_pw_aff = ISLFunction.create(
    "isl_multi_union_pw_aff_set_union_pw_aff",
    Take("MultiUnionPwAff"),
    Param(int, ctype=c_int),
    Take("UnionPwAff"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_list = ISLFunction.create(
    "isl_multi_union_pw_aff_list",
    Keep("MultiUnionPwAff"),
    return_=Give("UnionPwAffList"),
    lib=_lib,
)

_isl_multi_union_pw_aff_extract_multi_pw_aff = ISLFunction.create(
    "isl_multi_union_pw_aff_extract_multi_pw_aff",
    Keep("MultiUnionPwAff"),
    Take("Space"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_from_union_pw_multi_aff = ISLFunction.create(
    "isl_multi_union_pw_aff_from_union_pw_multi_aff",
    Take("UnionPwMultiAff"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_read_from_str = ISLFunction.create(
    "isl_multi_union_pw_aff_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_to_str = ISLFunction.create(
    "isl_multi_union_pw_aff_to_str",
    Keep("MultiUnionPwAff"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_multi_union_pw_aff_range_is_wrapping = ISLFunction.create(
    "isl_multi_union_pw_aff_range_is_wrapping",
    Keep("MultiUnionPwAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_union_pw_aff_involves_nan = ISLFunction.create(
    "isl_multi_union_pw_aff_involves_nan",
    Keep("MultiUnionPwAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_union_pw_aff_plain_is_equal = ISLFunction.create(
    "isl_multi_union_pw_aff_plain_is_equal",
    Keep("MultiUnionPwAff"),
    Keep("MultiUnionPwAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_union_pw_aff_bind = ISLFunction.create(
    "isl_multi_union_pw_aff_bind",
    Take("MultiUnionPwAff"),
    Take("MultiId"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_multi_union_pw_aff_domain = ISLFunction.create(
    "isl_multi_union_pw_aff_domain",
    Take("MultiUnionPwAff"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_multi_union_pw_aff_from_range = ISLFunction.create(
    "isl_multi_union_pw_aff_from_range",
    Take("MultiUnionPwAff"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_zero_union_set = ISLFunction.create(
    "isl_multi_union_pw_aff_zero_union_set",
    Take("MultiUnionPwAff"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_multi_union_pw_aff_from_union_map = ISLFunction.create(
    "isl_multi_union_pw_aff_from_union_map",
    Take("UnionMap"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_coalesce = ISLFunction.create(
    "isl_multi_union_pw_aff_coalesce",
    Take("MultiUnionPwAff"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_min_multi_val = ISLFunction.create(
    "isl_multi_union_pw_aff_min_multi_val",
    Take("MultiUnionPwAff"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_union_pw_aff_max_multi_val = ISLFunction.create(
    "isl_multi_union_pw_aff_max_multi_val",
    Take("MultiUnionPwAff"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_union_pw_aff_flatten_range = ISLFunction.create(
    "isl_multi_union_pw_aff_flatten_range",
    Take("MultiUnionPwAff"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_align_params = ISLFunction.create(
    "isl_multi_union_pw_aff_align_params",
    Take("MultiUnionPwAff"),
    Take("Space"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_neg = ISLFunction.create(
    "isl_multi_union_pw_aff_neg",
    Take("MultiUnionPwAff"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_floor = ISLFunction.create(
    "isl_multi_union_pw_aff_floor",
    Take("MultiUnionPwAff"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_drop_dims = ISLFunction.create(
    "isl_multi_union_pw_aff_drop_dims",
    Take("MultiUnionPwAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_intersect_domain = ISLFunction.create(
    "isl_multi_union_pw_aff_intersect_domain",
    Take("MultiUnionPwAff"),
    Take("UnionSet"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_intersect_params = ISLFunction.create(
    "isl_multi_union_pw_aff_intersect_params",
    Take("MultiUnionPwAff"),
    Take("Set"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_intersect_range = ISLFunction.create(
    "isl_multi_union_pw_aff_intersect_range",
    Take("MultiUnionPwAff"),
    Take("Set"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_apply_aff = ISLFunction.create(
    "isl_multi_union_pw_aff_apply_aff",
    Take("MultiUnionPwAff"),
    Take("Aff"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_apply_pw_aff = ISLFunction.create(
    "isl_multi_union_pw_aff_apply_pw_aff",
    Take("MultiUnionPwAff"),
    Take("PwAff"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_apply_multi_aff = ISLFunction.create(
    "isl_multi_union_pw_aff_apply_multi_aff",
    Take("MultiUnionPwAff"),
    Take("MultiAff"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_apply_pw_multi_aff = ISLFunction.create(
    "isl_multi_union_pw_aff_apply_pw_multi_aff",
    Take("MultiUnionPwAff"),
    Take("PwMultiAff"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_pullback_union_pw_multi_aff = ISLFunction.create(
    "isl_multi_union_pw_aff_pullback_union_pw_multi_aff",
    Take("MultiUnionPwAff"),
    Take("UnionPwMultiAff"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_range_product = ISLFunction.create(
    "isl_multi_union_pw_aff_range_product",
    Take("MultiUnionPwAff"),
    Take("MultiUnionPwAff"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_flat_range_product = ISLFunction.create(
    "isl_multi_union_pw_aff_flat_range_product",
    Take("MultiUnionPwAff"),
    Take("MultiUnionPwAff"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_factor_range = ISLFunction.create(
    "isl_multi_union_pw_aff_factor_range",
    Take("MultiUnionPwAff"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_range_factor_domain = ISLFunction.create(
    "isl_multi_union_pw_aff_range_factor_domain",
    Take("MultiUnionPwAff"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_range_factor_range = ISLFunction.create(
    "isl_multi_union_pw_aff_range_factor_range",
    Take("MultiUnionPwAff"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_range_splice = ISLFunction.create(
    "isl_multi_union_pw_aff_range_splice",
    Take("MultiUnionPwAff"),
    Param(int, ctype=c_uint),
    Take("MultiUnionPwAff"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_gist_params = ISLFunction.create(
    "isl_multi_union_pw_aff_gist_params",
    Take("MultiUnionPwAff"),
    Take("Set"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_gist = ISLFunction.create(
    "isl_multi_union_pw_aff_gist",
    Take("MultiUnionPwAff"),
    Take("UnionSet"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_add = ISLFunction.create(
    "isl_multi_union_pw_aff_add",
    Take("MultiUnionPwAff"),
    Take("MultiUnionPwAff"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_sub = ISLFunction.create(
    "isl_multi_union_pw_aff_sub",
    Take("MultiUnionPwAff"),
    Take("MultiUnionPwAff"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_union_add = ISLFunction.create(
    "isl_multi_union_pw_aff_union_add",
    Take("MultiUnionPwAff"),
    Take("MultiUnionPwAff"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_scale_val = ISLFunction.create(
    "isl_multi_union_pw_aff_scale_val",
    Take("MultiUnionPwAff"),
    Take("Val"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_scale_down_val = ISLFunction.create(
    "isl_multi_union_pw_aff_scale_down_val",
    Take("MultiUnionPwAff"),
    Take("Val"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_mod_multi_val = ISLFunction.create(
    "isl_multi_union_pw_aff_mod_multi_val",
    Take("MultiUnionPwAff"),
    Take("MultiVal"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_scale_multi_val = ISLFunction.create(
    "isl_multi_union_pw_aff_scale_multi_val",
    Take("MultiUnionPwAff"),
    Take("MultiVal"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_union_pw_aff_scale_down_multi_val = ISLFunction.create(
    "isl_multi_union_pw_aff_scale_down_multi_val",
    Take("MultiUnionPwAff"),
    Take("MultiVal"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)
