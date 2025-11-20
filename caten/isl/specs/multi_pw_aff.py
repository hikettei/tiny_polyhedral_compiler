from __future__ import annotations

from ctypes import (
    c_char_p,
    c_int,
    c_uint,
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

class MultiPwAff(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_multi_pw_aff_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_multi_pw_aff_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_multi_pw_aff_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_multi_pw_aff_free(handle)

    def __str__(self) -> str:
        return _isl_multi_pw_aff_to_str(self)

    def __repr__(self) -> str:
        return f"MultiPwAff({self.__str__()})"

    def get_ctx(self) -> "Ctx":
        return _isl_multi_pw_aff_get_ctx(self)

    def get_domain_space(self) -> "Space":
        return _isl_multi_pw_aff_get_domain_space(self)

    def get_space(self) -> "Space":
        return _isl_multi_pw_aff_get_space(self)

    def dim(self, type: int) -> int:
        return _isl_multi_pw_aff_dim(self, type)

    def set_dim_id(self, type: int, pos: int, id: "Id") -> "MultiPwAff":
        return _isl_multi_pw_aff_set_dim_id(self, type, pos, id)

    def get_dim_id(self, type: int, pos: int) -> "Id":
        return _isl_multi_pw_aff_get_dim_id(self, type, pos)

    def set_dim_name(self, type: int, pos: int, s: str) -> "MultiPwAff":
        return _isl_multi_pw_aff_set_dim_name(self, type, pos, s)

    def find_dim_by_id(self, type: int, id: "Id") -> int:
        return _isl_multi_pw_aff_find_dim_by_id(self, type, id)

    def find_dim_by_name(self, type: int, name: str) -> int:
        return _isl_multi_pw_aff_find_dim_by_name(self, type, name)

    def set_range_tuple_id(self, id: "Id") -> "MultiPwAff":
        return _isl_multi_pw_aff_set_range_tuple_id(self, id)

    def reset_range_tuple_id(self) -> "MultiPwAff":
        return _isl_multi_pw_aff_reset_range_tuple_id(self)

    def reset_tuple_id(self, type: int) -> "MultiPwAff":
        return _isl_multi_pw_aff_reset_tuple_id(self, type)

    def has_range_tuple_id(self) -> bool:
        return _isl_multi_pw_aff_has_range_tuple_id(self)

    def get_range_tuple_id(self) -> "Id":
        return _isl_multi_pw_aff_get_range_tuple_id(self)

    def has_tuple_id(self, type: int) -> bool:
        return _isl_multi_pw_aff_has_tuple_id(self, type)

    def get_tuple_id(self, type: int) -> "Id":
        return _isl_multi_pw_aff_get_tuple_id(self, type)

    def set_tuple_name(self, type: int, s: str) -> "MultiPwAff":
        return _isl_multi_pw_aff_set_tuple_name(self, type, s)

    def reset_user(self) -> "MultiPwAff":
        return _isl_multi_pw_aff_reset_user(self)

    def as_set(self) -> "Set":
        return _isl_multi_pw_aff_as_set(self)

    def as_map(self) -> "Map":
        return _isl_multi_pw_aff_as_map(self)

    @classmethod
    def zero(cls, space: "Space") -> "MultiPwAff":
        return _isl_multi_pw_aff_zero(space)

    @classmethod
    def identity_on_domain_space(cls, space: "Space") -> "MultiPwAff":
        return _isl_multi_pw_aff_identity_on_domain_space(space)

    @classmethod
    def identity(cls, space: "Space") -> "MultiPwAff":
        return _isl_multi_pw_aff_identity(space)

    def identity_multi_pw_aff(self) -> "MultiPwAff":
        return _isl_multi_pw_aff_identity_multi_pw_aff(self)

    @classmethod
    def from_pw_aff(cls, pa: "PwAff") -> "MultiPwAff":
        return _isl_multi_pw_aff_from_pw_aff(pa)

    @classmethod
    def from_pw_aff_list(cls, space: "Space", list: "PwAffList") -> "MultiPwAff":
        return _isl_multi_pw_aff_from_pw_aff_list(space, list)

    @classmethod
    def from_aff(cls, aff: "Aff") -> "MultiPwAff":
        return _isl_multi_pw_aff_from_aff(aff)

    @classmethod
    def from_multi_aff(cls, ma: "MultiAff") -> "MultiPwAff":
        return _isl_multi_pw_aff_from_multi_aff(ma)

    def size(self) -> int:
        return _isl_multi_pw_aff_size(self)

    def get_at(self, pos: int) -> "PwAff":
        return _isl_multi_pw_aff_get_at(self, pos)

    def get_pw_aff(self, pos: int) -> "PwAff":
        return _isl_multi_pw_aff_get_pw_aff(self, pos)

    def set_at(self, pos: int, pa: "PwAff") -> "MultiPwAff":
        return _isl_multi_pw_aff_set_at(self, pos, pa)

    def set_pw_aff(self, pos: int, pa: "PwAff") -> "MultiPwAff":
        return _isl_multi_pw_aff_set_pw_aff(self, pos, pa)

    def get_list(self) -> "PwAffList":
        return _isl_multi_pw_aff_get_list(self)

    def isa_multi_aff(self) -> bool:
        return _isl_multi_pw_aff_isa_multi_aff(self)

    def as_multi_aff(self) -> "MultiAff":
        return _isl_multi_pw_aff_as_multi_aff(self)

    @classmethod
    def from_pw_multi_aff(cls, pma: "PwMultiAff") -> "MultiPwAff":
        return _isl_multi_pw_aff_from_pw_multi_aff(pma)

    def involves_dims(self, type: int, first: int, n: int) -> bool:
        return _isl_multi_pw_aff_involves_dims(self, type, first, n)

    def involves_param_id(self, id: "Id") -> bool:
        return _isl_multi_pw_aff_involves_param_id(self, id)

    def involves_param_id_list(self, list: "IdList") -> bool:
        return _isl_multi_pw_aff_involves_param_id_list(self, list)

    def range_is_wrapping(self) -> bool:
        return _isl_multi_pw_aff_range_is_wrapping(self)

    def is_cst(self) -> bool:
        return _isl_multi_pw_aff_is_cst(self)

    def involves_nan(self) -> bool:
        return _isl_multi_pw_aff_involves_nan(self)

    def is_equal(self, mpa2: "MultiPwAff") -> bool:
        return _isl_multi_pw_aff_plain_is_equal(self, mpa2)

    def is_equal(self, mpa2: "MultiPwAff") -> bool:
        return _isl_multi_pw_aff_is_equal(self, mpa2)

    def domain_reverse(self) -> "MultiPwAff":
        return _isl_multi_pw_aff_domain_reverse(self)

    def bind_domain(self, tuple: "MultiId") -> "MultiPwAff":
        return _isl_multi_pw_aff_bind_domain(self, tuple)

    def bind_domain_wrapped_domain(self, tuple: "MultiId") -> "MultiPwAff":
        return _isl_multi_pw_aff_bind_domain_wrapped_domain(self, tuple)

    def bind(self, tuple: "MultiId") -> "Set":
        return _isl_multi_pw_aff_bind(self, tuple)

    def project_domain_on_params(self) -> "MultiPwAff":
        return _isl_multi_pw_aff_project_domain_on_params(self)

    def domain(self) -> "Set":
        return _isl_multi_pw_aff_domain(self)

    def unbind_params_insert_domain(self, domain: "MultiId") -> "MultiPwAff":
        return _isl_multi_pw_aff_unbind_params_insert_domain(self, domain)

    def insert_domain(self, domain: "Space") -> "MultiPwAff":
        return _isl_multi_pw_aff_insert_domain(self, domain)

    def from_range(self) -> "MultiPwAff":
        return _isl_multi_pw_aff_from_range(self)

    def coalesce(self) -> "MultiPwAff":
        return _isl_multi_pw_aff_coalesce(self)

    def min_multi_val(self) -> "MultiVal":
        return _isl_multi_pw_aff_min_multi_val(self)

    def max_multi_val(self) -> "MultiVal":
        return _isl_multi_pw_aff_max_multi_val(self)

    def flatten_range(self) -> "MultiPwAff":
        return _isl_multi_pw_aff_flatten_range(self)

    def neg(self) -> "MultiPwAff":
        return _isl_multi_pw_aff_neg(self)

    def insert_dims(self, type: int, first: int, n: int) -> "MultiPwAff":
        return _isl_multi_pw_aff_insert_dims(self, type, first, n)

    def add_dims(self, type: int, n: int) -> "MultiPwAff":
        return _isl_multi_pw_aff_add_dims(self, type, n)

    def move_dims(self, dst_type: int, dst_pos: int, src_type: int, src_pos: int, n: int) -> "MultiPwAff":
        return _isl_multi_pw_aff_move_dims(self, dst_type, dst_pos, src_type, src_pos, n)

    def intersect_domain(self, domain: "Set") -> "MultiPwAff":
        return _isl_multi_pw_aff_intersect_domain(self, domain)

    def intersect_params(self, set: "Set") -> "MultiPwAff":
        return _isl_multi_pw_aff_intersect_params(self, set)

    def pullback_multi_aff(self, ma: "MultiAff") -> "MultiPwAff":
        return _isl_multi_pw_aff_pullback_multi_aff(self, ma)

    def pullback_pw_multi_aff(self, pma: "PwMultiAff") -> "MultiPwAff":
        return _isl_multi_pw_aff_pullback_pw_multi_aff(self, pma)

    def pullback_multi_pw_aff(self, mpa2: "MultiPwAff") -> "MultiPwAff":
        return _isl_multi_pw_aff_pullback_multi_pw_aff(self, mpa2)

    def eq_map(self, mpa2: "MultiPwAff") -> "Map":
        return _isl_multi_pw_aff_eq_map(self, mpa2)

    def lex_le_map(self, mpa2: "MultiPwAff") -> "Map":
        return _isl_multi_pw_aff_lex_le_map(self, mpa2)

    def lex_lt_map(self, mpa2: "MultiPwAff") -> "Map":
        return _isl_multi_pw_aff_lex_lt_map(self, mpa2)

    def lex_ge_map(self, mpa2: "MultiPwAff") -> "Map":
        return _isl_multi_pw_aff_lex_ge_map(self, mpa2)

    def lex_gt_map(self, mpa2: "MultiPwAff") -> "Map":
        return _isl_multi_pw_aff_lex_gt_map(self, mpa2)

    def range_product(self, mpa2: "MultiPwAff") -> "MultiPwAff":
        return _isl_multi_pw_aff_range_product(self, mpa2)

    def product(self, mpa2: "MultiPwAff") -> "MultiPwAff":
        return _isl_multi_pw_aff_product(self, mpa2)

    def flat_range_product(self, mpa2: "MultiPwAff") -> "MultiPwAff":
        return _isl_multi_pw_aff_flat_range_product(self, mpa2)

    def factor_range(self) -> "MultiPwAff":
        return _isl_multi_pw_aff_factor_range(self)

    def range_factor_domain(self) -> "MultiPwAff":
        return _isl_multi_pw_aff_range_factor_domain(self)

    def range_factor_range(self) -> "MultiPwAff":
        return _isl_multi_pw_aff_range_factor_range(self)

    def range_splice(self, pos: int, mpa2: "MultiPwAff") -> "MultiPwAff":
        return _isl_multi_pw_aff_range_splice(self, pos, mpa2)

    def splice(self, in_pos: int, out_pos: int, mpa2: "MultiPwAff") -> "MultiPwAff":
        return _isl_multi_pw_aff_splice(self, in_pos, out_pos, mpa2)

    def gist_params(self, set: "Set") -> "MultiPwAff":
        return _isl_multi_pw_aff_gist_params(self, set)

    def gist(self, set: "Set") -> "MultiPwAff":
        return _isl_multi_pw_aff_gist(self, set)

    def add(self, mpa2: "MultiPwAff") -> "MultiPwAff":
        return _isl_multi_pw_aff_add(self, mpa2)

    def add_constant_val(self, v: "Val") -> "MultiPwAff":
        return _isl_multi_pw_aff_add_constant_val(self, v)

    def add_constant_multi_val(self, mv: "MultiVal") -> "MultiPwAff":
        return _isl_multi_pw_aff_add_constant_multi_val(self, mv)

    def min(self, mpa2: "MultiPwAff") -> "MultiPwAff":
        return _isl_multi_pw_aff_min(self, mpa2)

    def max(self, mpa2: "MultiPwAff") -> "MultiPwAff":
        return _isl_multi_pw_aff_max(self, mpa2)

    def sub(self, mpa2: "MultiPwAff") -> "MultiPwAff":
        return _isl_multi_pw_aff_sub(self, mpa2)

    def union_add(self, mpa2: "MultiPwAff") -> "MultiPwAff":
        return _isl_multi_pw_aff_union_add(self, mpa2)

    def scale_val(self, v: "Val") -> "MultiPwAff":
        return _isl_multi_pw_aff_scale_val(self, v)

    def scale_down_val(self, v: "Val") -> "MultiPwAff":
        return _isl_multi_pw_aff_scale_down_val(self, v)

    def mod_multi_val(self, mv: "MultiVal") -> "MultiPwAff":
        return _isl_multi_pw_aff_mod_multi_val(self, mv)

    def scale_multi_val(self, mv: "MultiVal") -> "MultiPwAff":
        return _isl_multi_pw_aff_scale_multi_val(self, mv)

    def scale_down_multi_val(self, mv: "MultiVal") -> "MultiPwAff":
        return _isl_multi_pw_aff_scale_down_multi_val(self, mv)


register_type("MultiPwAff", MultiPwAff)

_isl_multi_pw_aff_get_ctx = ISLFunction.create(
    "isl_multi_pw_aff_get_ctx",
    Keep("MultiPwAff"),
    return_=Give("Ctx"),
    lib=_lib,
)

_isl_multi_pw_aff_get_domain_space = ISLFunction.create(
    "isl_multi_pw_aff_get_domain_space",
    Keep("MultiPwAff"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_multi_pw_aff_get_space = ISLFunction.create(
    "isl_multi_pw_aff_get_space",
    Keep("MultiPwAff"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_multi_pw_aff_dim = ISLFunction.create(
    "isl_multi_pw_aff_dim",
    Keep("MultiPwAff"),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_multi_pw_aff_set_dim_id = ISLFunction.create(
    "isl_multi_pw_aff_set_dim_id",
    Take("MultiPwAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Take("Id"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_get_dim_id = ISLFunction.create(
    "isl_multi_pw_aff_get_dim_id",
    Keep("MultiPwAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Give("Id"),
    lib=_lib,
)

_isl_multi_pw_aff_set_dim_name = ISLFunction.create(
    "isl_multi_pw_aff_set_dim_name",
    Take("MultiPwAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(str, ctype=c_char_p),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_find_dim_by_id = ISLFunction.create(
    "isl_multi_pw_aff_find_dim_by_id",
    Keep("MultiPwAff"),
    Param(int, ctype=c_int),
    Keep("Id"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_multi_pw_aff_find_dim_by_name = ISLFunction.create(
    "isl_multi_pw_aff_find_dim_by_name",
    Keep("MultiPwAff"),
    Param(int, ctype=c_int),
    Param(str, ctype=c_char_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_multi_pw_aff_set_range_tuple_id = ISLFunction.create(
    "isl_multi_pw_aff_set_range_tuple_id",
    Take("MultiPwAff"),
    Take("Id"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_reset_range_tuple_id = ISLFunction.create(
    "isl_multi_pw_aff_reset_range_tuple_id",
    Take("MultiPwAff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_reset_tuple_id = ISLFunction.create(
    "isl_multi_pw_aff_reset_tuple_id",
    Take("MultiPwAff"),
    Param(int, ctype=c_int),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_has_range_tuple_id = ISLFunction.create(
    "isl_multi_pw_aff_has_range_tuple_id",
    Keep("MultiPwAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_pw_aff_get_range_tuple_id = ISLFunction.create(
    "isl_multi_pw_aff_get_range_tuple_id",
    Keep("MultiPwAff"),
    return_=Give("Id"),
    lib=_lib,
)

_isl_multi_pw_aff_has_tuple_id = ISLFunction.create(
    "isl_multi_pw_aff_has_tuple_id",
    Keep("MultiPwAff"),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_pw_aff_get_tuple_id = ISLFunction.create(
    "isl_multi_pw_aff_get_tuple_id",
    Keep("MultiPwAff"),
    Param(int, ctype=c_int),
    return_=Give("Id"),
    lib=_lib,
)

_isl_multi_pw_aff_set_tuple_name = ISLFunction.create(
    "isl_multi_pw_aff_set_tuple_name",
    Take("MultiPwAff"),
    Param(int, ctype=c_int),
    Param(str, ctype=c_char_p),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_reset_user = ISLFunction.create(
    "isl_multi_pw_aff_reset_user",
    Take("MultiPwAff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_as_set = ISLFunction.create(
    "isl_multi_pw_aff_as_set",
    Take("MultiPwAff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_multi_pw_aff_as_map = ISLFunction.create(
    "isl_multi_pw_aff_as_map",
    Take("MultiPwAff"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_multi_pw_aff_zero = ISLFunction.create(
    "isl_multi_pw_aff_zero",
    Take("Space"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_identity_on_domain_space = ISLFunction.create(
    "isl_multi_pw_aff_identity_on_domain_space",
    Take("Space"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_identity = ISLFunction.create(
    "isl_multi_pw_aff_identity",
    Take("Space"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_identity_multi_pw_aff = ISLFunction.create(
    "isl_multi_pw_aff_identity_multi_pw_aff",
    Take("MultiPwAff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_from_pw_aff = ISLFunction.create(
    "isl_multi_pw_aff_from_pw_aff",
    Take("PwAff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_from_pw_aff_list = ISLFunction.create(
    "isl_multi_pw_aff_from_pw_aff_list",
    Take("Space"),
    Take("PwAffList"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_from_aff = ISLFunction.create(
    "isl_multi_pw_aff_from_aff",
    Take("Aff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_from_multi_aff = ISLFunction.create(
    "isl_multi_pw_aff_from_multi_aff",
    Take("MultiAff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_copy = ISLFunction.create(
    "isl_multi_pw_aff_copy",
    Keep("MultiPwAff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_free = ISLFunction.create(
    "isl_multi_pw_aff_free",
    Take("MultiPwAff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_size = ISLFunction.create(
    "isl_multi_pw_aff_size",
    Keep("MultiPwAff"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_multi_pw_aff_get_at = ISLFunction.create(
    "isl_multi_pw_aff_get_at",
    Keep("MultiPwAff"),
    Param(int, ctype=c_int),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_get_pw_aff = ISLFunction.create(
    "isl_multi_pw_aff_get_pw_aff",
    Keep("MultiPwAff"),
    Param(int, ctype=c_int),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_set_at = ISLFunction.create(
    "isl_multi_pw_aff_set_at",
    Take("MultiPwAff"),
    Param(int, ctype=c_int),
    Take("PwAff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_set_pw_aff = ISLFunction.create(
    "isl_multi_pw_aff_set_pw_aff",
    Take("MultiPwAff"),
    Param(int, ctype=c_int),
    Take("PwAff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_get_list = ISLFunction.create(
    "isl_multi_pw_aff_get_list",
    Keep("MultiPwAff"),
    return_=Give("PwAffList"),
    lib=_lib,
)

_isl_multi_pw_aff_isa_multi_aff = ISLFunction.create(
    "isl_multi_pw_aff_isa_multi_aff",
    Keep("MultiPwAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_pw_aff_as_multi_aff = ISLFunction.create(
    "isl_multi_pw_aff_as_multi_aff",
    Take("MultiPwAff"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_pw_aff_from_pw_multi_aff = ISLFunction.create(
    "isl_multi_pw_aff_from_pw_multi_aff",
    Take("PwMultiAff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_read_from_str = ISLFunction.create(
    "isl_multi_pw_aff_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_to_str = ISLFunction.create(
    "isl_multi_pw_aff_to_str",
    Keep("MultiPwAff"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_multi_pw_aff_involves_dims = ISLFunction.create(
    "isl_multi_pw_aff_involves_dims",
    Keep("MultiPwAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_pw_aff_involves_param_id = ISLFunction.create(
    "isl_multi_pw_aff_involves_param_id",
    Keep("MultiPwAff"),
    Keep("Id"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_pw_aff_involves_param_id_list = ISLFunction.create(
    "isl_multi_pw_aff_involves_param_id_list",
    Keep("MultiPwAff"),
    Keep("IdList"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_pw_aff_range_is_wrapping = ISLFunction.create(
    "isl_multi_pw_aff_range_is_wrapping",
    Keep("MultiPwAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_pw_aff_is_cst = ISLFunction.create(
    "isl_multi_pw_aff_is_cst",
    Keep("MultiPwAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_pw_aff_involves_nan = ISLFunction.create(
    "isl_multi_pw_aff_involves_nan",
    Keep("MultiPwAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_pw_aff_plain_is_equal = ISLFunction.create(
    "isl_multi_pw_aff_plain_is_equal",
    Keep("MultiPwAff"),
    Keep("MultiPwAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_pw_aff_is_equal = ISLFunction.create(
    "isl_multi_pw_aff_is_equal",
    Keep("MultiPwAff"),
    Keep("MultiPwAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_pw_aff_domain_reverse = ISLFunction.create(
    "isl_multi_pw_aff_domain_reverse",
    Take("MultiPwAff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_bind_domain = ISLFunction.create(
    "isl_multi_pw_aff_bind_domain",
    Take("MultiPwAff"),
    Take("MultiId"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_bind_domain_wrapped_domain = ISLFunction.create(
    "isl_multi_pw_aff_bind_domain_wrapped_domain",
    Take("MultiPwAff"),
    Take("MultiId"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_bind = ISLFunction.create(
    "isl_multi_pw_aff_bind",
    Take("MultiPwAff"),
    Take("MultiId"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_multi_pw_aff_project_domain_on_params = ISLFunction.create(
    "isl_multi_pw_aff_project_domain_on_params",
    Take("MultiPwAff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_domain = ISLFunction.create(
    "isl_multi_pw_aff_domain",
    Take("MultiPwAff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_multi_pw_aff_unbind_params_insert_domain = ISLFunction.create(
    "isl_multi_pw_aff_unbind_params_insert_domain",
    Take("MultiPwAff"),
    Take("MultiId"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_insert_domain = ISLFunction.create(
    "isl_multi_pw_aff_insert_domain",
    Take("MultiPwAff"),
    Take("Space"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_from_range = ISLFunction.create(
    "isl_multi_pw_aff_from_range",
    Take("MultiPwAff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_coalesce = ISLFunction.create(
    "isl_multi_pw_aff_coalesce",
    Take("MultiPwAff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_min_multi_val = ISLFunction.create(
    "isl_multi_pw_aff_min_multi_val",
    Take("MultiPwAff"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_pw_aff_max_multi_val = ISLFunction.create(
    "isl_multi_pw_aff_max_multi_val",
    Take("MultiPwAff"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_pw_aff_flatten_range = ISLFunction.create(
    "isl_multi_pw_aff_flatten_range",
    Take("MultiPwAff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_neg = ISLFunction.create(
    "isl_multi_pw_aff_neg",
    Take("MultiPwAff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_insert_dims = ISLFunction.create(
    "isl_multi_pw_aff_insert_dims",
    Take("MultiPwAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_add_dims = ISLFunction.create(
    "isl_multi_pw_aff_add_dims",
    Take("MultiPwAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_move_dims = ISLFunction.create(
    "isl_multi_pw_aff_move_dims",
    Take("MultiPwAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_intersect_domain = ISLFunction.create(
    "isl_multi_pw_aff_intersect_domain",
    Take("MultiPwAff"),
    Take("Set"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_intersect_params = ISLFunction.create(
    "isl_multi_pw_aff_intersect_params",
    Take("MultiPwAff"),
    Take("Set"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_pullback_multi_aff = ISLFunction.create(
    "isl_multi_pw_aff_pullback_multi_aff",
    Take("MultiPwAff"),
    Take("MultiAff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_pullback_pw_multi_aff = ISLFunction.create(
    "isl_multi_pw_aff_pullback_pw_multi_aff",
    Take("MultiPwAff"),
    Take("PwMultiAff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_pullback_multi_pw_aff = ISLFunction.create(
    "isl_multi_pw_aff_pullback_multi_pw_aff",
    Take("MultiPwAff"),
    Take("MultiPwAff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_eq_map = ISLFunction.create(
    "isl_multi_pw_aff_eq_map",
    Take("MultiPwAff"),
    Take("MultiPwAff"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_multi_pw_aff_lex_le_map = ISLFunction.create(
    "isl_multi_pw_aff_lex_le_map",
    Take("MultiPwAff"),
    Take("MultiPwAff"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_multi_pw_aff_lex_lt_map = ISLFunction.create(
    "isl_multi_pw_aff_lex_lt_map",
    Take("MultiPwAff"),
    Take("MultiPwAff"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_multi_pw_aff_lex_ge_map = ISLFunction.create(
    "isl_multi_pw_aff_lex_ge_map",
    Take("MultiPwAff"),
    Take("MultiPwAff"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_multi_pw_aff_lex_gt_map = ISLFunction.create(
    "isl_multi_pw_aff_lex_gt_map",
    Take("MultiPwAff"),
    Take("MultiPwAff"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_multi_pw_aff_range_product = ISLFunction.create(
    "isl_multi_pw_aff_range_product",
    Take("MultiPwAff"),
    Take("MultiPwAff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_product = ISLFunction.create(
    "isl_multi_pw_aff_product",
    Take("MultiPwAff"),
    Take("MultiPwAff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_flat_range_product = ISLFunction.create(
    "isl_multi_pw_aff_flat_range_product",
    Take("MultiPwAff"),
    Take("MultiPwAff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_factor_range = ISLFunction.create(
    "isl_multi_pw_aff_factor_range",
    Take("MultiPwAff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_range_factor_domain = ISLFunction.create(
    "isl_multi_pw_aff_range_factor_domain",
    Take("MultiPwAff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_range_factor_range = ISLFunction.create(
    "isl_multi_pw_aff_range_factor_range",
    Take("MultiPwAff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_range_splice = ISLFunction.create(
    "isl_multi_pw_aff_range_splice",
    Take("MultiPwAff"),
    Param(int, ctype=c_uint),
    Take("MultiPwAff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_splice = ISLFunction.create(
    "isl_multi_pw_aff_splice",
    Take("MultiPwAff"),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    Take("MultiPwAff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_gist_params = ISLFunction.create(
    "isl_multi_pw_aff_gist_params",
    Take("MultiPwAff"),
    Take("Set"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_gist = ISLFunction.create(
    "isl_multi_pw_aff_gist",
    Take("MultiPwAff"),
    Take("Set"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_add = ISLFunction.create(
    "isl_multi_pw_aff_add",
    Take("MultiPwAff"),
    Take("MultiPwAff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_add_constant_val = ISLFunction.create(
    "isl_multi_pw_aff_add_constant_val",
    Take("MultiPwAff"),
    Take("Val"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_add_constant_multi_val = ISLFunction.create(
    "isl_multi_pw_aff_add_constant_multi_val",
    Take("MultiPwAff"),
    Take("MultiVal"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_min = ISLFunction.create(
    "isl_multi_pw_aff_min",
    Take("MultiPwAff"),
    Take("MultiPwAff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_max = ISLFunction.create(
    "isl_multi_pw_aff_max",
    Take("MultiPwAff"),
    Take("MultiPwAff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_sub = ISLFunction.create(
    "isl_multi_pw_aff_sub",
    Take("MultiPwAff"),
    Take("MultiPwAff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_union_add = ISLFunction.create(
    "isl_multi_pw_aff_union_add",
    Take("MultiPwAff"),
    Take("MultiPwAff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_scale_val = ISLFunction.create(
    "isl_multi_pw_aff_scale_val",
    Take("MultiPwAff"),
    Take("Val"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_scale_down_val = ISLFunction.create(
    "isl_multi_pw_aff_scale_down_val",
    Take("MultiPwAff"),
    Take("Val"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_mod_multi_val = ISLFunction.create(
    "isl_multi_pw_aff_mod_multi_val",
    Take("MultiPwAff"),
    Take("MultiVal"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_scale_multi_val = ISLFunction.create(
    "isl_multi_pw_aff_scale_multi_val",
    Take("MultiPwAff"),
    Take("MultiVal"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_pw_aff_scale_down_multi_val = ISLFunction.create(
    "isl_multi_pw_aff_scale_down_multi_val",
    Take("MultiPwAff"),
    Take("MultiVal"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)
