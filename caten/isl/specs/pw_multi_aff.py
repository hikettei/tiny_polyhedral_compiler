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

class PwMultiAff(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_pw_multi_aff_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_pw_multi_aff_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_pw_multi_aff_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_pw_multi_aff_free(handle)

    def __str__(self) -> str:
        return _isl_pw_multi_aff_to_str(self)

    def __repr__(self) -> str:
        return f"PwMultiAff({self.__str__()})"

    def get_ctx(self) -> "Ctx":
        return _isl_pw_multi_aff_get_ctx(self)

    def get_domain_space(self) -> "Space":
        return _isl_pw_multi_aff_get_domain_space(self)

    def get_space(self) -> "Space":
        return _isl_pw_multi_aff_get_space(self)

    def dim(self, type: int) -> int:
        return _isl_pw_multi_aff_dim(self, type)

    def get_dim_id(self, type: int, pos: int) -> "Id":
        return _isl_pw_multi_aff_get_dim_id(self, type, pos)

    def get_dim_name(self, type: int, pos: int) -> str:
        return _isl_pw_multi_aff_get_dim_name(self, type, pos)

    def find_dim_by_name(self, type: int, name: str) -> int:
        return _isl_pw_multi_aff_find_dim_by_name(self, type, name)

    def set_range_tuple_id(self, id: "Id") -> "PwMultiAff":
        return _isl_pw_multi_aff_set_range_tuple_id(self, id)

    def set_tuple_id(self, type: int, id: "Id") -> "PwMultiAff":
        return _isl_pw_multi_aff_set_tuple_id(self, type, id)

    def reset_tuple_id(self, type: int) -> "PwMultiAff":
        return _isl_pw_multi_aff_reset_tuple_id(self, type)

    def has_range_tuple_id(self) -> bool:
        return _isl_pw_multi_aff_has_range_tuple_id(self)

    def has_tuple_id(self, type: int) -> bool:
        return _isl_pw_multi_aff_has_tuple_id(self, type)

    def get_range_tuple_id(self) -> "Id":
        return _isl_pw_multi_aff_get_range_tuple_id(self)

    def get_tuple_id(self, type: int) -> "Id":
        return _isl_pw_multi_aff_get_tuple_id(self, type)

    def has_tuple_name(self, type: int) -> bool:
        return _isl_pw_multi_aff_has_tuple_name(self, type)

    def get_tuple_name(self, type: int) -> str:
        return _isl_pw_multi_aff_get_tuple_name(self, type)

    def reset_user(self) -> "PwMultiAff":
        return _isl_pw_multi_aff_reset_user(self)

    def as_set(self) -> "Set":
        return _isl_pw_multi_aff_as_set(self)

    def as_map(self) -> "Map":
        return _isl_pw_multi_aff_as_map(self)

    @classmethod
    def empty(cls, space: "Space") -> "PwMultiAff":
        return _isl_pw_multi_aff_empty(space)

    @classmethod
    def from_multi_aff(cls, ma: "MultiAff") -> "PwMultiAff":
        return _isl_pw_multi_aff_from_multi_aff(ma)

    def isa_multi_aff(self) -> bool:
        return _isl_pw_multi_aff_isa_multi_aff(self)

    def as_multi_aff(self) -> "MultiAff":
        return _isl_pw_multi_aff_as_multi_aff(self)

    @classmethod
    def alloc(cls, set: "Set", maff: "MultiAff") -> "PwMultiAff":
        return _isl_pw_multi_aff_alloc(set, maff)

    @classmethod
    def zero(cls, space: "Space") -> "PwMultiAff":
        return _isl_pw_multi_aff_zero(space)

    @classmethod
    def identity_on_domain_space(cls, space: "Space") -> "PwMultiAff":
        return _isl_pw_multi_aff_identity_on_domain_space(space)

    @classmethod
    def identity(cls, space: "Space") -> "PwMultiAff":
        return _isl_pw_multi_aff_identity(space)

    @classmethod
    def domain_map(cls, space: "Space") -> "PwMultiAff":
        return _isl_pw_multi_aff_domain_map(space)

    @classmethod
    def range_map(cls, space: "Space") -> "PwMultiAff":
        return _isl_pw_multi_aff_range_map(space)

    @classmethod
    def project_out_map(cls, space: "Space", type: int, first: int, n: int) -> "PwMultiAff":
        return _isl_pw_multi_aff_project_out_map(space, type, first, n)

    @classmethod
    def multi_val_on_domain(cls, domain: "Set", mv: "MultiVal") -> "PwMultiAff":
        return _isl_pw_multi_aff_multi_val_on_domain(domain, mv)

    @classmethod
    def from_pw_aff(cls, pa: "PwAff") -> "PwMultiAff":
        return _isl_pw_multi_aff_from_pw_aff(pa)

    def n_piece(self) -> int:
        return _isl_pw_multi_aff_n_piece(self)

    def foreach_piece(self, fn: Any, user: Any = None) -> int:
        return _isl_pw_multi_aff_foreach_piece(self, fn, user)

    def every_piece(self, test: Any, user: Any = None) -> bool:
        return _isl_pw_multi_aff_every_piece(self, test, user)

    def get_at(self, pos: int) -> "PwAff":
        return _isl_pw_multi_aff_get_at(self, pos)

    def get_pw_aff(self, pos: int) -> "PwAff":
        return _isl_pw_multi_aff_get_pw_aff(self, pos)

    def set_pw_aff(self, pos: int, pa: "PwAff") -> "PwMultiAff":
        return _isl_pw_multi_aff_set_pw_aff(self, pos, pa)

    @classmethod
    def from_multi_pw_aff(cls, mpa: "MultiPwAff") -> "PwMultiAff":
        return _isl_pw_multi_aff_from_multi_pw_aff(mpa)

    def to_multi_pw_aff(self) -> "MultiPwAff":
        return _isl_pw_multi_aff_to_multi_pw_aff(self)

    def to_union_pw_multi_aff(self) -> "UnionPwMultiAff":
        return _isl_pw_multi_aff_to_union_pw_multi_aff(self)

    def involves_locals(self) -> bool:
        return _isl_pw_multi_aff_involves_locals(self)

    def involves_param_id(self, id: "Id") -> bool:
        return _isl_pw_multi_aff_involves_param_id(self, id)

    def involves_dims(self, type: int, first: int, n: int) -> bool:
        return _isl_pw_multi_aff_involves_dims(self, type, first, n)

    def involves_nan(self) -> bool:
        return _isl_pw_multi_aff_involves_nan(self)

    def is_equal(self, pma2: "PwMultiAff") -> bool:
        return _isl_pw_multi_aff_plain_is_equal(self, pma2)

    def is_equal(self, pma2: "PwMultiAff") -> bool:
        return _isl_pw_multi_aff_is_equal(self, pma2)

    def domain_reverse(self) -> "PwMultiAff":
        return _isl_pw_multi_aff_domain_reverse(self)

    def bind_domain(self, tuple: "MultiId") -> "PwMultiAff":
        return _isl_pw_multi_aff_bind_domain(self, tuple)

    def bind_domain_wrapped_domain(self, tuple: "MultiId") -> "PwMultiAff":
        return _isl_pw_multi_aff_bind_domain_wrapped_domain(self, tuple)

    def project_domain_on_params(self) -> "PwMultiAff":
        return _isl_pw_multi_aff_project_domain_on_params(self)

    def domain(self) -> "Set":
        return _isl_pw_multi_aff_domain(self)

    def insert_domain(self, domain: "Space") -> "PwMultiAff":
        return _isl_pw_multi_aff_insert_domain(self, domain)

    @classmethod
    def from_domain(cls, set: "Set") -> "PwMultiAff":
        return _isl_pw_multi_aff_from_domain(set)

    def fix_si(self, type: int, pos: int, value: int) -> "PwMultiAff":
        return _isl_pw_multi_aff_fix_si(self, type, pos, value)

    @classmethod
    def from_set(cls, set: "Set") -> "PwMultiAff":
        return _isl_pw_multi_aff_from_set(set)

    @classmethod
    def from_map(cls, map: "Map") -> "PwMultiAff":
        return _isl_pw_multi_aff_from_map(map)

    def coalesce(self) -> "PwMultiAff":
        return _isl_pw_multi_aff_coalesce(self)

    def min_multi_val(self) -> "MultiVal":
        return _isl_pw_multi_aff_min_multi_val(self)

    def max_multi_val(self) -> "MultiVal":
        return _isl_pw_multi_aff_max_multi_val(self)

    def align_params(self, model: "Space") -> "PwMultiAff":
        return _isl_pw_multi_aff_align_params(self, model)

    def drop_unused_params(self) -> "PwMultiAff":
        return _isl_pw_multi_aff_drop_unused_params(self)

    def neg(self) -> "PwMultiAff":
        return _isl_pw_multi_aff_neg(self)

    def drop_dims(self, type: int, first: int, n: int) -> "PwMultiAff":
        return _isl_pw_multi_aff_drop_dims(self, type, first, n)

    def intersect_domain(self, set: "Set") -> "PwMultiAff":
        return _isl_pw_multi_aff_intersect_domain(self, set)

    def intersect_domain_wrapped_domain(self, set: "Set") -> "PwMultiAff":
        return _isl_pw_multi_aff_intersect_domain_wrapped_domain(self, set)

    def intersect_domain_wrapped_range(self, set: "Set") -> "PwMultiAff":
        return _isl_pw_multi_aff_intersect_domain_wrapped_range(self, set)

    def intersect_params(self, set: "Set") -> "PwMultiAff":
        return _isl_pw_multi_aff_intersect_params(self, set)

    def subtract_domain(self, set: "Set") -> "PwMultiAff":
        return _isl_pw_multi_aff_subtract_domain(self, set)

    def preimage_domain_wrapped_domain_pw_multi_aff(self, pma2: "PwMultiAff") -> "PwMultiAff":
        return _isl_pw_multi_aff_preimage_domain_wrapped_domain_pw_multi_aff(self, pma2)

    def pullback_multi_aff(self, ma: "MultiAff") -> "PwMultiAff":
        return _isl_pw_multi_aff_pullback_multi_aff(self, ma)

    def pullback_pw_multi_aff(self, pma2: "PwMultiAff") -> "PwMultiAff":
        return _isl_pw_multi_aff_pullback_pw_multi_aff(self, pma2)

    def range_product(self, pma2: "PwMultiAff") -> "PwMultiAff":
        return _isl_pw_multi_aff_range_product(self, pma2)

    def product(self, pma2: "PwMultiAff") -> "PwMultiAff":
        return _isl_pw_multi_aff_product(self, pma2)

    def flat_range_product(self, pma2: "PwMultiAff") -> "PwMultiAff":
        return _isl_pw_multi_aff_flat_range_product(self, pma2)

    def range_factor_domain(self) -> "PwMultiAff":
        return _isl_pw_multi_aff_range_factor_domain(self)

    def range_factor_range(self) -> "PwMultiAff":
        return _isl_pw_multi_aff_range_factor_range(self)

    def gist_params(self, set: "Set") -> "PwMultiAff":
        return _isl_pw_multi_aff_gist_params(self, set)

    def gist(self, set: "Set") -> "PwMultiAff":
        return _isl_pw_multi_aff_gist(self, set)

    def add(self, pma2: "PwMultiAff") -> "PwMultiAff":
        return _isl_pw_multi_aff_add(self, pma2)

    def add_constant_val(self, v: "Val") -> "PwMultiAff":
        return _isl_pw_multi_aff_add_constant_val(self, v)

    def add_constant_multi_val(self, mv: "MultiVal") -> "PwMultiAff":
        return _isl_pw_multi_aff_add_constant_multi_val(self, mv)

    def sub(self, pma2: "PwMultiAff") -> "PwMultiAff":
        return _isl_pw_multi_aff_sub(self, pma2)

    def union_add(self, pma2: "PwMultiAff") -> "PwMultiAff":
        return _isl_pw_multi_aff_union_add(self, pma2)

    def scale_val(self, v: "Val") -> "PwMultiAff":
        return _isl_pw_multi_aff_scale_val(self, v)

    def scale_down_val(self, v: "Val") -> "PwMultiAff":
        return _isl_pw_multi_aff_scale_down_val(self, v)

    def scale_multi_val(self, mv: "MultiVal") -> "PwMultiAff":
        return _isl_pw_multi_aff_scale_multi_val(self, mv)

    def scale_down_multi_val(self, mv: "MultiVal") -> "PwMultiAff":
        return _isl_pw_multi_aff_scale_down_multi_val(self, mv)

    def union_lexmin(self, pma2: "PwMultiAff") -> "PwMultiAff":
        return _isl_pw_multi_aff_union_lexmin(self, pma2)

    def union_lexmax(self, pma2: "PwMultiAff") -> "PwMultiAff":
        return _isl_pw_multi_aff_union_lexmax(self, pma2)


register_type("PwMultiAff", PwMultiAff)

_isl_pw_multi_aff_get_ctx = ISLFunction.create(
    "isl_pw_multi_aff_get_ctx",
    Keep("PwMultiAff"),
    return_=Give("Ctx"),
    lib=_lib,
)

_isl_pw_multi_aff_get_domain_space = ISLFunction.create(
    "isl_pw_multi_aff_get_domain_space",
    Keep("PwMultiAff"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_pw_multi_aff_get_space = ISLFunction.create(
    "isl_pw_multi_aff_get_space",
    Keep("PwMultiAff"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_pw_multi_aff_dim = ISLFunction.create(
    "isl_pw_multi_aff_dim",
    Keep("PwMultiAff"),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_pw_multi_aff_get_dim_id = ISLFunction.create(
    "isl_pw_multi_aff_get_dim_id",
    Keep("PwMultiAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Give("Id"),
    lib=_lib,
)

_isl_pw_multi_aff_get_dim_name = ISLFunction.create(
    "isl_pw_multi_aff_get_dim_name",
    Keep("PwMultiAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_pw_multi_aff_find_dim_by_name = ISLFunction.create(
    "isl_pw_multi_aff_find_dim_by_name",
    Keep("PwMultiAff"),
    Param(int, ctype=c_int),
    Param(str, ctype=c_char_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_pw_multi_aff_set_range_tuple_id = ISLFunction.create(
    "isl_pw_multi_aff_set_range_tuple_id",
    Take("PwMultiAff"),
    Take("Id"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_set_tuple_id = ISLFunction.create(
    "isl_pw_multi_aff_set_tuple_id",
    Take("PwMultiAff"),
    Param(int, ctype=c_int),
    Take("Id"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_reset_tuple_id = ISLFunction.create(
    "isl_pw_multi_aff_reset_tuple_id",
    Take("PwMultiAff"),
    Param(int, ctype=c_int),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_has_range_tuple_id = ISLFunction.create(
    "isl_pw_multi_aff_has_range_tuple_id",
    Keep("PwMultiAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_multi_aff_has_tuple_id = ISLFunction.create(
    "isl_pw_multi_aff_has_tuple_id",
    Keep("PwMultiAff"),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_multi_aff_get_range_tuple_id = ISLFunction.create(
    "isl_pw_multi_aff_get_range_tuple_id",
    Keep("PwMultiAff"),
    return_=Give("Id"),
    lib=_lib,
)

_isl_pw_multi_aff_get_tuple_id = ISLFunction.create(
    "isl_pw_multi_aff_get_tuple_id",
    Keep("PwMultiAff"),
    Param(int, ctype=c_int),
    return_=Give("Id"),
    lib=_lib,
)

_isl_pw_multi_aff_has_tuple_name = ISLFunction.create(
    "isl_pw_multi_aff_has_tuple_name",
    Keep("PwMultiAff"),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_multi_aff_get_tuple_name = ISLFunction.create(
    "isl_pw_multi_aff_get_tuple_name",
    Keep("PwMultiAff"),
    Param(int, ctype=c_int),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_pw_multi_aff_reset_user = ISLFunction.create(
    "isl_pw_multi_aff_reset_user",
    Take("PwMultiAff"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_as_set = ISLFunction.create(
    "isl_pw_multi_aff_as_set",
    Take("PwMultiAff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_pw_multi_aff_as_map = ISLFunction.create(
    "isl_pw_multi_aff_as_map",
    Take("PwMultiAff"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_pw_multi_aff_empty = ISLFunction.create(
    "isl_pw_multi_aff_empty",
    Take("Space"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_from_multi_aff = ISLFunction.create(
    "isl_pw_multi_aff_from_multi_aff",
    Take("MultiAff"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_isa_multi_aff = ISLFunction.create(
    "isl_pw_multi_aff_isa_multi_aff",
    Keep("PwMultiAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_multi_aff_as_multi_aff = ISLFunction.create(
    "isl_pw_multi_aff_as_multi_aff",
    Take("PwMultiAff"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_alloc = ISLFunction.create(
    "isl_pw_multi_aff_alloc",
    Take("Set"),
    Take("MultiAff"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_zero = ISLFunction.create(
    "isl_pw_multi_aff_zero",
    Take("Space"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_identity_on_domain_space = ISLFunction.create(
    "isl_pw_multi_aff_identity_on_domain_space",
    Take("Space"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_identity = ISLFunction.create(
    "isl_pw_multi_aff_identity",
    Take("Space"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_domain_map = ISLFunction.create(
    "isl_pw_multi_aff_domain_map",
    Take("Space"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_range_map = ISLFunction.create(
    "isl_pw_multi_aff_range_map",
    Take("Space"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_project_out_map = ISLFunction.create(
    "isl_pw_multi_aff_project_out_map",
    Take("Space"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_multi_val_on_domain = ISLFunction.create(
    "isl_pw_multi_aff_multi_val_on_domain",
    Take("Set"),
    Take("MultiVal"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_from_pw_aff = ISLFunction.create(
    "isl_pw_multi_aff_from_pw_aff",
    Take("PwAff"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_copy = ISLFunction.create(
    "isl_pw_multi_aff_copy",
    Keep("PwMultiAff"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_free = ISLFunction.create(
    "isl_pw_multi_aff_free",
    Take("PwMultiAff"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_n_piece = ISLFunction.create(
    "isl_pw_multi_aff_n_piece",
    Keep("PwMultiAff"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_pw_multi_aff_foreach_piece = ISLFunction.create(
    "isl_pw_multi_aff_foreach_piece",
    Keep("PwMultiAff"),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_pw_multi_aff_every_piece = ISLFunction.create(
    "isl_pw_multi_aff_every_piece",
    Keep("PwMultiAff"),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_multi_aff_get_at = ISLFunction.create(
    "isl_pw_multi_aff_get_at",
    Keep("PwMultiAff"),
    Param(int, ctype=c_int),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_multi_aff_get_pw_aff = ISLFunction.create(
    "isl_pw_multi_aff_get_pw_aff",
    Keep("PwMultiAff"),
    Param(int, ctype=c_int),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_multi_aff_set_pw_aff = ISLFunction.create(
    "isl_pw_multi_aff_set_pw_aff",
    Take("PwMultiAff"),
    Param(int, ctype=c_uint),
    Take("PwAff"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_from_multi_pw_aff = ISLFunction.create(
    "isl_pw_multi_aff_from_multi_pw_aff",
    Take("MultiPwAff"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_to_multi_pw_aff = ISLFunction.create(
    "isl_pw_multi_aff_to_multi_pw_aff",
    Take("PwMultiAff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_pw_multi_aff_to_union_pw_multi_aff = ISLFunction.create(
    "isl_pw_multi_aff_to_union_pw_multi_aff",
    Take("PwMultiAff"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_read_from_str = ISLFunction.create(
    "isl_pw_multi_aff_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_to_str = ISLFunction.create(
    "isl_pw_multi_aff_to_str",
    Keep("PwMultiAff"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_pw_multi_aff_involves_locals = ISLFunction.create(
    "isl_pw_multi_aff_involves_locals",
    Keep("PwMultiAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_multi_aff_involves_param_id = ISLFunction.create(
    "isl_pw_multi_aff_involves_param_id",
    Keep("PwMultiAff"),
    Keep("Id"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_multi_aff_involves_dims = ISLFunction.create(
    "isl_pw_multi_aff_involves_dims",
    Keep("PwMultiAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_multi_aff_involves_nan = ISLFunction.create(
    "isl_pw_multi_aff_involves_nan",
    Keep("PwMultiAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_multi_aff_plain_is_equal = ISLFunction.create(
    "isl_pw_multi_aff_plain_is_equal",
    Keep("PwMultiAff"),
    Keep("PwMultiAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_multi_aff_is_equal = ISLFunction.create(
    "isl_pw_multi_aff_is_equal",
    Keep("PwMultiAff"),
    Keep("PwMultiAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_multi_aff_domain_reverse = ISLFunction.create(
    "isl_pw_multi_aff_domain_reverse",
    Take("PwMultiAff"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_bind_domain = ISLFunction.create(
    "isl_pw_multi_aff_bind_domain",
    Take("PwMultiAff"),
    Take("MultiId"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_bind_domain_wrapped_domain = ISLFunction.create(
    "isl_pw_multi_aff_bind_domain_wrapped_domain",
    Take("PwMultiAff"),
    Take("MultiId"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_project_domain_on_params = ISLFunction.create(
    "isl_pw_multi_aff_project_domain_on_params",
    Take("PwMultiAff"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_domain = ISLFunction.create(
    "isl_pw_multi_aff_domain",
    Take("PwMultiAff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_pw_multi_aff_insert_domain = ISLFunction.create(
    "isl_pw_multi_aff_insert_domain",
    Take("PwMultiAff"),
    Take("Space"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_from_domain = ISLFunction.create(
    "isl_pw_multi_aff_from_domain",
    Take("Set"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_fix_si = ISLFunction.create(
    "isl_pw_multi_aff_fix_si",
    Take("PwMultiAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_int),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_from_set = ISLFunction.create(
    "isl_pw_multi_aff_from_set",
    Take("Set"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_from_map = ISLFunction.create(
    "isl_pw_multi_aff_from_map",
    Take("Map"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_coalesce = ISLFunction.create(
    "isl_pw_multi_aff_coalesce",
    Take("PwMultiAff"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_min_multi_val = ISLFunction.create(
    "isl_pw_multi_aff_min_multi_val",
    Take("PwMultiAff"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_pw_multi_aff_max_multi_val = ISLFunction.create(
    "isl_pw_multi_aff_max_multi_val",
    Take("PwMultiAff"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_pw_multi_aff_align_params = ISLFunction.create(
    "isl_pw_multi_aff_align_params",
    Take("PwMultiAff"),
    Take("Space"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_drop_unused_params = ISLFunction.create(
    "isl_pw_multi_aff_drop_unused_params",
    Take("PwMultiAff"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_neg = ISLFunction.create(
    "isl_pw_multi_aff_neg",
    Take("PwMultiAff"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_drop_dims = ISLFunction.create(
    "isl_pw_multi_aff_drop_dims",
    Take("PwMultiAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_intersect_domain = ISLFunction.create(
    "isl_pw_multi_aff_intersect_domain",
    Take("PwMultiAff"),
    Take("Set"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_intersect_domain_wrapped_domain = ISLFunction.create(
    "isl_pw_multi_aff_intersect_domain_wrapped_domain",
    Take("PwMultiAff"),
    Take("Set"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_intersect_domain_wrapped_range = ISLFunction.create(
    "isl_pw_multi_aff_intersect_domain_wrapped_range",
    Take("PwMultiAff"),
    Take("Set"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_intersect_params = ISLFunction.create(
    "isl_pw_multi_aff_intersect_params",
    Take("PwMultiAff"),
    Take("Set"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_subtract_domain = ISLFunction.create(
    "isl_pw_multi_aff_subtract_domain",
    Take("PwMultiAff"),
    Take("Set"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_preimage_domain_wrapped_domain_pw_multi_aff = ISLFunction.create(
    "isl_pw_multi_aff_preimage_domain_wrapped_domain_pw_multi_aff",
    Take("PwMultiAff"),
    Take("PwMultiAff"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_pullback_multi_aff = ISLFunction.create(
    "isl_pw_multi_aff_pullback_multi_aff",
    Take("PwMultiAff"),
    Take("MultiAff"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_pullback_pw_multi_aff = ISLFunction.create(
    "isl_pw_multi_aff_pullback_pw_multi_aff",
    Take("PwMultiAff"),
    Take("PwMultiAff"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_range_product = ISLFunction.create(
    "isl_pw_multi_aff_range_product",
    Take("PwMultiAff"),
    Take("PwMultiAff"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_product = ISLFunction.create(
    "isl_pw_multi_aff_product",
    Take("PwMultiAff"),
    Take("PwMultiAff"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_flat_range_product = ISLFunction.create(
    "isl_pw_multi_aff_flat_range_product",
    Take("PwMultiAff"),
    Take("PwMultiAff"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_range_factor_domain = ISLFunction.create(
    "isl_pw_multi_aff_range_factor_domain",
    Take("PwMultiAff"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_range_factor_range = ISLFunction.create(
    "isl_pw_multi_aff_range_factor_range",
    Take("PwMultiAff"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_gist_params = ISLFunction.create(
    "isl_pw_multi_aff_gist_params",
    Take("PwMultiAff"),
    Take("Set"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_gist = ISLFunction.create(
    "isl_pw_multi_aff_gist",
    Take("PwMultiAff"),
    Take("Set"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_add = ISLFunction.create(
    "isl_pw_multi_aff_add",
    Take("PwMultiAff"),
    Take("PwMultiAff"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_add_constant_val = ISLFunction.create(
    "isl_pw_multi_aff_add_constant_val",
    Take("PwMultiAff"),
    Take("Val"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_add_constant_multi_val = ISLFunction.create(
    "isl_pw_multi_aff_add_constant_multi_val",
    Take("PwMultiAff"),
    Take("MultiVal"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_sub = ISLFunction.create(
    "isl_pw_multi_aff_sub",
    Take("PwMultiAff"),
    Take("PwMultiAff"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_union_add = ISLFunction.create(
    "isl_pw_multi_aff_union_add",
    Take("PwMultiAff"),
    Take("PwMultiAff"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_scale_val = ISLFunction.create(
    "isl_pw_multi_aff_scale_val",
    Take("PwMultiAff"),
    Take("Val"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_scale_down_val = ISLFunction.create(
    "isl_pw_multi_aff_scale_down_val",
    Take("PwMultiAff"),
    Take("Val"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_scale_multi_val = ISLFunction.create(
    "isl_pw_multi_aff_scale_multi_val",
    Take("PwMultiAff"),
    Take("MultiVal"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_scale_down_multi_val = ISLFunction.create(
    "isl_pw_multi_aff_scale_down_multi_val",
    Take("PwMultiAff"),
    Take("MultiVal"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_union_lexmin = ISLFunction.create(
    "isl_pw_multi_aff_union_lexmin",
    Take("PwMultiAff"),
    Take("PwMultiAff"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_pw_multi_aff_union_lexmax = ISLFunction.create(
    "isl_pw_multi_aff_union_lexmax",
    Take("PwMultiAff"),
    Take("PwMultiAff"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)
