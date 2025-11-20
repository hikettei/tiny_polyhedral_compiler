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

class MultiAff(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_multi_aff_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_multi_aff_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_multi_aff_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_multi_aff_free(handle)

    def __str__(self) -> str:
        return _isl_multi_aff_to_str(self)

    def __repr__(self) -> str:
        return f"MultiAff({self.__str__()})"

    def get_ctx(self) -> "Ctx":
        return _isl_multi_aff_get_ctx(self)

    def get_domain_space(self) -> "Space":
        return _isl_multi_aff_get_domain_space(self)

    def get_space(self) -> "Space":
        return _isl_multi_aff_get_space(self)

    def dim(self, type: int) -> int:
        return _isl_multi_aff_dim(self, type)

    def set_dim_id(self, type: int, pos: int, id: "Id") -> "MultiAff":
        return _isl_multi_aff_set_dim_id(self, type, pos, id)

    def get_dim_id(self, type: int, pos: int) -> "Id":
        return _isl_multi_aff_get_dim_id(self, type, pos)

    def set_dim_name(self, type: int, pos: int, s: str) -> "MultiAff":
        return _isl_multi_aff_set_dim_name(self, type, pos, s)

    def find_dim_by_id(self, type: int, id: "Id") -> int:
        return _isl_multi_aff_find_dim_by_id(self, type, id)

    def find_dim_by_name(self, type: int, name: str) -> int:
        return _isl_multi_aff_find_dim_by_name(self, type, name)

    def set_range_tuple_id(self, id: "Id") -> "MultiAff":
        return _isl_multi_aff_set_range_tuple_id(self, id)

    def set_tuple_id(self, type: int, id: "Id") -> "MultiAff":
        return _isl_multi_aff_set_tuple_id(self, type, id)

    def reset_range_tuple_id(self) -> "MultiAff":
        return _isl_multi_aff_reset_range_tuple_id(self)

    def reset_tuple_id(self, type: int) -> "MultiAff":
        return _isl_multi_aff_reset_tuple_id(self, type)

    def has_range_tuple_id(self) -> bool:
        return _isl_multi_aff_has_range_tuple_id(self)

    def get_range_tuple_id(self) -> "Id":
        return _isl_multi_aff_get_range_tuple_id(self)

    def has_tuple_id(self, type: int) -> bool:
        return _isl_multi_aff_has_tuple_id(self, type)

    def get_tuple_id(self, type: int) -> "Id":
        return _isl_multi_aff_get_tuple_id(self, type)

    def set_tuple_name(self, type: int, s: str) -> "MultiAff":
        return _isl_multi_aff_set_tuple_name(self, type, s)

    def get_tuple_name(self, type: int) -> str:
        return _isl_multi_aff_get_tuple_name(self, type)

    def reset_user(self) -> "MultiAff":
        return _isl_multi_aff_reset_user(self)

    def as_set(self) -> "Set":
        return _isl_multi_aff_as_set(self)

    def as_map(self) -> "Map":
        return _isl_multi_aff_as_map(self)

    @classmethod
    def zero(cls, space: "Space") -> "MultiAff":
        return _isl_multi_aff_zero(space)

    @classmethod
    def identity(cls, space: "Space") -> "MultiAff":
        return _isl_multi_aff_identity(space)

    def identity_multi_aff(self) -> "MultiAff":
        return _isl_multi_aff_identity_multi_aff(self)

    @classmethod
    def domain_map(cls, space: "Space") -> "MultiAff":
        return _isl_multi_aff_domain_map(space)

    @classmethod
    def range_map(cls, space: "Space") -> "MultiAff":
        return _isl_multi_aff_range_map(space)

    @classmethod
    def project_out_map(cls, space: "Space", type: int, first: int, n: int) -> "MultiAff":
        return _isl_multi_aff_project_out_map(space, type, first, n)

    @classmethod
    def from_aff(cls, aff: "Aff") -> "MultiAff":
        return _isl_multi_aff_from_aff(aff)

    @classmethod
    def from_aff_list(cls, space: "Space", list: "AffList") -> "MultiAff":
        return _isl_multi_aff_from_aff_list(space, list)

    def to_multi_pw_aff(self) -> "MultiPwAff":
        return _isl_multi_aff_to_multi_pw_aff(self)

    def to_multi_union_pw_aff(self) -> "MultiUnionPwAff":
        return _isl_multi_aff_to_multi_union_pw_aff(self)

    @classmethod
    def multi_val_on_domain_space(cls, space: "Space", mv: "MultiVal") -> "MultiAff":
        return _isl_multi_aff_multi_val_on_domain_space(space, mv)

    @classmethod
    def multi_val_on_space(cls, space: "Space", mv: "MultiVal") -> "MultiAff":
        return _isl_multi_aff_multi_val_on_space(space, mv)

    def size(self) -> int:
        return _isl_multi_aff_size(self)

    def get_aff(self, pos: int) -> "Aff":
        return _isl_multi_aff_get_aff(self, pos)

    def set_at(self, pos: int, aff: "Aff") -> "MultiAff":
        return _isl_multi_aff_set_at(self, pos, aff)

    def set_aff(self, pos: int, aff: "Aff") -> "MultiAff":
        return _isl_multi_aff_set_aff(self, pos, aff)

    def get_list(self) -> "AffList":
        return _isl_multi_aff_get_list(self)

    def get_constant_multi_val(self) -> "MultiVal":
        return _isl_multi_aff_get_constant_multi_val(self)

    def to_pw_multi_aff(self) -> "PwMultiAff":
        return _isl_multi_aff_to_pw_multi_aff(self)

    def involves_locals(self) -> bool:
        return _isl_multi_aff_involves_locals(self)

    def involves_dims(self, type: int, first: int, n: int) -> bool:
        return _isl_multi_aff_involves_dims(self, type, first, n)

    def range_is_wrapping(self) -> bool:
        return _isl_multi_aff_range_is_wrapping(self)

    def involves_nan(self) -> bool:
        return _isl_multi_aff_involves_nan(self)

    def is_equal(self, maff2: "MultiAff") -> bool:
        return _isl_multi_aff_plain_is_equal(self, maff2)

    def plain_cmp(self, ma2: "MultiAff") -> int:
        return _isl_multi_aff_plain_cmp(self, ma2)

    def domain_reverse(self) -> "MultiAff":
        return _isl_multi_aff_domain_reverse(self)

    def bind_domain(self, tuple: "MultiId") -> "MultiAff":
        return _isl_multi_aff_bind_domain(self, tuple)

    def bind_domain_wrapped_domain(self, tuple: "MultiId") -> "MultiAff":
        return _isl_multi_aff_bind_domain_wrapped_domain(self, tuple)

    def bind(self, tuple: "MultiId") -> "BasicSet":
        return _isl_multi_aff_bind(self, tuple)

    def project_domain_on_params(self) -> "MultiAff":
        return _isl_multi_aff_project_domain_on_params(self)

    def unbind_params_insert_domain(self, domain: "MultiId") -> "MultiAff":
        return _isl_multi_aff_unbind_params_insert_domain(self, domain)

    def insert_domain(self, domain: "Space") -> "MultiAff":
        return _isl_multi_aff_insert_domain(self, domain)

    def from_range(self) -> "MultiAff":
        return _isl_multi_aff_from_range(self)

    def flatten_domain(self) -> "MultiAff":
        return _isl_multi_aff_flatten_domain(self)

    def flatten_range(self) -> "MultiAff":
        return _isl_multi_aff_flatten_range(self)

    def lift(self, ls: "LocalSpace") -> "MultiAff":
        return _isl_multi_aff_lift(self, ls)

    def align_params(self, model: "Space") -> "MultiAff":
        return _isl_multi_aff_align_params(self, model)

    def neg(self) -> "MultiAff":
        return _isl_multi_aff_neg(self)

    def floor(self) -> "MultiAff":
        return _isl_multi_aff_floor(self)

    def insert_dims(self, type: int, first: int, n: int) -> "MultiAff":
        return _isl_multi_aff_insert_dims(self, type, first, n)

    def add_dims(self, type: int, n: int) -> "MultiAff":
        return _isl_multi_aff_add_dims(self, type, n)

    def drop_dims(self, type: int, first: int, n: int) -> "MultiAff":
        return _isl_multi_aff_drop_dims(self, type, first, n)

    def move_dims(self, dst_type: int, dst_pos: int, src_type: int, src_pos: int, n: int) -> "MultiAff":
        return _isl_multi_aff_move_dims(self, dst_type, dst_pos, src_type, src_pos, n)

    def pullback_multi_aff(self, ma2: "MultiAff") -> "MultiAff":
        return _isl_multi_aff_pullback_multi_aff(self, ma2)

    def lex_le_set(self, ma2: "MultiAff") -> "Set":
        return _isl_multi_aff_lex_le_set(self, ma2)

    def lex_lt_set(self, ma2: "MultiAff") -> "Set":
        return _isl_multi_aff_lex_lt_set(self, ma2)

    def lex_ge_set(self, ma2: "MultiAff") -> "Set":
        return _isl_multi_aff_lex_ge_set(self, ma2)

    def lex_gt_set(self, ma2: "MultiAff") -> "Set":
        return _isl_multi_aff_lex_gt_set(self, ma2)

    def range_product(self, ma2: "MultiAff") -> "MultiAff":
        return _isl_multi_aff_range_product(self, ma2)

    def product(self, ma2: "MultiAff") -> "MultiAff":
        return _isl_multi_aff_product(self, ma2)

    def flat_range_product(self, ma2: "MultiAff") -> "MultiAff":
        return _isl_multi_aff_flat_range_product(self, ma2)

    def factor_range(self) -> "MultiAff":
        return _isl_multi_aff_factor_range(self)

    def range_factor_domain(self) -> "MultiAff":
        return _isl_multi_aff_range_factor_domain(self)

    def range_factor_range(self) -> "MultiAff":
        return _isl_multi_aff_range_factor_range(self)

    def range_splice(self, pos: int, ma2: "MultiAff") -> "MultiAff":
        return _isl_multi_aff_range_splice(self, pos, ma2)

    def splice(self, in_pos: int, out_pos: int, ma2: "MultiAff") -> "MultiAff":
        return _isl_multi_aff_splice(self, in_pos, out_pos, ma2)

    def gist_params(self, context: "Set") -> "MultiAff":
        return _isl_multi_aff_gist_params(self, context)

    def gist(self, context: "Set") -> "MultiAff":
        return _isl_multi_aff_gist(self, context)

    def add(self, maff2: "MultiAff") -> "MultiAff":
        return _isl_multi_aff_add(self, maff2)

    def add_constant_val(self, v: "Val") -> "MultiAff":
        return _isl_multi_aff_add_constant_val(self, v)

    def add_constant_multi_val(self, mv: "MultiVal") -> "MultiAff":
        return _isl_multi_aff_add_constant_multi_val(self, mv)

    def sub(self, ma2: "MultiAff") -> "MultiAff":
        return _isl_multi_aff_sub(self, ma2)

    def mod_multi_val(self, mv: "MultiVal") -> "MultiAff":
        return _isl_multi_aff_mod_multi_val(self, mv)

    def scale_multi_val(self, mv: "MultiVal") -> "MultiAff":
        return _isl_multi_aff_scale_multi_val(self, mv)

    def scale_down_multi_val(self, mv: "MultiVal") -> "MultiAff":
        return _isl_multi_aff_scale_down_multi_val(self, mv)


register_type("MultiAff", MultiAff)

_isl_multi_aff_get_ctx = ISLFunction.create(
    "isl_multi_aff_get_ctx",
    Keep("MultiAff"),
    return_=Give("Ctx"),
    lib=_lib,
)

_isl_multi_aff_get_domain_space = ISLFunction.create(
    "isl_multi_aff_get_domain_space",
    Keep("MultiAff"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_multi_aff_get_space = ISLFunction.create(
    "isl_multi_aff_get_space",
    Keep("MultiAff"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_multi_aff_dim = ISLFunction.create(
    "isl_multi_aff_dim",
    Keep("MultiAff"),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_multi_aff_set_dim_id = ISLFunction.create(
    "isl_multi_aff_set_dim_id",
    Take("MultiAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Take("Id"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_get_dim_id = ISLFunction.create(
    "isl_multi_aff_get_dim_id",
    Keep("MultiAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Give("Id"),
    lib=_lib,
)

_isl_multi_aff_set_dim_name = ISLFunction.create(
    "isl_multi_aff_set_dim_name",
    Take("MultiAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(str, ctype=c_char_p),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_find_dim_by_id = ISLFunction.create(
    "isl_multi_aff_find_dim_by_id",
    Keep("MultiAff"),
    Param(int, ctype=c_int),
    Keep("Id"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_multi_aff_find_dim_by_name = ISLFunction.create(
    "isl_multi_aff_find_dim_by_name",
    Keep("MultiAff"),
    Param(int, ctype=c_int),
    Param(str, ctype=c_char_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_multi_aff_set_range_tuple_id = ISLFunction.create(
    "isl_multi_aff_set_range_tuple_id",
    Take("MultiAff"),
    Take("Id"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_set_tuple_id = ISLFunction.create(
    "isl_multi_aff_set_tuple_id",
    Take("MultiAff"),
    Param(int, ctype=c_int),
    Take("Id"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_reset_range_tuple_id = ISLFunction.create(
    "isl_multi_aff_reset_range_tuple_id",
    Take("MultiAff"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_reset_tuple_id = ISLFunction.create(
    "isl_multi_aff_reset_tuple_id",
    Take("MultiAff"),
    Param(int, ctype=c_int),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_has_range_tuple_id = ISLFunction.create(
    "isl_multi_aff_has_range_tuple_id",
    Keep("MultiAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_aff_get_range_tuple_id = ISLFunction.create(
    "isl_multi_aff_get_range_tuple_id",
    Keep("MultiAff"),
    return_=Give("Id"),
    lib=_lib,
)

_isl_multi_aff_has_tuple_id = ISLFunction.create(
    "isl_multi_aff_has_tuple_id",
    Keep("MultiAff"),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_aff_get_tuple_id = ISLFunction.create(
    "isl_multi_aff_get_tuple_id",
    Keep("MultiAff"),
    Param(int, ctype=c_int),
    return_=Give("Id"),
    lib=_lib,
)

_isl_multi_aff_set_tuple_name = ISLFunction.create(
    "isl_multi_aff_set_tuple_name",
    Take("MultiAff"),
    Param(int, ctype=c_int),
    Param(str, ctype=c_char_p),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_get_tuple_name = ISLFunction.create(
    "isl_multi_aff_get_tuple_name",
    Keep("MultiAff"),
    Param(int, ctype=c_int),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_multi_aff_reset_user = ISLFunction.create(
    "isl_multi_aff_reset_user",
    Take("MultiAff"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_as_set = ISLFunction.create(
    "isl_multi_aff_as_set",
    Take("MultiAff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_multi_aff_as_map = ISLFunction.create(
    "isl_multi_aff_as_map",
    Take("MultiAff"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_multi_aff_zero = ISLFunction.create(
    "isl_multi_aff_zero",
    Take("Space"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_identity = ISLFunction.create(
    "isl_multi_aff_identity",
    Take("Space"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_identity_multi_aff = ISLFunction.create(
    "isl_multi_aff_identity_multi_aff",
    Take("MultiAff"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_domain_map = ISLFunction.create(
    "isl_multi_aff_domain_map",
    Take("Space"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_range_map = ISLFunction.create(
    "isl_multi_aff_range_map",
    Take("Space"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_project_out_map = ISLFunction.create(
    "isl_multi_aff_project_out_map",
    Take("Space"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_from_aff = ISLFunction.create(
    "isl_multi_aff_from_aff",
    Take("Aff"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_from_aff_list = ISLFunction.create(
    "isl_multi_aff_from_aff_list",
    Take("Space"),
    Take("AffList"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_to_multi_pw_aff = ISLFunction.create(
    "isl_multi_aff_to_multi_pw_aff",
    Take("MultiAff"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_multi_aff_to_multi_union_pw_aff = ISLFunction.create(
    "isl_multi_aff_to_multi_union_pw_aff",
    Take("MultiAff"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_multi_aff_multi_val_on_domain_space = ISLFunction.create(
    "isl_multi_aff_multi_val_on_domain_space",
    Take("Space"),
    Take("MultiVal"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_multi_val_on_space = ISLFunction.create(
    "isl_multi_aff_multi_val_on_space",
    Take("Space"),
    Take("MultiVal"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_copy = ISLFunction.create(
    "isl_multi_aff_copy",
    Keep("MultiAff"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_free = ISLFunction.create(
    "isl_multi_aff_free",
    Take("MultiAff"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_size = ISLFunction.create(
    "isl_multi_aff_size",
    Keep("MultiAff"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_multi_aff_get_aff = ISLFunction.create(
    "isl_multi_aff_get_aff",
    Keep("MultiAff"),
    Param(int, ctype=c_int),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_multi_aff_set_at = ISLFunction.create(
    "isl_multi_aff_set_at",
    Take("MultiAff"),
    Param(int, ctype=c_int),
    Take("Aff"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_set_aff = ISLFunction.create(
    "isl_multi_aff_set_aff",
    Take("MultiAff"),
    Param(int, ctype=c_int),
    Take("Aff"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_get_list = ISLFunction.create(
    "isl_multi_aff_get_list",
    Keep("MultiAff"),
    return_=Give("AffList"),
    lib=_lib,
)

_isl_multi_aff_get_constant_multi_val = ISLFunction.create(
    "isl_multi_aff_get_constant_multi_val",
    Keep("MultiAff"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_aff_to_pw_multi_aff = ISLFunction.create(
    "isl_multi_aff_to_pw_multi_aff",
    Take("MultiAff"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_multi_aff_read_from_str = ISLFunction.create(
    "isl_multi_aff_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_to_str = ISLFunction.create(
    "isl_multi_aff_to_str",
    Keep("MultiAff"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_multi_aff_involves_locals = ISLFunction.create(
    "isl_multi_aff_involves_locals",
    Keep("MultiAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_aff_involves_dims = ISLFunction.create(
    "isl_multi_aff_involves_dims",
    Keep("MultiAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_aff_range_is_wrapping = ISLFunction.create(
    "isl_multi_aff_range_is_wrapping",
    Keep("MultiAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_aff_involves_nan = ISLFunction.create(
    "isl_multi_aff_involves_nan",
    Keep("MultiAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_aff_plain_is_equal = ISLFunction.create(
    "isl_multi_aff_plain_is_equal",
    Keep("MultiAff"),
    Keep("MultiAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_aff_plain_cmp = ISLFunction.create(
    "isl_multi_aff_plain_cmp",
    Keep("MultiAff"),
    Keep("MultiAff"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_multi_aff_domain_reverse = ISLFunction.create(
    "isl_multi_aff_domain_reverse",
    Take("MultiAff"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_bind_domain = ISLFunction.create(
    "isl_multi_aff_bind_domain",
    Take("MultiAff"),
    Take("MultiId"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_bind_domain_wrapped_domain = ISLFunction.create(
    "isl_multi_aff_bind_domain_wrapped_domain",
    Take("MultiAff"),
    Take("MultiId"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_bind = ISLFunction.create(
    "isl_multi_aff_bind",
    Take("MultiAff"),
    Take("MultiId"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_multi_aff_project_domain_on_params = ISLFunction.create(
    "isl_multi_aff_project_domain_on_params",
    Take("MultiAff"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_unbind_params_insert_domain = ISLFunction.create(
    "isl_multi_aff_unbind_params_insert_domain",
    Take("MultiAff"),
    Take("MultiId"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_insert_domain = ISLFunction.create(
    "isl_multi_aff_insert_domain",
    Take("MultiAff"),
    Take("Space"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_from_range = ISLFunction.create(
    "isl_multi_aff_from_range",
    Take("MultiAff"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_flatten_domain = ISLFunction.create(
    "isl_multi_aff_flatten_domain",
    Take("MultiAff"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_flatten_range = ISLFunction.create(
    "isl_multi_aff_flatten_range",
    Take("MultiAff"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_lift = ISLFunction.create(
    "isl_multi_aff_lift",
    Take("MultiAff"),
    Give("LocalSpace"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_align_params = ISLFunction.create(
    "isl_multi_aff_align_params",
    Take("MultiAff"),
    Take("Space"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_neg = ISLFunction.create(
    "isl_multi_aff_neg",
    Take("MultiAff"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_floor = ISLFunction.create(
    "isl_multi_aff_floor",
    Take("MultiAff"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_insert_dims = ISLFunction.create(
    "isl_multi_aff_insert_dims",
    Take("MultiAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_add_dims = ISLFunction.create(
    "isl_multi_aff_add_dims",
    Take("MultiAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_drop_dims = ISLFunction.create(
    "isl_multi_aff_drop_dims",
    Take("MultiAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_move_dims = ISLFunction.create(
    "isl_multi_aff_move_dims",
    Take("MultiAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_pullback_multi_aff = ISLFunction.create(
    "isl_multi_aff_pullback_multi_aff",
    Take("MultiAff"),
    Take("MultiAff"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_lex_le_set = ISLFunction.create(
    "isl_multi_aff_lex_le_set",
    Take("MultiAff"),
    Take("MultiAff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_multi_aff_lex_lt_set = ISLFunction.create(
    "isl_multi_aff_lex_lt_set",
    Take("MultiAff"),
    Take("MultiAff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_multi_aff_lex_ge_set = ISLFunction.create(
    "isl_multi_aff_lex_ge_set",
    Take("MultiAff"),
    Take("MultiAff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_multi_aff_lex_gt_set = ISLFunction.create(
    "isl_multi_aff_lex_gt_set",
    Take("MultiAff"),
    Take("MultiAff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_multi_aff_range_product = ISLFunction.create(
    "isl_multi_aff_range_product",
    Take("MultiAff"),
    Take("MultiAff"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_product = ISLFunction.create(
    "isl_multi_aff_product",
    Take("MultiAff"),
    Take("MultiAff"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_flat_range_product = ISLFunction.create(
    "isl_multi_aff_flat_range_product",
    Take("MultiAff"),
    Take("MultiAff"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_factor_range = ISLFunction.create(
    "isl_multi_aff_factor_range",
    Take("MultiAff"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_range_factor_domain = ISLFunction.create(
    "isl_multi_aff_range_factor_domain",
    Take("MultiAff"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_range_factor_range = ISLFunction.create(
    "isl_multi_aff_range_factor_range",
    Take("MultiAff"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_range_splice = ISLFunction.create(
    "isl_multi_aff_range_splice",
    Take("MultiAff"),
    Param(int, ctype=c_uint),
    Take("MultiAff"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_splice = ISLFunction.create(
    "isl_multi_aff_splice",
    Take("MultiAff"),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    Take("MultiAff"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_gist_params = ISLFunction.create(
    "isl_multi_aff_gist_params",
    Take("MultiAff"),
    Take("Set"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_gist = ISLFunction.create(
    "isl_multi_aff_gist",
    Take("MultiAff"),
    Take("Set"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_add = ISLFunction.create(
    "isl_multi_aff_add",
    Take("MultiAff"),
    Take("MultiAff"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_add_constant_val = ISLFunction.create(
    "isl_multi_aff_add_constant_val",
    Take("MultiAff"),
    Take("Val"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_add_constant_multi_val = ISLFunction.create(
    "isl_multi_aff_add_constant_multi_val",
    Take("MultiAff"),
    Take("MultiVal"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_sub = ISLFunction.create(
    "isl_multi_aff_sub",
    Take("MultiAff"),
    Take("MultiAff"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_mod_multi_val = ISLFunction.create(
    "isl_multi_aff_mod_multi_val",
    Take("MultiAff"),
    Take("MultiVal"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_scale_multi_val = ISLFunction.create(
    "isl_multi_aff_scale_multi_val",
    Take("MultiAff"),
    Take("MultiVal"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_multi_aff_scale_down_multi_val = ISLFunction.create(
    "isl_multi_aff_scale_down_multi_val",
    Take("MultiAff"),
    Take("MultiVal"),
    return_=Give("MultiAff"),
    lib=_lib,
)
