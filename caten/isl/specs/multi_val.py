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

class MultiVal(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_multi_val_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_multi_val_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_multi_val_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_multi_val_free(handle)

    def __str__(self) -> str:
        return _isl_multi_val_to_str(self)

    def __repr__(self) -> str:
        return f"MultiVal({self.__str__()})"

    def get_ctx(self) -> "Ctx":
        return _isl_multi_val_get_ctx(self)

    def get_space(self) -> "Space":
        return _isl_multi_val_get_space(self)

    def dim(self, type: int) -> int:
        return _isl_multi_val_dim(self, type)

    def set_dim_id(self, type: int, pos: int, id: "Id") -> "MultiVal":
        return _isl_multi_val_set_dim_id(self, type, pos, id)

    def get_dim_id(self, type: int, pos: int) -> "Id":
        return _isl_multi_val_get_dim_id(self, type, pos)

    def set_dim_name(self, type: int, pos: int, s: str) -> "MultiVal":
        return _isl_multi_val_set_dim_name(self, type, pos, s)

    def find_dim_by_id(self, type: int, id: "Id") -> int:
        return _isl_multi_val_find_dim_by_id(self, type, id)

    def find_dim_by_name(self, type: int, name: str) -> int:
        return _isl_multi_val_find_dim_by_name(self, type, name)

    def set_range_tuple_id(self, id: "Id") -> "MultiVal":
        return _isl_multi_val_set_range_tuple_id(self, id)

    def set_tuple_id(self, type: int, id: "Id") -> "MultiVal":
        return _isl_multi_val_set_tuple_id(self, type, id)

    def reset_range_tuple_id(self) -> "MultiVal":
        return _isl_multi_val_reset_range_tuple_id(self)

    def reset_tuple_id(self, type: int) -> "MultiVal":
        return _isl_multi_val_reset_tuple_id(self, type)

    def has_range_tuple_id(self) -> bool:
        return _isl_multi_val_has_range_tuple_id(self)

    def get_range_tuple_id(self) -> "Id":
        return _isl_multi_val_get_range_tuple_id(self)

    def has_tuple_id(self, type: int) -> bool:
        return _isl_multi_val_has_tuple_id(self, type)

    def get_tuple_id(self, type: int) -> "Id":
        return _isl_multi_val_get_tuple_id(self, type)

    def set_tuple_name(self, type: int, s: str) -> "MultiVal":
        return _isl_multi_val_set_tuple_name(self, type, s)

    def get_tuple_name(self, type: int) -> str:
        return _isl_multi_val_get_tuple_name(self, type)

    def reset_user(self) -> "MultiVal":
        return _isl_multi_val_reset_user(self)

    @classmethod
    def zero(cls, space: "Space") -> "MultiVal":
        return _isl_multi_val_zero(space)

    @classmethod
    def from_val_list(cls, space: "Space", list: "ValList") -> "MultiVal":
        return _isl_multi_val_from_val_list(space, list)

    def size(self) -> int:
        return _isl_multi_val_size(self)

    def get_at(self, pos: int) -> "Val":
        return _isl_multi_val_get_at(self, pos)

    def get_val(self, pos: int) -> "Val":
        return _isl_multi_val_get_val(self, pos)

    def set_at(self, pos: int, val: "Val") -> "MultiVal":
        return _isl_multi_val_set_at(self, pos, val)

    def set_val(self, pos: int, val: "Val") -> "MultiVal":
        return _isl_multi_val_set_val(self, pos, val)

    def get_list(self) -> "ValList":
        return _isl_multi_val_get_list(self)

    def range_is_wrapping(self) -> bool:
        return _isl_multi_val_range_is_wrapping(self)

    def involves_nan(self) -> bool:
        return _isl_multi_val_involves_nan(self)

    def is_zero(self) -> bool:
        return _isl_multi_val_is_zero(self)

    def is_equal(self, mv2: "MultiVal") -> bool:
        return _isl_multi_val_plain_is_equal(self, mv2)

    def from_range(self) -> "MultiVal":
        return _isl_multi_val_from_range(self)

    def flatten_range(self) -> "MultiVal":
        return _isl_multi_val_flatten_range(self)

    def align_params(self, model: "Space") -> "MultiVal":
        return _isl_multi_val_align_params(self, model)

    def neg(self) -> "MultiVal":
        return _isl_multi_val_neg(self)

    def insert_dims(self, type: int, first: int, n: int) -> "MultiVal":
        return _isl_multi_val_insert_dims(self, type, first, n)

    def add_dims(self, type: int, n: int) -> "MultiVal":
        return _isl_multi_val_add_dims(self, type, n)

    def drop_dims(self, type: int, first: int, n: int) -> "MultiVal":
        return _isl_multi_val_drop_dims(self, type, first, n)

    def range_product(self, mv2: "MultiVal") -> "MultiVal":
        return _isl_multi_val_range_product(self, mv2)

    def product(self, mv2: "MultiVal") -> "MultiVal":
        return _isl_multi_val_product(self, mv2)

    def flat_range_product(self, mv2: "MultiVal") -> "MultiVal":
        return _isl_multi_val_flat_range_product(self, mv2)

    def factor_range(self) -> "MultiVal":
        return _isl_multi_val_factor_range(self)

    def range_factor_domain(self) -> "MultiVal":
        return _isl_multi_val_range_factor_domain(self)

    def range_factor_range(self) -> "MultiVal":
        return _isl_multi_val_range_factor_range(self)

    def range_splice(self, pos: int, mv2: "MultiVal") -> "MultiVal":
        return _isl_multi_val_range_splice(self, pos, mv2)

    def add(self, mv2: "MultiVal") -> "MultiVal":
        return _isl_multi_val_add(self, mv2)

    def sub(self, mv2: "MultiVal") -> "MultiVal":
        return _isl_multi_val_sub(self, mv2)

    def min(self, mv2: "MultiVal") -> "MultiVal":
        return _isl_multi_val_min(self, mv2)

    def max(self, mv2: "MultiVal") -> "MultiVal":
        return _isl_multi_val_max(self, mv2)

    def add_val(self, v: "Val") -> "MultiVal":
        return _isl_multi_val_add_val(self, v)

    def mod_val(self, v: "Val") -> "MultiVal":
        return _isl_multi_val_mod_val(self, v)

    def scale_val(self, v: "Val") -> "MultiVal":
        return _isl_multi_val_scale_val(self, v)

    def scale_down_val(self, v: "Val") -> "MultiVal":
        return _isl_multi_val_scale_down_val(self, v)

    def mod_multi_val(self, mv2: "MultiVal") -> "MultiVal":
        return _isl_multi_val_mod_multi_val(self, mv2)

    def scale_multi_val(self, mv2: "MultiVal") -> "MultiVal":
        return _isl_multi_val_scale_multi_val(self, mv2)

    def scale_down_multi_val(self, mv2: "MultiVal") -> "MultiVal":
        return _isl_multi_val_scale_down_multi_val(self, mv2)


register_type("MultiVal", MultiVal)

_isl_multi_val_get_ctx = ISLFunction.create(
    "isl_multi_val_get_ctx",
    Keep("MultiVal"),
    return_=Give("Ctx"),
    lib=_lib,
)

_isl_multi_val_get_space = ISLFunction.create(
    "isl_multi_val_get_space",
    Keep("MultiVal"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_multi_val_dim = ISLFunction.create(
    "isl_multi_val_dim",
    Keep("MultiVal"),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_multi_val_set_dim_id = ISLFunction.create(
    "isl_multi_val_set_dim_id",
    Take("MultiVal"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Take("Id"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_get_dim_id = ISLFunction.create(
    "isl_multi_val_get_dim_id",
    Keep("MultiVal"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Give("Id"),
    lib=_lib,
)

_isl_multi_val_set_dim_name = ISLFunction.create(
    "isl_multi_val_set_dim_name",
    Take("MultiVal"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(str, ctype=c_char_p),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_find_dim_by_id = ISLFunction.create(
    "isl_multi_val_find_dim_by_id",
    Keep("MultiVal"),
    Param(int, ctype=c_int),
    Keep("Id"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_multi_val_find_dim_by_name = ISLFunction.create(
    "isl_multi_val_find_dim_by_name",
    Keep("MultiVal"),
    Param(int, ctype=c_int),
    Param(str, ctype=c_char_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_multi_val_set_range_tuple_id = ISLFunction.create(
    "isl_multi_val_set_range_tuple_id",
    Take("MultiVal"),
    Take("Id"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_set_tuple_id = ISLFunction.create(
    "isl_multi_val_set_tuple_id",
    Take("MultiVal"),
    Param(int, ctype=c_int),
    Take("Id"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_reset_range_tuple_id = ISLFunction.create(
    "isl_multi_val_reset_range_tuple_id",
    Take("MultiVal"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_reset_tuple_id = ISLFunction.create(
    "isl_multi_val_reset_tuple_id",
    Take("MultiVal"),
    Param(int, ctype=c_int),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_has_range_tuple_id = ISLFunction.create(
    "isl_multi_val_has_range_tuple_id",
    Keep("MultiVal"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_val_get_range_tuple_id = ISLFunction.create(
    "isl_multi_val_get_range_tuple_id",
    Keep("MultiVal"),
    return_=Give("Id"),
    lib=_lib,
)

_isl_multi_val_has_tuple_id = ISLFunction.create(
    "isl_multi_val_has_tuple_id",
    Keep("MultiVal"),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_val_get_tuple_id = ISLFunction.create(
    "isl_multi_val_get_tuple_id",
    Keep("MultiVal"),
    Param(int, ctype=c_int),
    return_=Give("Id"),
    lib=_lib,
)

_isl_multi_val_set_tuple_name = ISLFunction.create(
    "isl_multi_val_set_tuple_name",
    Take("MultiVal"),
    Param(int, ctype=c_int),
    Param(str, ctype=c_char_p),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_get_tuple_name = ISLFunction.create(
    "isl_multi_val_get_tuple_name",
    Keep("MultiVal"),
    Param(int, ctype=c_int),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_multi_val_reset_user = ISLFunction.create(
    "isl_multi_val_reset_user",
    Take("MultiVal"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_zero = ISLFunction.create(
    "isl_multi_val_zero",
    Take("Space"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_from_val_list = ISLFunction.create(
    "isl_multi_val_from_val_list",
    Take("Space"),
    Take("ValList"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_copy = ISLFunction.create(
    "isl_multi_val_copy",
    Keep("MultiVal"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_free = ISLFunction.create(
    "isl_multi_val_free",
    Take("MultiVal"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_size = ISLFunction.create(
    "isl_multi_val_size",
    Keep("MultiVal"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_multi_val_get_at = ISLFunction.create(
    "isl_multi_val_get_at",
    Keep("MultiVal"),
    Param(int, ctype=c_int),
    return_=Give("Val"),
    lib=_lib,
)

_isl_multi_val_get_val = ISLFunction.create(
    "isl_multi_val_get_val",
    Keep("MultiVal"),
    Param(int, ctype=c_int),
    return_=Give("Val"),
    lib=_lib,
)

_isl_multi_val_set_at = ISLFunction.create(
    "isl_multi_val_set_at",
    Take("MultiVal"),
    Param(int, ctype=c_int),
    Take("Val"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_set_val = ISLFunction.create(
    "isl_multi_val_set_val",
    Take("MultiVal"),
    Param(int, ctype=c_int),
    Take("Val"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_get_list = ISLFunction.create(
    "isl_multi_val_get_list",
    Keep("MultiVal"),
    return_=Give("ValList"),
    lib=_lib,
)

_isl_multi_val_read_from_str = ISLFunction.create(
    "isl_multi_val_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_to_str = ISLFunction.create(
    "isl_multi_val_to_str",
    Keep("MultiVal"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_multi_val_range_is_wrapping = ISLFunction.create(
    "isl_multi_val_range_is_wrapping",
    Keep("MultiVal"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_val_involves_nan = ISLFunction.create(
    "isl_multi_val_involves_nan",
    Keep("MultiVal"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_val_is_zero = ISLFunction.create(
    "isl_multi_val_is_zero",
    Keep("MultiVal"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_val_plain_is_equal = ISLFunction.create(
    "isl_multi_val_plain_is_equal",
    Keep("MultiVal"),
    Keep("MultiVal"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_val_from_range = ISLFunction.create(
    "isl_multi_val_from_range",
    Take("MultiVal"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_flatten_range = ISLFunction.create(
    "isl_multi_val_flatten_range",
    Take("MultiVal"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_align_params = ISLFunction.create(
    "isl_multi_val_align_params",
    Take("MultiVal"),
    Take("Space"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_neg = ISLFunction.create(
    "isl_multi_val_neg",
    Take("MultiVal"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_insert_dims = ISLFunction.create(
    "isl_multi_val_insert_dims",
    Take("MultiVal"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_add_dims = ISLFunction.create(
    "isl_multi_val_add_dims",
    Take("MultiVal"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_drop_dims = ISLFunction.create(
    "isl_multi_val_drop_dims",
    Take("MultiVal"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_range_product = ISLFunction.create(
    "isl_multi_val_range_product",
    Take("MultiVal"),
    Take("MultiVal"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_product = ISLFunction.create(
    "isl_multi_val_product",
    Take("MultiVal"),
    Take("MultiVal"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_flat_range_product = ISLFunction.create(
    "isl_multi_val_flat_range_product",
    Take("MultiVal"),
    Take("MultiVal"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_factor_range = ISLFunction.create(
    "isl_multi_val_factor_range",
    Take("MultiVal"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_range_factor_domain = ISLFunction.create(
    "isl_multi_val_range_factor_domain",
    Take("MultiVal"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_range_factor_range = ISLFunction.create(
    "isl_multi_val_range_factor_range",
    Take("MultiVal"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_range_splice = ISLFunction.create(
    "isl_multi_val_range_splice",
    Take("MultiVal"),
    Param(int, ctype=c_uint),
    Take("MultiVal"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_add = ISLFunction.create(
    "isl_multi_val_add",
    Take("MultiVal"),
    Take("MultiVal"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_sub = ISLFunction.create(
    "isl_multi_val_sub",
    Take("MultiVal"),
    Take("MultiVal"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_min = ISLFunction.create(
    "isl_multi_val_min",
    Take("MultiVal"),
    Take("MultiVal"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_max = ISLFunction.create(
    "isl_multi_val_max",
    Take("MultiVal"),
    Take("MultiVal"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_add_val = ISLFunction.create(
    "isl_multi_val_add_val",
    Take("MultiVal"),
    Take("Val"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_mod_val = ISLFunction.create(
    "isl_multi_val_mod_val",
    Take("MultiVal"),
    Take("Val"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_scale_val = ISLFunction.create(
    "isl_multi_val_scale_val",
    Take("MultiVal"),
    Take("Val"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_scale_down_val = ISLFunction.create(
    "isl_multi_val_scale_down_val",
    Take("MultiVal"),
    Take("Val"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_mod_multi_val = ISLFunction.create(
    "isl_multi_val_mod_multi_val",
    Take("MultiVal"),
    Take("MultiVal"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_scale_multi_val = ISLFunction.create(
    "isl_multi_val_scale_multi_val",
    Take("MultiVal"),
    Take("MultiVal"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_multi_val_scale_down_multi_val = ISLFunction.create(
    "isl_multi_val_scale_down_multi_val",
    Take("MultiVal"),
    Take("MultiVal"),
    return_=Give("MultiVal"),
    lib=_lib,
)
