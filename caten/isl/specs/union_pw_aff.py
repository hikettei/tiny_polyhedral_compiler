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

class UnionPwAff(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_union_pw_aff_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_union_pw_aff_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_union_pw_aff_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_union_pw_aff_free(handle)

    def __str__(self) -> str:
        return _isl_union_pw_aff_to_str(self)

    def __repr__(self) -> str:
        return f"UnionPwAff({self.__str__()})"

    def get_ctx(self) -> "Ctx":
        return _isl_union_pw_aff_get_ctx(self)

    def get_space(self) -> "Space":
        return _isl_union_pw_aff_get_space(self)

    def dim(self, type: int) -> int:
        return _isl_union_pw_aff_dim(self, type)

    def set_dim_name(self, type: int, pos: int, s: str) -> "UnionPwAff":
        return _isl_union_pw_aff_set_dim_name(self, type, pos, s)

    def find_dim_by_name(self, type: int, name: str) -> int:
        return _isl_union_pw_aff_find_dim_by_name(self, type, name)

    def reset_user(self) -> "UnionPwAff":
        return _isl_union_pw_aff_reset_user(self)

    @classmethod
    def empty_ctx(cls) -> "UnionPwAff":
        return _isl_union_pw_aff_empty_ctx()

    @classmethod
    def empty_space(cls, space: "Space") -> "UnionPwAff":
        return _isl_union_pw_aff_empty_space(space)

    @classmethod
    def empty(cls, space: "Space") -> "UnionPwAff":
        return _isl_union_pw_aff_empty(space)

    @classmethod
    def from_pw_aff(cls, pa: "PwAff") -> "UnionPwAff":
        return _isl_union_pw_aff_from_pw_aff(pa)

    @classmethod
    def from_aff(cls, aff: "Aff") -> "UnionPwAff":
        return _isl_union_pw_aff_from_aff(aff)

    @classmethod
    def val_on_domain(cls, domain: "UnionSet", v: "Val") -> "UnionPwAff":
        return _isl_union_pw_aff_val_on_domain(domain, v)

    @classmethod
    def param_on_domain_id(cls, domain: "UnionSet", id: "Id") -> "UnionPwAff":
        return _isl_union_pw_aff_param_on_domain_id(domain, id)

    @classmethod
    def aff_on_domain(cls, domain: "UnionSet", aff: "Aff") -> "UnionPwAff":
        return _isl_union_pw_aff_aff_on_domain(domain, aff)

    @classmethod
    def pw_aff_on_domain(cls, domain: "UnionSet", pa: "PwAff") -> "UnionPwAff":
        return _isl_union_pw_aff_pw_aff_on_domain(domain, pa)

    def add_pw_aff(self, pa: "PwAff") -> "UnionPwAff":
        return _isl_union_pw_aff_add_pw_aff(self, pa)

    def n_pw_aff(self) -> int:
        return _isl_union_pw_aff_n_pw_aff(self)

    def foreach_pw_aff(self, fn: Any, user: Any = None) -> int:
        return _isl_union_pw_aff_foreach_pw_aff(self, fn, user)

    def every_pw_aff(self, test: Any, user: Any = None) -> bool:
        return _isl_union_pw_aff_every_pw_aff(self, test, user)

    def extract_pw_aff(self, space: "Space") -> "PwAff":
        return _isl_union_pw_aff_extract_pw_aff(self, space)

    def get_pw_aff_list(self) -> "PwAffList":
        return _isl_union_pw_aff_get_pw_aff_list(self)

    def involves_nan(self) -> bool:
        return _isl_union_pw_aff_involves_nan(self)

    def is_equal(self, upa2: "UnionPwAff") -> bool:
        return _isl_union_pw_aff_plain_is_equal(self, upa2)

    def bind_id(self, id: "Id") -> "UnionSet":
        return _isl_union_pw_aff_bind_id(self, id)

    def domain(self) -> "UnionSet":
        return _isl_union_pw_aff_domain(self)

    def zero_union_set(self) -> "UnionSet":
        return _isl_union_pw_aff_zero_union_set(self)

    def coalesce(self) -> "UnionPwAff":
        return _isl_union_pw_aff_coalesce(self)

    def min_val(self) -> "Val":
        return _isl_union_pw_aff_min_val(self)

    def max_val(self) -> "Val":
        return _isl_union_pw_aff_max_val(self)

    def align_params(self, model: "Space") -> "UnionPwAff":
        return _isl_union_pw_aff_align_params(self, model)

    def drop_unused_params(self) -> "UnionPwAff":
        return _isl_union_pw_aff_drop_unused_params(self)

    def neg(self) -> "UnionPwAff":
        return _isl_union_pw_aff_neg(self)

    def floor(self) -> "UnionPwAff":
        return _isl_union_pw_aff_floor(self)

    def drop_dims(self, type: int, first: int, n: int) -> "UnionPwAff":
        return _isl_union_pw_aff_drop_dims(self, type, first, n)

    def intersect_domain_space(self, space: "Space") -> "UnionPwAff":
        return _isl_union_pw_aff_intersect_domain_space(self, space)

    def intersect_domain_union_set(self, uset: "UnionSet") -> "UnionPwAff":
        return _isl_union_pw_aff_intersect_domain_union_set(self, uset)

    def intersect_domain(self, uset: "UnionSet") -> "UnionPwAff":
        return _isl_union_pw_aff_intersect_domain(self, uset)

    def intersect_domain_wrapped_domain(self, uset: "UnionSet") -> "UnionPwAff":
        return _isl_union_pw_aff_intersect_domain_wrapped_domain(self, uset)

    def intersect_domain_wrapped_range(self, uset: "UnionSet") -> "UnionPwAff":
        return _isl_union_pw_aff_intersect_domain_wrapped_range(self, uset)

    def intersect_params(self, set: "Set") -> "UnionPwAff":
        return _isl_union_pw_aff_intersect_params(self, set)

    def subtract_domain_union_set(self, uset: "UnionSet") -> "UnionPwAff":
        return _isl_union_pw_aff_subtract_domain_union_set(self, uset)

    def subtract_domain_space(self, space: "Space") -> "UnionPwAff":
        return _isl_union_pw_aff_subtract_domain_space(self, space)

    def subtract_domain(self, uset: "UnionSet") -> "UnionPwAff":
        return _isl_union_pw_aff_subtract_domain(self, uset)

    def pullback_union_pw_multi_aff(self, upma: "UnionPwMultiAff") -> "UnionPwAff":
        return _isl_union_pw_aff_pullback_union_pw_multi_aff(self, upma)

    def gist(self, context: "UnionSet") -> "UnionPwAff":
        return _isl_union_pw_aff_gist(self, context)

    def gist_params(self, context: "Set") -> "UnionPwAff":
        return _isl_union_pw_aff_gist_params(self, context)

    def add(self, upa2: "UnionPwAff") -> "UnionPwAff":
        return _isl_union_pw_aff_add(self, upa2)

    def sub(self, upa2: "UnionPwAff") -> "UnionPwAff":
        return _isl_union_pw_aff_sub(self, upa2)

    def union_add(self, upa2: "UnionPwAff") -> "UnionPwAff":
        return _isl_union_pw_aff_union_add(self, upa2)

    def mod_val(self, f: "Val") -> "UnionPwAff":
        return _isl_union_pw_aff_mod_val(self, f)

    def scale_val(self, f: "Val") -> "UnionPwAff":
        return _isl_union_pw_aff_scale_val(self, f)

    def scale_down_val(self, v: "Val") -> "UnionPwAff":
        return _isl_union_pw_aff_scale_down_val(self, v)


register_type("UnionPwAff", UnionPwAff)

_isl_union_pw_aff_get_ctx = ISLFunction.create(
    "isl_union_pw_aff_get_ctx",
    Keep("UnionPwAff"),
    return_=Give("Ctx"),
    lib=_lib,
)

_isl_union_pw_aff_get_space = ISLFunction.create(
    "isl_union_pw_aff_get_space",
    Keep("UnionPwAff"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_union_pw_aff_dim = ISLFunction.create(
    "isl_union_pw_aff_dim",
    Keep("UnionPwAff"),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_aff_set_dim_name = ISLFunction.create(
    "isl_union_pw_aff_set_dim_name",
    Take("UnionPwAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(str, ctype=c_char_p),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_find_dim_by_name = ISLFunction.create(
    "isl_union_pw_aff_find_dim_by_name",
    Keep("UnionPwAff"),
    Param(int, ctype=c_int),
    Param(str, ctype=c_char_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_aff_reset_user = ISLFunction.create(
    "isl_union_pw_aff_reset_user",
    Take("UnionPwAff"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_empty_ctx = ISLFunction.create(
    "isl_union_pw_aff_empty_ctx",
    Context(),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_empty_space = ISLFunction.create(
    "isl_union_pw_aff_empty_space",
    Take("Space"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_empty = ISLFunction.create(
    "isl_union_pw_aff_empty",
    Take("Space"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_from_pw_aff = ISLFunction.create(
    "isl_union_pw_aff_from_pw_aff",
    Take("PwAff"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_from_aff = ISLFunction.create(
    "isl_union_pw_aff_from_aff",
    Take("Aff"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_val_on_domain = ISLFunction.create(
    "isl_union_pw_aff_val_on_domain",
    Take("UnionSet"),
    Take("Val"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_param_on_domain_id = ISLFunction.create(
    "isl_union_pw_aff_param_on_domain_id",
    Take("UnionSet"),
    Take("Id"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_aff_on_domain = ISLFunction.create(
    "isl_union_pw_aff_aff_on_domain",
    Take("UnionSet"),
    Take("Aff"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_pw_aff_on_domain = ISLFunction.create(
    "isl_union_pw_aff_pw_aff_on_domain",
    Take("UnionSet"),
    Take("PwAff"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_add_pw_aff = ISLFunction.create(
    "isl_union_pw_aff_add_pw_aff",
    Take("UnionPwAff"),
    Take("PwAff"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_copy = ISLFunction.create(
    "isl_union_pw_aff_copy",
    Keep("UnionPwAff"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_free = ISLFunction.create(
    "isl_union_pw_aff_free",
    Take("UnionPwAff"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_n_pw_aff = ISLFunction.create(
    "isl_union_pw_aff_n_pw_aff",
    Keep("UnionPwAff"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_aff_foreach_pw_aff = ISLFunction.create(
    "isl_union_pw_aff_foreach_pw_aff",
    Keep("UnionPwAff"),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_aff_every_pw_aff = ISLFunction.create(
    "isl_union_pw_aff_every_pw_aff",
    Keep("UnionPwAff"),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_aff_extract_pw_aff = ISLFunction.create(
    "isl_union_pw_aff_extract_pw_aff",
    Keep("UnionPwAff"),
    Take("Space"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_union_pw_aff_get_pw_aff_list = ISLFunction.create(
    "isl_union_pw_aff_get_pw_aff_list",
    Keep("UnionPwAff"),
    return_=Give("PwAffList"),
    lib=_lib,
)

_isl_union_pw_aff_read_from_str = ISLFunction.create(
    "isl_union_pw_aff_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_to_str = ISLFunction.create(
    "isl_union_pw_aff_to_str",
    Keep("UnionPwAff"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_union_pw_aff_involves_nan = ISLFunction.create(
    "isl_union_pw_aff_involves_nan",
    Keep("UnionPwAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_aff_plain_is_equal = ISLFunction.create(
    "isl_union_pw_aff_plain_is_equal",
    Keep("UnionPwAff"),
    Keep("UnionPwAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_aff_bind_id = ISLFunction.create(
    "isl_union_pw_aff_bind_id",
    Take("UnionPwAff"),
    Take("Id"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_pw_aff_domain = ISLFunction.create(
    "isl_union_pw_aff_domain",
    Take("UnionPwAff"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_pw_aff_zero_union_set = ISLFunction.create(
    "isl_union_pw_aff_zero_union_set",
    Take("UnionPwAff"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_pw_aff_coalesce = ISLFunction.create(
    "isl_union_pw_aff_coalesce",
    Take("UnionPwAff"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_min_val = ISLFunction.create(
    "isl_union_pw_aff_min_val",
    Take("UnionPwAff"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_union_pw_aff_max_val = ISLFunction.create(
    "isl_union_pw_aff_max_val",
    Take("UnionPwAff"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_union_pw_aff_align_params = ISLFunction.create(
    "isl_union_pw_aff_align_params",
    Take("UnionPwAff"),
    Take("Space"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_drop_unused_params = ISLFunction.create(
    "isl_union_pw_aff_drop_unused_params",
    Take("UnionPwAff"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_neg = ISLFunction.create(
    "isl_union_pw_aff_neg",
    Take("UnionPwAff"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_floor = ISLFunction.create(
    "isl_union_pw_aff_floor",
    Take("UnionPwAff"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_drop_dims = ISLFunction.create(
    "isl_union_pw_aff_drop_dims",
    Take("UnionPwAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_intersect_domain_space = ISLFunction.create(
    "isl_union_pw_aff_intersect_domain_space",
    Take("UnionPwAff"),
    Take("Space"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_intersect_domain_union_set = ISLFunction.create(
    "isl_union_pw_aff_intersect_domain_union_set",
    Take("UnionPwAff"),
    Take("UnionSet"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_intersect_domain = ISLFunction.create(
    "isl_union_pw_aff_intersect_domain",
    Take("UnionPwAff"),
    Take("UnionSet"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_intersect_domain_wrapped_domain = ISLFunction.create(
    "isl_union_pw_aff_intersect_domain_wrapped_domain",
    Take("UnionPwAff"),
    Take("UnionSet"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_intersect_domain_wrapped_range = ISLFunction.create(
    "isl_union_pw_aff_intersect_domain_wrapped_range",
    Take("UnionPwAff"),
    Take("UnionSet"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_intersect_params = ISLFunction.create(
    "isl_union_pw_aff_intersect_params",
    Take("UnionPwAff"),
    Take("Set"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_subtract_domain_union_set = ISLFunction.create(
    "isl_union_pw_aff_subtract_domain_union_set",
    Take("UnionPwAff"),
    Take("UnionSet"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_subtract_domain_space = ISLFunction.create(
    "isl_union_pw_aff_subtract_domain_space",
    Take("UnionPwAff"),
    Take("Space"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_subtract_domain = ISLFunction.create(
    "isl_union_pw_aff_subtract_domain",
    Take("UnionPwAff"),
    Take("UnionSet"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_pullback_union_pw_multi_aff = ISLFunction.create(
    "isl_union_pw_aff_pullback_union_pw_multi_aff",
    Take("UnionPwAff"),
    Take("UnionPwMultiAff"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_gist = ISLFunction.create(
    "isl_union_pw_aff_gist",
    Take("UnionPwAff"),
    Take("UnionSet"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_gist_params = ISLFunction.create(
    "isl_union_pw_aff_gist_params",
    Take("UnionPwAff"),
    Take("Set"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_add = ISLFunction.create(
    "isl_union_pw_aff_add",
    Take("UnionPwAff"),
    Take("UnionPwAff"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_sub = ISLFunction.create(
    "isl_union_pw_aff_sub",
    Take("UnionPwAff"),
    Take("UnionPwAff"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_union_add = ISLFunction.create(
    "isl_union_pw_aff_union_add",
    Take("UnionPwAff"),
    Take("UnionPwAff"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_mod_val = ISLFunction.create(
    "isl_union_pw_aff_mod_val",
    Take("UnionPwAff"),
    Take("Val"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_scale_val = ISLFunction.create(
    "isl_union_pw_aff_scale_val",
    Take("UnionPwAff"),
    Take("Val"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_aff_scale_down_val = ISLFunction.create(
    "isl_union_pw_aff_scale_down_val",
    Take("UnionPwAff"),
    Take("Val"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)
