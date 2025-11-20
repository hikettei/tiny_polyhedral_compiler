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
    from .context import Context
    from .point import Point
    from .pw_qpolynomial_fold import PwQpolynomialFold
    from .set import Set
    from .space import Space
    from .union_set import UnionSet
    from .val import Val

_lib = load_libisl()

class UnionPwQpolynomialFold(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_union_pw_qpolynomial_fold_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_union_pw_qpolynomial_fold_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_union_pw_qpolynomial_fold_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_union_pw_qpolynomial_fold_free(handle)

    def get_space(self) -> "Space":
        return _isl_union_pw_qpolynomial_fold_get_space(self)

    def dim(self, type: int) -> int:
        return _isl_union_pw_qpolynomial_fold_dim(self, type)

    def set_dim_name(self, type: int, pos: int, s: str) -> "UnionPwQpolynomialFold":
        return _isl_union_pw_qpolynomial_fold_set_dim_name(self, type, pos, s)

    def find_dim_by_name(self, type: int, name: str) -> int:
        return _isl_union_pw_qpolynomial_fold_find_dim_by_name(self, type, name)

    def reset_user(self) -> "UnionPwQpolynomialFold":
        return _isl_union_pw_qpolynomial_fold_reset_user(self)

    def get_type(self) -> int:
        return _isl_union_pw_qpolynomial_fold_get_type(self)

    @classmethod
    def from_pw_qpolynomial_fold(cls, pwf: "PwQpolynomialFold") -> "UnionPwQpolynomialFold":
        return _isl_union_pw_qpolynomial_fold_from_pw_qpolynomial_fold(pwf)

    def n_pw_qpolynomial_fold(self) -> int:
        return _isl_union_pw_qpolynomial_fold_n_pw_qpolynomial_fold(self)

    def foreach_pw_qpolynomial_fold(self, fn: Any, user: Any, user_: Any = None) -> int:
        return _isl_union_pw_qpolynomial_fold_foreach_pw_qpolynomial_fold(self, fn, user, user_)

    def every_pw_qpolynomial_fold(self, test: Any, user: Any, user_: Any = None) -> bool:
        return _isl_union_pw_qpolynomial_fold_every_pw_qpolynomial_fold(self, test, user, user_)

    def involves_nan(self) -> bool:
        return _isl_union_pw_qpolynomial_fold_involves_nan(self)

    def plain_is_equal(self, upwf2: "UnionPwQpolynomialFold") -> bool:
        return _isl_union_pw_qpolynomial_fold_plain_is_equal(self, upwf2)

    def domain(self) -> "UnionSet":
        return _isl_union_pw_qpolynomial_fold_domain(self)

    def coalesce(self) -> "UnionPwQpolynomialFold":
        return _isl_union_pw_qpolynomial_fold_coalesce(self)

    def drop_unused_params(self) -> "UnionPwQpolynomialFold":
        return _isl_union_pw_qpolynomial_fold_drop_unused_params(self)

    def eval(self, pnt: "Point") -> "Val":
        return _isl_union_pw_qpolynomial_fold_eval(self, pnt)

    def drop_dims(self, type: int, first: int, n: int) -> "UnionPwQpolynomialFold":
        return _isl_union_pw_qpolynomial_fold_drop_dims(self, type, first, n)

    def intersect_domain_space(self, space: "Space") -> "UnionPwQpolynomialFold":
        return _isl_union_pw_qpolynomial_fold_intersect_domain_space(self, space)

    def intersect_domain_union_set(self, uset: "UnionSet") -> "UnionPwQpolynomialFold":
        return _isl_union_pw_qpolynomial_fold_intersect_domain_union_set(self, uset)

    def intersect_domain(self, uset: "UnionSet") -> "UnionPwQpolynomialFold":
        return _isl_union_pw_qpolynomial_fold_intersect_domain(self, uset)

    def intersect_domain_wrapped_domain(self, uset: "UnionSet") -> "UnionPwQpolynomialFold":
        return _isl_union_pw_qpolynomial_fold_intersect_domain_wrapped_domain(self, uset)

    def intersect_domain_wrapped_range(self, uset: "UnionSet") -> "UnionPwQpolynomialFold":
        return _isl_union_pw_qpolynomial_fold_intersect_domain_wrapped_range(self, uset)

    def intersect_params(self, set: "Set") -> "UnionPwQpolynomialFold":
        return _isl_union_pw_qpolynomial_fold_intersect_params(self, set)

    def subtract_domain_union_set(self, uset: "UnionSet") -> "UnionPwQpolynomialFold":
        return _isl_union_pw_qpolynomial_fold_subtract_domain_union_set(self, uset)

    def subtract_domain_space(self, space: "Space") -> "UnionPwQpolynomialFold":
        return _isl_union_pw_qpolynomial_fold_subtract_domain_space(self, space)

    def subtract_domain(self, uset: "UnionSet") -> "UnionPwQpolynomialFold":
        return _isl_union_pw_qpolynomial_fold_subtract_domain(self, uset)

    def gist(self, context: "UnionSet") -> "UnionPwQpolynomialFold":
        return _isl_union_pw_qpolynomial_fold_gist(self, context)

    def gist_params(self, context: "Set") -> "UnionPwQpolynomialFold":
        return _isl_union_pw_qpolynomial_fold_gist_params(self, context)

    def fold(self, upwf2: "UnionPwQpolynomialFold") -> "UnionPwQpolynomialFold":
        return _isl_union_pw_qpolynomial_fold_fold(self, upwf2)

    def scale_val(self, v: "Val") -> "UnionPwQpolynomialFold":
        return _isl_union_pw_qpolynomial_fold_scale_val(self, v)

    def scale_down_val(self, v: "Val") -> "UnionPwQpolynomialFold":
        return _isl_union_pw_qpolynomial_fold_scale_down_val(self, v)


register_type("UnionPwQpolynomialFold", UnionPwQpolynomialFold)

_isl_union_pw_qpolynomial_fold_get_space = ISLFunction.create(
    "isl_union_pw_qpolynomial_fold_get_space",
    Keep("UnionPwQpolynomialFold"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_fold_dim = ISLFunction.create(
    "isl_union_pw_qpolynomial_fold_dim",
    Keep("UnionPwQpolynomialFold"),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_qpolynomial_fold_set_dim_name = ISLFunction.create(
    "isl_union_pw_qpolynomial_fold_set_dim_name",
    Take("UnionPwQpolynomialFold"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(str, ctype=c_char_p),
    return_=Give("UnionPwQpolynomialFold"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_fold_find_dim_by_name = ISLFunction.create(
    "isl_union_pw_qpolynomial_fold_find_dim_by_name",
    Keep("UnionPwQpolynomialFold"),
    Param(int, ctype=c_int),
    Param(str, ctype=c_char_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_qpolynomial_fold_reset_user = ISLFunction.create(
    "isl_union_pw_qpolynomial_fold_reset_user",
    Take("UnionPwQpolynomialFold"),
    return_=Give("UnionPwQpolynomialFold"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_fold_get_type = ISLFunction.create(
    "isl_union_pw_qpolynomial_fold_get_type",
    Keep("UnionPwQpolynomialFold"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_qpolynomial_fold_from_pw_qpolynomial_fold = ISLFunction.create(
    "isl_union_pw_qpolynomial_fold_from_pw_qpolynomial_fold",
    Take("PwQpolynomialFold"),
    return_=Give("UnionPwQpolynomialFold"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_fold_copy = ISLFunction.create(
    "isl_union_pw_qpolynomial_fold_copy",
    Keep("UnionPwQpolynomialFold"),
    return_=Give("UnionPwQpolynomialFold"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_fold_free = ISLFunction.create(
    "isl_union_pw_qpolynomial_fold_free",
    Take("UnionPwQpolynomialFold"),
    return_=Give("UnionPwQpolynomialFold"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_fold_n_pw_qpolynomial_fold = ISLFunction.create(
    "isl_union_pw_qpolynomial_fold_n_pw_qpolynomial_fold",
    Keep("UnionPwQpolynomialFold"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_qpolynomial_fold_foreach_pw_qpolynomial_fold = ISLFunction.create(
    "isl_union_pw_qpolynomial_fold_foreach_pw_qpolynomial_fold",
    Keep("UnionPwQpolynomialFold"),
    Param(None, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_qpolynomial_fold_every_pw_qpolynomial_fold = ISLFunction.create(
    "isl_union_pw_qpolynomial_fold_every_pw_qpolynomial_fold",
    Keep("UnionPwQpolynomialFold"),
    Param(None, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_qpolynomial_fold_involves_nan = ISLFunction.create(
    "isl_union_pw_qpolynomial_fold_involves_nan",
    Keep("UnionPwQpolynomialFold"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_qpolynomial_fold_plain_is_equal = ISLFunction.create(
    "isl_union_pw_qpolynomial_fold_plain_is_equal",
    Keep("UnionPwQpolynomialFold"),
    Keep("UnionPwQpolynomialFold"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_qpolynomial_fold_domain = ISLFunction.create(
    "isl_union_pw_qpolynomial_fold_domain",
    Take("UnionPwQpolynomialFold"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_fold_coalesce = ISLFunction.create(
    "isl_union_pw_qpolynomial_fold_coalesce",
    Take("UnionPwQpolynomialFold"),
    return_=Give("UnionPwQpolynomialFold"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_fold_drop_unused_params = ISLFunction.create(
    "isl_union_pw_qpolynomial_fold_drop_unused_params",
    Take("UnionPwQpolynomialFold"),
    return_=Give("UnionPwQpolynomialFold"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_fold_eval = ISLFunction.create(
    "isl_union_pw_qpolynomial_fold_eval",
    Take("UnionPwQpolynomialFold"),
    Take("Point"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_fold_drop_dims = ISLFunction.create(
    "isl_union_pw_qpolynomial_fold_drop_dims",
    Take("UnionPwQpolynomialFold"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("UnionPwQpolynomialFold"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_fold_intersect_domain_space = ISLFunction.create(
    "isl_union_pw_qpolynomial_fold_intersect_domain_space",
    Take("UnionPwQpolynomialFold"),
    Take("Space"),
    return_=Give("UnionPwQpolynomialFold"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_fold_intersect_domain_union_set = ISLFunction.create(
    "isl_union_pw_qpolynomial_fold_intersect_domain_union_set",
    Take("UnionPwQpolynomialFold"),
    Take("UnionSet"),
    return_=Give("UnionPwQpolynomialFold"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_fold_intersect_domain = ISLFunction.create(
    "isl_union_pw_qpolynomial_fold_intersect_domain",
    Take("UnionPwQpolynomialFold"),
    Take("UnionSet"),
    return_=Give("UnionPwQpolynomialFold"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_fold_intersect_domain_wrapped_domain = ISLFunction.create(
    "isl_union_pw_qpolynomial_fold_intersect_domain_wrapped_domain",
    Take("UnionPwQpolynomialFold"),
    Take("UnionSet"),
    return_=Give("UnionPwQpolynomialFold"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_fold_intersect_domain_wrapped_range = ISLFunction.create(
    "isl_union_pw_qpolynomial_fold_intersect_domain_wrapped_range",
    Take("UnionPwQpolynomialFold"),
    Take("UnionSet"),
    return_=Give("UnionPwQpolynomialFold"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_fold_intersect_params = ISLFunction.create(
    "isl_union_pw_qpolynomial_fold_intersect_params",
    Take("UnionPwQpolynomialFold"),
    Take("Set"),
    return_=Give("UnionPwQpolynomialFold"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_fold_subtract_domain_union_set = ISLFunction.create(
    "isl_union_pw_qpolynomial_fold_subtract_domain_union_set",
    Take("UnionPwQpolynomialFold"),
    Take("UnionSet"),
    return_=Give("UnionPwQpolynomialFold"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_fold_subtract_domain_space = ISLFunction.create(
    "isl_union_pw_qpolynomial_fold_subtract_domain_space",
    Take("UnionPwQpolynomialFold"),
    Take("Space"),
    return_=Give("UnionPwQpolynomialFold"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_fold_subtract_domain = ISLFunction.create(
    "isl_union_pw_qpolynomial_fold_subtract_domain",
    Take("UnionPwQpolynomialFold"),
    Take("UnionSet"),
    return_=Give("UnionPwQpolynomialFold"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_fold_gist = ISLFunction.create(
    "isl_union_pw_qpolynomial_fold_gist",
    Take("UnionPwQpolynomialFold"),
    Take("UnionSet"),
    return_=Give("UnionPwQpolynomialFold"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_fold_gist_params = ISLFunction.create(
    "isl_union_pw_qpolynomial_fold_gist_params",
    Take("UnionPwQpolynomialFold"),
    Take("Set"),
    return_=Give("UnionPwQpolynomialFold"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_fold_fold = ISLFunction.create(
    "isl_union_pw_qpolynomial_fold_fold",
    Take("UnionPwQpolynomialFold"),
    Take("UnionPwQpolynomialFold"),
    return_=Give("UnionPwQpolynomialFold"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_fold_scale_val = ISLFunction.create(
    "isl_union_pw_qpolynomial_fold_scale_val",
    Take("UnionPwQpolynomialFold"),
    Take("Val"),
    return_=Give("UnionPwQpolynomialFold"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_fold_scale_down_val = ISLFunction.create(
    "isl_union_pw_qpolynomial_fold_scale_down_val",
    Take("UnionPwQpolynomialFold"),
    Take("Val"),
    return_=Give("UnionPwQpolynomialFold"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_fold_read_from_str = ISLFunction.create(
    "isl_union_pw_qpolynomial_fold_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("UnionPwQpolynomialFold"),
    lib=_lib,
)
