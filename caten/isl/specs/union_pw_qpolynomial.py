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
    from .pw_qpolynomial import PwQpolynomial
    from .set import Set
    from .space import Space
    from .union_set import UnionSet
    from .val import Val

_lib = load_libisl()

class UnionPwQpolynomial(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_union_pw_qpolynomial_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_union_pw_qpolynomial_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_union_pw_qpolynomial_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_union_pw_qpolynomial_free(handle)

    def __str__(self) -> str:
        return _isl_union_pw_qpolynomial_to_str(self)

    def __repr__(self) -> str:
        return f"UnionPwQpolynomial({self.__str__()})"

    def get_space(self) -> "Space":
        return _isl_union_pw_qpolynomial_get_space(self)

    def dim(self, type: int) -> int:
        return _isl_union_pw_qpolynomial_dim(self, type)

    def set_dim_name(self, type: int, pos: int, s: str) -> "UnionPwQpolynomial":
        return _isl_union_pw_qpolynomial_set_dim_name(self, type, pos, s)

    def find_dim_by_name(self, type: int, name: str) -> int:
        return _isl_union_pw_qpolynomial_find_dim_by_name(self, type, name)

    def reset_user(self) -> "UnionPwQpolynomial":
        return _isl_union_pw_qpolynomial_reset_user(self)

    @classmethod
    def zero_ctx(cls) -> "UnionPwQpolynomial":
        return _isl_union_pw_qpolynomial_zero_ctx()

    @classmethod
    def zero_space(cls, space: "Space") -> "UnionPwQpolynomial":
        return _isl_union_pw_qpolynomial_zero_space(space)

    @classmethod
    def zero(cls, space: "Space") -> "UnionPwQpolynomial":
        return _isl_union_pw_qpolynomial_zero(space)

    @classmethod
    def from_pw_qpolynomial(cls, pwqp: "PwQpolynomial") -> "UnionPwQpolynomial":
        return _isl_union_pw_qpolynomial_from_pw_qpolynomial(pwqp)

    def add_pw_qpolynomial(self, pwqp: "PwQpolynomial") -> "UnionPwQpolynomial":
        return _isl_union_pw_qpolynomial_add_pw_qpolynomial(self, pwqp)

    def n_pw_qpolynomial(self) -> int:
        return _isl_union_pw_qpolynomial_n_pw_qpolynomial(self)

    def foreach_pw_qpolynomial(self, fn: Any, user: Any, user_: Any = None) -> int:
        return _isl_union_pw_qpolynomial_foreach_pw_qpolynomial(self, fn, user, user_)

    def every_pw_qpolynomial(self, test: Any, user: Any, user_: Any = None) -> bool:
        return _isl_union_pw_qpolynomial_every_pw_qpolynomial(self, test, user, user_)

    def extract_pw_qpolynomial(self, space: "Space") -> "PwQpolynomial":
        return _isl_union_pw_qpolynomial_extract_pw_qpolynomial(self, space)

    def involves_nan(self) -> bool:
        return _isl_union_pw_qpolynomial_involves_nan(self)

    def plain_is_equal(self, upwqp2: "UnionPwQpolynomial") -> bool:
        return _isl_union_pw_qpolynomial_plain_is_equal(self, upwqp2)

    def domain_reverse(self) -> "UnionPwQpolynomial":
        return _isl_union_pw_qpolynomial_domain_reverse(self)

    def domain(self) -> "UnionSet":
        return _isl_union_pw_qpolynomial_domain(self)

    def coalesce(self) -> "UnionPwQpolynomial":
        return _isl_union_pw_qpolynomial_coalesce(self)

    def to_polynomial(self, sign: int) -> "UnionPwQpolynomial":
        return _isl_union_pw_qpolynomial_to_polynomial(self, sign)

    def drop_unused_params(self) -> "UnionPwQpolynomial":
        return _isl_union_pw_qpolynomial_drop_unused_params(self)

    def neg(self) -> "UnionPwQpolynomial":
        return _isl_union_pw_qpolynomial_neg(self)

    def eval(self, pnt: "Point") -> "Val":
        return _isl_union_pw_qpolynomial_eval(self, pnt)

    def drop_dims(self, type: int, first: int, n: int) -> "UnionPwQpolynomial":
        return _isl_union_pw_qpolynomial_drop_dims(self, type, first, n)

    def intersect_domain_space(self, space: "Space") -> "UnionPwQpolynomial":
        return _isl_union_pw_qpolynomial_intersect_domain_space(self, space)

    def intersect_domain_union_set(self, uset: "UnionSet") -> "UnionPwQpolynomial":
        return _isl_union_pw_qpolynomial_intersect_domain_union_set(self, uset)

    def intersect_domain(self, uset: "UnionSet") -> "UnionPwQpolynomial":
        return _isl_union_pw_qpolynomial_intersect_domain(self, uset)

    def intersect_domain_wrapped_domain(self, uset: "UnionSet") -> "UnionPwQpolynomial":
        return _isl_union_pw_qpolynomial_intersect_domain_wrapped_domain(self, uset)

    def intersect_domain_wrapped_range(self, uset: "UnionSet") -> "UnionPwQpolynomial":
        return _isl_union_pw_qpolynomial_intersect_domain_wrapped_range(self, uset)

    def intersect_params(self, set: "Set") -> "UnionPwQpolynomial":
        return _isl_union_pw_qpolynomial_intersect_params(self, set)

    def subtract_domain_union_set(self, uset: "UnionSet") -> "UnionPwQpolynomial":
        return _isl_union_pw_qpolynomial_subtract_domain_union_set(self, uset)

    def subtract_domain_space(self, space: "Space") -> "UnionPwQpolynomial":
        return _isl_union_pw_qpolynomial_subtract_domain_space(self, space)

    def subtract_domain(self, uset: "UnionSet") -> "UnionPwQpolynomial":
        return _isl_union_pw_qpolynomial_subtract_domain(self, uset)

    def gist_params(self, context: "Set") -> "UnionPwQpolynomial":
        return _isl_union_pw_qpolynomial_gist_params(self, context)

    def gist(self, context: "UnionSet") -> "UnionPwQpolynomial":
        return _isl_union_pw_qpolynomial_gist(self, context)

    def add(self, upwqp2: "UnionPwQpolynomial") -> "UnionPwQpolynomial":
        return _isl_union_pw_qpolynomial_add(self, upwqp2)

    def sub(self, upwqp2: "UnionPwQpolynomial") -> "UnionPwQpolynomial":
        return _isl_union_pw_qpolynomial_sub(self, upwqp2)

    def scale_val(self, v: "Val") -> "UnionPwQpolynomial":
        return _isl_union_pw_qpolynomial_scale_val(self, v)

    def scale_down_val(self, v: "Val") -> "UnionPwQpolynomial":
        return _isl_union_pw_qpolynomial_scale_down_val(self, v)

    def mul(self, upwqp2: "UnionPwQpolynomial") -> "UnionPwQpolynomial":
        return _isl_union_pw_qpolynomial_mul(self, upwqp2)


register_type("UnionPwQpolynomial", UnionPwQpolynomial)

_isl_union_pw_qpolynomial_get_space = ISLFunction.create(
    "isl_union_pw_qpolynomial_get_space",
    Keep("UnionPwQpolynomial"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_dim = ISLFunction.create(
    "isl_union_pw_qpolynomial_dim",
    Keep("UnionPwQpolynomial"),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_qpolynomial_set_dim_name = ISLFunction.create(
    "isl_union_pw_qpolynomial_set_dim_name",
    Take("UnionPwQpolynomial"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(str, ctype=c_char_p),
    return_=Give("UnionPwQpolynomial"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_find_dim_by_name = ISLFunction.create(
    "isl_union_pw_qpolynomial_find_dim_by_name",
    Keep("UnionPwQpolynomial"),
    Param(int, ctype=c_int),
    Param(str, ctype=c_char_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_qpolynomial_reset_user = ISLFunction.create(
    "isl_union_pw_qpolynomial_reset_user",
    Take("UnionPwQpolynomial"),
    return_=Give("UnionPwQpolynomial"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_zero_ctx = ISLFunction.create(
    "isl_union_pw_qpolynomial_zero_ctx",
    Context(),
    return_=Give("UnionPwQpolynomial"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_zero_space = ISLFunction.create(
    "isl_union_pw_qpolynomial_zero_space",
    Take("Space"),
    return_=Give("UnionPwQpolynomial"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_zero = ISLFunction.create(
    "isl_union_pw_qpolynomial_zero",
    Take("Space"),
    return_=Give("UnionPwQpolynomial"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_from_pw_qpolynomial = ISLFunction.create(
    "isl_union_pw_qpolynomial_from_pw_qpolynomial",
    Take("PwQpolynomial"),
    return_=Give("UnionPwQpolynomial"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_add_pw_qpolynomial = ISLFunction.create(
    "isl_union_pw_qpolynomial_add_pw_qpolynomial",
    Take("UnionPwQpolynomial"),
    Take("PwQpolynomial"),
    return_=Give("UnionPwQpolynomial"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_copy = ISLFunction.create(
    "isl_union_pw_qpolynomial_copy",
    Keep("UnionPwQpolynomial"),
    return_=Give("UnionPwQpolynomial"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_free = ISLFunction.create(
    "isl_union_pw_qpolynomial_free",
    Take("UnionPwQpolynomial"),
    return_=Give("UnionPwQpolynomial"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_n_pw_qpolynomial = ISLFunction.create(
    "isl_union_pw_qpolynomial_n_pw_qpolynomial",
    Keep("UnionPwQpolynomial"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_qpolynomial_foreach_pw_qpolynomial = ISLFunction.create(
    "isl_union_pw_qpolynomial_foreach_pw_qpolynomial",
    Keep("UnionPwQpolynomial"),
    Param(None, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_qpolynomial_every_pw_qpolynomial = ISLFunction.create(
    "isl_union_pw_qpolynomial_every_pw_qpolynomial",
    Keep("UnionPwQpolynomial"),
    Param(None, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_qpolynomial_extract_pw_qpolynomial = ISLFunction.create(
    "isl_union_pw_qpolynomial_extract_pw_qpolynomial",
    Keep("UnionPwQpolynomial"),
    Take("Space"),
    return_=Give("PwQpolynomial"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_read_from_str = ISLFunction.create(
    "isl_union_pw_qpolynomial_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("UnionPwQpolynomial"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_to_str = ISLFunction.create(
    "isl_union_pw_qpolynomial_to_str",
    Keep("UnionPwQpolynomial"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_union_pw_qpolynomial_involves_nan = ISLFunction.create(
    "isl_union_pw_qpolynomial_involves_nan",
    Keep("UnionPwQpolynomial"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_qpolynomial_plain_is_equal = ISLFunction.create(
    "isl_union_pw_qpolynomial_plain_is_equal",
    Keep("UnionPwQpolynomial"),
    Keep("UnionPwQpolynomial"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_qpolynomial_domain_reverse = ISLFunction.create(
    "isl_union_pw_qpolynomial_domain_reverse",
    Take("UnionPwQpolynomial"),
    return_=Give("UnionPwQpolynomial"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_domain = ISLFunction.create(
    "isl_union_pw_qpolynomial_domain",
    Take("UnionPwQpolynomial"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_coalesce = ISLFunction.create(
    "isl_union_pw_qpolynomial_coalesce",
    Take("UnionPwQpolynomial"),
    return_=Give("UnionPwQpolynomial"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_to_polynomial = ISLFunction.create(
    "isl_union_pw_qpolynomial_to_polynomial",
    Take("UnionPwQpolynomial"),
    Param(int, ctype=c_int),
    return_=Give("UnionPwQpolynomial"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_drop_unused_params = ISLFunction.create(
    "isl_union_pw_qpolynomial_drop_unused_params",
    Take("UnionPwQpolynomial"),
    return_=Give("UnionPwQpolynomial"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_neg = ISLFunction.create(
    "isl_union_pw_qpolynomial_neg",
    Take("UnionPwQpolynomial"),
    return_=Give("UnionPwQpolynomial"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_eval = ISLFunction.create(
    "isl_union_pw_qpolynomial_eval",
    Take("UnionPwQpolynomial"),
    Take("Point"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_drop_dims = ISLFunction.create(
    "isl_union_pw_qpolynomial_drop_dims",
    Take("UnionPwQpolynomial"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("UnionPwQpolynomial"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_intersect_domain_space = ISLFunction.create(
    "isl_union_pw_qpolynomial_intersect_domain_space",
    Take("UnionPwQpolynomial"),
    Take("Space"),
    return_=Give("UnionPwQpolynomial"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_intersect_domain_union_set = ISLFunction.create(
    "isl_union_pw_qpolynomial_intersect_domain_union_set",
    Take("UnionPwQpolynomial"),
    Take("UnionSet"),
    return_=Give("UnionPwQpolynomial"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_intersect_domain = ISLFunction.create(
    "isl_union_pw_qpolynomial_intersect_domain",
    Take("UnionPwQpolynomial"),
    Take("UnionSet"),
    return_=Give("UnionPwQpolynomial"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_intersect_domain_wrapped_domain = ISLFunction.create(
    "isl_union_pw_qpolynomial_intersect_domain_wrapped_domain",
    Take("UnionPwQpolynomial"),
    Take("UnionSet"),
    return_=Give("UnionPwQpolynomial"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_intersect_domain_wrapped_range = ISLFunction.create(
    "isl_union_pw_qpolynomial_intersect_domain_wrapped_range",
    Take("UnionPwQpolynomial"),
    Take("UnionSet"),
    return_=Give("UnionPwQpolynomial"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_intersect_params = ISLFunction.create(
    "isl_union_pw_qpolynomial_intersect_params",
    Take("UnionPwQpolynomial"),
    Take("Set"),
    return_=Give("UnionPwQpolynomial"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_subtract_domain_union_set = ISLFunction.create(
    "isl_union_pw_qpolynomial_subtract_domain_union_set",
    Take("UnionPwQpolynomial"),
    Take("UnionSet"),
    return_=Give("UnionPwQpolynomial"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_subtract_domain_space = ISLFunction.create(
    "isl_union_pw_qpolynomial_subtract_domain_space",
    Take("UnionPwQpolynomial"),
    Take("Space"),
    return_=Give("UnionPwQpolynomial"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_subtract_domain = ISLFunction.create(
    "isl_union_pw_qpolynomial_subtract_domain",
    Take("UnionPwQpolynomial"),
    Take("UnionSet"),
    return_=Give("UnionPwQpolynomial"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_gist_params = ISLFunction.create(
    "isl_union_pw_qpolynomial_gist_params",
    Take("UnionPwQpolynomial"),
    Take("Set"),
    return_=Give("UnionPwQpolynomial"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_gist = ISLFunction.create(
    "isl_union_pw_qpolynomial_gist",
    Take("UnionPwQpolynomial"),
    Take("UnionSet"),
    return_=Give("UnionPwQpolynomial"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_add = ISLFunction.create(
    "isl_union_pw_qpolynomial_add",
    Take("UnionPwQpolynomial"),
    Take("UnionPwQpolynomial"),
    return_=Give("UnionPwQpolynomial"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_sub = ISLFunction.create(
    "isl_union_pw_qpolynomial_sub",
    Take("UnionPwQpolynomial"),
    Take("UnionPwQpolynomial"),
    return_=Give("UnionPwQpolynomial"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_scale_val = ISLFunction.create(
    "isl_union_pw_qpolynomial_scale_val",
    Take("UnionPwQpolynomial"),
    Take("Val"),
    return_=Give("UnionPwQpolynomial"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_scale_down_val = ISLFunction.create(
    "isl_union_pw_qpolynomial_scale_down_val",
    Take("UnionPwQpolynomial"),
    Take("Val"),
    return_=Give("UnionPwQpolynomial"),
    lib=_lib,
)

_isl_union_pw_qpolynomial_mul = ISLFunction.create(
    "isl_union_pw_qpolynomial_mul",
    Take("UnionPwQpolynomial"),
    Take("UnionPwQpolynomial"),
    return_=Give("UnionPwQpolynomial"),
    lib=_lib,
)
