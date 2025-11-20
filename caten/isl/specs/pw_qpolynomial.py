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
    from .id import Id
    from .point import Point
    from .pw_aff import PwAff
    from .qpolynomial import Qpolynomial
    from .set import Set
    from .space import Space
    from .union_pw_qpolynomial import UnionPwQpolynomial
    from .val import Val

_lib = load_libisl()

class PwQpolynomial(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_pw_qpolynomial_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_pw_qpolynomial_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_pw_qpolynomial_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_pw_qpolynomial_free(handle)

    def __str__(self) -> str:
        return _isl_pw_qpolynomial_to_str(self)

    def __repr__(self) -> str:
        return f"PwQpolynomial({self.__str__()})"

    def get_domain_space(self) -> "Space":
        return _isl_pw_qpolynomial_get_domain_space(self)

    def get_space(self) -> "Space":
        return _isl_pw_qpolynomial_get_space(self)

    def set_dim_name(self, type: int, pos: int, s: str) -> "PwQpolynomial":
        return _isl_pw_qpolynomial_set_dim_name(self, type, pos, s)

    def find_dim_by_name(self, type: int, name: str) -> int:
        return _isl_pw_qpolynomial_find_dim_by_name(self, type, name)

    def reset_user(self) -> "PwQpolynomial":
        return _isl_pw_qpolynomial_reset_user(self)

    @classmethod
    def from_qpolynomial(cls, qp: "Qpolynomial") -> "PwQpolynomial":
        return _isl_pw_qpolynomial_from_qpolynomial(qp)

    def isa_qpolynomial(self) -> bool:
        return _isl_pw_qpolynomial_isa_qpolynomial(self)

    def as_qpolynomial(self) -> "Qpolynomial":
        return _isl_pw_qpolynomial_as_qpolynomial(self)

    @classmethod
    def alloc(cls, set: "Set", qp: "Qpolynomial") -> "PwQpolynomial":
        return _isl_pw_qpolynomial_alloc(set, qp)

    @classmethod
    def zero(cls, space: "Space") -> "PwQpolynomial":
        return _isl_pw_qpolynomial_zero(space)

    @classmethod
    def from_pw_aff(cls, pwaff: "PwAff") -> "PwQpolynomial":
        return _isl_pw_qpolynomial_from_pw_aff(pwaff)

    def n_piece(self) -> int:
        return _isl_pw_qpolynomial_n_piece(self)

    def foreach_piece(self, fn: Any, user: Any = None) -> int:
        return _isl_pw_qpolynomial_foreach_piece(self, fn, user)

    def every_piece(self, test: Any, user: Any = None) -> bool:
        return _isl_pw_qpolynomial_every_piece(self, test, user)

    def foreach_lifted_piece(self, fn: Any, user: Any = None) -> int:
        return _isl_pw_qpolynomial_foreach_lifted_piece(self, fn, user)

    def to_union_pw_qpolynomial(self) -> "UnionPwQpolynomial":
        return _isl_pw_qpolynomial_to_union_pw_qpolynomial(self)

    def involves_param_id(self, id: "Id") -> bool:
        return _isl_pw_qpolynomial_involves_param_id(self, id)

    def involves_nan(self) -> bool:
        return _isl_pw_qpolynomial_involves_nan(self)

    def domain_reverse(self) -> "PwQpolynomial":
        return _isl_pw_qpolynomial_domain_reverse(self)

    def project_domain_on_params(self) -> "PwQpolynomial":
        return _isl_pw_qpolynomial_project_domain_on_params(self)

    def domain(self) -> "Set":
        return _isl_pw_qpolynomial_domain(self)

    def from_range(self) -> "PwQpolynomial":
        return _isl_pw_qpolynomial_from_range(self)

    def fix_val(self, type: int, n: int, v: "Val") -> "PwQpolynomial":
        return _isl_pw_qpolynomial_fix_val(self, type, n, v)

    def to_polynomial(self, sign: int) -> "PwQpolynomial":
        return _isl_pw_qpolynomial_to_polynomial(self, sign)

    def drop_unused_params(self) -> "PwQpolynomial":
        return _isl_pw_qpolynomial_drop_unused_params(self)

    def neg(self) -> "PwQpolynomial":
        return _isl_pw_qpolynomial_neg(self)

    def pow(self, exponent: int) -> "PwQpolynomial":
        return _isl_pw_qpolynomial_pow(self, exponent)

    def eval(self, pnt: "Point") -> "Val":
        return _isl_pw_qpolynomial_eval(self, pnt)

    def intersect_domain(self, set: "Set") -> "PwQpolynomial":
        return _isl_pw_qpolynomial_intersect_domain(self, set)

    def intersect_domain_wrapped_domain(self, set: "Set") -> "PwQpolynomial":
        return _isl_pw_qpolynomial_intersect_domain_wrapped_domain(self, set)

    def intersect_domain_wrapped_range(self, set: "Set") -> "PwQpolynomial":
        return _isl_pw_qpolynomial_intersect_domain_wrapped_range(self, set)

    def intersect_params(self, set: "Set") -> "PwQpolynomial":
        return _isl_pw_qpolynomial_intersect_params(self, set)

    def subtract_domain(self, set: "Set") -> "PwQpolynomial":
        return _isl_pw_qpolynomial_subtract_domain(self, set)

    def gist_params(self, context: "Set") -> "PwQpolynomial":
        return _isl_pw_qpolynomial_gist_params(self, context)

    def gist(self, context: "Set") -> "PwQpolynomial":
        return _isl_pw_qpolynomial_gist(self, context)

    def add(self, pwqp2: "PwQpolynomial") -> "PwQpolynomial":
        return _isl_pw_qpolynomial_add(self, pwqp2)

    def add_disjoint(self, pwqp2: "PwQpolynomial") -> "PwQpolynomial":
        return _isl_pw_qpolynomial_add_disjoint(self, pwqp2)

    def sub(self, pwqp2: "PwQpolynomial") -> "PwQpolynomial":
        return _isl_pw_qpolynomial_sub(self, pwqp2)

    def scale_val(self, v: "Val") -> "PwQpolynomial":
        return _isl_pw_qpolynomial_scale_val(self, v)

    def scale_down_val(self, v: "Val") -> "PwQpolynomial":
        return _isl_pw_qpolynomial_scale_down_val(self, v)

    def mul(self, pwqp2: "PwQpolynomial") -> "PwQpolynomial":
        return _isl_pw_qpolynomial_mul(self, pwqp2)


register_type("PwQpolynomial", PwQpolynomial)

_isl_pw_qpolynomial_get_domain_space = ISLFunction.create(
    "isl_pw_qpolynomial_get_domain_space",
    Keep("PwQpolynomial"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_pw_qpolynomial_get_space = ISLFunction.create(
    "isl_pw_qpolynomial_get_space",
    Keep("PwQpolynomial"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_pw_qpolynomial_set_dim_name = ISLFunction.create(
    "isl_pw_qpolynomial_set_dim_name",
    Take("PwQpolynomial"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(str, ctype=c_char_p),
    return_=Give("PwQpolynomial"),
    lib=_lib,
)

_isl_pw_qpolynomial_find_dim_by_name = ISLFunction.create(
    "isl_pw_qpolynomial_find_dim_by_name",
    Keep("PwQpolynomial"),
    Param(int, ctype=c_int),
    Param(str, ctype=c_char_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_pw_qpolynomial_reset_user = ISLFunction.create(
    "isl_pw_qpolynomial_reset_user",
    Take("PwQpolynomial"),
    return_=Give("PwQpolynomial"),
    lib=_lib,
)

_isl_pw_qpolynomial_from_qpolynomial = ISLFunction.create(
    "isl_pw_qpolynomial_from_qpolynomial",
    Take("Qpolynomial"),
    return_=Give("PwQpolynomial"),
    lib=_lib,
)

_isl_pw_qpolynomial_isa_qpolynomial = ISLFunction.create(
    "isl_pw_qpolynomial_isa_qpolynomial",
    Keep("PwQpolynomial"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_qpolynomial_as_qpolynomial = ISLFunction.create(
    "isl_pw_qpolynomial_as_qpolynomial",
    Take("PwQpolynomial"),
    return_=Give("Qpolynomial"),
    lib=_lib,
)

_isl_pw_qpolynomial_alloc = ISLFunction.create(
    "isl_pw_qpolynomial_alloc",
    Take("Set"),
    Take("Qpolynomial"),
    return_=Give("PwQpolynomial"),
    lib=_lib,
)

_isl_pw_qpolynomial_zero = ISLFunction.create(
    "isl_pw_qpolynomial_zero",
    Take("Space"),
    return_=Give("PwQpolynomial"),
    lib=_lib,
)

_isl_pw_qpolynomial_from_pw_aff = ISLFunction.create(
    "isl_pw_qpolynomial_from_pw_aff",
    Take("PwAff"),
    return_=Give("PwQpolynomial"),
    lib=_lib,
)

_isl_pw_qpolynomial_copy = ISLFunction.create(
    "isl_pw_qpolynomial_copy",
    Keep("PwQpolynomial"),
    return_=Give("PwQpolynomial"),
    lib=_lib,
)

_isl_pw_qpolynomial_free = ISLFunction.create(
    "isl_pw_qpolynomial_free",
    Take("PwQpolynomial"),
    return_=Give("PwQpolynomial"),
    lib=_lib,
)

_isl_pw_qpolynomial_n_piece = ISLFunction.create(
    "isl_pw_qpolynomial_n_piece",
    Keep("PwQpolynomial"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_pw_qpolynomial_foreach_piece = ISLFunction.create(
    "isl_pw_qpolynomial_foreach_piece",
    Keep("PwQpolynomial"),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_pw_qpolynomial_every_piece = ISLFunction.create(
    "isl_pw_qpolynomial_every_piece",
    Keep("PwQpolynomial"),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_qpolynomial_foreach_lifted_piece = ISLFunction.create(
    "isl_pw_qpolynomial_foreach_lifted_piece",
    Keep("PwQpolynomial"),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_pw_qpolynomial_to_union_pw_qpolynomial = ISLFunction.create(
    "isl_pw_qpolynomial_to_union_pw_qpolynomial",
    Take("PwQpolynomial"),
    return_=Give("UnionPwQpolynomial"),
    lib=_lib,
)

_isl_pw_qpolynomial_to_str = ISLFunction.create(
    "isl_pw_qpolynomial_to_str",
    Keep("PwQpolynomial"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_pw_qpolynomial_involves_param_id = ISLFunction.create(
    "isl_pw_qpolynomial_involves_param_id",
    Keep("PwQpolynomial"),
    Keep("Id"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_qpolynomial_involves_nan = ISLFunction.create(
    "isl_pw_qpolynomial_involves_nan",
    Keep("PwQpolynomial"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_qpolynomial_domain_reverse = ISLFunction.create(
    "isl_pw_qpolynomial_domain_reverse",
    Take("PwQpolynomial"),
    return_=Give("PwQpolynomial"),
    lib=_lib,
)

_isl_pw_qpolynomial_project_domain_on_params = ISLFunction.create(
    "isl_pw_qpolynomial_project_domain_on_params",
    Take("PwQpolynomial"),
    return_=Give("PwQpolynomial"),
    lib=_lib,
)

_isl_pw_qpolynomial_domain = ISLFunction.create(
    "isl_pw_qpolynomial_domain",
    Take("PwQpolynomial"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_pw_qpolynomial_from_range = ISLFunction.create(
    "isl_pw_qpolynomial_from_range",
    Take("PwQpolynomial"),
    return_=Give("PwQpolynomial"),
    lib=_lib,
)

_isl_pw_qpolynomial_fix_val = ISLFunction.create(
    "isl_pw_qpolynomial_fix_val",
    Take("PwQpolynomial"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Take("Val"),
    return_=Give("PwQpolynomial"),
    lib=_lib,
)

_isl_pw_qpolynomial_to_polynomial = ISLFunction.create(
    "isl_pw_qpolynomial_to_polynomial",
    Take("PwQpolynomial"),
    Param(int, ctype=c_int),
    return_=Give("PwQpolynomial"),
    lib=_lib,
)

_isl_pw_qpolynomial_drop_unused_params = ISLFunction.create(
    "isl_pw_qpolynomial_drop_unused_params",
    Take("PwQpolynomial"),
    return_=Give("PwQpolynomial"),
    lib=_lib,
)

_isl_pw_qpolynomial_neg = ISLFunction.create(
    "isl_pw_qpolynomial_neg",
    Take("PwQpolynomial"),
    return_=Give("PwQpolynomial"),
    lib=_lib,
)

_isl_pw_qpolynomial_pow = ISLFunction.create(
    "isl_pw_qpolynomial_pow",
    Take("PwQpolynomial"),
    Param(int, ctype=c_uint),
    return_=Give("PwQpolynomial"),
    lib=_lib,
)

_isl_pw_qpolynomial_eval = ISLFunction.create(
    "isl_pw_qpolynomial_eval",
    Take("PwQpolynomial"),
    Take("Point"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_pw_qpolynomial_intersect_domain = ISLFunction.create(
    "isl_pw_qpolynomial_intersect_domain",
    Take("PwQpolynomial"),
    Take("Set"),
    return_=Give("PwQpolynomial"),
    lib=_lib,
)

_isl_pw_qpolynomial_intersect_domain_wrapped_domain = ISLFunction.create(
    "isl_pw_qpolynomial_intersect_domain_wrapped_domain",
    Take("PwQpolynomial"),
    Take("Set"),
    return_=Give("PwQpolynomial"),
    lib=_lib,
)

_isl_pw_qpolynomial_intersect_domain_wrapped_range = ISLFunction.create(
    "isl_pw_qpolynomial_intersect_domain_wrapped_range",
    Take("PwQpolynomial"),
    Take("Set"),
    return_=Give("PwQpolynomial"),
    lib=_lib,
)

_isl_pw_qpolynomial_intersect_params = ISLFunction.create(
    "isl_pw_qpolynomial_intersect_params",
    Take("PwQpolynomial"),
    Take("Set"),
    return_=Give("PwQpolynomial"),
    lib=_lib,
)

_isl_pw_qpolynomial_subtract_domain = ISLFunction.create(
    "isl_pw_qpolynomial_subtract_domain",
    Take("PwQpolynomial"),
    Take("Set"),
    return_=Give("PwQpolynomial"),
    lib=_lib,
)

_isl_pw_qpolynomial_gist_params = ISLFunction.create(
    "isl_pw_qpolynomial_gist_params",
    Take("PwQpolynomial"),
    Take("Set"),
    return_=Give("PwQpolynomial"),
    lib=_lib,
)

_isl_pw_qpolynomial_gist = ISLFunction.create(
    "isl_pw_qpolynomial_gist",
    Take("PwQpolynomial"),
    Take("Set"),
    return_=Give("PwQpolynomial"),
    lib=_lib,
)

_isl_pw_qpolynomial_add = ISLFunction.create(
    "isl_pw_qpolynomial_add",
    Take("PwQpolynomial"),
    Take("PwQpolynomial"),
    return_=Give("PwQpolynomial"),
    lib=_lib,
)

_isl_pw_qpolynomial_add_disjoint = ISLFunction.create(
    "isl_pw_qpolynomial_add_disjoint",
    Take("PwQpolynomial"),
    Take("PwQpolynomial"),
    return_=Give("PwQpolynomial"),
    lib=_lib,
)

_isl_pw_qpolynomial_sub = ISLFunction.create(
    "isl_pw_qpolynomial_sub",
    Take("PwQpolynomial"),
    Take("PwQpolynomial"),
    return_=Give("PwQpolynomial"),
    lib=_lib,
)

_isl_pw_qpolynomial_scale_val = ISLFunction.create(
    "isl_pw_qpolynomial_scale_val",
    Take("PwQpolynomial"),
    Take("Val"),
    return_=Give("PwQpolynomial"),
    lib=_lib,
)

_isl_pw_qpolynomial_scale_down_val = ISLFunction.create(
    "isl_pw_qpolynomial_scale_down_val",
    Take("PwQpolynomial"),
    Take("Val"),
    return_=Give("PwQpolynomial"),
    lib=_lib,
)

_isl_pw_qpolynomial_mul = ISLFunction.create(
    "isl_pw_qpolynomial_mul",
    Take("PwQpolynomial"),
    Take("PwQpolynomial"),
    return_=Give("PwQpolynomial"),
    lib=_lib,
)

_isl_pw_qpolynomial_read_from_str = ISLFunction.create(
    "isl_pw_qpolynomial_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("PwQpolynomial"),
    lib=_lib,
)
