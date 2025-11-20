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
    from .qpolynomial_fold import QpolynomialFold
    from .set import Set
    from .space import Space
    from .union_pw_qpolynomial_fold import UnionPwQpolynomialFold
    from .val import Val

_lib = load_libisl()

class PwQpolynomialFold(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_pw_qpolynomial_fold_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_pw_qpolynomial_fold_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_pw_qpolynomial_fold_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_pw_qpolynomial_fold_free(handle)

    def get_domain_space(self) -> "Space":
        return _isl_pw_qpolynomial_fold_get_domain_space(self)

    def get_space(self) -> "Space":
        return _isl_pw_qpolynomial_fold_get_space(self)

    def set_dim_name(self, type: int, pos: int, s: str) -> "PwQpolynomialFold":
        return _isl_pw_qpolynomial_fold_set_dim_name(self, type, pos, s)

    def find_dim_by_name(self, type: int, name: str) -> int:
        return _isl_pw_qpolynomial_fold_find_dim_by_name(self, type, name)

    def reset_user(self) -> "PwQpolynomialFold":
        return _isl_pw_qpolynomial_fold_reset_user(self)

    def get_type(self) -> int:
        return _isl_pw_qpolynomial_fold_get_type(self)

    @classmethod
    def from_qpolynomial_fold(cls, fold: "QpolynomialFold") -> "PwQpolynomialFold":
        return _isl_pw_qpolynomial_fold_from_qpolynomial_fold(fold)

    def isa_qpolynomial_fold(self) -> bool:
        return _isl_pw_qpolynomial_fold_isa_qpolynomial_fold(self)

    def as_qpolynomial_fold(self) -> "QpolynomialFold":
        return _isl_pw_qpolynomial_fold_as_qpolynomial_fold(self)

    def n_piece(self) -> int:
        return _isl_pw_qpolynomial_fold_n_piece(self)

    def foreach_piece(self, fn: Any, fold: "QpolynomialFold", user: Any, user_: Any = None) -> int:
        return _isl_pw_qpolynomial_fold_foreach_piece(self, fn, fold, user, user_)

    def every_piece(self, test: Any, fold: "QpolynomialFold", user: Any, user_: Any = None) -> bool:
        return _isl_pw_qpolynomial_fold_every_piece(self, test, fold, user, user_)

    def foreach_lifted_piece(self, fn: Any, fold: "QpolynomialFold", user: Any, user_: Any = None) -> int:
        return _isl_pw_qpolynomial_fold_foreach_lifted_piece(self, fn, fold, user, user_)

    def to_union_pw_qpolynomial_fold(self) -> "UnionPwQpolynomialFold":
        return _isl_pw_qpolynomial_fold_to_union_pw_qpolynomial_fold(self)

    def involves_param_id(self, id: "Id") -> bool:
        return _isl_pw_qpolynomial_fold_involves_param_id(self, id)

    def involves_nan(self) -> bool:
        return _isl_pw_qpolynomial_fold_involves_nan(self)

    def project_domain_on_params(self) -> "PwQpolynomialFold":
        return _isl_pw_qpolynomial_fold_project_domain_on_params(self)

    def from_range(self) -> "PwQpolynomialFold":
        return _isl_pw_qpolynomial_fold_from_range(self)

    def fix_val(self, type: int, n: int, v: "Val") -> "PwQpolynomialFold":
        return _isl_pw_qpolynomial_fold_fix_val(self, type, n, v)

    def coalesce(self) -> "PwQpolynomialFold":
        return _isl_pw_qpolynomial_fold_coalesce(self)

    def drop_unused_params(self) -> "PwQpolynomialFold":
        return _isl_pw_qpolynomial_fold_drop_unused_params(self)

    def eval(self, pnt: "Point") -> "Val":
        return _isl_pw_qpolynomial_fold_eval(self, pnt)

    def intersect_domain_wrapped_domain(self, set: "Set") -> "PwQpolynomialFold":
        return _isl_pw_qpolynomial_fold_intersect_domain_wrapped_domain(self, set)

    def intersect_domain_wrapped_range(self, set: "Set") -> "PwQpolynomialFold":
        return _isl_pw_qpolynomial_fold_intersect_domain_wrapped_range(self, set)

    def intersect_params(self, set: "Set") -> "PwQpolynomialFold":
        return _isl_pw_qpolynomial_fold_intersect_params(self, set)

    def subtract_domain(self, set: "Set") -> "PwQpolynomialFold":
        return _isl_pw_qpolynomial_fold_subtract_domain(self, set)

    def gist(self, context: "Set") -> "PwQpolynomialFold":
        return _isl_pw_qpolynomial_fold_gist(self, context)

    def gist_params(self, context: "Set") -> "PwQpolynomialFold":
        return _isl_pw_qpolynomial_fold_gist_params(self, context)

    def add(self, pwf2: "PwQpolynomialFold") -> "PwQpolynomialFold":
        return _isl_pw_qpolynomial_fold_add(self, pwf2)

    def fold(self, pwf2: "PwQpolynomialFold") -> "PwQpolynomialFold":
        return _isl_pw_qpolynomial_fold_fold(self, pwf2)

    def scale_val(self, v: "Val") -> "PwQpolynomialFold":
        return _isl_pw_qpolynomial_fold_scale_val(self, v)

    def scale_down_val(self, v: "Val") -> "PwQpolynomialFold":
        return _isl_pw_qpolynomial_fold_scale_down_val(self, v)


register_type("PwQpolynomialFold", PwQpolynomialFold)

_isl_pw_qpolynomial_fold_get_domain_space = ISLFunction.create(
    "isl_pw_qpolynomial_fold_get_domain_space",
    Keep("PwQpolynomialFold"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_pw_qpolynomial_fold_get_space = ISLFunction.create(
    "isl_pw_qpolynomial_fold_get_space",
    Keep("PwQpolynomialFold"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_pw_qpolynomial_fold_set_dim_name = ISLFunction.create(
    "isl_pw_qpolynomial_fold_set_dim_name",
    Take("PwQpolynomialFold"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(str, ctype=c_char_p),
    return_=Give("PwQpolynomialFold"),
    lib=_lib,
)

_isl_pw_qpolynomial_fold_find_dim_by_name = ISLFunction.create(
    "isl_pw_qpolynomial_fold_find_dim_by_name",
    Keep("PwQpolynomialFold"),
    Param(int, ctype=c_int),
    Param(str, ctype=c_char_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_pw_qpolynomial_fold_reset_user = ISLFunction.create(
    "isl_pw_qpolynomial_fold_reset_user",
    Take("PwQpolynomialFold"),
    return_=Give("PwQpolynomialFold"),
    lib=_lib,
)

_isl_pw_qpolynomial_fold_get_type = ISLFunction.create(
    "isl_pw_qpolynomial_fold_get_type",
    Keep("PwQpolynomialFold"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_pw_qpolynomial_fold_from_qpolynomial_fold = ISLFunction.create(
    "isl_pw_qpolynomial_fold_from_qpolynomial_fold",
    Take("QpolynomialFold"),
    return_=Give("PwQpolynomialFold"),
    lib=_lib,
)

_isl_pw_qpolynomial_fold_isa_qpolynomial_fold = ISLFunction.create(
    "isl_pw_qpolynomial_fold_isa_qpolynomial_fold",
    Keep("PwQpolynomialFold"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_qpolynomial_fold_as_qpolynomial_fold = ISLFunction.create(
    "isl_pw_qpolynomial_fold_as_qpolynomial_fold",
    Take("PwQpolynomialFold"),
    return_=Give("QpolynomialFold"),
    lib=_lib,
)

_isl_pw_qpolynomial_fold_copy = ISLFunction.create(
    "isl_pw_qpolynomial_fold_copy",
    Keep("PwQpolynomialFold"),
    return_=Give("PwQpolynomialFold"),
    lib=_lib,
)

_isl_pw_qpolynomial_fold_free = ISLFunction.create(
    "isl_pw_qpolynomial_fold_free",
    Take("PwQpolynomialFold"),
    return_=Give("PwQpolynomialFold"),
    lib=_lib,
)

_isl_pw_qpolynomial_fold_n_piece = ISLFunction.create(
    "isl_pw_qpolynomial_fold_n_piece",
    Keep("PwQpolynomialFold"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_pw_qpolynomial_fold_foreach_piece = ISLFunction.create(
    "isl_pw_qpolynomial_fold_foreach_piece",
    Keep("PwQpolynomialFold"),
    Param(None, ctype=c_void_p),
    Take("QpolynomialFold"),
    Param(Any, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_pw_qpolynomial_fold_every_piece = ISLFunction.create(
    "isl_pw_qpolynomial_fold_every_piece",
    Keep("PwQpolynomialFold"),
    Param(None, ctype=c_void_p),
    Keep("QpolynomialFold"),
    Param(Any, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_qpolynomial_fold_foreach_lifted_piece = ISLFunction.create(
    "isl_pw_qpolynomial_fold_foreach_lifted_piece",
    Keep("PwQpolynomialFold"),
    Param(None, ctype=c_void_p),
    Take("QpolynomialFold"),
    Param(Any, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_pw_qpolynomial_fold_to_union_pw_qpolynomial_fold = ISLFunction.create(
    "isl_pw_qpolynomial_fold_to_union_pw_qpolynomial_fold",
    Take("PwQpolynomialFold"),
    return_=Give("UnionPwQpolynomialFold"),
    lib=_lib,
)

_isl_pw_qpolynomial_fold_read_from_str = ISLFunction.create(
    "isl_pw_qpolynomial_fold_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("PwQpolynomialFold"),
    lib=_lib,
)

_isl_pw_qpolynomial_fold_involves_param_id = ISLFunction.create(
    "isl_pw_qpolynomial_fold_involves_param_id",
    Keep("PwQpolynomialFold"),
    Keep("Id"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_qpolynomial_fold_involves_nan = ISLFunction.create(
    "isl_pw_qpolynomial_fold_involves_nan",
    Keep("PwQpolynomialFold"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_qpolynomial_fold_project_domain_on_params = ISLFunction.create(
    "isl_pw_qpolynomial_fold_project_domain_on_params",
    Take("PwQpolynomialFold"),
    return_=Give("PwQpolynomialFold"),
    lib=_lib,
)

_isl_pw_qpolynomial_fold_from_range = ISLFunction.create(
    "isl_pw_qpolynomial_fold_from_range",
    Take("PwQpolynomialFold"),
    return_=Give("PwQpolynomialFold"),
    lib=_lib,
)

_isl_pw_qpolynomial_fold_fix_val = ISLFunction.create(
    "isl_pw_qpolynomial_fold_fix_val",
    Take("PwQpolynomialFold"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Take("Val"),
    return_=Give("PwQpolynomialFold"),
    lib=_lib,
)

_isl_pw_qpolynomial_fold_coalesce = ISLFunction.create(
    "isl_pw_qpolynomial_fold_coalesce",
    Take("PwQpolynomialFold"),
    return_=Give("PwQpolynomialFold"),
    lib=_lib,
)

_isl_pw_qpolynomial_fold_drop_unused_params = ISLFunction.create(
    "isl_pw_qpolynomial_fold_drop_unused_params",
    Take("PwQpolynomialFold"),
    return_=Give("PwQpolynomialFold"),
    lib=_lib,
)

_isl_pw_qpolynomial_fold_eval = ISLFunction.create(
    "isl_pw_qpolynomial_fold_eval",
    Take("PwQpolynomialFold"),
    Take("Point"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_pw_qpolynomial_fold_intersect_domain_wrapped_domain = ISLFunction.create(
    "isl_pw_qpolynomial_fold_intersect_domain_wrapped_domain",
    Take("PwQpolynomialFold"),
    Take("Set"),
    return_=Give("PwQpolynomialFold"),
    lib=_lib,
)

_isl_pw_qpolynomial_fold_intersect_domain_wrapped_range = ISLFunction.create(
    "isl_pw_qpolynomial_fold_intersect_domain_wrapped_range",
    Take("PwQpolynomialFold"),
    Take("Set"),
    return_=Give("PwQpolynomialFold"),
    lib=_lib,
)

_isl_pw_qpolynomial_fold_intersect_params = ISLFunction.create(
    "isl_pw_qpolynomial_fold_intersect_params",
    Take("PwQpolynomialFold"),
    Take("Set"),
    return_=Give("PwQpolynomialFold"),
    lib=_lib,
)

_isl_pw_qpolynomial_fold_subtract_domain = ISLFunction.create(
    "isl_pw_qpolynomial_fold_subtract_domain",
    Take("PwQpolynomialFold"),
    Take("Set"),
    return_=Give("PwQpolynomialFold"),
    lib=_lib,
)

_isl_pw_qpolynomial_fold_gist = ISLFunction.create(
    "isl_pw_qpolynomial_fold_gist",
    Take("PwQpolynomialFold"),
    Take("Set"),
    return_=Give("PwQpolynomialFold"),
    lib=_lib,
)

_isl_pw_qpolynomial_fold_gist_params = ISLFunction.create(
    "isl_pw_qpolynomial_fold_gist_params",
    Take("PwQpolynomialFold"),
    Take("Set"),
    return_=Give("PwQpolynomialFold"),
    lib=_lib,
)

_isl_pw_qpolynomial_fold_add = ISLFunction.create(
    "isl_pw_qpolynomial_fold_add",
    Take("PwQpolynomialFold"),
    Take("PwQpolynomialFold"),
    return_=Give("PwQpolynomialFold"),
    lib=_lib,
)

_isl_pw_qpolynomial_fold_fold = ISLFunction.create(
    "isl_pw_qpolynomial_fold_fold",
    Take("PwQpolynomialFold"),
    Take("PwQpolynomialFold"),
    return_=Give("PwQpolynomialFold"),
    lib=_lib,
)

_isl_pw_qpolynomial_fold_scale_val = ISLFunction.create(
    "isl_pw_qpolynomial_fold_scale_val",
    Take("PwQpolynomialFold"),
    Take("Val"),
    return_=Give("PwQpolynomialFold"),
    lib=_lib,
)

_isl_pw_qpolynomial_fold_scale_down_val = ISLFunction.create(
    "isl_pw_qpolynomial_fold_scale_down_val",
    Take("PwQpolynomialFold"),
    Take("Val"),
    return_=Give("PwQpolynomialFold"),
    lib=_lib,
)
