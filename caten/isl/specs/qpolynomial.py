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
    from .aff import Aff
    from .context import Context
    from .set import Set
    from .space import Space
    from .val import Val

_lib = load_libisl()

class Qpolynomial(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_qpolynomial_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_qpolynomial_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_qpolynomial_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_qpolynomial_free(handle)

    @classmethod
    def from_aff(cls, aff: "Aff") -> "Qpolynomial":
        return _isl_qpolynomial_from_aff(aff)

    def get_domain_space(self) -> "Space":
        return _isl_qpolynomial_get_domain_space(self)

    def get_space(self) -> "Space":
        return _isl_qpolynomial_get_space(self)

    def set_dim_name(self, type: int, pos: int, s: str) -> "Qpolynomial":
        return _isl_qpolynomial_set_dim_name(self, type, pos, s)

    @classmethod
    def zero_on_domain(cls, domain: "Space") -> "Qpolynomial":
        return _isl_qpolynomial_zero_on_domain(domain)

    @classmethod
    def one_on_domain(cls, domain: "Space") -> "Qpolynomial":
        return _isl_qpolynomial_one_on_domain(domain)

    @classmethod
    def infty_on_domain(cls, domain: "Space") -> "Qpolynomial":
        return _isl_qpolynomial_infty_on_domain(domain)

    @classmethod
    def neginfty_on_domain(cls, domain: "Space") -> "Qpolynomial":
        return _isl_qpolynomial_neginfty_on_domain(domain)

    @classmethod
    def nan_on_domain(cls, domain: "Space") -> "Qpolynomial":
        return _isl_qpolynomial_nan_on_domain(domain)

    @classmethod
    def val_on_domain(cls, domain: "Space", val: "Val") -> "Qpolynomial":
        return _isl_qpolynomial_val_on_domain(domain, val)

    @classmethod
    def var_on_domain(cls, domain: "Space", type: int, pos: int) -> "Qpolynomial":
        return _isl_qpolynomial_var_on_domain(domain, type, pos)

    def isa_aff(self) -> bool:
        return _isl_qpolynomial_isa_aff(self)

    def as_aff(self) -> "Aff":
        return _isl_qpolynomial_as_aff(self)

    def get_constant_val(self) -> "Val":
        return _isl_qpolynomial_get_constant_val(self)

    def foreach_term(self, fn: Any, user: Any, user_: Any = None) -> int:
        return _isl_qpolynomial_foreach_term(self, fn, user, user_)

    def involves_dims(self, type: int, first: int, n: int) -> bool:
        return _isl_qpolynomial_involves_dims(self, type, first, n)

    def is_nan(self) -> bool:
        return _isl_qpolynomial_is_nan(self)

    def domain_reverse(self) -> "Qpolynomial":
        return _isl_qpolynomial_domain_reverse(self)

    def project_domain_on_params(self) -> "Qpolynomial":
        return _isl_qpolynomial_project_domain_on_params(self)

    def align_params(self, model: "Space") -> "Qpolynomial":
        return _isl_qpolynomial_align_params(self, model)

    def neg(self) -> "Qpolynomial":
        return _isl_qpolynomial_neg(self)

    def pow(self, exponent: int) -> "Qpolynomial":
        return _isl_qpolynomial_pow(self, exponent)

    def gist_params(self, context: "Set") -> "Qpolynomial":
        return _isl_qpolynomial_gist_params(self, context)

    def gist(self, context: "Set") -> "Qpolynomial":
        return _isl_qpolynomial_gist(self, context)

    def add(self, qp2: "Qpolynomial") -> "Qpolynomial":
        return _isl_qpolynomial_add(self, qp2)

    def sub(self, qp2: "Qpolynomial") -> "Qpolynomial":
        return _isl_qpolynomial_sub(self, qp2)

    def scale_val(self, v: "Val") -> "Qpolynomial":
        return _isl_qpolynomial_scale_val(self, v)

    def scale_down_val(self, v: "Val") -> "Qpolynomial":
        return _isl_qpolynomial_scale_down_val(self, v)

    def mul(self, qp2: "Qpolynomial") -> "Qpolynomial":
        return _isl_qpolynomial_mul(self, qp2)


register_type("Qpolynomial", Qpolynomial)

_isl_qpolynomial_from_aff = ISLFunction.create(
    "isl_qpolynomial_from_aff",
    Take("Aff"),
    return_=Give("Qpolynomial"),
    lib=_lib,
)

_isl_qpolynomial_get_domain_space = ISLFunction.create(
    "isl_qpolynomial_get_domain_space",
    Keep("Qpolynomial"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_qpolynomial_get_space = ISLFunction.create(
    "isl_qpolynomial_get_space",
    Keep("Qpolynomial"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_qpolynomial_set_dim_name = ISLFunction.create(
    "isl_qpolynomial_set_dim_name",
    Take("Qpolynomial"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(str, ctype=c_char_p),
    return_=Give("Qpolynomial"),
    lib=_lib,
)

_isl_qpolynomial_zero_on_domain = ISLFunction.create(
    "isl_qpolynomial_zero_on_domain",
    Take("Space"),
    return_=Give("Qpolynomial"),
    lib=_lib,
)

_isl_qpolynomial_one_on_domain = ISLFunction.create(
    "isl_qpolynomial_one_on_domain",
    Take("Space"),
    return_=Give("Qpolynomial"),
    lib=_lib,
)

_isl_qpolynomial_infty_on_domain = ISLFunction.create(
    "isl_qpolynomial_infty_on_domain",
    Take("Space"),
    return_=Give("Qpolynomial"),
    lib=_lib,
)

_isl_qpolynomial_neginfty_on_domain = ISLFunction.create(
    "isl_qpolynomial_neginfty_on_domain",
    Take("Space"),
    return_=Give("Qpolynomial"),
    lib=_lib,
)

_isl_qpolynomial_nan_on_domain = ISLFunction.create(
    "isl_qpolynomial_nan_on_domain",
    Take("Space"),
    return_=Give("Qpolynomial"),
    lib=_lib,
)

_isl_qpolynomial_val_on_domain = ISLFunction.create(
    "isl_qpolynomial_val_on_domain",
    Take("Space"),
    Take("Val"),
    return_=Give("Qpolynomial"),
    lib=_lib,
)

_isl_qpolynomial_var_on_domain = ISLFunction.create(
    "isl_qpolynomial_var_on_domain",
    Take("Space"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Give("Qpolynomial"),
    lib=_lib,
)

_isl_qpolynomial_isa_aff = ISLFunction.create(
    "isl_qpolynomial_isa_aff",
    Keep("Qpolynomial"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_qpolynomial_as_aff = ISLFunction.create(
    "isl_qpolynomial_as_aff",
    Take("Qpolynomial"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_qpolynomial_copy = ISLFunction.create(
    "isl_qpolynomial_copy",
    Keep("Qpolynomial"),
    return_=Give("Qpolynomial"),
    lib=_lib,
)

_isl_qpolynomial_free = ISLFunction.create(
    "isl_qpolynomial_free",
    Take("Qpolynomial"),
    return_=Give("Qpolynomial"),
    lib=_lib,
)

_isl_qpolynomial_get_constant_val = ISLFunction.create(
    "isl_qpolynomial_get_constant_val",
    Keep("Qpolynomial"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_qpolynomial_foreach_term = ISLFunction.create(
    "isl_qpolynomial_foreach_term",
    Keep("Qpolynomial"),
    Param(None, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_qpolynomial_involves_dims = ISLFunction.create(
    "isl_qpolynomial_involves_dims",
    Keep("Qpolynomial"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_qpolynomial_is_nan = ISLFunction.create(
    "isl_qpolynomial_is_nan",
    Keep("Qpolynomial"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_qpolynomial_domain_reverse = ISLFunction.create(
    "isl_qpolynomial_domain_reverse",
    Take("Qpolynomial"),
    return_=Give("Qpolynomial"),
    lib=_lib,
)

_isl_qpolynomial_project_domain_on_params = ISLFunction.create(
    "isl_qpolynomial_project_domain_on_params",
    Take("Qpolynomial"),
    return_=Give("Qpolynomial"),
    lib=_lib,
)

_isl_qpolynomial_align_params = ISLFunction.create(
    "isl_qpolynomial_align_params",
    Take("Qpolynomial"),
    Take("Space"),
    return_=Give("Qpolynomial"),
    lib=_lib,
)

_isl_qpolynomial_neg = ISLFunction.create(
    "isl_qpolynomial_neg",
    Take("Qpolynomial"),
    return_=Give("Qpolynomial"),
    lib=_lib,
)

_isl_qpolynomial_pow = ISLFunction.create(
    "isl_qpolynomial_pow",
    Take("Qpolynomial"),
    Param(int, ctype=c_uint),
    return_=Give("Qpolynomial"),
    lib=_lib,
)

_isl_qpolynomial_gist_params = ISLFunction.create(
    "isl_qpolynomial_gist_params",
    Take("Qpolynomial"),
    Take("Set"),
    return_=Give("Qpolynomial"),
    lib=_lib,
)

_isl_qpolynomial_gist = ISLFunction.create(
    "isl_qpolynomial_gist",
    Take("Qpolynomial"),
    Take("Set"),
    return_=Give("Qpolynomial"),
    lib=_lib,
)

_isl_qpolynomial_add = ISLFunction.create(
    "isl_qpolynomial_add",
    Take("Qpolynomial"),
    Take("Qpolynomial"),
    return_=Give("Qpolynomial"),
    lib=_lib,
)

_isl_qpolynomial_sub = ISLFunction.create(
    "isl_qpolynomial_sub",
    Take("Qpolynomial"),
    Take("Qpolynomial"),
    return_=Give("Qpolynomial"),
    lib=_lib,
)

_isl_qpolynomial_scale_val = ISLFunction.create(
    "isl_qpolynomial_scale_val",
    Take("Qpolynomial"),
    Take("Val"),
    return_=Give("Qpolynomial"),
    lib=_lib,
)

_isl_qpolynomial_scale_down_val = ISLFunction.create(
    "isl_qpolynomial_scale_down_val",
    Take("Qpolynomial"),
    Take("Val"),
    return_=Give("Qpolynomial"),
    lib=_lib,
)

_isl_qpolynomial_mul = ISLFunction.create(
    "isl_qpolynomial_mul",
    Take("Qpolynomial"),
    Take("Qpolynomial"),
    return_=Give("Qpolynomial"),
    lib=_lib,
)

_isl_qpolynomial_read_from_str = ISLFunction.create(
    "isl_qpolynomial_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("Qpolynomial"),
    lib=_lib,
)
