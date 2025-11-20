from __future__ import annotations

from ctypes import (
    c_char_p,
    c_int,
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

class QpolynomialFold(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_qpolynomial_fold_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_qpolynomial_fold_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_qpolynomial_fold_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_qpolynomial_fold_free(handle)

    def get_domain_space(self) -> "Space":
        return _isl_qpolynomial_fold_get_domain_space(self)

    def get_space(self) -> "Space":
        return _isl_qpolynomial_fold_get_space(self)

    def get_type(self) -> int:
        return _isl_qpolynomial_fold_get_type(self)

    def foreach_qpolynomial(self, fn: Any, user: Any = None) -> int:
        return _isl_qpolynomial_fold_foreach_qpolynomial(self, fn, user)

    def is_nan(self) -> bool:
        return _isl_qpolynomial_fold_is_nan(self)

    def gist_params(self, context: "Set") -> "QpolynomialFold":
        return _isl_qpolynomial_fold_gist_params(self, context)

    def gist(self, context: "Set") -> "QpolynomialFold":
        return _isl_qpolynomial_fold_gist(self, context)

    def scale_val(self, v: "Val") -> "QpolynomialFold":
        return _isl_qpolynomial_fold_scale_val(self, v)

    def scale_down_val(self, v: "Val") -> "QpolynomialFold":
        return _isl_qpolynomial_fold_scale_down_val(self, v)


register_type("QpolynomialFold", QpolynomialFold)

_isl_qpolynomial_fold_get_domain_space = ISLFunction.create(
    "isl_qpolynomial_fold_get_domain_space",
    Keep("QpolynomialFold"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_qpolynomial_fold_get_space = ISLFunction.create(
    "isl_qpolynomial_fold_get_space",
    Keep("QpolynomialFold"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_qpolynomial_fold_copy = ISLFunction.create(
    "isl_qpolynomial_fold_copy",
    Keep("QpolynomialFold"),
    return_=Give("QpolynomialFold"),
    lib=_lib,
)

_isl_qpolynomial_fold_free = ISLFunction.create(
    "isl_qpolynomial_fold_free",
    Take("QpolynomialFold"),
    return_=Give("QpolynomialFold"),
    lib=_lib,
)

_isl_qpolynomial_fold_get_type = ISLFunction.create(
    "isl_qpolynomial_fold_get_type",
    Keep("QpolynomialFold"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_qpolynomial_fold_foreach_qpolynomial = ISLFunction.create(
    "isl_qpolynomial_fold_foreach_qpolynomial",
    Keep("QpolynomialFold"),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_qpolynomial_fold_is_nan = ISLFunction.create(
    "isl_qpolynomial_fold_is_nan",
    Keep("QpolynomialFold"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_qpolynomial_fold_gist_params = ISLFunction.create(
    "isl_qpolynomial_fold_gist_params",
    Take("QpolynomialFold"),
    Take("Set"),
    return_=Give("QpolynomialFold"),
    lib=_lib,
)

_isl_qpolynomial_fold_gist = ISLFunction.create(
    "isl_qpolynomial_fold_gist",
    Take("QpolynomialFold"),
    Take("Set"),
    return_=Give("QpolynomialFold"),
    lib=_lib,
)

_isl_qpolynomial_fold_scale_val = ISLFunction.create(
    "isl_qpolynomial_fold_scale_val",
    Take("QpolynomialFold"),
    Take("Val"),
    return_=Give("QpolynomialFold"),
    lib=_lib,
)

_isl_qpolynomial_fold_scale_down_val = ISLFunction.create(
    "isl_qpolynomial_fold_scale_down_val",
    Take("QpolynomialFold"),
    Take("Val"),
    return_=Give("QpolynomialFold"),
    lib=_lib,
)

_isl_qpolynomial_fold_read_from_str = ISLFunction.create(
    "isl_qpolynomial_fold_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("QpolynomialFold"),
    lib=_lib,
)
