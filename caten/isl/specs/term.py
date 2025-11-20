from __future__ import annotations

from ctypes import c_char_p, c_int, c_uint
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
    from .val import Val

_lib = load_libisl()

class Term(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_term_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_term_read_from_str(spec)


    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_term_free(handle)

    def dim(self, type: int) -> int:
        return _isl_term_dim(self, type)

    def get_coefficient_val(self) -> "Val":
        return _isl_term_get_coefficient_val(self)

    def get_exp(self, type: int, pos: int) -> int:
        return _isl_term_get_exp(self, type, pos)

    def get_div(self, pos: int) -> "Aff":
        return _isl_term_get_div(self, pos)


register_type("Term", Term)

_isl_term_dim = ISLFunction.create(
    "isl_term_dim",
    Keep("Term"),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_term_get_coefficient_val = ISLFunction.create(
    "isl_term_get_coefficient_val",
    Keep("Term"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_term_get_exp = ISLFunction.create(
    "isl_term_get_exp",
    Keep("Term"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_term_get_div = ISLFunction.create(
    "isl_term_get_div",
    Keep("Term"),
    Param(int, ctype=c_uint),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_term_free = ISLFunction.create(
    "isl_term_free",
    Take("Term"),
    return_=Give("Term"),
    lib=_lib,
)

_isl_term_read_from_str = ISLFunction.create(
    "isl_term_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("Term"),
    lib=_lib,
)
