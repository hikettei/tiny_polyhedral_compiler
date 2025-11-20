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
    from .context import Context
    from .val import Val

_lib = load_libisl()

class Mat(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_mat_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_mat_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_mat_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_mat_free(handle)

    def get_ctx(self) -> "Context":
        return _isl_mat_get_ctx(self)

    @classmethod
    def alloc(cls, n_row: int, n_col: int) -> "Mat":
        return _isl_mat_alloc(n_row, n_col)

    def rows(self) -> int:
        return _isl_mat_rows(self)

    def cols(self) -> int:
        return _isl_mat_cols(self)

    def get_element_val(self, row: int, col: int) -> "Val":
        return _isl_mat_get_element_val(self, row, col)

    def set_element_si(self, row: int, col: int, v: int) -> "Mat":
        return _isl_mat_set_element_si(self, row, col, v)

    def set_element_val(self, row: int, col: int, v: "Val") -> "Mat":
        return _isl_mat_set_element_val(self, row, col, v)

    def rank(self) -> int:
        return _isl_mat_rank(self)

    def right_inverse(self) -> "Mat":
        return _isl_mat_right_inverse(self)

    def right_kernel(self) -> "Mat":
        return _isl_mat_right_kernel(self)

    def row_basis(self) -> "Mat":
        return _isl_mat_row_basis(self)

    def row_basis_extension(self, mat2: "Mat") -> "Mat":
        return _isl_mat_row_basis_extension(self, mat2)

    def has_linearly_independent_rows(self, mat2: "Mat") -> bool:
        return _isl_mat_has_linearly_independent_rows(self, mat2)


register_type("Mat", Mat)

_isl_mat_get_ctx = ISLFunction.create(
    "isl_mat_get_ctx",
    Keep("Mat"),
    return_=Give("Context"),
    lib=_lib,
)

_isl_mat_alloc = ISLFunction.create(
    "isl_mat_alloc",
    Context(),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("Mat"),
    lib=_lib,
)

_isl_mat_copy = ISLFunction.create(
    "isl_mat_copy",
    Keep("Mat"),
    return_=Give("Mat"),
    lib=_lib,
)

_isl_mat_free = ISLFunction.create(
    "isl_mat_free",
    Take("Mat"),
    return_=Give("Mat"),
    lib=_lib,
)

_isl_mat_rows = ISLFunction.create(
    "isl_mat_rows",
    Keep("Mat"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_mat_cols = ISLFunction.create(
    "isl_mat_cols",
    Keep("Mat"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_mat_get_element_val = ISLFunction.create(
    "isl_mat_get_element_val",
    Keep("Mat"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give("Val"),
    lib=_lib,
)

_isl_mat_set_element_si = ISLFunction.create(
    "isl_mat_set_element_si",
    Take("Mat"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give("Mat"),
    lib=_lib,
)

_isl_mat_set_element_val = ISLFunction.create(
    "isl_mat_set_element_val",
    Take("Mat"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Take("Val"),
    return_=Give("Mat"),
    lib=_lib,
)

_isl_mat_rank = ISLFunction.create(
    "isl_mat_rank",
    Keep("Mat"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_mat_right_inverse = ISLFunction.create(
    "isl_mat_right_inverse",
    Take("Mat"),
    return_=Give("Mat"),
    lib=_lib,
)

_isl_mat_right_kernel = ISLFunction.create(
    "isl_mat_right_kernel",
    Take("Mat"),
    return_=Give("Mat"),
    lib=_lib,
)

_isl_mat_row_basis = ISLFunction.create(
    "isl_mat_row_basis",
    Take("Mat"),
    return_=Give("Mat"),
    lib=_lib,
)

_isl_mat_row_basis_extension = ISLFunction.create(
    "isl_mat_row_basis_extension",
    Take("Mat"),
    Take("Mat"),
    return_=Give("Mat"),
    lib=_lib,
)

_isl_mat_has_linearly_independent_rows = ISLFunction.create(
    "isl_mat_has_linearly_independent_rows",
    Keep("Mat"),
    Keep("Mat"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_mat_read_from_str = ISLFunction.create(
    "isl_mat_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("Mat"),
    lib=_lib,
)
