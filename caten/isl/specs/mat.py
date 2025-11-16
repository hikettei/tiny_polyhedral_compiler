from __future__ import annotations

from ctypes import c_int, c_void_p

from ..ffi import FfiPointer, load_libisl
from ..func import ISLFunction
from ..obj import ISLObject
from ..qualifier import Give, Keep, Null, Param, Take
from .context import Context
from .val import Val

_lib = load_libisl()


class Mat(ISLObject):
    """Minimal wrapper around ``isl_mat``."""

    __slots__ = ()

    def __init__(self, handle: FfiPointer) -> None:
        super().__init__(handle)

    @classmethod
    def alloc(cls, n_row: int, n_col: int) -> "Mat":
        return _isl_mat_alloc(n_row, n_col)

    def copy_handle(self) -> FfiPointer:
        return _isl_mat_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_mat_free(handle)

    def rows(self) -> int:
        return _isl_mat_rows(self)

    def cols(self) -> int:
        return _isl_mat_cols(self)

    def get_element_si(self, row: int, col: int) -> int:
        val = _isl_mat_get_element_val(self, row, col)
        return val.num_si()

    def set_element_si(self, row: int, col: int, value: int) -> "Mat":
        return _isl_mat_set_element_si(self, row, col, value)

    def __repr__(self) -> str:  # pragma: no cover
        try:
            rows = self.rows()
            cols = self.cols()
            lines = []
            for i in range(rows):
                elems = [str(self.get_element_si(i, j)) for j in range(cols)]
                lines.append("[" + " ".join(elems) + "]")
            body = "\n    ".join(lines)
            return f"Mat(rows={rows}, cols={cols},\n    {body})"
        except Exception:
            return f"Mat(rows={self.rows()}, cols={self.cols()})"


_isl_mat_alloc = ISLFunction.create(
    _lib.isl_mat_alloc,
    Context(),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Mat),
    lib=_lib,
)

_isl_mat_copy = ISLFunction.create(
    _lib.isl_mat_copy,
    Keep(Mat),
    return_=Give(Mat),
    lib=_lib,
)

_isl_mat_free = ISLFunction.create(
    _lib.isl_mat_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

_isl_mat_rows = ISLFunction.create(
    _lib.isl_mat_rows,
    Keep(Mat),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_mat_cols = ISLFunction.create(
    _lib.isl_mat_cols,
    Keep(Mat),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_mat_set_element_si = ISLFunction.create(
    _lib.isl_mat_set_element_si,
    Take(Mat),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Mat),
    lib=_lib,
)

# Fetch element as isl_val then convert to int; isl_mat_get_element_val is available broadly.
_isl_mat_get_element_val = ISLFunction.create(
    _lib.isl_mat_get_element_val,
    Keep(Mat),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Val),
    lib=_lib,
)

__all__ = ["Mat"]
