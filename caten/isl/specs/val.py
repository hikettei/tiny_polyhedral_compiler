from __future__ import annotations

from ctypes import (
    c_char_p,
    c_double,
    c_int,
    c_long,
    c_size_t,
    c_ulong,
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

class Val(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_val_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_val_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_val_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_val_free(handle)

    def __str__(self) -> str:
        return _isl_val_to_str(self)

    def __repr__(self) -> str:
        return f"Val({self.__str__()})"

    def get_ctx(self) -> "Ctx":
        return _isl_val_get_ctx(self)

    @classmethod
    def zero(cls) -> "Val":
        return _isl_val_zero()

    @classmethod
    def one(cls) -> "Val":
        return _isl_val_one()

    @classmethod
    def negone(cls) -> "Val":
        return _isl_val_negone()

    @classmethod
    def nan(cls) -> "Val":
        return _isl_val_nan()

    @classmethod
    def infty(cls) -> "Val":
        return _isl_val_infty()

    @classmethod
    def neginfty(cls) -> "Val":
        return _isl_val_neginfty()

    @classmethod
    def int_from_si(cls, i: int) -> "Val":
        return _isl_val_int_from_si(i)

    @classmethod
    def int_from_ui(cls, u: int) -> "Val":
        return _isl_val_int_from_ui(u)

    @classmethod
    def int_from_chunks(cls, n: int, size: int, chunks: Any) -> "Val":
        return _isl_val_int_from_chunks(n, size, chunks)

    def get_num_si(self) -> int:
        return _isl_val_get_num_si(self)

    def get_den_si(self) -> int:
        return _isl_val_get_den_si(self)

    def get_den_val(self) -> "Val":
        return _isl_val_get_den_val(self)

    def get_d(self) -> float:
        return _isl_val_get_d(self)

    def n_abs_num_chunks(self, size: int) -> int:
        return _isl_val_n_abs_num_chunks(self, size)

    def get_abs_num_chunks(self, size: int, chunks: Any) -> int:
        return _isl_val_get_abs_num_chunks(self, size, chunks)

    def set_si(self, i: int) -> "Val":
        return _isl_val_set_si(self, i)

    def sgn(self) -> int:
        return _isl_val_sgn(self)

    def is_zero(self) -> bool:
        return _isl_val_is_zero(self)

    def is_one(self) -> bool:
        return _isl_val_is_one(self)

    def is_negone(self) -> bool:
        return _isl_val_is_negone(self)

    def is_nonneg(self) -> bool:
        return _isl_val_is_nonneg(self)

    def is_nonpos(self) -> bool:
        return _isl_val_is_nonpos(self)

    def is_pos(self) -> bool:
        return _isl_val_is_pos(self)

    def is_neg(self) -> bool:
        return _isl_val_is_neg(self)

    def is_int(self) -> bool:
        return _isl_val_is_int(self)

    def is_rat(self) -> bool:
        return _isl_val_is_rat(self)

    def is_nan(self) -> bool:
        return _isl_val_is_nan(self)

    def is_infty(self) -> bool:
        return _isl_val_is_infty(self)

    def is_neginfty(self) -> bool:
        return _isl_val_is_neginfty(self)

    def lt(self, v2: "Val") -> bool:
        return _isl_val_lt(self, v2)

    def le(self, v2: "Val") -> bool:
        return _isl_val_le(self, v2)

    def gt(self, v2: "Val") -> bool:
        return _isl_val_gt(self, v2)

    def ge(self, v2: "Val") -> bool:
        return _isl_val_ge(self, v2)

    def eq(self, v2: "Val") -> bool:
        return _isl_val_eq(self, v2)

    def ne(self, v2: "Val") -> bool:
        return _isl_val_ne(self, v2)

    def abs_eq(self, v2: "Val") -> bool:
        return _isl_val_abs_eq(self, v2)

    def is_divisible_by(self, v2: "Val") -> bool:
        return _isl_val_is_divisible_by(self, v2)

    def gt_si(self, i: int) -> bool:
        return _isl_val_gt_si(self, i)

    def eq_si(self, i: int) -> bool:
        return _isl_val_eq_si(self, i)

    def cmp_si(self, i: int) -> int:
        return _isl_val_cmp_si(self, i)

    def abs(self) -> "Val":
        return _isl_val_abs(self)

    def neg(self) -> "Val":
        return _isl_val_neg(self)

    def floor(self) -> "Val":
        return _isl_val_floor(self)

    def ceil(self) -> "Val":
        return _isl_val_ceil(self)

    def trunc(self) -> "Val":
        return _isl_val_trunc(self)

    def inv(self) -> "Val":
        return _isl_val_inv(self)

    def min(self, v2: "Val") -> "Val":
        return _isl_val_min(self, v2)

    def max(self, v2: "Val") -> "Val":
        return _isl_val_max(self, v2)

    def add(self, v2: "Val") -> "Val":
        return _isl_val_add(self, v2)

    def add_ui(self, v2: int) -> "Val":
        return _isl_val_add_ui(self, v2)

    def sub(self, v2: "Val") -> "Val":
        return _isl_val_sub(self, v2)

    def sub_ui(self, v2: int) -> "Val":
        return _isl_val_sub_ui(self, v2)

    def mul(self, v2: "Val") -> "Val":
        return _isl_val_mul(self, v2)

    def mul_ui(self, v2: int) -> "Val":
        return _isl_val_mul_ui(self, v2)

    def div(self, v2: "Val") -> "Val":
        return _isl_val_div(self, v2)

    def div_ui(self, v2: int) -> "Val":
        return _isl_val_div_ui(self, v2)

    def pow2(self) -> "Val":
        return _isl_val_pow2(self)

    def exp2(self) -> "Val":
        return _isl_val_2exp(self)

    def mod(self, v2: "Val") -> "Val":
        return _isl_val_mod(self, v2)

    def gcd(self, v2: "Val") -> "Val":
        return _isl_val_gcd(self, v2)

    def gcdext(self, v2: "Val", x: "Val", y: "Val") -> "Val":
        return _isl_val_gcdext(self, v2, x, y)


register_type("Val", Val)

_isl_val_get_ctx = ISLFunction.create(
    "isl_val_get_ctx",
    Keep("Val"),
    return_=Give("Ctx"),
    lib=_lib,
)

_isl_val_zero = ISLFunction.create(
    "isl_val_zero",
    Context(),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_one = ISLFunction.create(
    "isl_val_one",
    Context(),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_negone = ISLFunction.create(
    "isl_val_negone",
    Context(),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_nan = ISLFunction.create(
    "isl_val_nan",
    Context(),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_infty = ISLFunction.create(
    "isl_val_infty",
    Context(),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_neginfty = ISLFunction.create(
    "isl_val_neginfty",
    Context(),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_int_from_si = ISLFunction.create(
    "isl_val_int_from_si",
    Context(),
    Param(int, ctype=c_long),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_int_from_ui = ISLFunction.create(
    "isl_val_int_from_ui",
    Context(),
    Param(int, ctype=c_ulong),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_int_from_chunks = ISLFunction.create(
    "isl_val_int_from_chunks",
    Context(),
    Param(int, ctype=c_size_t),
    Param(int, ctype=c_size_t),
    Param(None, ctype=c_void_p),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_copy = ISLFunction.create(
    "isl_val_copy",
    Keep("Val"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_free = ISLFunction.create(
    "isl_val_free",
    Take("Val"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_get_num_si = ISLFunction.create(
    "isl_val_get_num_si",
    Keep("Val"),
    return_=Param(int, ctype=c_long),
    lib=_lib,
)

_isl_val_get_den_si = ISLFunction.create(
    "isl_val_get_den_si",
    Keep("Val"),
    return_=Param(int, ctype=c_long),
    lib=_lib,
)

_isl_val_get_den_val = ISLFunction.create(
    "isl_val_get_den_val",
    Keep("Val"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_get_d = ISLFunction.create(
    "isl_val_get_d",
    Keep("Val"),
    return_=Param(float, ctype=c_double),
    lib=_lib,
)

_isl_val_n_abs_num_chunks = ISLFunction.create(
    "isl_val_n_abs_num_chunks",
    Keep("Val"),
    Param(int, ctype=c_size_t),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_val_get_abs_num_chunks = ISLFunction.create(
    "isl_val_get_abs_num_chunks",
    Keep("Val"),
    Param(int, ctype=c_size_t),
    Param(None, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_val_set_si = ISLFunction.create(
    "isl_val_set_si",
    Take("Val"),
    Param(int, ctype=c_long),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_sgn = ISLFunction.create(
    "isl_val_sgn",
    Keep("Val"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_val_is_zero = ISLFunction.create(
    "isl_val_is_zero",
    Keep("Val"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_val_is_one = ISLFunction.create(
    "isl_val_is_one",
    Keep("Val"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_val_is_negone = ISLFunction.create(
    "isl_val_is_negone",
    Keep("Val"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_val_is_nonneg = ISLFunction.create(
    "isl_val_is_nonneg",
    Keep("Val"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_val_is_nonpos = ISLFunction.create(
    "isl_val_is_nonpos",
    Keep("Val"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_val_is_pos = ISLFunction.create(
    "isl_val_is_pos",
    Keep("Val"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_val_is_neg = ISLFunction.create(
    "isl_val_is_neg",
    Keep("Val"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_val_is_int = ISLFunction.create(
    "isl_val_is_int",
    Keep("Val"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_val_is_rat = ISLFunction.create(
    "isl_val_is_rat",
    Keep("Val"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_val_is_nan = ISLFunction.create(
    "isl_val_is_nan",
    Keep("Val"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_val_is_infty = ISLFunction.create(
    "isl_val_is_infty",
    Keep("Val"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_val_is_neginfty = ISLFunction.create(
    "isl_val_is_neginfty",
    Keep("Val"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_val_lt = ISLFunction.create(
    "isl_val_lt",
    Keep("Val"),
    Keep("Val"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_val_le = ISLFunction.create(
    "isl_val_le",
    Keep("Val"),
    Keep("Val"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_val_gt = ISLFunction.create(
    "isl_val_gt",
    Keep("Val"),
    Keep("Val"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_val_ge = ISLFunction.create(
    "isl_val_ge",
    Keep("Val"),
    Keep("Val"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_val_eq = ISLFunction.create(
    "isl_val_eq",
    Keep("Val"),
    Keep("Val"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_val_ne = ISLFunction.create(
    "isl_val_ne",
    Keep("Val"),
    Keep("Val"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_val_abs_eq = ISLFunction.create(
    "isl_val_abs_eq",
    Keep("Val"),
    Keep("Val"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_val_is_divisible_by = ISLFunction.create(
    "isl_val_is_divisible_by",
    Keep("Val"),
    Keep("Val"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_val_gt_si = ISLFunction.create(
    "isl_val_gt_si",
    Keep("Val"),
    Param(int, ctype=c_long),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_val_eq_si = ISLFunction.create(
    "isl_val_eq_si",
    Keep("Val"),
    Param(int, ctype=c_long),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_val_cmp_si = ISLFunction.create(
    "isl_val_cmp_si",
    Keep("Val"),
    Param(int, ctype=c_long),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_val_abs = ISLFunction.create(
    "isl_val_abs",
    Take("Val"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_neg = ISLFunction.create(
    "isl_val_neg",
    Take("Val"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_floor = ISLFunction.create(
    "isl_val_floor",
    Take("Val"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_ceil = ISLFunction.create(
    "isl_val_ceil",
    Take("Val"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_trunc = ISLFunction.create(
    "isl_val_trunc",
    Take("Val"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_inv = ISLFunction.create(
    "isl_val_inv",
    Take("Val"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_min = ISLFunction.create(
    "isl_val_min",
    Take("Val"),
    Take("Val"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_max = ISLFunction.create(
    "isl_val_max",
    Take("Val"),
    Take("Val"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_add = ISLFunction.create(
    "isl_val_add",
    Take("Val"),
    Take("Val"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_add_ui = ISLFunction.create(
    "isl_val_add_ui",
    Take("Val"),
    Param(int, ctype=c_ulong),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_sub = ISLFunction.create(
    "isl_val_sub",
    Take("Val"),
    Take("Val"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_sub_ui = ISLFunction.create(
    "isl_val_sub_ui",
    Take("Val"),
    Param(int, ctype=c_ulong),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_mul = ISLFunction.create(
    "isl_val_mul",
    Take("Val"),
    Take("Val"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_mul_ui = ISLFunction.create(
    "isl_val_mul_ui",
    Take("Val"),
    Param(int, ctype=c_ulong),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_div = ISLFunction.create(
    "isl_val_div",
    Take("Val"),
    Take("Val"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_div_ui = ISLFunction.create(
    "isl_val_div_ui",
    Take("Val"),
    Param(int, ctype=c_ulong),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_pow2 = ISLFunction.create(
    "isl_val_pow2",
    Take("Val"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_2exp = ISLFunction.create(
    "isl_val_2exp",
    Take("Val"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_mod = ISLFunction.create(
    "isl_val_mod",
    Take("Val"),
    Take("Val"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_gcd = ISLFunction.create(
    "isl_val_gcd",
    Take("Val"),
    Take("Val"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_gcdext = ISLFunction.create(
    "isl_val_gcdext",
    Take("Val"),
    Take("Val"),
    Give("Val"),
    Give("Val"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_read_from_str = ISLFunction.create(
    "isl_val_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("Val"),
    lib=_lib,
)

_isl_val_to_str = ISLFunction.create(
    "isl_val_to_str",
    Keep("Val"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)
