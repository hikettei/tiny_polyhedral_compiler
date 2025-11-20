from __future__ import annotations

from ctypes import c_char_p, c_int
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
    from .multi_aff import MultiAff
    from .multi_val import MultiVal
    from .space import Space

_lib = load_libisl()

class FixedBox(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_fixed_box_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_fixed_box_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_fixed_box_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_fixed_box_free(handle)

    def __str__(self) -> str:
        return _isl_fixed_box_to_str(self)

    def __repr__(self) -> str:
        return f"FixedBox({self.__str__()})"

    def get_ctx(self) -> "Context":
        return _isl_fixed_box_get_ctx(self)

    def get_space(self) -> "Space":
        return _isl_fixed_box_get_space(self)

    def is_valid(self) -> bool:
        return _isl_fixed_box_is_valid(self)

    def get_offset(self) -> "MultiAff":
        return _isl_fixed_box_get_offset(self)

    def get_size(self) -> "MultiVal":
        return _isl_fixed_box_get_size(self)


register_type("FixedBox", FixedBox)

_isl_fixed_box_get_ctx = ISLFunction.create(
    "isl_fixed_box_get_ctx",
    Keep("FixedBox"),
    return_=Give("Context"),
    lib=_lib,
)

_isl_fixed_box_get_space = ISLFunction.create(
    "isl_fixed_box_get_space",
    Keep("FixedBox"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_fixed_box_is_valid = ISLFunction.create(
    "isl_fixed_box_is_valid",
    Keep("FixedBox"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_fixed_box_get_offset = ISLFunction.create(
    "isl_fixed_box_get_offset",
    Keep("FixedBox"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_fixed_box_get_size = ISLFunction.create(
    "isl_fixed_box_get_size",
    Keep("FixedBox"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_fixed_box_copy = ISLFunction.create(
    "isl_fixed_box_copy",
    Keep("FixedBox"),
    return_=Give("FixedBox"),
    lib=_lib,
)

_isl_fixed_box_free = ISLFunction.create(
    "isl_fixed_box_free",
    Take("FixedBox"),
    return_=Give("FixedBox"),
    lib=_lib,
)

_isl_fixed_box_read_from_str = ISLFunction.create(
    "isl_fixed_box_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("FixedBox"),
    lib=_lib,
)

_isl_fixed_box_to_str = ISLFunction.create(
    "isl_fixed_box_to_str",
    Keep("FixedBox"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)
