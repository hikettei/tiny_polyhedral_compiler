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
    from .id import Id
    from .id_list import IdList
    from .space import Space

_lib = load_libisl()

class MultiId(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_multi_id_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_multi_id_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_multi_id_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_multi_id_free(handle)

    def __str__(self) -> str:
        return _isl_multi_id_to_str(self)

    def __repr__(self) -> str:
        return f"MultiId({self.__str__()})"

    def get_ctx(self) -> "Context":
        return _isl_multi_id_get_ctx(self)

    def get_space(self) -> "Space":
        return _isl_multi_id_get_space(self)

    def reset_user(self) -> "MultiId":
        return _isl_multi_id_reset_user(self)

    @classmethod
    def from_id_list(cls, space: "Space", list: "IdList") -> "MultiId":
        return _isl_multi_id_from_id_list(space, list)

    def size(self) -> int:
        return _isl_multi_id_size(self)

    def get_at(self, pos: int) -> "Id":
        return _isl_multi_id_get_at(self, pos)

    def get_id(self, pos: int) -> "Id":
        return _isl_multi_id_get_id(self, pos)

    def set_at(self, pos: int, id: "Id") -> "MultiId":
        return _isl_multi_id_set_at(self, pos, id)

    def set_id(self, pos: int, id: "Id") -> "MultiId":
        return _isl_multi_id_set_id(self, pos, id)

    def get_list(self) -> "IdList":
        return _isl_multi_id_get_list(self)

    def range_is_wrapping(self) -> bool:
        return _isl_multi_id_range_is_wrapping(self)

    def plain_is_equal(self, mi2: "MultiId") -> bool:
        return _isl_multi_id_plain_is_equal(self, mi2)

    def from_range(self) -> "MultiId":
        return _isl_multi_id_from_range(self)

    def flatten_range(self) -> "MultiId":
        return _isl_multi_id_flatten_range(self)

    def align_params(self, model: "Space") -> "MultiId":
        return _isl_multi_id_align_params(self, model)

    def range_product(self, mi2: "MultiId") -> "MultiId":
        return _isl_multi_id_range_product(self, mi2)

    def flat_range_product(self, mi2: "MultiId") -> "MultiId":
        return _isl_multi_id_flat_range_product(self, mi2)

    def factor_range(self) -> "MultiId":
        return _isl_multi_id_factor_range(self)

    def range_factor_domain(self) -> "MultiId":
        return _isl_multi_id_range_factor_domain(self)

    def range_factor_range(self) -> "MultiId":
        return _isl_multi_id_range_factor_range(self)

    def range_splice(self, pos: int, mi2: "MultiId") -> "MultiId":
        return _isl_multi_id_range_splice(self, pos, mi2)


register_type("MultiId", MultiId)

_isl_multi_id_get_ctx = ISLFunction.create(
    "isl_multi_id_get_ctx",
    Keep("MultiId"),
    return_=Give("Context"),
    lib=_lib,
)

_isl_multi_id_get_space = ISLFunction.create(
    "isl_multi_id_get_space",
    Keep("MultiId"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_multi_id_reset_user = ISLFunction.create(
    "isl_multi_id_reset_user",
    Take("MultiId"),
    return_=Give("MultiId"),
    lib=_lib,
)

_isl_multi_id_from_id_list = ISLFunction.create(
    "isl_multi_id_from_id_list",
    Take("Space"),
    Take("IdList"),
    return_=Give("MultiId"),
    lib=_lib,
)

_isl_multi_id_copy = ISLFunction.create(
    "isl_multi_id_copy",
    Keep("MultiId"),
    return_=Give("MultiId"),
    lib=_lib,
)

_isl_multi_id_free = ISLFunction.create(
    "isl_multi_id_free",
    Take("MultiId"),
    return_=Give("MultiId"),
    lib=_lib,
)

_isl_multi_id_size = ISLFunction.create(
    "isl_multi_id_size",
    Keep("MultiId"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_multi_id_get_at = ISLFunction.create(
    "isl_multi_id_get_at",
    Keep("MultiId"),
    Param(int, ctype=c_int),
    return_=Give("Id"),
    lib=_lib,
)

_isl_multi_id_get_id = ISLFunction.create(
    "isl_multi_id_get_id",
    Keep("MultiId"),
    Param(int, ctype=c_int),
    return_=Give("Id"),
    lib=_lib,
)

_isl_multi_id_set_at = ISLFunction.create(
    "isl_multi_id_set_at",
    Take("MultiId"),
    Param(int, ctype=c_int),
    Take("Id"),
    return_=Give("MultiId"),
    lib=_lib,
)

_isl_multi_id_set_id = ISLFunction.create(
    "isl_multi_id_set_id",
    Take("MultiId"),
    Param(int, ctype=c_int),
    Take("Id"),
    return_=Give("MultiId"),
    lib=_lib,
)

_isl_multi_id_get_list = ISLFunction.create(
    "isl_multi_id_get_list",
    Keep("MultiId"),
    return_=Give("IdList"),
    lib=_lib,
)

_isl_multi_id_read_from_str = ISLFunction.create(
    "isl_multi_id_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("MultiId"),
    lib=_lib,
)

_isl_multi_id_to_str = ISLFunction.create(
    "isl_multi_id_to_str",
    Keep("MultiId"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_multi_id_range_is_wrapping = ISLFunction.create(
    "isl_multi_id_range_is_wrapping",
    Keep("MultiId"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_id_plain_is_equal = ISLFunction.create(
    "isl_multi_id_plain_is_equal",
    Keep("MultiId"),
    Keep("MultiId"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_id_from_range = ISLFunction.create(
    "isl_multi_id_from_range",
    Take("MultiId"),
    return_=Give("MultiId"),
    lib=_lib,
)

_isl_multi_id_flatten_range = ISLFunction.create(
    "isl_multi_id_flatten_range",
    Take("MultiId"),
    return_=Give("MultiId"),
    lib=_lib,
)

_isl_multi_id_align_params = ISLFunction.create(
    "isl_multi_id_align_params",
    Take("MultiId"),
    Take("Space"),
    return_=Give("MultiId"),
    lib=_lib,
)

_isl_multi_id_range_product = ISLFunction.create(
    "isl_multi_id_range_product",
    Take("MultiId"),
    Take("MultiId"),
    return_=Give("MultiId"),
    lib=_lib,
)

_isl_multi_id_flat_range_product = ISLFunction.create(
    "isl_multi_id_flat_range_product",
    Take("MultiId"),
    Take("MultiId"),
    return_=Give("MultiId"),
    lib=_lib,
)

_isl_multi_id_factor_range = ISLFunction.create(
    "isl_multi_id_factor_range",
    Take("MultiId"),
    return_=Give("MultiId"),
    lib=_lib,
)

_isl_multi_id_range_factor_domain = ISLFunction.create(
    "isl_multi_id_range_factor_domain",
    Take("MultiId"),
    return_=Give("MultiId"),
    lib=_lib,
)

_isl_multi_id_range_factor_range = ISLFunction.create(
    "isl_multi_id_range_factor_range",
    Take("MultiId"),
    return_=Give("MultiId"),
    lib=_lib,
)

_isl_multi_id_range_splice = ISLFunction.create(
    "isl_multi_id_range_splice",
    Take("MultiId"),
    Param(int, ctype=c_uint),
    Take("MultiId"),
    return_=Give("MultiId"),
    lib=_lib,
)
