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
    from .basic_map import BasicMap
    from .context import Context
    from .id import Id
    from .space import Space

_lib = load_libisl()

class LocalSpace(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_local_space_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_local_space_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_local_space_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_local_space_free(handle)

    def __str__(self) -> str:
        return _isl_local_space_to_str(self)

    def __repr__(self) -> str:
        return f"LocalSpace({self.__str__()})"

    def get_ctx(self) -> "Context":
        return _isl_local_space_get_ctx(self)

    def dim(self, type: int) -> int:
        return _isl_local_space_dim(self, type)

    def set_dim_id(self, type: int, pos: int, id: "Id") -> "LocalSpace":
        return _isl_local_space_set_dim_id(self, type, pos, id)

    def has_dim_id(self, type: int, pos: int) -> bool:
        return _isl_local_space_has_dim_id(self, type, pos)

    def get_dim_id(self, type: int, pos: int) -> "Id":
        return _isl_local_space_get_dim_id(self, type, pos)

    def set_dim_name(self, type: int, pos: int, s: str) -> "LocalSpace":
        return _isl_local_space_set_dim_name(self, type, pos, s)

    def has_dim_name(self, type: int, pos: int) -> bool:
        return _isl_local_space_has_dim_name(self, type, pos)

    def get_dim_name(self, type: int, pos: int) -> str:
        return _isl_local_space_get_dim_name(self, type, pos)

    def find_dim_by_name(self, type: int, name: str) -> int:
        return _isl_local_space_find_dim_by_name(self, type, name)

    def set_tuple_id(self, type: int, id: "Id") -> "LocalSpace":
        return _isl_local_space_set_tuple_id(self, type, id)

    @classmethod
    def from_space(cls, space: "Space") -> "LocalSpace":
        return _isl_local_space_from_space(space)

    def is_params(self) -> bool:
        return _isl_local_space_is_params(self)

    def is_set(self) -> bool:
        return _isl_local_space_is_set(self)

    def get_space(self) -> "Space":
        return _isl_local_space_get_space(self)

    def get_div(self, pos: int) -> "Aff":
        return _isl_local_space_get_div(self, pos)

    def is_equal(self, ls2: "LocalSpace") -> bool:
        return _isl_local_space_is_equal(self, ls2)

    def domain(self) -> "LocalSpace":
        return _isl_local_space_domain(self)

    def range(self) -> "LocalSpace":
        return _isl_local_space_range(self)

    def set_from_params(self) -> "LocalSpace":
        return _isl_local_space_set_from_params(self)

    def from_domain(self) -> "LocalSpace":
        return _isl_local_space_from_domain(self)

    def wrap(self) -> "LocalSpace":
        return _isl_local_space_wrap(self)

    def flatten_domain(self) -> "LocalSpace":
        return _isl_local_space_flatten_domain(self)

    def flatten_range(self) -> "LocalSpace":
        return _isl_local_space_flatten_range(self)

    def lifting(self) -> "BasicMap":
        return _isl_local_space_lifting(self)

    def add_dims(self, type: int, n: int) -> "LocalSpace":
        return _isl_local_space_add_dims(self, type, n)

    def insert_dims(self, type: int, first: int, n: int) -> "LocalSpace":
        return _isl_local_space_insert_dims(self, type, first, n)

    def drop_dims(self, type: int, first: int, n: int) -> "LocalSpace":
        return _isl_local_space_drop_dims(self, type, first, n)

    def intersect(self, ls2: "LocalSpace") -> "LocalSpace":
        return _isl_local_space_intersect(self, ls2)


register_type("LocalSpace", LocalSpace)

_isl_local_space_get_ctx = ISLFunction.create(
    "isl_local_space_get_ctx",
    Keep("LocalSpace"),
    return_=Give("Context"),
    lib=_lib,
)

_isl_local_space_dim = ISLFunction.create(
    "isl_local_space_dim",
    Keep("LocalSpace"),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_local_space_set_dim_id = ISLFunction.create(
    "isl_local_space_set_dim_id",
    Take("LocalSpace"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Take("Id"),
    return_=Give("LocalSpace"),
    lib=_lib,
)

_isl_local_space_has_dim_id = ISLFunction.create(
    "isl_local_space_has_dim_id",
    Keep("LocalSpace"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_local_space_get_dim_id = ISLFunction.create(
    "isl_local_space_get_dim_id",
    Keep("LocalSpace"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Give("Id"),
    lib=_lib,
)

_isl_local_space_set_dim_name = ISLFunction.create(
    "isl_local_space_set_dim_name",
    Take("LocalSpace"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(str, ctype=c_char_p),
    return_=Give("LocalSpace"),
    lib=_lib,
)

_isl_local_space_has_dim_name = ISLFunction.create(
    "isl_local_space_has_dim_name",
    Keep("LocalSpace"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_local_space_get_dim_name = ISLFunction.create(
    "isl_local_space_get_dim_name",
    Keep("LocalSpace"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_local_space_find_dim_by_name = ISLFunction.create(
    "isl_local_space_find_dim_by_name",
    Keep("LocalSpace"),
    Param(int, ctype=c_int),
    Param(str, ctype=c_char_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_local_space_set_tuple_id = ISLFunction.create(
    "isl_local_space_set_tuple_id",
    Take("LocalSpace"),
    Param(int, ctype=c_int),
    Take("Id"),
    return_=Give("LocalSpace"),
    lib=_lib,
)

_isl_local_space_from_space = ISLFunction.create(
    "isl_local_space_from_space",
    Take("Space"),
    return_=Give("LocalSpace"),
    lib=_lib,
)

_isl_local_space_is_params = ISLFunction.create(
    "isl_local_space_is_params",
    Keep("LocalSpace"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_local_space_is_set = ISLFunction.create(
    "isl_local_space_is_set",
    Keep("LocalSpace"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_local_space_get_space = ISLFunction.create(
    "isl_local_space_get_space",
    Keep("LocalSpace"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_local_space_get_div = ISLFunction.create(
    "isl_local_space_get_div",
    Keep("LocalSpace"),
    Param(int, ctype=c_int),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_local_space_copy = ISLFunction.create(
    "isl_local_space_copy",
    Keep("LocalSpace"),
    return_=Give("LocalSpace"),
    lib=_lib,
)

_isl_local_space_free = ISLFunction.create(
    "isl_local_space_free",
    Take("LocalSpace"),
    return_=Give("LocalSpace"),
    lib=_lib,
)

_isl_local_space_is_equal = ISLFunction.create(
    "isl_local_space_is_equal",
    Keep("LocalSpace"),
    Keep("LocalSpace"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_local_space_domain = ISLFunction.create(
    "isl_local_space_domain",
    Take("LocalSpace"),
    return_=Give("LocalSpace"),
    lib=_lib,
)

_isl_local_space_range = ISLFunction.create(
    "isl_local_space_range",
    Take("LocalSpace"),
    return_=Give("LocalSpace"),
    lib=_lib,
)

_isl_local_space_set_from_params = ISLFunction.create(
    "isl_local_space_set_from_params",
    Take("LocalSpace"),
    return_=Give("LocalSpace"),
    lib=_lib,
)

_isl_local_space_from_domain = ISLFunction.create(
    "isl_local_space_from_domain",
    Take("LocalSpace"),
    return_=Give("LocalSpace"),
    lib=_lib,
)

_isl_local_space_wrap = ISLFunction.create(
    "isl_local_space_wrap",
    Take("LocalSpace"),
    return_=Give("LocalSpace"),
    lib=_lib,
)

_isl_local_space_flatten_domain = ISLFunction.create(
    "isl_local_space_flatten_domain",
    Take("LocalSpace"),
    return_=Give("LocalSpace"),
    lib=_lib,
)

_isl_local_space_flatten_range = ISLFunction.create(
    "isl_local_space_flatten_range",
    Take("LocalSpace"),
    return_=Give("LocalSpace"),
    lib=_lib,
)

_isl_local_space_lifting = ISLFunction.create(
    "isl_local_space_lifting",
    Take("LocalSpace"),
    return_=Give("BasicMap"),
    lib=_lib,
)

_isl_local_space_add_dims = ISLFunction.create(
    "isl_local_space_add_dims",
    Take("LocalSpace"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Give("LocalSpace"),
    lib=_lib,
)

_isl_local_space_insert_dims = ISLFunction.create(
    "isl_local_space_insert_dims",
    Take("LocalSpace"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("LocalSpace"),
    lib=_lib,
)

_isl_local_space_drop_dims = ISLFunction.create(
    "isl_local_space_drop_dims",
    Take("LocalSpace"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("LocalSpace"),
    lib=_lib,
)

_isl_local_space_intersect = ISLFunction.create(
    "isl_local_space_intersect",
    Take("LocalSpace"),
    Take("LocalSpace"),
    return_=Give("LocalSpace"),
    lib=_lib,
)

_isl_local_space_read_from_str = ISLFunction.create(
    "isl_local_space_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("LocalSpace"),
    lib=_lib,
)

_isl_local_space_to_str = ISLFunction.create(
    "isl_local_space_to_str",
    Keep("LocalSpace"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)
