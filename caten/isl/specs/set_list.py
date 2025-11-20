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
    from .set import Set

_lib = load_libisl()

class SetList(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_set_list_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_set_list_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_set_list_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_set_list_free(handle)

    def __str__(self) -> str:
        return _isl_set_list_to_str(self)

    def __repr__(self) -> str:
        return f"SetList({self.__str__()})"

    def get_ctx(self) -> "Context":
        return _isl_set_list_get_ctx(self)

    def union(self) -> "Set":
        return _isl_set_list_union(self)

    @classmethod
    def from_set(cls, el: "Set") -> "SetList":
        return _isl_set_list_from_set(el)

    @classmethod
    def alloc(cls, n: int) -> "SetList":
        return _isl_set_list_alloc(n)

    def insert(self, pos: int, el: "Set") -> "SetList":
        return _isl_set_list_insert(self, pos, el)

    def add(self, el: "Set") -> "SetList":
        return _isl_set_list_add(self, el)

    def drop(self, first: int, n: int) -> "SetList":
        return _isl_set_list_drop(self, first, n)

    def clear(self) -> "SetList":
        return _isl_set_list_clear(self)

    def swap(self, pos1: int, pos2: int) -> "SetList":
        return _isl_set_list_swap(self, pos1, pos2)

    def reverse(self) -> "SetList":
        return _isl_set_list_reverse(self)

    def set_at(self, index: int, set: "Set") -> "SetList":
        return _isl_set_list_set_at(self, index, set)

    def set_set(self, index: int, set: "Set") -> "SetList":
        return _isl_set_list_set_set(self, index, set)

    def concat(self, list2: "SetList") -> "SetList":
        return _isl_set_list_concat(self, list2)

    def map(self, fn: Any, user: Any = None) -> "SetList":
        return _isl_set_list_map(self, fn, user)

    def sort(self, cmp: Any, user: Any = None) -> "SetList":
        return _isl_set_list_sort(self, cmp, user)

    def size(self) -> int:
        return _isl_set_list_size(self)

    def n_set(self) -> int:
        return _isl_set_list_n_set(self)

    def get_at(self, index: int) -> "Set":
        return _isl_set_list_get_at(self, index)

    def get_set(self, index: int) -> "Set":
        return _isl_set_list_get_set(self, index)

    def foreach(self, fn: Any, user: Any = None) -> int:
        return _isl_set_list_foreach(self, fn, user)

    def every(self, test: Any, user: Any = None) -> bool:
        return _isl_set_list_every(self, test, user)

    def foreach_scc(self, follows: Any, follows_user: Any, fn: Any, fn_user: Any) -> int:
        return _isl_set_list_foreach_scc(self, follows, follows_user, fn, fn_user)


register_type("SetList", SetList)

_isl_set_list_get_ctx = ISLFunction.create(
    "isl_set_list_get_ctx",
    Keep("SetList"),
    return_=Give("Context"),
    lib=_lib,
)

_isl_set_list_union = ISLFunction.create(
    "isl_set_list_union",
    Take("SetList"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_list_from_set = ISLFunction.create(
    "isl_set_list_from_set",
    Take("Set"),
    return_=Give("SetList"),
    lib=_lib,
)

_isl_set_list_alloc = ISLFunction.create(
    "isl_set_list_alloc",
    Context(),
    Param(int, ctype=c_int),
    return_=Give("SetList"),
    lib=_lib,
)

_isl_set_list_copy = ISLFunction.create(
    "isl_set_list_copy",
    Keep("SetList"),
    return_=Give("SetList"),
    lib=_lib,
)

_isl_set_list_insert = ISLFunction.create(
    "isl_set_list_insert",
    Take("SetList"),
    Param(int, ctype=c_uint),
    Take("Set"),
    return_=Give("SetList"),
    lib=_lib,
)

_isl_set_list_add = ISLFunction.create(
    "isl_set_list_add",
    Take("SetList"),
    Take("Set"),
    return_=Give("SetList"),
    lib=_lib,
)

_isl_set_list_drop = ISLFunction.create(
    "isl_set_list_drop",
    Take("SetList"),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("SetList"),
    lib=_lib,
)

_isl_set_list_clear = ISLFunction.create(
    "isl_set_list_clear",
    Take("SetList"),
    return_=Give("SetList"),
    lib=_lib,
)

_isl_set_list_swap = ISLFunction.create(
    "isl_set_list_swap",
    Take("SetList"),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("SetList"),
    lib=_lib,
)

_isl_set_list_reverse = ISLFunction.create(
    "isl_set_list_reverse",
    Take("SetList"),
    return_=Give("SetList"),
    lib=_lib,
)

_isl_set_list_set_at = ISLFunction.create(
    "isl_set_list_set_at",
    Take("SetList"),
    Param(int, ctype=c_int),
    Take("Set"),
    return_=Give("SetList"),
    lib=_lib,
)

_isl_set_list_set_set = ISLFunction.create(
    "isl_set_list_set_set",
    Take("SetList"),
    Param(int, ctype=c_int),
    Take("Set"),
    return_=Give("SetList"),
    lib=_lib,
)

_isl_set_list_concat = ISLFunction.create(
    "isl_set_list_concat",
    Take("SetList"),
    Take("SetList"),
    return_=Give("SetList"),
    lib=_lib,
)

_isl_set_list_map = ISLFunction.create(
    "isl_set_list_map",
    Take("SetList"),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Give("SetList"),
    lib=_lib,
)

_isl_set_list_sort = ISLFunction.create(
    "isl_set_list_sort",
    Take("SetList"),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Give("SetList"),
    lib=_lib,
)

_isl_set_list_free = ISLFunction.create(
    "isl_set_list_free",
    Take("SetList"),
    return_=Give("SetList"),
    lib=_lib,
)

_isl_set_list_size = ISLFunction.create(
    "isl_set_list_size",
    Keep("SetList"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_set_list_n_set = ISLFunction.create(
    "isl_set_list_n_set",
    Keep("SetList"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_set_list_get_at = ISLFunction.create(
    "isl_set_list_get_at",
    Keep("SetList"),
    Param(int, ctype=c_int),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_list_get_set = ISLFunction.create(
    "isl_set_list_get_set",
    Keep("SetList"),
    Param(int, ctype=c_int),
    return_=Give("Set"),
    lib=_lib,
)

_isl_set_list_foreach = ISLFunction.create(
    "isl_set_list_foreach",
    Keep("SetList"),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_set_list_every = ISLFunction.create(
    "isl_set_list_every",
    Keep("SetList"),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_list_foreach_scc = ISLFunction.create(
    "isl_set_list_foreach_scc",
    Keep("SetList"),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_set_list_to_str = ISLFunction.create(
    "isl_set_list_to_str",
    Keep("SetList"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_set_list_read_from_str = ISLFunction.create(
    "isl_set_list_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("SetList"),
    lib=_lib,
)
