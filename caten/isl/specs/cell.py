from __future__ import annotations

from ctypes import c_char_p, c_int, c_void_p
from typing import TYPE_CHECKING, Any

from ..ffi import load_libisl
from ..func import ISLFunction
from ..mixin import ISLObjectMixin
from ..obj import ISLObject
from ..qualifier import Give, Keep, Param, Take
from ..registry import register_type
from .context import Context

if TYPE_CHECKING:
    from .basic_set import BasicSet
    from .context import Context

_lib = load_libisl()

class Cell(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_cell_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_cell_read_from_str(spec)


    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_cell_free(handle)

    def get_ctx(self) -> "Context":
        return _isl_cell_get_ctx(self)

    def foreach_vertex(self, fn: Any, user: Any, user_: Any = None) -> int:
        return _isl_cell_foreach_vertex(self, fn, user, user_)

    def get_domain(self) -> "BasicSet":
        return _isl_cell_get_domain(self)


register_type("Cell", Cell)

_isl_cell_get_ctx = ISLFunction.create(
    "isl_cell_get_ctx",
    Keep("Cell"),
    return_=Give("Context"),
    lib=_lib,
)

_isl_cell_foreach_vertex = ISLFunction.create(
    "isl_cell_foreach_vertex",
    Keep("Cell"),
    Param(None, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_cell_get_domain = ISLFunction.create(
    "isl_cell_get_domain",
    Keep("Cell"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_cell_free = ISLFunction.create(
    "isl_cell_free",
    Take("Cell"),
    return_=Give("Cell"),
    lib=_lib,
)

_isl_cell_read_from_str = ISLFunction.create(
    "isl_cell_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("Cell"),
    lib=_lib,
)
