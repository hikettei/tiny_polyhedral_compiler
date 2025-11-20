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
    from .context import Context

_lib = load_libisl()

class Vertices(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_vertices_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_vertices_read_from_str(spec)


    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_vertices_free(handle)

    def get_ctx(self) -> "Context":
        return _isl_vertices_get_ctx(self)

    def foreach_vertex(self, fn: Any, user: Any, user_: Any = None) -> int:
        return _isl_vertices_foreach_vertex(self, fn, user, user_)

    def foreach_cell(self, fn: Any, user: Any, user_: Any = None) -> int:
        return _isl_vertices_foreach_cell(self, fn, user, user_)

    def get_n_vertices(self) -> int:
        return _isl_vertices_get_n_vertices(self)


register_type("Vertices", Vertices)

_isl_vertices_get_ctx = ISLFunction.create(
    "isl_vertices_get_ctx",
    Keep("Vertices"),
    return_=Give("Context"),
    lib=_lib,
)

_isl_vertices_foreach_vertex = ISLFunction.create(
    "isl_vertices_foreach_vertex",
    Keep("Vertices"),
    Param(None, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_vertices_foreach_cell = ISLFunction.create(
    "isl_vertices_foreach_cell",
    Keep("Vertices"),
    Param(None, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_vertices_get_n_vertices = ISLFunction.create(
    "isl_vertices_get_n_vertices",
    Keep("Vertices"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_vertices_free = ISLFunction.create(
    "isl_vertices_free",
    Take("Vertices"),
    return_=Give("Vertices"),
    lib=_lib,
)

_isl_vertices_read_from_str = ISLFunction.create(
    "isl_vertices_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("Vertices"),
    lib=_lib,
)
