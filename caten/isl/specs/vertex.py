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
    from .basic_set import BasicSet
    from .context import Context
    from .multi_aff import MultiAff

_lib = load_libisl()

class Vertex(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_vertex_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_vertex_read_from_str(spec)

    def copy_handle(self) -> Any:
        raise NotImplementedError(f"{type(self).__name__} does not support copy.")

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_vertex_free(handle)

    def get_ctx(self) -> "Context":
        return _isl_vertex_get_ctx(self)

    def get_id(self) -> int:
        return _isl_vertex_get_id(self)

    def get_domain(self) -> "BasicSet":
        return _isl_vertex_get_domain(self)

    def get_expr(self) -> "MultiAff":
        return _isl_vertex_get_expr(self)


register_type("Vertex", Vertex)

_isl_vertex_get_ctx = ISLFunction.create(
    "isl_vertex_get_ctx",
    Keep("Vertex"),
    return_=Give("Context"),
    lib=_lib,
)

_isl_vertex_get_id = ISLFunction.create(
    "isl_vertex_get_id",
    Keep("Vertex"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_vertex_get_domain = ISLFunction.create(
    "isl_vertex_get_domain",
    Keep("Vertex"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_vertex_get_expr = ISLFunction.create(
    "isl_vertex_get_expr",
    Keep("Vertex"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_vertex_free = ISLFunction.create(
    "isl_vertex_free",
    Take("Vertex"),
    return_=Give("Vertex"),
    lib=_lib,
)

_isl_vertex_read_from_str = ISLFunction.create(
    "isl_vertex_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("Vertex"),
    lib=_lib,
)
