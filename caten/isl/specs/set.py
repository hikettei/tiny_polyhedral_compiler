from __future__ import annotations

from ..ffi import FfiPointer, load_libisl
from ..func import ISLFunction
from ..obj import ISLObject
from ..qualifier import Give, Param
from .context import Context

_lib = load_libisl()

class Set(ISLObject):
    """High-level wrapper around ``isl_set`` handles."""

    __slots__ = ()
    def __init__(self, handle: FfiPointer) -> None:
        super().__init__(handle)

    @classmethod
    def from_str(cls, handle: str) -> None:
        return _isl_set_read_from_str(handle)
    
    def copy_handle(self) -> FfiPointer:
        pass #return _isl_set_copy_handle(self)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        pass #_lib.isl_set_free(handle)

    @classmethod
    def from_union_set(cls, union: UnionSet) -> "Set":
        return _isl_set_from_union_set(union)

    def union(self, other: "Set") -> "Set":
        return _isl_set_union(self, other)

    def add_constraint(self, constraint: Constraint) -> "Set":
        return _isl_set_add_constraint(self, constraint)

_isl_set_read_from_str = ISLFunction.create(
    _lib.isl_set_read_from_str,
    Context(),
    Param(str),
    return_=Give(Set),
    lib=_lib,
)

__all__ = ["Set"]
