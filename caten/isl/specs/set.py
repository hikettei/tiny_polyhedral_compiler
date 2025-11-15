from __future__ import annotations

from typing import TYPE_CHECKING

from ..ffi import FfiPointer, load_libisl
from ..func import ISLFunction
from ..obj import ISLObject
from ..qualifier import Give, Keep, Param, Take
from .context import ISLContext, current, expect_handle

if TYPE_CHECKING:  # pragma: no cover - type checking only
    from .constraint import Constraint
    from .union_set import UnionSet
else:  # Minimal scaffolding until the dedicated modules land.
    Constraint = ISLObject  # type: ignore[assignment]
    UnionSet = ISLObject  # type: ignore[assignment]

_lib = load_libisl()

def _encode_set_spec(spec: str) -> bytes:
    text = spec.strip()
    if not text:
        raise ValueError("Set specifications must be non-empty strings.")
    return text.encode("utf-8")

_isl_set_read_from_str = ISLFunction.create(
    _lib.isl_set_read_from_str,
    Keep(ISLContext),
    Param(str, converter=_encode_set_spec),
)

class Set(ISLObject):
    """High-level wrapper around ``isl_set`` handles."""

    __slots__ = ()

    def __init__(self, spec_or_handle: str | FfiPointer) -> None:
        if isinstance(spec_or_handle, str):
            ctx = current(required=True)
            handle = expect_handle(
                _isl_set_read_from_str(ctx, spec_or_handle),
                ctx=ctx,
                func="isl_set_read_from_str",
            )
        else:
            handle = expect_handle(spec_or_handle, func="isl_set_from_ptr")
        super().__init__(handle)

    def copy_handle(self) -> FfiPointer:
        return expect_handle(
            _isl_set_copy_handle(self),
            func="isl_set_copy",
        )

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _lib.isl_set_free(handle)

    @classmethod
    def from_union_set(cls, union: UnionSet) -> "Set":
        return _isl_set_from_union_set(union)

    def union(self, other: "Set") -> "Set":
        return _isl_set_union(self, other)

    def add_constraint(self, constraint: Constraint) -> "Set":
        return _isl_set_add_constraint(self, constraint)


_isl_set_copy_handle = ISLFunction.create(
    _lib.isl_set_copy,
    Keep(Set),
)

_isl_set_union = ISLFunction.create(
    "isl_set_union",
    Take(Set),
    Take(Set),
    return_=Give(Set),
    lib=_lib,
)

_isl_set_from_union_set = ISLFunction.create(
    "isl_set_from_union_set",
    Take(UnionSet),
    return_=Give(Set),
    lib=_lib,
)

_isl_set_add_constraint = ISLFunction.create(
    "isl_set_add_constraint",
    Take(Set),
    Take(Constraint),
    return_=Give(Set),
    lib=_lib,
)

__all__ = ["Set"]
