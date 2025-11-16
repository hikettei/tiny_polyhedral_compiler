from __future__ import annotations

from ctypes import c_int, c_void_p

from ..ffi import FfiPointer, load_libisl
from ..func import ISLFunction
from ..obj import ISLObject
from ..qualifier import Give, Keep, Null, Param, Take
from .context import Context
from .set import Set

_lib = load_libisl()

class UnionSet(ISLObject):
    """High-level wrapper around ``isl_union_set`` handles."""

    __slots__ = ()

    def __init__(self, handle_or_spec: str | FfiPointer) -> None:
        handle: FfiPointer
        if isinstance(handle_or_spec, str):
            handle = _isl_union_set_read_from_str(handle_or_spec, return_raw_pointer=True)
        else:
            handle = handle_or_spec
        super().__init__(handle)

    @classmethod
    def from_str(cls, spec: str) -> "UnionSet":
        return _isl_union_set_read_from_str(spec)

    def copy_handle(self) -> FfiPointer:
        return _isl_union_set_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_union_set_free(handle)

    # ----------------- queries -----------------
    def is_empty(self) -> bool:
        return _isl_union_set_is_empty(self)

    def is_params(self) -> bool:
        return _isl_union_set_is_params(self)

    def is_equal(self, other: "UnionSet") -> bool:
        return _isl_union_set_is_equal(self, other)

    def is_disjoint(self, other: "UnionSet") -> bool:
        return _isl_union_set_is_disjoint(self, other)

    def is_subset(self, other: "UnionSet") -> bool:
        return _isl_union_set_is_subset(self, other)

    def is_strict_subset(self, other: "UnionSet") -> bool:
        return _isl_union_set_is_strict_subset(self, other)

    def n_set(self) -> int:
        return _isl_union_set_n_set(self)

    # ----------------- transforms -----------------
    def union(self, other: "UnionSet") -> "UnionSet":
        return _isl_union_set_union(self, other)

    def intersect(self, other: "UnionSet") -> "UnionSet":
        return _isl_union_set_intersect(self, other)

    def subtract(self, other: "UnionSet") -> "UnionSet":
        return _isl_union_set_subtract(self, other)

    def product(self, other: "UnionSet") -> "UnionSet":
        return _isl_union_set_product(self, other)

    def gist(self, context: "UnionSet") -> "UnionSet":
        return _isl_union_set_gist(self, context)

    def coalesce(self) -> "UnionSet":
        return _isl_union_set_coalesce(self)

    def detect_equalities(self) -> "UnionSet":
        return _isl_union_set_detect_equalities(self)

    def remove_redundancies(self) -> "UnionSet":
        return _isl_union_set_remove_redundancies(self)

    def project_out_all_params(self) -> "UnionSet":
        return _isl_union_set_project_out_all_params(self)

    def lift(self) -> "UnionSet":
        return _isl_union_set_lift(self)

    def drop_unused_params(self) -> "UnionSet":
        if _isl_union_set_drop_unused_params is None:
            raise NotImplementedError(
                "isl_union_set_drop_unused_params not available in linked libisl"
            )
        return _isl_union_set_drop_unused_params(self)

    def lexmin(self) -> "UnionSet":
        return _isl_union_set_lexmin(self)

    def lexmax(self) -> "UnionSet":
        return _isl_union_set_lexmax(self)

    # ----------------- conversion -----------------
    def params(self) -> Set:
        return _isl_union_set_params(self)

    # ----------------- string -----------------
    def __str__(self) -> str:  # pragma: no cover
        return _isl_union_set_to_str(self)

    def __repr__(self) -> str:  # pragma: no cover
        return f"UnionSet({self.__str__()})"

    def __eq__(self, other: object) -> bool:  # pragma: no cover
        if not isinstance(other, UnionSet):
            return NotImplemented
        return self.is_equal(other)


_isl_union_set_read_from_str = ISLFunction.create(
    _lib.isl_union_set_read_from_str,
    Context(),
    Param(str),
    return_=Give(UnionSet),
    lib=_lib,
)

_isl_union_set_copy = ISLFunction.create(
    _lib.isl_union_set_copy,
    Keep(UnionSet),
    return_=Give(UnionSet),
    lib=_lib,
)

_isl_union_set_free = ISLFunction.create(
    _lib.isl_union_set_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

_isl_union_set_to_str = ISLFunction.create(
    _lib.isl_union_set_to_str,
    Keep(UnionSet),
    return_=Param(str),
    lib=_lib,
)

_isl_union_set_is_empty = ISLFunction.create(
    _lib.isl_union_set_is_empty,
    Keep(UnionSet),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_set_is_params = ISLFunction.create(
    _lib.isl_union_set_is_params,
    Keep(UnionSet),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_set_is_equal = ISLFunction.create(
    _lib.isl_union_set_is_equal,
    Keep(UnionSet),
    Keep(UnionSet),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_set_is_disjoint = ISLFunction.create(
    _lib.isl_union_set_is_disjoint,
    Keep(UnionSet),
    Keep(UnionSet),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_set_is_subset = ISLFunction.create(
    _lib.isl_union_set_is_subset,
    Keep(UnionSet),
    Keep(UnionSet),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_set_is_strict_subset = ISLFunction.create(
    _lib.isl_union_set_is_strict_subset,
    Keep(UnionSet),
    Keep(UnionSet),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_set_n_set = ISLFunction.create(
    _lib.isl_union_set_n_set,
    Keep(UnionSet),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_set_union = ISLFunction.create(
    _lib.isl_union_set_union,
    Take(UnionSet),
    Take(UnionSet),
    return_=Give(UnionSet),
    lib=_lib,
)

_isl_union_set_intersect = ISLFunction.create(
    _lib.isl_union_set_intersect,
    Take(UnionSet),
    Take(UnionSet),
    return_=Give(UnionSet),
    lib=_lib,
)

_isl_union_set_subtract = ISLFunction.create(
    _lib.isl_union_set_subtract,
    Take(UnionSet),
    Take(UnionSet),
    return_=Give(UnionSet),
    lib=_lib,
)

_isl_union_set_product = ISLFunction.create(
    _lib.isl_union_set_product,
    Take(UnionSet),
    Take(UnionSet),
    return_=Give(UnionSet),
    lib=_lib,
)

_isl_union_set_gist = ISLFunction.create(
    _lib.isl_union_set_gist,
    Take(UnionSet),
    Take(UnionSet),
    return_=Give(UnionSet),
    lib=_lib,
)

_isl_union_set_coalesce = ISLFunction.create(
    _lib.isl_union_set_coalesce,
    Take(UnionSet),
    return_=Give(UnionSet),
    lib=_lib,
)

_isl_union_set_detect_equalities = ISLFunction.create(
    _lib.isl_union_set_detect_equalities,
    Take(UnionSet),
    return_=Give(UnionSet),
    lib=_lib,
)

_isl_union_set_remove_redundancies = ISLFunction.create(
    _lib.isl_union_set_remove_redundancies,
    Take(UnionSet),
    return_=Give(UnionSet),
    lib=_lib,
)

_isl_union_set_project_out_all_params = ISLFunction.create(
    _lib.isl_union_set_project_out_all_params,
    Take(UnionSet),
    return_=Give(UnionSet),
    lib=_lib,
)

_isl_union_set_lift = ISLFunction.create(
    _lib.isl_union_set_lift,
    Take(UnionSet),
    return_=Give(UnionSet),
    lib=_lib,
)

_isl_union_set_drop_unused_params = ISLFunction.create(
    _lib.isl_union_set_drop_unused_params
    if hasattr(_lib, "isl_union_set_drop_unused_params")
    else None,
    Take(UnionSet),
    return_=Give(UnionSet),
    lib=_lib,
) if hasattr(_lib, "isl_union_set_drop_unused_params") else None

_isl_union_set_lexmin = ISLFunction.create(
    _lib.isl_union_set_lexmin,
    Take(UnionSet),
    return_=Give(UnionSet),
    lib=_lib,
)

_isl_union_set_lexmax = ISLFunction.create(
    _lib.isl_union_set_lexmax,
    Take(UnionSet),
    return_=Give(UnionSet),
    lib=_lib,
)

_isl_union_set_params = ISLFunction.create(
    _lib.isl_union_set_params,
    Take(UnionSet),
    return_=Give(Set),
    lib=_lib,
)

_isl_union_set_list_alloc = ISLFunction.create(
    _lib.isl_union_set_list_alloc,
    Context(),
    Param(int, ctype=c_int),
    return_=Give(lambda: UnionSetList),  # type: ignore[arg-type]
    lib=_lib,
)

_isl_union_set_list_from_union_set = ISLFunction.create(
    _lib.isl_union_set_list_from_union_set,
    Take(UnionSet),
    return_=Give(lambda: UnionSetList),  # type: ignore[arg-type]
    lib=_lib,
)

_isl_union_set_list_copy = ISLFunction.create(
    _lib.isl_union_set_list_copy,
    Keep(lambda: UnionSetList),  # type: ignore[arg-type]
    return_=Give(lambda: UnionSetList),  # type: ignore[arg-type]
    lib=_lib,
)

_isl_union_set_list_free = ISLFunction.create(
    _lib.isl_union_set_list_free,
    Take(lambda: UnionSetList),  # type: ignore[arg-type]
    return_=Null(),
    lib=_lib,
)

_isl_union_set_list_add = ISLFunction.create(
    _lib.isl_union_set_list_add,
    Take(lambda: UnionSetList),  # type: ignore[arg-type]
    Take(UnionSet),
    return_=Give(lambda: UnionSetList),  # type: ignore[arg-type]
    lib=_lib,
)

_isl_union_set_list_n_union_set = ISLFunction.create(
    _lib.isl_union_set_list_n_union_set,
    Keep(lambda: UnionSetList),  # type: ignore[arg-type]
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_set_list_get_union_set = ISLFunction.create(
    _lib.isl_union_set_list_get_union_set,
    Keep(lambda: UnionSetList),  # type: ignore[arg-type]
    Param(int, ctype=c_int),
    return_=Give(UnionSet),
    lib=_lib,
)

__all__ = ["UnionSet", "UnionSetList"]


class UnionSetList(ISLObject):
    __slots__ = ()

    @classmethod
    def alloc(cls, ctx: Context, min_size: int = 0) -> "UnionSetList":
        return _isl_union_set_list_alloc(ctx, min_size)

    @classmethod
    def from_union_set(cls, uset: "UnionSet") -> "UnionSetList":
        return _isl_union_set_list_from_union_set(uset)

    def add(self, uset: "UnionSet") -> "UnionSetList":
        return _isl_union_set_list_add(self, uset)

    def n_union_set(self) -> int:
        return _isl_union_set_list_n_union_set(self)

    def get(self, pos: int) -> "UnionSet":
        return _isl_union_set_list_get_union_set(self, pos)

    def copy_handle(self) -> FfiPointer:
        return _isl_union_set_list_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_union_set_list_free(handle)
