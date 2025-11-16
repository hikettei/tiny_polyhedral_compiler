from __future__ import annotations

from ctypes import c_int, c_void_p

from ..ffi import FfiPointer, load_libisl
from ..func import ISLFunction
from ..obj import ISLObject
from ..qualifier import Give, Keep, Null, Param, Take
from .aff import Aff, PwAff
from .context import Context
from .space import Space

_lib = load_libisl()


class UnionPwAff(ISLObject):
    """Wrapper for ``isl_union_pw_aff``."""

    __slots__ = ()

    def __init__(self, handle_or_spec: str | FfiPointer) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_union_pw_aff_read_from_str(handle_or_spec, return_raw_pointer=True)
        else:
            handle = handle_or_spec
        super().__init__(handle)

    @classmethod
    def from_str(cls, spec: str) -> "UnionPwAff":
        return _isl_union_pw_aff_read_from_str(spec)

    @classmethod
    def empty(cls, space: Space) -> "UnionPwAff":
        return _isl_union_pw_aff_empty_space(space)

    @classmethod
    def from_pw_aff(cls, pa: PwAff) -> "UnionPwAff":
        return _isl_union_pw_aff_from_pw_aff(pa)

    @classmethod
    def from_aff(cls, aff: Aff) -> "UnionPwAff":
        return _isl_union_pw_aff_from_aff(aff)

    def copy_handle(self) -> FfiPointer:
        return _isl_union_pw_aff_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_union_pw_aff_free(handle)

    def dim(self, dim_type: int) -> int:
        return _isl_union_pw_aff_dim(self, dim_type)

    def get_space(self) -> Space:
        return _isl_union_pw_aff_get_space(self)

    def is_equal(self, other: "UnionPwAff") -> bool:
        return _isl_union_pw_aff_is_equal(self, other)

    def add_pw_aff(self, pa: PwAff) -> "UnionPwAff":
        return _isl_union_pw_aff_add_pw_aff(self, pa)

    def __str__(self) -> str:  # pragma: no cover
        return _isl_union_pw_aff_to_str(self)

    def __repr__(self) -> str:  # pragma: no cover
        return f"UnionPwAff({self.__str__()})"


class UnionPwMultiAff(ISLObject):
    """Wrapper for ``isl_union_pw_multi_aff``."""

    __slots__ = ()

    def __init__(self, handle_or_spec: str | FfiPointer) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_union_pw_multi_aff_read_from_str(handle_or_spec, return_raw_pointer=True)
        else:
            handle = handle_or_spec
        super().__init__(handle)

    @classmethod
    def from_str(cls, spec: str) -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_read_from_str(spec)

    def copy_handle(self) -> FfiPointer:
        return _isl_union_pw_multi_aff_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_union_pw_multi_aff_free(handle)

    def dim(self, dim_type: int) -> int:
        return _isl_union_pw_multi_aff_dim(self, dim_type)

    def __str__(self) -> str:  # pragma: no cover
        return _isl_union_pw_multi_aff_to_str(self)

    def __repr__(self) -> str:  # pragma: no cover
        return f"UnionPwMultiAff({self.__str__()})"


class MultiUnionPwAff(ISLObject):
    """Wrapper for ``isl_multi_union_pw_aff``."""

    __slots__ = ()

    def __init__(self, handle_or_spec: str | FfiPointer) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_multi_union_pw_aff_read_from_str(handle_or_spec, return_raw_pointer=True)
        else:
            handle = handle_or_spec
        super().__init__(handle)

    @classmethod
    def from_str(cls, spec: str) -> "MultiUnionPwAff":
        return _isl_multi_union_pw_aff_read_from_str(spec)

    def copy_handle(self) -> FfiPointer:
        return _isl_multi_union_pw_aff_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_multi_union_pw_aff_free(handle)

    def dim(self, dim_type: int) -> int:
        return _isl_multi_union_pw_aff_dim(self, dim_type)

    def __str__(self) -> str:  # pragma: no cover
        return _isl_multi_union_pw_aff_to_str(self)

    def __repr__(self) -> str:  # pragma: no cover
        return f"MultiUnionPwAff({self.__str__()})"


# UnionPwAff primitives
_isl_union_pw_aff_read_from_str = ISLFunction.create(
    _lib.isl_union_pw_aff_read_from_str,
    Context(),
    Param(str),
    return_=Give(UnionPwAff),
    lib=_lib,
)

_isl_union_pw_aff_copy = ISLFunction.create(
    _lib.isl_union_pw_aff_copy,
    Keep(UnionPwAff),
    return_=Give(UnionPwAff),
    lib=_lib,
)

_isl_union_pw_aff_free = ISLFunction.create(
    _lib.isl_union_pw_aff_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

_isl_union_pw_aff_dim = ISLFunction.create(
    _lib.isl_union_pw_aff_dim,
    Keep(UnionPwAff),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_aff_get_space = ISLFunction.create(
    _lib.isl_union_pw_aff_get_space,
    Keep(UnionPwAff),
    return_=Give(Space),
    lib=_lib,
)

_isl_union_pw_aff_is_equal = ISLFunction.create(
    _lib.isl_union_pw_aff_is_equal
    if hasattr(_lib, "isl_union_pw_aff_is_equal")
    else _lib.isl_union_pw_aff_plain_is_equal,
    Keep(UnionPwAff),
    Keep(UnionPwAff),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_aff_add_pw_aff = ISLFunction.create(
    _lib.isl_union_pw_aff_add_pw_aff,
    Take(UnionPwAff),
    Take(PwAff),
    return_=Give(UnionPwAff),
    lib=_lib,
)

_isl_union_pw_aff_empty_space = ISLFunction.create(
    _lib.isl_union_pw_aff_empty_space,
    Take(Space),
    return_=Give(UnionPwAff),
    lib=_lib,
)

_isl_union_pw_aff_from_pw_aff = ISLFunction.create(
    _lib.isl_union_pw_aff_from_pw_aff,
    Take(PwAff),
    return_=Give(UnionPwAff),
    lib=_lib,
)

_isl_union_pw_aff_from_aff = ISLFunction.create(
    _lib.isl_union_pw_aff_from_aff,
    Take(Aff),
    return_=Give(UnionPwAff),
    lib=_lib,
)

_isl_union_pw_aff_to_str = ISLFunction.create(
    _lib.isl_union_pw_aff_to_str,
    Keep(UnionPwAff),
    return_=Param(str),
    lib=_lib,
)


# UnionPwMultiAff primitives
_isl_union_pw_multi_aff_read_from_str = ISLFunction.create(
    _lib.isl_union_pw_multi_aff_read_from_str,
    Context(),
    Param(str),
    return_=Give(UnionPwMultiAff),
    lib=_lib,
)

_isl_union_pw_multi_aff_copy = ISLFunction.create(
    _lib.isl_union_pw_multi_aff_copy,
    Keep(UnionPwMultiAff),
    return_=Give(UnionPwMultiAff),
    lib=_lib,
)

_isl_union_pw_multi_aff_free = ISLFunction.create(
    _lib.isl_union_pw_multi_aff_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

_isl_union_pw_multi_aff_dim = ISLFunction.create(
    _lib.isl_union_pw_multi_aff_dim,
    Keep(UnionPwMultiAff),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_multi_aff_to_str = ISLFunction.create(
    _lib.isl_union_pw_multi_aff_to_str,
    Keep(UnionPwMultiAff),
    return_=Param(str),
    lib=_lib,
)


# MultiUnionPwAff primitives
_isl_multi_union_pw_aff_read_from_str = ISLFunction.create(
    _lib.isl_multi_union_pw_aff_read_from_str,
    Context(),
    Param(str),
    return_=Give(MultiUnionPwAff),
    lib=_lib,
)

_isl_multi_union_pw_aff_copy = ISLFunction.create(
    _lib.isl_multi_union_pw_aff_copy,
    Keep(MultiUnionPwAff),
    return_=Give(MultiUnionPwAff),
    lib=_lib,
)

_isl_multi_union_pw_aff_free = ISLFunction.create(
    _lib.isl_multi_union_pw_aff_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

_isl_multi_union_pw_aff_dim = ISLFunction.create(
    _lib.isl_multi_union_pw_aff_dim,
    Keep(MultiUnionPwAff),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_multi_union_pw_aff_to_str = ISLFunction.create(
    _lib.isl_multi_union_pw_aff_to_str,
    Keep(MultiUnionPwAff),
    return_=Param(str),
    lib=_lib,
)

__all__ = [
    "UnionPwAff",
    "UnionPwMultiAff",
    "MultiUnionPwAff",
]
