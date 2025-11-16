from __future__ import annotations

from ctypes import c_int, c_void_p

from ..ffi import FfiPointer, load_libisl
from ..func import ISLFunction
from ..obj import ISLObject
from ..qualifier import Give, Keep, Null, Param, Take
from .context import Context
from .space import Space

_lib = load_libisl()


class Aff(ISLObject):
    """Wrapper for ``isl_aff``."""

    __slots__ = ()

    def __init__(self, handle_or_spec: str | FfiPointer) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_aff_read_from_str(handle_or_spec, return_raw_pointer=True)
        else:
            handle = handle_or_spec
        super().__init__(handle)

    @classmethod
    def from_str(cls, spec: str) -> "Aff":
        return _isl_aff_read_from_str(spec)

    def copy_handle(self) -> FfiPointer:
        return _isl_aff_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_aff_free(handle)

    def dim(self, dim_type: int) -> int:
        return _isl_aff_dim(self, dim_type)

    def get_space(self) -> Space:
        return _isl_aff_get_space(self)

    def get_domain_space(self) -> Space:
        return _isl_aff_get_domain_space(self)

    def plain_is_equal(self, other: "Aff") -> bool:
        return _isl_aff_plain_is_equal(self, other)

    def to_pw_aff(self) -> "PwAff":
        return _isl_pw_aff_from_aff(self)

    @classmethod
    def zero_on_domain_space(cls, space: Space) -> "Aff":
        return _isl_aff_zero_on_domain_space(space)

    def __str__(self) -> str:  # pragma: no cover
        return _isl_aff_to_str(self)

    def __repr__(self) -> str:  # pragma: no cover
        return f"Aff({self.__str__()})"


class PwAff(ISLObject):
    """Wrapper for ``isl_pw_aff``."""

    __slots__ = ()

    def __init__(self, handle_or_spec: str | FfiPointer) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_pw_aff_read_from_str(handle_or_spec, return_raw_pointer=True)
        else:
            handle = handle_or_spec
        super().__init__(handle)

    @classmethod
    def from_str(cls, spec: str) -> "PwAff":
        return _isl_pw_aff_read_from_str(spec)

    @classmethod
    def from_aff(cls, aff: Aff) -> "PwAff":
        return _isl_pw_aff_from_aff(aff)

    def copy_handle(self) -> FfiPointer:
        return _isl_pw_aff_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_pw_aff_free(handle)

    def dim(self, dim_type: int) -> int:
        return _isl_pw_aff_dim(self, dim_type)

    def get_space(self) -> Space:
        return _isl_pw_aff_get_space(self)

    def get_domain_space(self) -> Space:
        return _isl_pw_aff_get_domain_space(self)

    def plain_is_equal(self, other: "PwAff") -> bool:
        return _isl_pw_aff_plain_is_equal(self, other)

    def __str__(self) -> str:  # pragma: no cover
        return _isl_pw_aff_to_str(self)

    def __repr__(self) -> str:  # pragma: no cover
        return f"PwAff({self.__str__()})"


class MultiAff(ISLObject):
    """Wrapper for ``isl_multi_aff``."""

    __slots__ = ()

    def __init__(self, handle_or_spec: str | FfiPointer) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_multi_aff_read_from_str(handle_or_spec, return_raw_pointer=True)
        else:
            handle = handle_or_spec
        super().__init__(handle)

    @classmethod
    def from_str(cls, spec: str) -> "MultiAff":
        return _isl_multi_aff_read_from_str(spec)

    def copy_handle(self) -> FfiPointer:
        return _isl_multi_aff_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_multi_aff_free(handle)

    def dim(self, dim_type: int) -> int:
        return _isl_multi_aff_dim(self, dim_type)

    def __str__(self) -> str:  # pragma: no cover
        return _isl_multi_aff_to_str(self)

    def __repr__(self) -> str:  # pragma: no cover
        return f"MultiAff({self.__str__()})"


class PwMultiAff(ISLObject):
    """Wrapper for ``isl_pw_multi_aff``."""

    __slots__ = ()

    def __init__(self, handle_or_spec: str | FfiPointer) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_pw_multi_aff_read_from_str(handle_or_spec, return_raw_pointer=True)
        else:
            handle = handle_or_spec
        super().__init__(handle)

    @classmethod
    def from_str(cls, spec: str) -> "PwMultiAff":
        return _isl_pw_multi_aff_read_from_str(spec)

    def copy_handle(self) -> FfiPointer:
        return _isl_pw_multi_aff_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_pw_multi_aff_free(handle)

    def dim(self, dim_type: int) -> int:
        return _isl_pw_multi_aff_dim(self, dim_type)

    def __str__(self) -> str:  # pragma: no cover
        return _isl_pw_multi_aff_to_str(self)

    def __repr__(self) -> str:  # pragma: no cover
        return f"PwMultiAff({self.__str__()})"


# --- Aff primitives ---

_isl_aff_read_from_str = ISLFunction.create(
    _lib.isl_aff_read_from_str,
    Context(),
    Param(str),
    return_=Give(Aff),
    lib=_lib,
)

_isl_aff_copy = ISLFunction.create(
    _lib.isl_aff_copy,
    Keep(Aff),
    return_=Give(Aff),
    lib=_lib,
)

_isl_aff_free = ISLFunction.create(
    _lib.isl_aff_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

_isl_aff_dim = ISLFunction.create(
    _lib.isl_aff_dim,
    Keep(Aff),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_aff_get_space = ISLFunction.create(
    _lib.isl_aff_get_space,
    Keep(Aff),
    return_=Give(Space),
    lib=_lib,
)

_isl_aff_get_domain_space = ISLFunction.create(
    _lib.isl_aff_get_domain_space,
    Keep(Aff),
    return_=Give(Space),
    lib=_lib,
)

_isl_aff_plain_is_equal = ISLFunction.create(
    _lib.isl_aff_plain_is_equal,
    Keep(Aff),
    Keep(Aff),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_aff_to_str = ISLFunction.create(
    _lib.isl_aff_to_str,
    Keep(Aff),
    return_=Param(str),
    lib=_lib,
)

_isl_aff_zero_on_domain_space = ISLFunction.create(
    _lib.isl_aff_zero_on_domain_space,
    Take(Space),
    return_=Give(Aff),
    lib=_lib,
)

# --- PwAff primitives ---

_isl_pw_aff_read_from_str = ISLFunction.create(
    _lib.isl_pw_aff_read_from_str,
    Context(),
    Param(str),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_copy = ISLFunction.create(
    _lib.isl_pw_aff_copy,
    Keep(PwAff),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_free = ISLFunction.create(
    _lib.isl_pw_aff_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

_isl_pw_aff_dim = ISLFunction.create(
    _lib.isl_pw_aff_dim,
    Keep(PwAff),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_get_space = ISLFunction.create(
    _lib.isl_pw_aff_get_space,
    Keep(PwAff),
    return_=Give(Space),
    lib=_lib,
)

_isl_pw_aff_get_domain_space = ISLFunction.create(
    _lib.isl_pw_aff_get_domain_space,
    Keep(PwAff),
    return_=Give(Space),
    lib=_lib,
)

_isl_pw_aff_plain_is_equal = ISLFunction.create(
    _lib.isl_pw_aff_plain_is_equal,
    Keep(PwAff),
    Keep(PwAff),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_from_aff = ISLFunction.create(
    _lib.isl_pw_aff_from_aff,
    Take(Aff),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_to_str = ISLFunction.create(
    _lib.isl_pw_aff_to_str,
    Keep(PwAff),
    return_=Param(str),
    lib=_lib,
)

# --- MultiAff primitives ---

_isl_multi_aff_read_from_str = ISLFunction.create(
    _lib.isl_multi_aff_read_from_str,
    Context(),
    Param(str),
    return_=Give(MultiAff),
    lib=_lib,
)

_isl_multi_aff_copy = ISLFunction.create(
    _lib.isl_multi_aff_copy,
    Keep(MultiAff),
    return_=Give(MultiAff),
    lib=_lib,
)

_isl_multi_aff_free = ISLFunction.create(
    _lib.isl_multi_aff_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

_isl_multi_aff_dim = ISLFunction.create(
    _lib.isl_multi_aff_dim,
    Keep(MultiAff),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_multi_aff_to_str = ISLFunction.create(
    _lib.isl_multi_aff_to_str,
    Keep(MultiAff),
    return_=Param(str),
    lib=_lib,
)

# --- PwMultiAff primitives ---

_isl_pw_multi_aff_read_from_str = ISLFunction.create(
    _lib.isl_pw_multi_aff_read_from_str,
    Context(),
    Param(str),
    return_=Give(PwMultiAff),
    lib=_lib,
)

_isl_pw_multi_aff_copy = ISLFunction.create(
    _lib.isl_pw_multi_aff_copy,
    Keep(PwMultiAff),
    return_=Give(PwMultiAff),
    lib=_lib,
)

_isl_pw_multi_aff_free = ISLFunction.create(
    _lib.isl_pw_multi_aff_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

_isl_pw_multi_aff_dim = ISLFunction.create(
    _lib.isl_pw_multi_aff_dim,
    Keep(PwMultiAff),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_pw_multi_aff_to_str = ISLFunction.create(
    _lib.isl_pw_multi_aff_to_str,
    Keep(PwMultiAff),
    return_=Param(str),
    lib=_lib,
)

__all__ = ["Aff", "PwAff", "MultiAff", "PwMultiAff"]
