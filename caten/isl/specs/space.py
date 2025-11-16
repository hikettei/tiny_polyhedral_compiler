from __future__ import annotations

from ctypes import c_int, c_void_p

from ..ffi import FfiPointer, load_libisl
from ..func import ISLFunction
from ..obj import ISLObject
from ..qualifier import Give, Keep, Null, Param, Take
from .context import Context

_lib = load_libisl()


class Space(ISLObject):
    """Wrapper around ``isl_space``."""

    __slots__ = ()

    def __init__(self, handle_or_spec: str | FfiPointer) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_space_read_from_str(handle_or_spec, return_raw_pointer=True)
        else:
            handle = handle_or_spec
        super().__init__(handle)

    @classmethod
    def alloc(cls, n_param: int, n_in: int, n_out: int) -> "Space":
        return _isl_space_alloc(n_param, n_in, n_out)

    @classmethod
    def params_alloc(cls, n_param: int) -> "Space":
        return _isl_space_params_alloc(n_param)

    @classmethod
    def set_alloc(cls, n_param: int, n_set: int) -> "Space":
        return _isl_space_set_alloc(n_param, n_set)

    @classmethod
    def from_str(cls, spec: str) -> "Space":
        return _isl_space_read_from_str(spec)

    def copy_handle(self) -> FfiPointer:
        return _isl_space_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_space_free(handle)

    def dim(self, dim_type: int) -> int:
        return _isl_space_dim(self, dim_type)

    def get_tuple_name(self, dim_type: int) -> str | None:
        return _isl_space_get_tuple_name(self, dim_type)

    def set_tuple_name(self, dim_type: int, name: str) -> "Space":
        return _isl_space_set_tuple_name(self, dim_type, name)

    def __str__(self) -> str:  # pragma: no cover
        return _isl_space_to_str(self)

    def __repr__(self) -> str:  # pragma: no cover
        return f"Space({self.__str__()})"


class LocalSpace(ISLObject):
    """Wrapper around ``isl_local_space``."""

    __slots__ = ()

    def __init__(self, handle: FfiPointer) -> None:
        super().__init__(handle)

    @classmethod
    def from_space(cls, space: Space) -> "LocalSpace":
        return _isl_local_space_from_space(space)

    def copy_handle(self) -> FfiPointer:
        return _isl_local_space_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_local_space_free(handle)

    def dim(self, dim_type: int) -> int:
        return _isl_local_space_dim(self, dim_type)

    def __str__(self) -> str:  # pragma: no cover
        return _isl_local_space_to_str(self)

    def __repr__(self) -> str:  # pragma: no cover
        return f"LocalSpace({self.__str__()})"


# --- Space primitives ---

_isl_space_alloc = ISLFunction.create(
    _lib.isl_space_alloc,
    Context(),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Space),
    lib=_lib,
)

_isl_space_params_alloc = ISLFunction.create(
    _lib.isl_space_params_alloc,
    Context(),
    Param(int, ctype=c_int),
    return_=Give(Space),
    lib=_lib,
)

_isl_space_set_alloc = ISLFunction.create(
    _lib.isl_space_set_alloc,
    Context(),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Space),
    lib=_lib,
)

_isl_space_read_from_str = ISLFunction.create(
    _lib.isl_space_read_from_str,
    Context(),
    Param(str),
    return_=Give(Space),
    lib=_lib,
)

_isl_space_copy = ISLFunction.create(
    _lib.isl_space_copy,
    Keep(Space),
    return_=Give(Space),
    lib=_lib,
)

_isl_space_free = ISLFunction.create(
    _lib.isl_space_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

_isl_space_dim = ISLFunction.create(
    _lib.isl_space_dim,
    Keep(Space),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_space_get_tuple_name = ISLFunction.create(
    _lib.isl_space_get_tuple_name,
    Keep(Space),
    Param(int, ctype=c_int),
    return_=Param(str),
    lib=_lib,
)

_isl_space_set_tuple_name = ISLFunction.create(
    _lib.isl_space_set_tuple_name,
    Take(Space),
    Param(int, ctype=c_int),
    Param(str),
    return_=Give(Space),
    lib=_lib,
)

_isl_space_to_str = ISLFunction.create(
    _lib.isl_space_to_str,
    Keep(Space),
    return_=Param(str),
    lib=_lib,
)


# --- LocalSpace primitives ---

_isl_local_space_from_space = ISLFunction.create(
    _lib.isl_local_space_from_space,
    Take(Space),
    return_=Give(LocalSpace),
    lib=_lib,
)

_isl_local_space_copy = ISLFunction.create(
    _lib.isl_local_space_copy,
    Keep(LocalSpace),
    return_=Give(LocalSpace),
    lib=_lib,
)

_isl_local_space_free = ISLFunction.create(
    _lib.isl_local_space_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

_isl_local_space_dim = ISLFunction.create(
    _lib.isl_local_space_dim,
    Keep(LocalSpace),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_local_space_to_str = ISLFunction.create(
    _lib.isl_local_space_to_str,
    Keep(LocalSpace),
    return_=Param(str),
    lib=_lib,
)

__all__ = ["Space", "LocalSpace"]

