from __future__ import annotations

from ..ffi import FfiPointer, load_libisl
from ..func import ISLFunction
from ..obj import ISLObject
from ..qualifier import Give, Param, Take
from .basic_map import BasicMap
from .context import Context

_lib = load_libisl()


class MapList(ISLObject):
    """Wrapper around ``isl_map_list``."""

    __slots__ = ()

    def __init__(self, handle_or_spec: FfiPointer | str) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_map_list_read_from_str(handle_or_spec, return_raw_pointer=True)
        else:
            handle = handle_or_spec
        super().__init__(handle)

    @classmethod
    def from_str(cls, spec: str) -> "MapList":
        return _isl_map_list_read_from_str(spec)

    def copy_handle(self) -> FfiPointer:
        raise NotImplementedError("isl_map_list_copy not available")

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        return None


class BasicMapList(ISLObject):
    """Wrapper around ``isl_basic_map_list``."""

    __slots__ = ()

    def __init__(self, handle: FfiPointer) -> None:
        super().__init__(handle)

    def intersect(self) -> BasicMap:
        return _isl_basic_map_list_intersect(self)

    def copy_handle(self) -> FfiPointer:
        raise NotImplementedError("isl_basic_map_list_copy not available")

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        return None


_isl_map_list_read_from_str = ISLFunction.create(
    _lib.isl_map_list_read_from_str,
    Context(),
    Param(str),
    return_=Give(MapList),
    lib=_lib,
)

_isl_basic_map_list_intersect = ISLFunction.create(
    _lib.isl_basic_map_list_intersect,
    Take(BasicMapList),
    return_=Give(BasicMap),
    lib=_lib,
)

__all__ = ["MapList", "BasicMapList"]
