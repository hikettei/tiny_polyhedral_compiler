from __future__ import annotations

import abc
import weakref
from typing import Any, Type, TypeVar

from .ffi import FfiPointer

T_ISLObject = TypeVar("T_ISLObject", bound="ISLObject")

class ISLObject(abc.ABC):
    """GC-reachable wrapper around raw libisl handles obtained via FFI.

    Slots
    -----
    _handle : FfiPointer | None
        Opaque pointer returned by the C FFI; ``None`` signals that the wrapper
        was consumed or freed.
    _in_place : bool
        Marks whether the next ``Take`` qualifier may steal the handle instead
        of cloning it.
    _finalizer : weakref.finalize
        Automatically calls :meth:`free_handle` when the wrapper becomes
        unreachable.
    """

    __slots__ = ("_handle", "_in_place", "_finalizer")
    def __init__(self, handle: FfiPointer) -> None:
        self._handle: FfiPointer = handle
        self._in_place: bool = False
        self._finalizer = weakref.finalize(self, _run_finalizer, type(self), handle)

    @property
    def handle(self) -> FfiPointer:
        self._assert_usable()
        return self._handle

    def copy(self: T_ISLObject) -> T_ISLObject:
        """Create a logically distinct copy via the primitive layer."""
        self._assert_usable()
        handle = self.copy_handle()
        cls: Type[T_ISLObject] = type(self)
        return cls.from_ptr(handle)

    def free(self) -> None:
        """Release the underlying handle immediately (idempotent)."""
        if self._handle is None:
            return
        self._finalizer()
        self._handle = None

    def request_inplace(self) -> None:
        """Allow the next ``Take`` qualifier to steal the handle."""
        self._assert_usable()
        if self._in_place:
            return
        self._in_place = True

    def _assert_usable(self) -> None:
        """Ensure the object still owns a valid handle."""
        if self._handle is None:
            raise RuntimeError("ISLObject handle was already freed.")

    @abc.abstractmethod
    def copy_handle(self) -> FfiPointer:
        """Return a new primitive handle cloned from the wrapped object."""

    @classmethod
    @abc.abstractmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        """Release ``handle`` via the primitive library (e.g., ``isl_set_free``)."""

    @classmethod
    def from_ptr(cls: Type[T_ISLObject], handle: FfiPointer) -> T_ISLObject:
        """Wrap a raw isl handle in this subclass."""
        return cls(handle)


    def __repr__(self) -> str:  # pragma: no cover - debugging helper
        status = "freed" if self._handle is None else hex(id(self._handle))
        return f"<{type(self).__name__} handle={status}>"

def InPlace(obj: T_ISLObject) -> T_ISLObject:
    """Mark ``obj`` so the next ``Take`` qualifier moves its handle."""
    if not isinstance(obj, ISLObject):
        raise TypeError("InPlace expects an ISLObject instance.")
    obj.request_inplace()
    return obj

def _run_finalizer(cls: type[ISLObject], handle: Any) -> None:
    """Invoke :meth:`free_handle` if ``handle`` is non-null."""
    if handle is None:
        return
    cls.free_handle(handle)
