from __future__ import annotations

import abc
from ctypes import c_char_p, c_double, c_int, c_longlong, c_void_p
from typing import TYPE_CHECKING, Any, Optional

from .ffi import FfiPointer
from .obj import ISLObject

if TYPE_CHECKING:  # pragma: no cover
    from .specs.context import Context


class Qualifier(abc.ABC):
    """Base descriptor for ISL argument/return policy.

    Implementations may operate on :class:`ISLObject` instances or raw FFI
    pointers (when wrapping return values directly from ``libisl``).
    """

    requires_argument: bool = True
    def __init__(self, target: Optional[type] = None) -> None:
        self.target = target

    def prepare(self, value: Any, *, ctx: "Context" | None, name: str) -> Any:
        self._validate_type(value, name)
        return self.view(value)

    def wrap(self, value: Any, *, ctx: "Context" | None, name: str = "return") -> Any:
        self._validate_type(value, name)
        return self.view(value)

    def describe(self) -> str:
        return type(self).__name__.lower()

    def view(self, value: Any) -> Any:
        return value

    @abc.abstractmethod
    def as_ctype(self) -> Any | None:
        """Return a ctypes type used to auto-configure libisl symbols."""

    def _validate_type(self, value: Any, name: str) -> None:
        if self.target is None or value is None:
            return
        if isinstance(self.target, tuple):
            if not isinstance(value, self.target):
                expected = ", ".join(t.__name__ for t in self.target)
                raise TypeError(f"Argument '{name}' must be one of ({expected}).")
            return
        if not isinstance(value, self.target):
            raise TypeError(f"Argument '{name}' must be {self.target.__name__}.")

class _ISLObjectQualifier(Qualifier):
    def _expect_isl_object(self, value: Any, name: str) -> ISLObject:
        if not isinstance(value, ISLObject):
            raise TypeError(f"Argument '{name}' must be an ISLObject instance.")
        if self.target is not None and not isinstance(value, self.target):
            raise TypeError(
                f"Argument '{name}' expects {self.target.__name__}, got {type(value).__name__}."
            )
        value._assert_usable()
        return value

    def prepare(self, value: Any, *, ctx: "Context" | None, name: str) -> FfiPointer:
        obj = self._expect_isl_object(value, name)
        return self.view(obj)

    def view(self, obj: ISLObject) -> FfiPointer:
        return obj.handle

    def as_ctype(self) -> Any:
        return c_void_p

class Take(_ISLObjectQualifier):
    def view(self, obj: ISLObject) -> FfiPointer:
        if getattr(obj, "_in_place", False):
            obj._in_place = False
            obj._finalizer.detach()
            handle = obj.handle
            obj._handle = None
            return handle
        return obj.copy_handle()

class Give(Qualifier):
    """Convert raw libisl pointers into freshly wrapped :class:`ISLObject`s."""

    def __init__(self, target: Optional[type] = None) -> None:
        super().__init__(target=target)

    def prepare(self, value: Any, *, ctx: "Context" | None, name: str) -> ISLObject:  # type: ignore[override]
        return self.view(value)

    def wrap(self, value: Any, *, ctx: "Context" | None, name: str = "return") -> ISLObject:  # type: ignore[override]
        return self.view(value)

    def view(self, value: Any) -> ISLObject:
        if isinstance(value, ISLObject):
            return value.copy()
        target = self.target
        if target is None or not issubclass(target, ISLObject):
            raise TypeError("Give qualifier requires an ISLObject subclass target.")
        if not isinstance(value, FfiPointer):
            raise TypeError("Give qualifier expects an FFI pointer.")
        return target.from_ptr(value)

    def as_ctype(self) -> Any:
        return c_void_p

class Keep(_ISLObjectQualifier):
    def view(self, obj: ISLObject) -> FfiPointer:
        return obj.handle

class Null(Qualifier):
    def __init__(self) -> None:
        super().__init__(target=type(None))

    def prepare(self, value: Any, *, ctx: "ISLContext" | None, name: str) -> None:
        if value is not None:
            raise TypeError(f"Argument '{name}' must be None to satisfy Null qualifier.")
        return None

    def as_ctype(self) -> None:
        return None

class Param(Qualifier):
    _PY_CTYPE_MAP = {
        int: c_longlong,
        float: c_double,
        str: c_char_p,
        bytes: c_char_p,
        bool: c_int,
    }
    def __init__(
        self,
        target: Optional[type] = None,
        *,
        ctype: Optional[Any] = None,
    ) -> None:
        super().__init__(target=target)
        self._ctype = ctype or (target and self._PY_CTYPE_MAP.get(target))
    
    def wrap(self, value: Any, *, ctx: "Context" | None, name: str = "return") -> Any:
        if self.target is bool and isinstance(value, int):
            if value == -1 and ctx is not None:
                ctx.raise_isl_error()
            return bool(value)
        if isinstance(value, bytes) and self.target is str:
            value = value.decode("utf-8")
        self._validate_type(value, name)
        return self.view(value)
    
    def prepare(self, value: Any, *, ctx: "ISLContext" | None, name: str) -> Any:
        self._validate_type(value, name)
        if self.target is bool:
            return int(bool(value))
        if isinstance(value, str):
            value = value.encode("utf-8")
        return self.view(value)

    def as_ctype(self) -> Any | None:
        return self._ctype
