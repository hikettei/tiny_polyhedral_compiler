from __future__ import annotations

import abc
from typing import Any, Callable, Optional

from .ffi import FfiPointer
from .obj import ISLObject

class Qualifier(abc.ABC):
    """Base descriptor for ISL argument/return policy.

    Implementations may operate on :class:`ISLObject` instances or raw FFI
    pointers (when wrapping return values directly from ``libisl``).
    """

    requires_argument: bool = True
    def __init__(self, target: Optional[type] = None) -> None:
        self.target = target

    def prepare(self, value: Any, *, ctx: ISLContext, name: str) -> Any:
        self._validate_type(value, name)
        return self.view(value)

    def wrap(self, value: Any, *, ctx: ISLContext, name: str = "return") -> Any:
        self._validate_type(value, name)
        return self.view(value)

    def describe(self) -> str:
        return type(self).__name__.lower()

    def view(self, value: Any) -> Any:
        return value

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

    def prepare(self, value: Any, *, ctx: ISLContext, name: str) -> ISLObject:
        obj = self._expect_isl_object(value, name)
        return self.view(obj)

    def view(self, obj: ISLObject) -> ISLObject:
        return obj


class Take(_ISLObjectQualifier):
    def view(self, obj: ISLObject) -> ISLObject:
        if getattr(obj, "_in_place", False):
            obj._in_place = False
            return obj
        return obj.copy()

class Give(Qualifier):
    """Convert raw libisl pointers into freshly wrapped :class:`ISLObject`s."""

    def __init__(self, target: Optional[type] = None) -> None:
        super().__init__(target=target)

    def prepare(self, value: Any, *, ctx, name: str) -> ISLObject:  # type: ignore[override]
        return self.view(value)

    def wrap(self, value: Any, *, ctx, name: str = "return") -> ISLObject:  # type: ignore[override]
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

class Keep(_ISLObjectQualifier):
    def view(self, obj: ISLObject) -> ISLObject:
        return obj

class Null(Qualifier):
    def __init__(self) -> None:
        super().__init__(target=type(None))

    def prepare(self, value: Any, *, ctx: ISLContext, name: str) -> None:
        if value is not None:
            raise TypeError(f"Argument '{name}' must be None to satisfy Null qualifier.")
        return None

class Param(Qualifier):
    def __init__(self, target: Optional[type] = None, *, converter: Optional[Callable[[Any], Any]] = None) -> None:
        super().__init__(target=target)
        self.converter = converter

    def prepare(self, value: Any, *, ctx: ISLContext, name: str) -> Any:
        if self.converter is not None:
            value = self.converter(value)
        self._validate_type(value, name)
        return self.view(value)
