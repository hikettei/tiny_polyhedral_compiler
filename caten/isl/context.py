from __future__ import annotations

import contextvars
from typing import Any, Callable, Optional


class ISLContextError(RuntimeError):
    """Raised when ISL operations are attempted without a valid context."""


_current_context: contextvars.ContextVar[Optional["ISLContext"]]
_current_context = contextvars.ContextVar("caten_isl_context", default=None)


class ISLContext:
    """Per-thread owner of all live ISL handles.

    The context does not yet talk to `islpy.Context`; instead it orchestrates
    object lifetime, enforces stack discipline, and offers hooks for future
    integration with the primitive layer.
    """

    __slots__ = (
        "name",
        "_prim_factory",
        "_prim_context",
        "_token",
        "_closed",
    )

    def __init__(self, *, prim_context_factory: Optional[Callable[[], Any]] = None, name: str = "isl") -> None:
        self.name = name
        self._prim_factory = prim_context_factory
        self._prim_context: Any = None
        self._token: Optional[contextvars.Token[Any]] = None
        self._closed = False

    # ------------------------------------------------------------------
    # Context stack helpers
    # ------------------------------------------------------------------
    @classmethod
    def current(cls, *, required: bool = False) -> Optional["ISLContext"]:
        ctx = _current_context.get()
        if ctx is None and required:
            raise ISLContextError("No active ISL context; wrap code with `with I.context():`. ")
        return ctx

    def ensure_active(self) -> None:
        if self._token is None:
            raise ISLContextError("ISLContext must be entered via `with` before use.")
        if self._closed:
            raise ISLContextError("ISLContext was already closed.")

    @property
    def closed(self) -> bool:
        return self._closed

    @property
    def prim(self) -> Any:
        """Underlying primitive context (if any)."""

        return self._prim_context

    # ------------------------------------------------------------------
    # Context manager protocol
    # ------------------------------------------------------------------
    def __enter__(self) -> "ISLContext":
        if self._token is not None:
            raise ISLContextError("Context is already active on this instance.")
        if self._closed:
            raise ISLContextError("Re-entering a closed ISLContext is not allowed.")
        if self._prim_factory is not None:
            self._prim_context = self._prim_factory()
        self._token = _current_context.set(self)
        return self

    def __exit__(self, exc_type, exc, tb) -> None:
        if self._token is not None:
            _current_context.reset(self._token)
            self._token = None
        self._prim_context = None
        self._closed = True


def context(*, prim_context_factory: Optional[Callable[[], Any]] = None, name: str = "isl") -> ISLContext:
    """Convenience factory mirroring ``with I.context():`` syntax."""

    return ISLContext(prim_context_factory=prim_context_factory, name=name)
