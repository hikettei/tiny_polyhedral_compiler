from __future__ import annotations

import contextvars
from ctypes import c_void_p
from types import TracebackType
from typing import Any, Callable, Optional, cast

from ..ffi import FfiPointer, load_libisl
from ..func import ISLFunction
from ..obj import ISLObject
from ..qualifier import Give, Null, Param, Qualifier

_lib = load_libisl()

class ISLError(RuntimeError):
    """Raised when libisl signals an operational error."""

class ISLContextError(RuntimeError):
    """Raised when ISL operations are attempted without a valid context."""

_current_context: contextvars.ContextVar[Optional["Context"]]
_current_context = contextvars.ContextVar("caten_isl_context", default=None)

class Context(ISLObject, Qualifier):
    requires_argument = False
    __slots__ = ("name", "_token", "_closed")
    def __init__(self, handle: Optional[FfiPointer] = None) -> None:
        ISLObject.__init__(self, cast(FfiPointer, handle))
        Qualifier.__init__(self)
        self.name = "isl"
        self._token: contextvars.Token[Optional["Context"]] | None = None
        self._closed = False

    def copy_handle(self) -> FfiPointer:
        raise ISLContextError("Context cannot be copied.")

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _lib.isl_ctx_free(handle)

    def raise_isl_error(self) -> None:
        if getattr(self, "_raising", False):  # pragma: no cover - defensive
            raise ISLError("Nested ISL error reporting failed.")
        self._raising = True
        parts: list[str] = []
        try:
            if (msg := _isl_ctx_last_error_msg()):
                parts.append(msg)
            if (file := _isl_ctx_last_error_file()):
                line = _isl_ctx_last_error_line()
                parts.append(f"{file}:{line}" if line is not None else file)
        finally:
            self._raising = False
        raise ISLError(" | ".join(parts) if parts else "Unknown ISL error")

    def __enter__(self) -> "Context":
        if self._token is not None:
            raise ISLContextError("Context is already active on this instance.")
        if self._closed:
            raise ISLContextError("Re-entering a closed Context is not allowed.")
        self._token = _current_context.set(self)
        return self

    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc: BaseException | None,
        tb: TracebackType | None,
    ) -> None:
        if self._token is not None:
            _current_context.reset(self._token)
            self._token = None
        self._closed = True
        self.free()
    # Qualifier protocol ----------------------------------------------------
    def view(self, value: Any) -> FfiPointer:  # type: ignore[override]
        if value is not None:
            raise TypeError("Context qualifier does not accept positional arguments.")
        ctx = current(required=True)
        assert ctx is not None
        return ctx.handle

    def ensure_active(self) -> None:
        if self._token is None:
            raise ISLContextError("Context must be entered via `with` before use.")
        if self._closed:
            raise ISLContextError("Context was already closed.")

    def as_ctype(self) -> Any:
        return c_void_p

    @classmethod
    def alloc(cls) -> "Context":
        return isl_ctx_alloc()

isl_ctx_alloc = ISLFunction.create(
    "isl_ctx_alloc",
    return_=Give(Context),
    lib=_lib,
)

isl_ctx_free = ISLFunction.create(
    "isl_ctx_free",
    Context(),
    return_=Null(),
    lib=_lib,
)

def current(*, required: bool = False) -> Optional[Context]:
    ctx = _current_context.get()
    if ctx is None and required:
        raise ISLContextError("No active ISL context; wrap code with `with I.context():`.")
    return ctx

def context(
    *,
    prim_context_factory: Optional[Callable[[], Context]] = None,
    name: str = "isl",
) -> Context:
    factory = prim_context_factory or isl_ctx_alloc
    ctx = factory()
    if not isinstance(ctx, Context):
        raise TypeError("Context factory must return an Context instance.")
    ctx.name = name
    return ctx

_isl_ctx_last_error_msg = ISLFunction.create(
    "isl_ctx_last_error_msg",
    Context(),
    return_=Param(str),
    lib=_lib,
)

_isl_ctx_last_error_file = ISLFunction.create(
    "isl_ctx_last_error_file",
    Context(),
    return_=Param(str),
    lib=_lib,
)

_isl_ctx_last_error_line = ISLFunction.create(
    "isl_ctx_last_error_line",
    Context(),
    return_=Param(int),
    lib=_lib,
)
