from __future__ import annotations

import contextvars
from ctypes import c_char_p, c_int, c_void_p
from types import TracebackType
from typing import Any, Callable, Optional

from ..ffi import FfiPointer, load_libisl
from ..func import ISLFunction
from ..obj import ISLObject
from ..qualifier import Give, Keep, Null, Param, Qualifier

_lib = load_libisl()

class ISLContextError(RuntimeError):
    """Raised when ISL operations are attempted without a valid context."""


class ISLError(RuntimeError):
    """Raised when libisl signals an operational error."""

_current_context: contextvars.ContextVar[Optional["ISLContext"]]
_current_context = contextvars.ContextVar("caten_isl_context", default=None)

class ISLContext(ISLObject, Qualifier):
    requires_argument = False
    __slots__ = ("name", "_token", "_closed")
    def __init__(self, handle: FfiPointer) -> None:
        ISLObject.__init__(self, handle)
        Qualifier.__init__(self)
        self.name = "isl"
        self._token: contextvars.Token[Optional["ISLContext"]] | None = None
        self._closed = False

    def copy_handle(self) -> FfiPointer:
        raise ISLContextError("Context cannot be copied.")

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        pass # TODO: _lib.isl_ctx_free(handle)

    def __enter__(self) -> "ISLContext":
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
    def alloc(cls) -> "ISLContext":
        return isl_ctx_alloc()

isl_ctx_alloc = ISLFunction.create(
    "isl_ctx_alloc",
    return_=Give(ISLContext),
    lib=_lib,
)

isl_ctx_free = ISLFunction.create(
    "isl_ctx_free",
    Keep(ISLContext),
    return_=Null(),
    lib=_lib,
)

def current(*, required: bool = False) -> Optional[ISLContext]:
    ctx = _current_context.get()
    if ctx is None and required:
        raise ISLContextError("No active ISL context; wrap code with `with I.context():`.")
    return ctx

def context(
    *,
    prim_context_factory: Optional[Callable[[], ISLContext]] = None,
    name: str = "isl",
) -> ISLContext:
    factory = prim_context_factory or isl_ctx_alloc
    ctx = factory()
    if not isinstance(ctx, ISLContext):
        raise TypeError("Context factory must return an ISLContext instance.")
    ctx.name = name
    return ctx


class _CStringResult(Qualifier):
    requires_argument = False

    def prepare(self, value: Any, *, ctx: "ISLContext" | None, name: str) -> Any:  # pragma: no cover - never used
        raise TypeError("CStringResult is only valid for return values.")

    def wrap(self, value: Any, *, ctx: "ISLContext" | None, name: str = "return") -> str | None:  # type: ignore[override]
        if value is None:
            return None
        if isinstance(value, bytes):
            return value.decode("utf-8", errors="replace")
        return str(value)

    def as_ctype(self) -> Any:
        return c_char_p


def last_error_details(ctx: ISLContext) -> tuple[str | None, str | None, int | None]:
    msg = _isl_ctx_last_error_msg(ctx)
    file = _isl_ctx_last_error_file(ctx)
    line = _isl_ctx_last_error_line(ctx)
    if isinstance(line, int) and line >= 0:
        line_no: int | None = line
    else:
        line_no = None
    return msg, file, line_no


def expect_handle(
    result: Any,
    *,
    ctx: ISLContext | None = None,
    func: str | None = None,
) -> FfiPointer:
    if result is None:
        return _raise_last_error(ctx, func)
    value = int(result)
    if value == 0:
        return _raise_last_error(ctx, func)
    return value


def _raise_last_error(ctx: ISLContext | None, func: str | None) -> FfiPointer:
    active_ctx = ctx or current(required=True)
    assert active_ctx is not None
    msg, file, line = last_error_details(active_ctx)
    parts: list[str] = []
    if func:
        parts.append(func)
    if msg:
        parts.append(msg)
    if file:
        location = f"{file}:{line}" if line is not None else file
        parts.append(location)
    reason = " | ".join(parts) or "libisl reported an unknown error"
    raise ISLError(reason)


_isl_ctx_last_error_msg = ISLFunction.create(
    "isl_ctx_last_error_msg",
    Keep(ISLContext),
    return_=_CStringResult(),
    lib=_lib,
)

_isl_ctx_last_error_file = ISLFunction.create(
    "isl_ctx_last_error_file",
    Keep(ISLContext),
    return_=_CStringResult(),
    lib=_lib,
)

_isl_ctx_last_error_line = ISLFunction.create(
    "isl_ctx_last_error_line",
    Keep(ISLContext),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)
