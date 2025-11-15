from __future__ import annotations

import contextvars
from ctypes import c_void_p
from typing import Callable, Optional

from ..ffi import FfiPointer, load_libisl
from ..func import ISLFunction
from ..obj import ISLObject, InPlace
from ..qualifier import Give, Null, Qualifier, Take

_lib = load_libisl()

class ISLContextError(RuntimeError):
    """Raised when ISL operations are attempted without a valid context."""

_current_context: contextvars.ContextVar[Optional["ISLContext"]]
_current_context = contextvars.ContextVar("caten_isl_context", default=None)

class ISLContext(ISLObject, Qualifier):
    requires_argument = False
    __slots__ = ("name", "_token", "_closed")
    def __init__(self, handle: FfiPointer) -> None:
        ISLObject.__init__(self, handle)
        Qualifier.__init__(self)
        self.name = "isl"
        self._token = None
        self._closed = False

    def copy_handle(self) -> FfiPointer:
        return _lib.isl_ctx_copy(self.handle)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _lib.isl_ctx_free(handle)

    def __enter__(self) -> "ISLContext":
        if self._token is not None:
            raise ISLContextError("Context is already active on this instance.")
        if self._closed:
            raise ISLContextError("Re-entering a closed Context is not allowed.")
        self._token = _current_context.set(self)
        return self

    def __exit__(self, exc_type, exc, tb) -> None:
        if self._token is not None:
            _current_context.reset(self._token)
            self._token = None
        self._closed = True
        self.free()
    # Qualifier protocol ----------------------------------------------------
    def view(self, value):  # type: ignore[override]
        if value is not None:
            raise TypeError("Context qualifier does not accept positional arguments.")
        ctx = current(required=True)
        return ctx.handle

    def ensure_active(self) -> None:
        if self._token is None:
            raise ISLContextError("Context must be entered via `with` before use.")
        if self._closed:
            raise ISLContextError("Context was already closed.")

    def as_ctype(self):
        return c_void_p

isl_ctx_alloc = ISLFunction.create(
    "isl_ctx_alloc",
    return_=Give(ISLContext),
    lib=_lib,
)

def current(*, required: bool = False) -> Optional[ISLContext]:
    ctx = _current_context.get()
    if ctx is None and required:
        raise ISLContextError("No active ISL context; wrap code with `with I.context():`.")
    return ctx

def context(*, prim_context_factory: Optional[Callable[[], ISLContext]] = None, name: str = "isl") -> ISLContext:
    factory = prim_context_factory or ctx_alloc
    ctx = factory()
    if not isinstance(ctx, ISLContext):
        raise TypeError("Context factory must return an ISLContext instance.")
    ctx.name = name
    return ctx
