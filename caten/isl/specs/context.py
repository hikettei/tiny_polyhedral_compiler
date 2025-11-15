from __future__ import annotations

import contextvars
from ctypes import c_void_p
from typing import Callable, Optional

from ..ffi import FfiPointer, load_libisl
from ..func import ISLFunction
from ..obj import ISLObject, InPlace
from ..qualifier import Give, Null, Qualifier, Take

_lib = load_libisl()

if _lib is not None:  # pragma: no cover - ctypes metadata setup
    _lib.isl_ctx_alloc.restype = c_void_p
    _lib.isl_ctx_alloc.argtypes = []
    _lib.isl_ctx_free.restype = None
    _lib.isl_ctx_free.argtypes = [c_void_p]
    if hasattr(_lib, "isl_ctx_ref"):
        _lib.isl_ctx_ref.restype = c_void_p
        _lib.isl_ctx_ref.argtypes = [c_void_p]


def _missing(*_args):  # pragma: no cover - best effort
    raise RuntimeError("libisl is required but could not be loaded.")

_alloc_c = getattr(_lib, "isl_ctx_alloc", None) or _missing
_free_c = getattr(_lib, "isl_ctx_free", None) or _missing
_ref_c = getattr(_lib, "isl_ctx_ref", None)


class ISLContextError(RuntimeError):
    """Raised when ISL operations are attempted without a valid context."""


_current_context: contextvars.ContextVar[Optional["ISLContext"]]
_current_context = contextvars.ContextVar("caten_isl_context", default=None)


class ISLContext(ISLObject, Qualifier):
    """GC-managed wrapper around ``isl_ctx`` handles with context-manager and qualifier roles."""

    requires_argument = False
    __slots__ = ("name", "_token", "_closed")

    def __init__(self, handle: FfiPointer) -> None:
        ISLObject.__init__(self, handle)
        Qualifier.__init__(self)
        self.name = "isl"
        self._token = None
        self._closed = False

    def copy_handle(self) -> FfiPointer:
        if _ref_c is None:
            raise RuntimeError("libisl missing isl_ctx_ref; cannot duplicate context handle.")
        return _ref_c(self.handle)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _free_c(handle)

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
        ctx_free(InPlace(self))

    # Qualifier protocol ----------------------------------------------------
    def view(self, value):  # type: ignore[override]
        if value is not None:
            raise TypeError("Context qualifier does not accept positional arguments.")
        ctx = current(required=True)
        if ctx.prim is None:
            raise ISLContextError("Active context has no primitive handle.")
        return ctx.prim.handle

    # Compatibility helpers -------------------------------------------------
    @property
    def prim(self) -> "ISLContext":
        return self

    def ensure_active(self) -> None:
        if self._token is None:
            raise ISLContextError("Context must be entered via `with` before use.")
        if self._closed:
            raise ISLContextError("Context was already closed.")


def _ctx_alloc_obj() -> ISLContext:
    return ISLContext(_alloc_c())


def _ctx_free_obj(ctx: ISLContext) -> None:
    ctx.free()


ctx_alloc = ISLFunction.create(
    _ctx_alloc_obj,
    return_=Give(ISLContext),
)

ctx_free = ISLFunction.create(
    _ctx_free_obj,
    Take(ISLContext),
    return_=Null(),
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
