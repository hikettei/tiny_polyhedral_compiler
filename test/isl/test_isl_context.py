import pytest

import caten.isl as I
from caten.isl.ffi import load_libisl
from caten.isl.specs.context import current, isl_ctx_alloc

load_libisl()  # ensure libisl present; test assumes dependency


def test_ctx_alloc_free_roundtrip() -> None:
    ctx = isl_ctx_alloc()
    try:
        assert ctx.handle is not None
    finally:        
        ctx.free()
        assert ctx._handle is None  # internal verification we relinquished handle


def test_context_manager_tracks_tls_and_view() -> None:
    with I.context() as ctx:
        assert ctx.handle is not None
        assert current(required=True) is ctx
        # Qualifier view should expose the raw pointer of the active context
        assert ctx.view(None) == ctx.handle
    with pytest.raises(I.ISLContextError):
        ctx.ensure_active()
