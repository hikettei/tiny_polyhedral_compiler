from __future__ import annotations

import contextvars
from typing import TYPE_CHECKING, Any, Optional

if TYPE_CHECKING:
    import caten.isl as I

class ScheduleBuilder:
    def __init__(self) -> None:
        self.current_node: Optional["I.ScheduleNode"] = None
        self.schedule: Optional["I.Schedule"] = None
        self.current_domain: Any = None

_builder_ctx: contextvars.ContextVar[Optional[ScheduleBuilder]] = contextvars.ContextVar("schedule_builder", default=None)

def get_builder() -> ScheduleBuilder:
    b = _builder_ctx.get()
    if b is None:
        b = ScheduleBuilder()
        _builder_ctx.set(b)
    return b
