from __future__ import annotations

import abc
from typing import TYPE_CHECKING, Any, Optional
import contextvars

if TYPE_CHECKING:
    import caten.isl as I

## ~~ ScheduleBuilder ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
class ScheduleNodeContext(metaclass=abc.ABCMeta):
    """
    Base class for schedule tree nodes that can be used as context managers.
    """
    def __init__(self) -> None:
        self.node: Optional["I.ScheduleNode"] = None

    def __enter__(self) -> "ScheduleNodeContext":
        

    def __exit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        # Default exit behavior: move up to parent
        builder = get_builder()
        if builder.current_node:
            builder.current_node = builder.current_node.parent()
