from __future__ import annotations

from typing import TYPE_CHECKING, Any, Optional

from ..context import get_builder

if TYPE_CHECKING:
    import caten.isl as I

class ScheduleNodeContext:
    """
    Base class for schedule tree nodes that can be used as context managers.
    """
    def __init__(self) -> None:
        self.node: Optional["I.ScheduleNode"] = None

    def __enter__(self) -> "ScheduleNodeContext":
        raise NotImplementedError

    def __exit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        # Default exit behavior: move up to parent
        builder = get_builder()
        if builder.current_node:
            builder.current_node = builder.current_node.parent()
