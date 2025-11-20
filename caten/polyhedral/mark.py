from __future__ import annotations

from typing import Any, Optional, Union

import caten.isl as I

from .context import get_builder


class mark:
    def __init__(self, mark_id: Union[str, "I.Id"]) -> None:
        self.mark_id = mark_id
        self.node: Optional["I.ScheduleNode"] = None

    def __enter__(self) -> "mark":
        builder = get_builder()
        if builder.current_node is None:
            raise RuntimeError("No active domain.")

        if isinstance(self.mark_id, str):
            self.mark_id = I.Id.alloc(self.mark_id)
            
        self.node = builder.current_node.insert_mark(self.mark_id)
        builder.current_node = self.node.child(0)
        return self

    def __exit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        builder = get_builder()
        if builder.current_node:
            builder.current_node = builder.current_node.parent()