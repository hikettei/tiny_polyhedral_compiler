from __future__ import annotations

from typing import Union

import caten.isl as I

from ..context import get_builder
from .base import ScheduleNodeContext


class mark(ScheduleNodeContext):
    def __init__(self, mark_id: Union[str, "I.Id"]) -> None:
        super().__init__()
        self.mark_id = mark_id

    def __enter__(self) -> "mark":
        builder = get_builder()
        if builder.current_node is None:
            raise RuntimeError("No active domain.")

        if isinstance(self.mark_id, str):
            self.mark_id = I.Id.alloc(self.mark_id)
            
        self.node = builder.current_node.insert_mark(self.mark_id)
        builder.current_node = self.node.child(0)
        return self
