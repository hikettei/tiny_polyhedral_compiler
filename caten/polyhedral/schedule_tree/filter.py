from __future__ import annotations

from typing import Union

import caten.isl as I

from ..context import get_builder
from .base import ScheduleNodeContext


class filter(ScheduleNodeContext):
    def __init__(self, filter_set: Union[str, "I.UnionSet"]) -> None:
        super().__init__()
        self.filter_set = filter_set

    def __enter__(self) -> "filter":
        builder = get_builder()
        if builder.current_node is None:
            raise RuntimeError("No active domain. Use 'with P.domain():' first.")

        if isinstance(self.filter_set, str):
            self.filter_set = I.UnionSet(self.filter_set)
            
        self.node = builder.current_node.insert_filter(self.filter_set)
        builder.current_node = self.node.child(0)
        return self
