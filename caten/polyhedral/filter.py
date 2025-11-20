from __future__ import annotations

from typing import Any, Optional, Union

import caten.isl as I

from .context import get_builder


class filter:
    def __init__(self, filter_set: Union[str, "I.UnionSet"]) -> None:
        self.filter_set = filter_set
        self.node: Optional["I.ScheduleNode"] = None

    def __enter__(self) -> "filter":
        builder = get_builder()
        if builder.current_node is None:
            raise RuntimeError("No active domain. Use 'with P.domain():' first.")

        if isinstance(self.filter_set, str):
            self.filter_set = I.UnionSet(self.filter_set)
            
        self.node = builder.current_node.insert_filter(self.filter_set)
        builder.current_node = self.node.child(0)
        return self

    def __exit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        builder = get_builder()
        if builder.current_node:
            builder.current_node = builder.current_node.parent()