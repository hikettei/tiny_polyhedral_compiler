from __future__ import annotations

from typing import Any, List, Union

import caten.isl as I

from ..context import get_builder
from .base import ScheduleNodeContext


class sequence(ScheduleNodeContext):
    def __init__(self, filters: List[Union[str, "I.UnionSet"]]) -> None:
        super().__init__()
        self.filters = filters

    def __enter__(self) -> "sequence":
        builder = get_builder()
        if builder.current_node is None:
            raise RuntimeError("No active domain.")
        
        n = len(self.filters)
        us_list = I.UnionSetList.alloc(n)
        
        for f in self.filters:
            if isinstance(f, str):
                us = I.UnionSet(f)
            else:
                us = f
            us_list = us_list.add(us)
            
        self.node = builder.current_node.insert_sequence(us_list)
        builder.current_node = self.node
        return self

    def child(self, index: int) -> "SequenceChildContext":
        return SequenceChildContext(self, index)

class SequenceChildContext(ScheduleNodeContext):
    def __init__(self, parent: sequence, index: int) -> None:
        super().__init__()
        self.parent = parent
        self.index = index

    def __enter__(self) -> "SequenceChildContext":
        builder = get_builder()
        if builder.current_node is None:
             raise RuntimeError("Context lost.")
        
        builder.current_node = builder.current_node.child(self.index).child(0)
        return self

    def __exit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        builder = get_builder()
        # Leaf -> Filter -> Sequence
        if builder.current_node:
            builder.current_node = builder.current_node.parent().parent()
