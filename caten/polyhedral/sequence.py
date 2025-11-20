from __future__ import annotations

from typing import Any, List, Optional, Union

import caten.isl as I

from .context import get_builder


class sequence:
    def __init__(self, filters: List[Union[str, "I.UnionSet"]]) -> None:
        self.filters = filters
        self.node: Optional["I.ScheduleNode"] = None

    def __enter__(self) -> "sequence":
        builder = get_builder()
        if builder.current_node is None:
            raise RuntimeError("No active domain.")
        
        # TODO: Implement sequence insertion
        # builder.current_node = ...
        return self

    def __exit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        # builder = get_builder()
        # if builder.current_node:
        #     builder.current_node = builder.current_node.parent()
        pass

    def child(self, index: int) -> "SequenceChildContext":
        return SequenceChildContext(self, index)

class SequenceChildContext:
    def __init__(self, parent: sequence, index: int) -> None:
        self.parent = parent
        self.index = index

    def __enter__(self) -> "SequenceChildContext":
        # builder = get_builder()
        # Move to the specific child filter node
        pass
        return self

    def __exit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        # builder = get_builder()
        pass