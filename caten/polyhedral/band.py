from __future__ import annotations

from typing import Any, List, Optional, Union

import caten.isl as I

from .context import get_builder


class band:
    """
    A context manager to insert a Band Node into the schedule tree.
    """
    def __init__(self, schedule: Union[str, "I.MultiUnionPwAff"]) -> None:
        self.schedule_spec = schedule
        self.node: Optional["I.ScheduleNode"] = None # The inserted band node

    def __enter__(self) -> "band":
        builder = get_builder()
        if builder.current_node is None:
            raise RuntimeError("No active domain. Use 'with P.domain():' first.")

        if isinstance(self.schedule_spec, str):
            umap = I.UnionMap(self.schedule_spec)
            self.schedule_spec = I.MultiUnionPwAff.from_union_map(umap)
            
        # Insert band at current position (Leaf)
        # returns the new Band Node
        self.node = builder.current_node.insert_partial_schedule(self.schedule_spec)
        
        # Move current_node to the child of the new Band (the Leaf)
        # to allow further nesting (inner loops)
        builder.current_node = self.node.child(0)
        
        return self

    def __exit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        builder = get_builder()
        # Move back up to the Band Node (parent of current Leaf)
        if builder.current_node:
             builder.current_node = builder.current_node.parent()

    def tile(self, sizes: List[int]) -> None:
        """Tile this band."""
        if not self.node:
            raise RuntimeError("Cannot tile before entering context.")
        
        # TODO: Implement tiling logic that updates the tree structure and builder state.
        pass