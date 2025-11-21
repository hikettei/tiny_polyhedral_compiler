from __future__ import annotations

from typing import List, Union

import caten.isl as I

from ..context import get_builder
from .base import ScheduleNodeContext


class band(ScheduleNodeContext):
    """
    A context manager to insert a Band Node into the schedule tree.
    """
    def __init__(self, schedule: Union[str, "I.MultiUnionPwAff"]) -> None:
        super().__init__()
        self.schedule_spec = schedule

    def __enter__(self) -> "band":
        builder = get_builder()
        if builder.current_node is None:
            raise RuntimeError("No active domain. Use 'with P.domain():' first.")

        if isinstance(self.schedule_spec, str):
            umap = I.UnionMap(self.schedule_spec)
            self.schedule_spec = I.MultiUnionPwAff.from_union_map(umap)
            
        self.node = builder.current_node.insert_partial_schedule(self.schedule_spec)
        builder.current_node = self.node.child(0)
        return self

    def tile(self, sizes: List[int]) -> "band":
        """Tile this band."""
        if not self.node:
            raise RuntimeError("Cannot tile before entering context.")
        
        space = self.node.band_get_space()
        mv = I.MultiVal.zero(space)
        
        for i, size in enumerate(sizes):
            v = I.Val.int_from_si(size)
            mv = mv.set_val(i, v)
            
        self.node = self.node.band_tile(mv)
        return self

    def split(self, pos: int) -> "band":
        """Split this band at pos."""
        if not self.node:
            raise RuntimeError("Cannot split before entering context.")
        
        self.node = self.node.band_split(pos)
        return self
