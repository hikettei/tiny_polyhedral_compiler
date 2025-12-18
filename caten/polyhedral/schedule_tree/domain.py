from __future__ import annotations

from typing import Any, Optional, Union, cast

import caten.isl as I

from ..analysis import compute_flow
from ..context import get_builder
from .base import ScheduleNodeContext


class domain(ScheduleNodeContext):
    def __init__(self, domain_set: Union[str, "I.Set", "I.UnionSet", Any] = None) -> None:
        super().__init__()
        self.domain_set = domain_set
        self.schedule: Optional["I.Schedule"] = None
        self.reads_map: Optional["I.UnionMap"] = None
        self.writes_map: Optional["I.UnionMap"] = None

    def access(self, reads: Optional[Union[str, "I.UnionMap"]] = None, writes: Optional[Union[str, "I.UnionMap"]] = None) -> None:
        if reads:
            if isinstance(reads, str):
                reads = I.UnionMap(reads)
            self.reads_map = reads
        if writes:
            if isinstance(writes, str):
                writes = I.UnionMap(writes)
            self.writes_map = writes

    def __enter__(self) -> "domain":
        uset: "I.UnionSet"
        if isinstance(self.domain_set, str):
            uset = I.UnionSet(self.domain_set)
        elif isinstance(self.domain_set, I.Set):
            uset = I.UnionSet.from_set(self.domain_set)
        else:
            uset = cast("I.UnionSet", self.domain_set)
            
        self.domain_set = uset
        
        # Create schedule from domain
        sched = I.Schedule.from_domain(uset)
        
        builder = get_builder()
        builder.schedule = sched
        # Root is domain node. We want to insert under it.
        # Initial tree: Domain -> Leaf
        # We set current_node to the child of Domain (the Leaf)
        builder.current_node = sched.get_root().child(0)
        
        self._prev_domain = builder.current_domain
        builder.current_domain = self
        
        return self

    def __exit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        builder = get_builder()
        if builder.current_node:
            self.schedule = builder.current_node.get_schedule()
        builder.current_node = None
        builder.current_domain = self._prev_domain

    def finalize(self, read: Optional[Union[str, "I.UnionMap"]] = None, write: Optional[Union[str, "I.UnionMap"]] = None) -> Any:
        from ..poly_schedule import PolyhedralSchedule
        
        if self.schedule is None:
             if self.domain_set:
                 uset = self.domain_set
                 if isinstance(uset, str):
                     uset = I.UnionSet(uset)
                 elif isinstance(uset, I.Set):
                     uset = I.UnionSet.from_set(uset)
                 self.schedule = I.Schedule.from_domain(uset)
             else:
                 raise RuntimeError("No domain set for schedule.")

        r = read if read else self.reads_map
        if isinstance(r, str):
            r = I.UnionMap(r)
        
        w = write if write else self.writes_map
        if isinstance(w, str):
            w = I.UnionMap(w)

        return PolyhedralSchedule(self.schedule, reads=r, writes=w)
