from __future__ import annotations

from typing import Any, Optional, Union, cast

import caten.isl as I

from ..context import get_builder
from .base import ScheduleNodeContext


class domain(ScheduleNodeContext):
    def __init__(self, domain_set: Union[str, "I.Set", "I.UnionSet", Any] = None) -> None:
        super().__init__()
        self.domain_set = domain_set
        self.schedule: Optional["I.Schedule"] = None

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
        
        return self

    def __exit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        builder = get_builder()
        if builder.current_node:
            self.schedule = builder.current_node.get_schedule()
        builder.current_node = None

    def finalize(self, op_context: Any = None) -> Any:
        # Placeholder for Kernel creation logic
        return self.schedule
