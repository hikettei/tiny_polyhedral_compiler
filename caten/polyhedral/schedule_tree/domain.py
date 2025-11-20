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

    def compute_at(self, target: "domain") -> "domain":
        if not self.writes_map or not target.reads_map:
            raise RuntimeError("Access relations (writes/reads) required for compute_at.")
            
        # Dependency Producer(self) -> Consumer(target)
        # Construct temporary schedule: P -> 0, C -> 1 to establish order
        # self.domain_set and target.domain_set might be str/Set/UnionSet. Ensure UnionSet.
        def to_uset(d: Any) -> "I.UnionSet":
            if isinstance(d, str):
                return I.UnionSet(d)
            if isinstance(d, I.Set):
                return I.UnionSet.from_set(d)
            return d
            
        p_dom = to_uset(self.domain_set)
        c_dom = to_uset(target.domain_set)
        
        def _make_temp_sched(d_set: "I.UnionSet", t: int) -> "I.UnionMap":
            # Create { [t] }
            time_set = I.UnionSet(f"{{ [{t}] }}")
            # Cross product { D -> [t] }
            return I.UnionMap.from_domain_and_range(d_set, time_set)
            
        p_sched = _make_temp_sched(p_dom, 0)
        c_sched = _make_temp_sched(c_dom, 1)
        full_sched = p_sched.union(c_sched)
        
        dep = compute_flow(sink=target.reads_map, must_source=self.writes_map, schedule=full_sched)
        if dep.is_empty():
             raise RuntimeError("No dependency found between domains.")
             
        if not target.schedule:
             raise RuntimeError("Target domain must be finalized (schedule created).")
        
        target_sched_map = target.schedule.get_map()
        
        # Map Producer to Consumer's schedule via Dependency
        prod_sched_map = dep.apply_range(target_sched_map)
        # Ensure single valued schedule (take lexmax for late scheduling)
        prod_sched_map = prod_sched_map.lexmax()
        
        combined_map = prod_sched_map.union(target_sched_map)
        mupa = I.MultiUnionPwAff.from_union_map(combined_map)
        
        new_domain_set = p_dom.union(c_dom)
        new_dom = domain(new_domain_set)
        
        # Construct new tree
        sched = I.Schedule.from_domain(new_domain_set)
        root = sched.get_root()
        child = root.child(0)
        
        new_band_node = child.insert_partial_schedule(mupa)
        new_dom.schedule = new_band_node.get_schedule()
        
        return new_dom

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