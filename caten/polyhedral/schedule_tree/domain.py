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
        """
        Fuse this domain (Producer) into the target domain (Consumer).
        Automatically computes the pre-image of the dependency to embed the producer loop
        inside the consumer loop nest.
        """
        if not self.writes_map or not target.reads_map:
            raise RuntimeError("Access relations (writes/reads) required for compute_at.")
            
        # 1. Dependency Analysis: P -> C
        def to_uset(d: Any) -> "I.UnionSet":
            if isinstance(d, str):
                return I.UnionSet(d)
            if isinstance(d, I.Set):
                return I.UnionSet.from_set(d)
            return d
            
        p_dom = to_uset(self.domain_set)
        c_dom = to_uset(target.domain_set)
        
        def _make_temp_sched(d_set: "I.UnionSet", t: int) -> "I.UnionMap":
            time_set = I.UnionSet(f"{{ [{t}] }}")
            return I.UnionMap.from_domain_and_range(d_set, time_set)
            
        p_sched_dummy = _make_temp_sched(p_dom, 0)
        c_sched_dummy = _make_temp_sched(c_dom, 1)
        full_sched_dummy = p_sched_dummy.union(c_sched_dummy)
        
        dep = compute_flow(sink=target.reads_map, must_source=self.writes_map, schedule=full_sched_dummy)
        if dep.is_empty():
             raise RuntimeError("No dependency found between domains.")
             
        # 2. Target Schedule (T_c)
        if not target.schedule:
             target.finalize()
             if not target.schedule:
                 target.schedule = I.Schedule.from_domain(c_dom)
        
        target_sched_map = target.schedule.get_map()
        
        # 3. Map Producer to Consumer's Time (T_p -> T_c)
        prod_outer = dep.apply_range(target_sched_map)
        prod_outer = prod_outer.lexmax()
        active_p_dom = prod_outer.domain()
        
        # 4. Construct Fused Schedule Tree
        common_sched = prod_outer.union(target_sched_map)
        mupa_outer = I.MultiUnionPwAff.from_union_map(common_sched)
        
        new_domain_set = active_p_dom.union(c_dom)
        
        sched = I.Schedule.from_domain(new_domain_set)
        root = sched.get_root()
        child = root.child(0) # Leaf
        
        band_node = child.insert_partial_schedule(mupa_outer)
        
        filters = I.UnionSetList.alloc(2)
        filters = filters.add(active_p_dom)
        filters = filters.add(c_dom)
        
        seq_node = band_node.child(0).insert_sequence(filters)
        
        # 5. Inner Schedule for Producer
        if self.schedule:
            p_inner = self.schedule.get_map()
        else:
            p_inner = I.UnionMap.from_domain(p_dom)
            
        mupa_p = I.MultiUnionPwAff.from_union_map(p_inner)
        p_node = seq_node.child(0).child(0).insert_partial_schedule(mupa_p)
        
        new_dom = domain(new_domain_set)
        new_dom.schedule = p_node.get_schedule()
        
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
        
        sched = I.Schedule.from_domain(uset)
        
        builder = get_builder()
        builder.schedule = sched
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
        from ..schedule import PolyhedralSchedule
        
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

        # Access maps are not used by PolyhedralSchedule anymore, but kept for compatibility if needed?
        # PolyhedralSchedule(schedule=...) only cares about the schedule.
        # If reads/writes are needed for analysis, they are on the domain object.
        
        # r = read if read else self.reads_map
        # w = write if write else self.writes_map
        
        return PolyhedralSchedule(schedule=self.schedule)
