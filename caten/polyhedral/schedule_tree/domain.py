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
        # We need a schedule for compute_flow to determine direction if domains overlap?
        # Here domains are usually disjoint (Conv vs Pool).
        # But compute_flow might return empty if no execution order is implied.
        # Let's try without explicit schedule first, relying on memory-based dependence check.
        # If that fails (as in previous test), we might need to assume P before C.
        
        # To ensure dependency detection, we provide a dummy schedule where P < C.
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
                 # Identity schedule if not defined
                 target.schedule = I.Schedule.from_domain(c_dom)
        
        target_sched_map = target.schedule.get_map()
        
        # 3. Map Producer to Consumer's Time (T_p -> T_c)
        # dep: { P -> C }
        # target_sched_map: { C -> T_c }
        # prod_outer: { P -> T_c }
        prod_outer = dep.apply_range(target_sched_map)
        
        # Use lexmax to schedule producer as late as possible (closest to consumer use)
        prod_outer = prod_outer.lexmax()
        
        # Restrict P domain to instances that are actually used (Active Domain)
        # This avoids "band node is not allowed to drop statement instances" error
        active_p_dom = prod_outer.domain()
        
        # 4. Construct Fused Schedule Tree
        # Outer Band: { P -> T_c; C -> T_c }
        common_sched = prod_outer.union(target_sched_map)
        mupa_outer = I.MultiUnionPwAff.from_union_map(common_sched)
        
        new_domain_set = active_p_dom.union(c_dom)
        
        sched = I.Schedule.from_domain(new_domain_set)
        root = sched.get_root()
        child = root.child(0) # Leaf
        
        # Insert Outer Band
        band_node = child.insert_partial_schedule(mupa_outer)
        
        # Insert Sequence: [Producer, Consumer]
        # Note: Order matters. Producer must be computed before Consumer within the same time tile.
        # Since we scheduled P at T_c (same time), the sequence ensures P executes, then C.
        
        # Create UnionSetList
        filters = I.UnionSetList.alloc(2)
        filters = filters.add(active_p_dom)
        filters = filters.add(c_dom)
        
        seq_node = band_node.child(0).insert_sequence(filters)
        
        # 5. Inner Schedule for Producer
        # We schedule the producer's domain P using its original identity (or specified) schedule.
        # Since outer loops are fixed by T_c, this effectively schedules the "local" loops (kh, kw etc).
        if self.schedule:
            p_inner = self.schedule.get_map()
        else:
            p_inner = I.UnionMap.from_domain(p_dom) # { P -> P }
            
        mupa_p = I.MultiUnionPwAff.from_union_map(p_inner)
        
        # Insert P's inner band under Sequence Child 0
        p_node = seq_node.child(0).child(0).insert_partial_schedule(mupa_p)
        
        # Consumer inner schedule?
        # If C had deeper loops not covered by T_c, we should add them.
        # But we used target.schedule.get_map() for T_c, which likely includes all dimensions.
        # So C is fully scheduled by outer band.
        
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