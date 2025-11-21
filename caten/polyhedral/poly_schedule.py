from typing import Optional

import caten.isl as I
from caten.polyhedral.analysis import compute_dependence_relation, schedule_is_legal_p
from caten.polyhedral.codegen import to_c


class PolyhedralSchedule:
    def __init__(self, schedule: "I.Schedule", reads: Optional["I.UnionMap"] = None, writes: Optional["I.UnionMap"] = None) -> None:
        self.isl_schedule = schedule
        self.reads = reads
        self.writes = writes
        self.raw_dep: Optional["I.UnionMap"] = None
        self.total_dep: Optional["I.UnionMap"] = None
        
        if reads and writes:
            self.compute_dependencies()

    def compute_dependencies(self) -> None:
        if not self.reads or not self.writes:
            return
        total, raw, waw, war = compute_dependence_relation(self.reads, self.writes, self.isl_schedule)
        self.raw_dep = raw
        self.total_dep = total
        
    def is_legal(self) -> bool:
        # Check legality against RAW dependencies
        if self.raw_dep:
            return schedule_is_legal_p(self.isl_schedule, self.raw_dep)
        return True

    def get_root(self) -> "I.ScheduleNode":
        return self.isl_schedule.get_root()

    def to_c(self) -> str:
        return to_c(self.isl_schedule)

    def __str__(self) -> str:
        return str(self.isl_schedule)

    def update(self, node: "I.ScheduleNode") -> None:
        """Update the internal schedule from a modified schedule node."""
        self.isl_schedule = node.get_schedule()

def schedule_sequence(schedules: list[PolyhedralSchedule]) -> PolyhedralSchedule:
    """
    Combine multiple PolyhedralSchedules into a single sequence.
    """
    if not schedules:
        raise ValueError("No schedules provided")
        
    # Combine domains
    combined_domain: Optional["I.UnionSet"] = None
    filters = I.UnionSetList.alloc(len(schedules))
    
    all_reads: Optional["I.UnionMap"] = None
    all_writes: Optional["I.UnionMap"] = None
    
    for sched in schedules:
        dom = sched.isl_schedule.get_domain()
        if combined_domain is None:
            combined_domain = dom
        else:
            combined_domain = combined_domain.union(dom)
            
        filters = filters.add(dom)
        
        # Combine access relations
        if sched.reads:
            if all_reads is None:
                all_reads = sched.reads
            else:
                all_reads = all_reads.union(sched.reads)
            
        if sched.writes:
            if all_writes is None:
                all_writes = sched.writes
            else:
                all_writes = all_writes.union(sched.writes)

    if combined_domain is None:
        raise ValueError("Could not compute combined domain")

    # Create new schedule from combined domain
    new_sched = I.Schedule.from_domain(combined_domain)
    root = new_sched.get_root()
    
    # Insert sequence
    seq_node = root.child(0).insert_sequence(filters)
    
    # Insert original schedules as children
    TYPE_BAND = 0
    
    current_node = seq_node
    for i, sched in enumerate(schedules):
        # Navigate to child i of CURRENT sequence node
        child = sched.isl_schedule.get_root().child(0)
        if child.get_type() == TYPE_BAND:
            mupa = child.band_get_partial_schedule()
            
            # current_node is Sequence.
            filter_node = current_node.child(i)
            new_band = filter_node.child(0).insert_partial_schedule(mupa)
            
            # new_band is the inserted band.
            # Parent is Filter. Parent is Sequence.
            current_node = new_band.parent().parent()
            
    # Create result
    res_sched = current_node.get_schedule()
    return PolyhedralSchedule(res_sched, reads=all_reads, writes=all_writes)
