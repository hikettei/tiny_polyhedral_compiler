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
    Combine multiple PolyhedralSchedules into a single sequence using ISL API.
    """
    if not schedules:
        raise ValueError("No schedules provided")
    
    # Start with the first schedule
    res_sched = schedules[0].isl_schedule
    all_reads = schedules[0].reads
    all_writes = schedules[0].writes
    
    # Iteratively sequence subsequent schedules
    for i in range(1, len(schedules)):
        next_sched_wrapper = schedules[i]
        
        # Use ISL's schedule_sequence to combine
        res_sched = res_sched.sequence(next_sched_wrapper.isl_schedule)
        
        # Combine access relations
        if next_sched_wrapper.reads:
            if all_reads is None:
                all_reads = next_sched_wrapper.reads
            else:
                all_reads = all_reads.union(next_sched_wrapper.reads)
            
        if next_sched_wrapper.writes:
            if all_writes is None:
                all_writes = next_sched_wrapper.writes
            else:
                all_writes = all_writes.union(next_sched_wrapper.writes)

    return PolyhedralSchedule(res_sched, reads=all_reads, writes=all_writes)
