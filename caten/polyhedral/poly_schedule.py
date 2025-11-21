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

    def sequence(self, other: "PolyhedralSchedule") -> "PolyhedralSchedule":
        """Combine this schedule with another using isl_schedule_sequence."""
        new_sched = self.isl_schedule.sequence(other.isl_schedule)
        
        new_reads = None
        if self.reads and other.reads:
            new_reads = self.reads.union(other.reads)
        elif self.reads:
            new_reads = self.reads
        elif other.reads:
            new_reads = other.reads
            
        new_writes = None
        if self.writes and other.writes:
            new_writes = self.writes.union(other.writes)
        elif self.writes:
            new_writes = self.writes
        elif other.writes:
            new_writes = other.writes
            
        return PolyhedralSchedule(new_sched, reads=new_reads, writes=new_writes)