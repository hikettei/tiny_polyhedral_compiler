from __future__ import annotations

from typing import Optional, Tuple, Union, cast

import caten.isl as I


def compute_flow(
    sink: Union[str, "I.UnionMap"], 
    must_source: Union[str, "I.UnionMap"], 
    may_source: Optional[Union[str, "I.UnionMap"]] = None,
    schedule: Optional[Union[str, "I.UnionMap", "I.Schedule"]] = None
) -> "I.UnionMap":
    """
    Compute flow dependence (Read-After-Write).
    Returns map from Source (Write) -> Sink (Read).
    """
    if isinstance(sink, str):
        sink = I.UnionMap(sink)
    if isinstance(must_source, str):
        must_source = I.UnionMap(must_source)
        
    if may_source is None:
        may_source = I.UnionMap("{}")
    elif isinstance(may_source, str):
        may_source = I.UnionMap(may_source)
        
    access = I.UnionAccessInfo.from_sink(sink)
    access = access.set_must_source(must_source)
    access = access.set_may_source(may_source)
    
    if schedule:
        if hasattr(schedule, "get_map"):
            access = access.set_schedule(cast("I.Schedule", schedule))
        elif isinstance(schedule, I.UnionMap):
            access = access.set_schedule_map(schedule)
        elif isinstance(schedule, str):
            access = access.set_schedule_map(I.UnionMap(schedule))
        
    flow = access.compute_flow()
    return flow.get_must_dependence()

def compute_dependence_relation(
    read: Union[str, "I.UnionMap"],
    write: Union[str, "I.UnionMap"],
    schedule: Union[str, "I.Schedule", "I.UnionMap"],
) -> Tuple["I.UnionMap", "I.UnionMap", "I.UnionMap", "I.UnionMap"]:
    """
    Compute memory dependence relation Delta = RAW U WAW U WAR.
    Returns (Total, RAW, WAW, WAR).
    """
    if isinstance(read, str):
        read = I.UnionMap(read)
    if isinstance(write, str):
        write = I.UnionMap(write)
    
    # RAW
    access = I.UnionAccessInfo.from_sink(read)
    access = access.set_must_source(write)
    
    if hasattr(schedule, "get_map"):
        access = access.set_schedule(cast("I.Schedule", schedule))
    elif isinstance(schedule, I.UnionMap):
        access = access.set_schedule_map(schedule)
    elif isinstance(schedule, str):
        access = access.set_schedule_map(I.UnionMap(schedule))
        
    flow = access.compute_flow()
    raw = flow.get_must_dependence()
    
    # WAW, WAR
    access = I.UnionAccessInfo.from_sink(write)
    access = access.set_must_source(write) # WAW
    access = access.set_may_source(read)   # WAR
    
    if hasattr(schedule, "get_map"):
        access = access.set_schedule(cast("I.Schedule", schedule))
    elif isinstance(schedule, I.UnionMap):
        access = access.set_schedule_map(schedule)
    elif isinstance(schedule, str):
        access = access.set_schedule_map(I.UnionMap(schedule))
        
    flow = access.compute_flow()
    waw = flow.get_must_dependence()
    war = flow.get_may_dependence()
    
    total = raw.union(waw).union(war)
    return total, raw, waw, war

def get_zero_union_set(uset: "I.UnionSet") -> "I.UnionSet":
    lst = uset.get_set_list()
    n = lst.n_set()
    res = None
    
    for i in range(n):
        s = lst.get_at(i)
        space = s.get_space()
        p = I.Point.zero(space)
        z = I.Set.from_point(p)
        uz = I.UnionSet.from_set(z)
        
        if res is None:
            res = uz
        else:
            res = res.union(uz)
            
    if res is None:
        return I.UnionSet.empty(uset.get_space())
    return res

def is_point_lex_negative(p: "I.Point") -> bool:
    # Check if point vector is lexicographically negative
    # isl_dim_set = 3
    dim_set = 3
    n = p.get_space().dim(dim_set)
    
    for i in range(n):
        val = p.get_coordinate_val(dim_set, i)
        sgn = val.sgn()
        if sgn < 0:
            return True
        if sgn > 0:
            return False
    return False # All zero

def schedule_is_legal_p(schedule: "I.Schedule", dep: "I.UnionMap") -> bool:
    """
    Check if schedule respects dependencies.
    For every (src, dst) in dep, schedule(src) <_lex schedule(dst).
    """
    if dep.is_empty():
        return True
        
    sched_map = schedule.get_map()
    
    # { t_src -> t_dst }
    time_dep = dep.apply_domain(sched_map)
    time_dep = time_dep.apply_range(sched_map)
    
    # { t_dst - t_src }
    delta = time_dep.deltas()
    if delta.is_empty():
        return True
        
    # Check if minimal delta is negative
    min_delta = delta.lexmin()
    
    if min_delta.is_empty():
        return True
        
    # Since we check global lexicographical minimum, one point is enough.
    # If the minimum is non-negative, all are non-negative.
    p = min_delta.sample_point()
    return not is_point_lex_negative(p)
