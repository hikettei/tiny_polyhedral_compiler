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
