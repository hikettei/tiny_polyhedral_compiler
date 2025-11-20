from __future__ import annotations

from typing import Optional, Union

import caten.isl as I


def compute_flow(
    sink: Union[str, "I.UnionMap"], 
    must_source: Union[str, "I.UnionMap"], 
    may_source: Optional[Union[str, "I.UnionMap"]] = None,
    schedule: Optional[Union[str, "I.UnionMap"]] = None
) -> "I.UnionMap":
    """
    Compute flow dependence (Read-After-Write).
    Returns map from Source (Write) -> Sink (Read).
    
    Args:
        sink: The read access relation { Statement -> Memory }
        must_source: The definite write access relation { Statement -> Memory }
        may_source: The potential write access relation { Statement -> Memory }
        schedule: The execution order { Statement -> [Time] }
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
        if isinstance(schedule, str):
            schedule = I.UnionMap(schedule)
        access = access.set_schedule_map(schedule)
        
    flow = access.compute_flow()
    return flow.get_must_dependence()
