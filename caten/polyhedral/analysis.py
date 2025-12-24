from __future__ import annotations

from typing import Optional, Tuple, Union, cast

import caten.isl as I

def compute_dependence_relation(read: I.UnionMap, write: I.UnionMap, schedule: I.Schedule) -> Tuple["I.UnionMap", "I.UnionMap", "I.UnionMap", "I.UnionMap"]:
    """
    Compute memory dependence relation Delta = RAW U WAW U WAR.
    Returns (Total, RAW, WAW, WAR).
    """
    # RAW
    access = I.UnionAccessInfo.from_sink(read)
    access = access.set_must_source(write)
    flow = access.compute_flow()
    raw = flow.get_must_dependence()
    # WAW, WAR
    access = I.UnionAccessInfo.from_sink(write)
    access = access.set_must_source(write) # WAW
    access = access.set_may_source(read)   # WAR        
    flow = access.compute_flow()
    waw = flow.get_must_dependence()
    war = flow.get_may_dependence()
    
    total = raw.union(waw).union(war)
    return total, raw, waw, war

def verify_legality():
    pass

def compute_parallel():
    pass
