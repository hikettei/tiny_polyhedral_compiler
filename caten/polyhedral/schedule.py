from __future__ import annotations

from typing import Optional, Union

import caten.isl as I


def schedule(domain: Union[str, "I.UnionSet"], validity: Optional[Union[str, "I.UnionMap"]] = None, proximity: Optional[Union[str, "I.UnionMap"]] = None) -> "I.Schedule":
    """
    Compute a schedule for the given domain, respecting validity dependencies and proximity goals.
    This is the automated scheduling entry point (Pluto-like).
    """
    if isinstance(domain, str):
        domain = I.UnionSet(domain)
    
    sc = I.ScheduleConstraints.on_domain(domain)
    
    if validity:
        if isinstance(validity, str):
            validity = I.UnionMap(validity)
        sc = sc.set_validity(validity)
        
    if proximity:
        if isinstance(proximity, str):
            proximity = I.UnionMap(proximity)
        sc = sc.set_proximity(proximity)
        
    return sc.compute_schedule()