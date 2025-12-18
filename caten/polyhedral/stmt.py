import re
from typing import List, Optional, Tuple

import caten.isl as I
from caten.polyhedral.schedule_tree import get_builder

def stmt(expr: str) -> None:
    dom = get_builder().domain
    if dom is None:
        raise RuntimeError("stmt() must be used within a P.domain context")
        
    if "=" not in expr:
        raise ValueError(f"Invalid statement expression (must contain assignment '='): {expr}")
        
    lhs_str, rhs_str = expr.split("=", 1)
        
    def extract_accesses(s: str) -> List[Tuple[str, str]]:
        return re.findall(r"([a-zA-Z_]\w*)\s*\[(.*?)\]", s)
        
    writes = extract_accesses(lhs_str)
    reads = extract_accesses(rhs_str)
    
    uset = dom.domain_set
    if isinstance(uset, str):
        uset = I.UnionSet(uset)
    elif isinstance(uset, I.Set):
        uset = I.UnionSet.from_set(uset)
    
    new_reads: Optional["I.UnionMap"] = None
    new_writes: Optional["I.UnionMap"] = None
    
    def process_set(s: "I.Set") -> None:
        nonlocal new_reads, new_writes
        s_str = str(s)
        if ":" in s_str:
            tuple_part = s_str.split(":")[0].strip()
            if tuple_part.startswith("{"):
                tuple_part = tuple_part[1:].strip()
        else:
            tuple_part = s_str.strip()
            if tuple_part.startswith("{") and tuple_part.endswith("}"):
                tuple_part = tuple_part[1:-1].strip()
                
        for (name, indices) in writes:
            m_str = f"{{ {tuple_part} -> {name}[{indices}] }}"
            m = I.UnionMap(m_str)
            if new_writes is None:
                new_writes = m
            else:
                new_writes = new_writes.union(m)
            
        for (name, indices) in reads:
            m_str = f"{{ {tuple_part} -> {name}[{indices}] }}"
            m = I.UnionMap(m_str)
            if new_reads is None:
                new_reads = m
            else:
                new_reads = new_reads.union(m)

    set_list = uset.get_set_list()
    n = set_list.n_set()
    for i in range(n):
        process_set(set_list.get_at(i))
    
    if new_reads:
        if dom.reads_map:
            dom.reads_map = dom.reads_map.union(new_reads)
        else:
            dom.reads_map = new_reads
        
    if new_writes:
        if dom.writes_map:
            dom.writes_map = dom.writes_map.union(new_writes)
        else:
            dom.writes_map = new_writes
