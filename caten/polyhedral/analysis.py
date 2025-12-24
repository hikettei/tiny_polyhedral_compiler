from __future__ import annotations

from typing import Tuple

import caten.isl as I


def compute_dependence_relation(read: I.UnionMap, write: I.UnionMap, schedule: I.Schedule) -> Tuple["I.UnionMap", "I.UnionMap", "I.UnionMap", "I.UnionMap"]:
    """
    Compute memory dependence relation Delta = RAW U WAW U WAR.
    Returns (Total, RAW, WAW, WAR).
    """
    # RAW
    access = I.UnionAccessInfo.from_sink(read)
    access = access.set_must_source(write)
    access = access.set_schedule(schedule)
    flow = access.compute_flow()
    raw = flow.get_must_dependence()
    # WAW, WAR
    access = I.UnionAccessInfo.from_sink(write)
    access = access.set_must_source(write) # WAW
    access = access.set_may_source(read)   # WAR
    access = access.set_schedule(schedule)
    flow = access.compute_flow()
    waw = flow.get_must_dependence()
    war = flow.get_may_dependence()
    total = raw.union(waw).union(war)
    return total, raw, waw, war

def zero_vector_union_set(delta_uset: I.UnionSet) -> I.UnionSet:
    """
    Given a union-set U ⊆ ℤ^d, return the singleton union-set {0⃗} in the
same space as U. This is constructed by creating a zero multi-affine
mapping in the space of (set-from-union-set U) and converting it back to
a union-set.

Inputs:
  delta-uset : isl::union-set U

Returns:
  isl::union-set {0⃗} ⊆ ℤ^d
    """
    delta_set = I.Set.from_union_set(delta_uset)
    ma = I.MultiAff.zero(delta_set.get_space())
    return I.UnionSet.from_set(I.Set.from_multi_aff(ma))

def compute_schedule_legality(schedule: I.Schedule, dep: I.UnionMap) -> bool:
    """"Return T iff the given schedule S respects all dependences Δ.

Inputs:
  schedule : isl::schedule S   — complete schedule
  dep      : isl::union-map Δ ⊆ D×D — dependence relation (RAW/WAW/WAR)

Semantics:
  The schedule is legally valid iff, for every (x, y) ∈ Δ, S(x) ≥_lex S(y),
  i.e., it does not reverse or violate the partial order induced by Δ.

Returns:
  boolean
    """
    if dep.is_empty(): return True
    sched_map = schedule.get_map()
    # { t_src -> t_dst }
    domain = dep.apply_domain(sched_map)
    domain = domain.apply_range(sched_map)
    # { t_dst - t_src }
    delta = domain.deltas()
    if delta.is_empty(): return False
    zeros = zero_vector_union_set(delta)
    le = delta.lex_le_union_set(zeros)
    return le.is_empty()

def compute_parallel():
    pass
