import caten.isl as I
import caten.polyhedral as P


def test_compute_flow_simple():
    with I.context():
        # S1: A[i] = ...
        # S2: ... = A[i]
        writes = I.UnionMap("{ S1[i] -> A[i] : 0 <= i < 10 }")
        reads = I.UnionMap("{ S2[i] -> A[i] : 0 <= i < 10 }")
        
        # Schedule: S1 before S2
        schedule = I.UnionMap("{ S1[i] -> [0, i]; S2[i] -> [1, i] }")
        
        # Must flow dependence S1 -> S2
        dep = P.compute_flow(sink=reads, must_source=writes, schedule=schedule)
        
        # Expect S1[i] -> S2[i]
        assert not dep.is_empty()
        
        # Check domain/range
        dom = dep.domain() # S1
        rng = dep.range() # S2
        assert "S1" in str(dom)
        assert "S2" in str(rng)

def test_compute_flow_no_schedule():
    with I.context():
        writes = I.UnionMap("{ S1[i] -> A[i] }")
        reads = I.UnionMap("{ S2[i] -> A[i] }")
        # Without schedule, ISL assumes everything is parallel? Or sequential?
        # Usually requires schedule for accurate flow analysis
        # But we check it runs
        dep = P.compute_flow(sink=reads, must_source=writes)
        assert dep is not None
