import caten.isl as I
import caten.polyhedral as P


def test_simple_schedule_construction():
    with I.context():
        # Define domain: 0 <= i < 10
        dom_str = "{ S[i] : 0 <= i < 10 }"
        
        with P.domain(dom_str) as d:
            # Schedule: i -> i
            with P.band("{ S[i] -> [i] }"):
                # Mark inner loop
                with P.mark("inner"):
                    pass
        
        sched = d.finalize()
        assert sched is not None
        
        # Verify structure (string representation)
        s_str = str(sched)
        assert "domain" in s_str
        assert "schedule" in s_str
        assert "mark" in s_str
        assert "inner" in s_str

def test_tiling_placeholder():
    with I.context():
        with P.domain("{ S[i] : 0 <= i < 100 }") as d:
            with P.band("{ S[i] -> [i] }") as b:
                # Just call to ensure no crash (logic is TODO)
                b.tile([10])
        
        sched = d.finalize()
        assert sched is not None