import caten.isl as I
import caten.polyhedral as P


def test_simple_schedule_construction():
    with I.context():
        dom_str = "{ S[i] : 0 <= i < 10 }"
        with P.domain(dom_str) as d:
            with P.band("{ S[i] -> [i] }"):
                with P.mark("inner"):
                    pass
        
        sched = d.finalize()
        assert sched is not None
        s_str = str(sched)
        assert "domain" in s_str
        assert "schedule" in s_str
        assert "mark" in s_str
        assert "inner" in s_str

def test_tiling_placeholder():
    with I.context():
        with P.domain("{ S[i] : 0 <= i < 100 }") as d:
            with P.band("{ S[i] -> [i] }") as b:
                b.tile([10])
        sched = d.finalize()
        assert sched is not None

def test_filter_context():
    with I.context():
        with P.domain("{ A[i] : 0<=i<10; B[i] : 0<=i<10 }") as d:
            with P.filter("{ A[i] }"):
                with P.band("{ A[i] -> [i] }"):
                    pass
        sched = d.finalize()
        assert "filter" in str(sched)

def test_sequence_context():
    with I.context():
        with P.domain("{ A[i] : 0<=i<10; B[i] : 0<=i<10 }") as d:
            with P.sequence(["{ A[i] }", "{ B[i] }"]) as seq:
                with seq.child(0):
                    with P.band("{ A[i] -> [i] }"):
                        pass
                with seq.child(1):
                    with P.band("{ B[i] -> [i] }"):
                        pass
        sched = d.finalize()
        assert "sequence" in str(sched)

def test_schedule_func():
    with I.context():
        sched = P.schedule("{ S[i] : 0<=i<10 }", validity="{ S[i] -> S[i+1] }")
        assert sched is not None
