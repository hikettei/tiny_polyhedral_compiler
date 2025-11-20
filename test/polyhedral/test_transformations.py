import caten.isl as I
import caten.polyhedral as P


def test_tiling():
    with I.context():
        with P.domain("{ S[i,j] : 0 <= i,j < 100 }") as d:
            # Schedule: [i, j]
            with P.band("{ S[i,j] -> [i,j] }") as b:
                b.tile([10, 10])
        
        sched = d.finalize()
        s_str = str(sched)
        # Tiling creates two bands. The exact string depends on ISL version/options
        # But it should contain nested structure
        assert "child" in s_str

def test_splitting():
    with I.context():
        with P.domain("{ S[i,j] : 0 <= i,j < 10 }") as d:
            with P.band("{ S[i,j] -> [i,j] }") as b:
                b.split(1) # Split i and j
        
        sched = d.finalize()
        s_str = str(sched)
        assert "child" in s_str

def test_sequence():
    with I.context():
        dom = "{ A[i] : 0 <= i < 10; B[i] : 0 <= i < 10 }"
        with P.domain(dom) as d:
            with P.sequence(["{ A[i] }", "{ B[i] }"]) as seq:
                with seq.child(0):
                    with P.band("{ A[i] -> [i] }"):
                        pass
                with seq.child(1):
                    with P.band("{ B[i] -> [i] }"):
                        pass
        
        sched = d.finalize()
        s_str = str(sched)
        assert "sequence" in s_str
