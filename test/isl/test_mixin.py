import caten.isl as I


def test_set_operators():
    with I.context():
        s1 = I.Set("{ [i] : 0 <= i < 5 }")
        s2 = I.Set("{ [i] : 5 <= i < 10 }")
        
        # Union
        s_union = s1 | s2
        assert not s_union.is_empty()
        
        # Intersection
        s_inter = s1 & s2
        assert s_inter.is_empty()
        
        # Subtraction
        s_sub = (s1 | s2) - s2
        assert s_sub.is_equal(s1)
        
        # Comparison
        assert s1 <= (s1 | s2) # subset
        assert s1 < (s1 | s2) # strict subset
        assert (s1 | s2) >= s1 # superset
        assert (s1 | s2) > s1 # strict superset
        assert s1 == s1
        assert s1 != s2

def test_val_operators():
    with I.context():
        v1 = I.Val.int_from_si(10)
        v2 = I.Val.int_from_si(20)
        
        assert (v1 + v2).get_num_si() == 30
        assert (v1 * v2).get_num_si() == 200
        assert (v2 - v1).get_num_si() == 10
        # assert (v2 / v1).get_num_si() == 2 # div might return Val?
        
        assert v1 < v2
        assert v1 <= v2
        assert v2 > v1
        assert v2 >= v1
        assert v1 != v2
        assert v1 == I.Val.int_from_si(10)
        
        assert (-v1).get_num_si() == -10
