from caten.kernel import Symbol
from caten.ops import BinaryOps, MetaOps, Node, PatternMatcher, UPat


def test_pattern_matcher_simple():
    # Rule: ADD(x, 0) -> x
    
    def rewrite_add_zero(x):
        return x
    
    pm = PatternMatcher([
        (UPat(BinaryOps.ADD, src=(UPat.var("x"), UPat(MetaOps.CONST, arg=0))), rewrite_add_zero)
    ])
    
    x = Node(MetaOps.VAR, (), arg=Symbol("x"))
    zero = Node(MetaOps.CONST, (), arg=0)
    add_node = Node(BinaryOps.ADD, (x, zero))
    
    res = pm.rewrite(add_node)
    assert res is x

def test_pattern_matcher_nested():
    # Rule: MUL(ADD(a, b), c)
    
    matched = False
    def callback(a, b, c):
        nonlocal matched
        matched = True
        return None
        
    pm = PatternMatcher([
        (UPat(BinaryOps.MUL, src=(
            UPat(BinaryOps.ADD, src=(UPat.var("a"), UPat.var("b"))),
            UPat.var("c")
        )), callback)
    ])
    
    a = Node(MetaOps.VAR, (), arg=Symbol("a"))
    b = Node(MetaOps.VAR, (), arg=Symbol("b"))
    c = Node(MetaOps.VAR, (), arg=Symbol("c"))
    
    add_node = Node(BinaryOps.ADD, (a, b))
    mul_node = Node(BinaryOps.MUL, (add_node, c))
    
    pm.rewrite(mul_node)
    assert matched