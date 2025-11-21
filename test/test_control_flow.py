import os

import caten as C
from caten.ops import ControlOps


def test_when_context():
    os.environ["RUNTIME"] = "CLANG"
    
    @C.kernel(get_kernel=True)
    def conditional_kernel(A: C.Tensor[10]):
        with C.range(10) as i:
            with C.when(i < 5):
                A[i] = 0.0
                
    k = conditional_kernel(C.Tensor(10, name="A"))
    
    has_if = False
    def visit(nodes):
        nonlocal has_if
        for n in nodes:
            if n.op == ControlOps.IF:
                has_if = True
                visit(n.arg[1]) # Then block
            if n.op == ControlOps.RANGE:
                visit(n.arg[2]) # Body block
    
    visit(k.graph)
    assert has_if
