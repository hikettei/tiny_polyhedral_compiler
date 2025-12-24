from __future__ import annotations

from abc import ABCMeta, abstractmethod
from typing import List, Dict, Any

@dataclass(frozen=True)
class ATenOp(metaclass=ABCMeta):
    args: List[AtenOp]
    @abstractmethod
    @classmethod
    def from_astexpr(cls):
        pass
## == Tensor Graph ============================================================
class UnaryOps():   def verify(self): verify_tensor_op(self, 1)
class BinaryOps():  def verify(self): verify_tensor_op(self, 2)
class TernaryOps(): def verify(self): verify_tensor_op(self, 3)

class Add(ATenOp, BinaryOps):
    """
    OUT = Add(X, Y)
    """
    @classmethod
    def from_ast_expr(cls):
        pass

class Mul(ATenOp, BinaryOps):
    """
    OUT = Mul(X, Y)
    """
    @classmethod
    def from_ast_expr(cls):
        pass
## == JIT =====================================================================
class Reduce(ATenOp):
    """
    OUT = Reduce(A, B, op=BinaryOps)
    """
    op: BinaryOps
    @classmethod
    def from_ast_expr(cls):
        pass


def Var():
    pass

a = T.Var("A[m n]", float32)
P.stmt("...")[a]
