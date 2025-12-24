from __future__ import annotations

from abc import ABCMeta, abstractmethod
from typing import List, Dict, Any
from dataclasses import dataclass
from .dtype import DType

@dataclass(frozen=True)
class ATenAxis():
    shape: ATenOp
    stride: ATenOp
    offset: ATenOp
    incf: ATenOp
    def index(self, i: ATenOp):
        # TODO: Assert i.T.dtype is dtype.index
        return Mul(self.stride, Add(Mul(i, self.incf), self.offset))

@dataclass(frozen=True)
class ATenOpType():
    shape: List[ATenAxis]
    dtype: DType
    offset: ATenOp
    
@dataclass(frozen=True)
class ATenOp(metaclass=ABCMeta):
    args: List[AtenOp]
    T: ATenOpType
    @classmethod
    @abstractmethod
    def from_astexpr(cls):
        pass
    
    @abstractmethod
    def infer_dtype(self):
        pass
## == Tensor Graph ============================================================
class UnaryOps():
    def verify(self): verify_tensor_op(self, 1)
class BinaryOps():
    def verify(self): verify_tensor_op(self, 2)
class TernaryOps():
    def verify(self): verify_tensor_op(self, 3)
### UnaryOps
class Neg(ATenOp, UnaryOps):
    """
    OUT = -X
    """
    pass

class Recip(ATenOp, UnaryOps):
    pass

class Sin(ATenOp, UnaryOps):
    pass

class Exp2(ATenOp, UnaryOps):
    pass

class Log2(ATenOp, UnaryOps):
    pass

class Sqrt(ATenOp, UnaryOps):
    pass

class Cast(ATenOp, UnaryOps):
    pass
    
class Bitcast(ATenOp, UnaryOps):
    pass

class Not(ATenOp, UnaryOps):
    """
    Logical not if the X is a boolean
    otherwise lognot ~x
    """
    pass
### BinaryOps
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

class IDiv(ATenOp, BinaryOps):
    pass

class And(ATenOp, BinaryOps):
    pass

class Or(ATenOp, BinaryOps):
    pass

class And(ATenOp, BinaryOps):
    pass

class Xor(ATenOp, BinaryOps):
    pass

class Max(ATenOp, BinaryOps):
    pass

class Mod(ATenOp, BinaryOps):
    pass

class Neq(ATenOp, BinaryOps):
    pass

class Lt(ATenOp, BinaryOps):
    pass
### TernaryOps
class Where(ATenOp, TernaryOps):
    pass

### Allocation
class Variable(ATenOp):
    symbol: str

class Allocate(ATenOp):
    """
    Allocate(S1, S2, S3, ...)
    """
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

class Store(ATenOp):
    pass
## ControlFlow
class Range(ATenOp):
    pass

class Loop(ATenOp):
    pass

class When(ATenOp):
    pass

class Progn(ATenOp):
    pass
## == ScheduleOps ============================================================
class Polyhedral(ATenOp):
    pass

def Var():
    pass

# e.g.:
# a = T.Var("A[m n]", float32)
# P.stmt("...")[a]
