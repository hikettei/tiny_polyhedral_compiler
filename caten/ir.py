from __future__ import annotations

from abc import ABCMeta, abstractmethod
from typing import List, Dict, Any
import itertools
from dataclasses import dataclass
from .dtype import DType, index

@dataclass(frozen=True)
class ATenAxis():
    shape: ATenOp
    stride: ATenOp
    offset: ATenOp
    incf: ATenOp
    def index(self, i: ATenOp):
        assert i.T.dtype == index, "ATenAxis.index: range index should be type of index."
        return Mul(self.stride, Add(Mul(i, self.incf), self.offset))

@dataclass(frozen=True)
class ATenOpType():
    shape: List[ATenAxis]
    dtype: DType
    offset: Union[ATenOp, None] = None
    def index(self, indices: List[ATenOp]):
        assert self.ndim == len(indices)
        total = itertools.accumlate([b.index(a) for (a, b) in zip(indices, self.shape)], lambda a, b: Add([a, b]), initial=Const.new(val, index))
        if self.offset: total = Add([total, self.offset])
        return total
    @property
    def ndim(self): return len(self.shape)
    @staticmethod
    def from_shape(shape: List[Any], dtype: DType) -> ATenOpType:
        def _const(val: int): return Const.new(val, index)
        def _mul(a, b):
            if not isinstance(a, Const): a = _const(a)
            if not isinstance(b, Const): b = _const(b)
            return Mul([a, b])
        strides = tuple(itertools.accumulate(reversed(shape[1:]), _mul, initial=_const(1)))[::-1]
        return ATenOpType(
            shape=[ATenAxis(shape=size, stride=stride, offset=_const(0), incf=_const(1)) for (size, stride) in zip(shape, strides)],
            dtype=dtype,
        )
    def reshape(self):
        pass
    def permute(self):
        pass
    def expand(self):
        pass

@dataclass(frozen=True)
class ATenOp(metaclass=ABCMeta):
    args: List[AtenOp]
    T: Union[ATenOpType, None] = None
    # TODO: Cached?
    # def __init__(self, ...)
    @classmethod
#    @abstractmethod
    def from_astexpr(cls):
        pass   
#    @abstractmethod
    def verify(self):
        pass

    def coalese(self):
        # Simplify myself
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
class Const(ATenOp):
    value: Union[int, float, str]
    @staticmethod
    def new(val: Union[int, float, str], dtype: DType):
        return Const(val, T=ATenOpType(shape=[], dtype=dtype))

class Allocate(ATenOp):
    """
    Allocate(S1, S2, S3, ...)
    """
    @staticmethod
    def new(shape: List[Any], dtype: DType):
        return Allocate(shape, T=ATenOpType.from_shape(shape, dtype))
        
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
