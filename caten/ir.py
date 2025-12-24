from __future__ import annotations

from abc import ABCMeta, abstractmethod
from typing import List, Dict, Any, Union
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
    is_ptr: bool = False # for vectorize
    def index(self, indices: List[ATenOp]):
        assert self.ndim == len(indices)
        total = itertools.accumulate([b.index(a) for (a, b) in zip(indices, self.shape)], lambda a, b: Add([a, b]), initial=Const.new(0, index))
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
    args: List[ATenOp]
    T: Union[ATenOpType, None] = None
    # TODO: Cached?
    # def __init__(self, ...)
    def predecessors(self):
        # TODO:
        # - Tに含まれるOpsをReadに含める
        pass
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
@dataclass(frozen=True)
class Neg(ATenOp, UnaryOps):
    """
    OUT = -X
    """
    pass

@dataclass(frozen=True)
class Recip(ATenOp, UnaryOps):
    pass

@dataclass(frozen=True)
class Sin(ATenOp, UnaryOps):
    pass

@dataclass(frozen=True)
class Exp2(ATenOp, UnaryOps):
    pass

@dataclass(frozen=True)
class Log2(ATenOp, UnaryOps):
    pass

@dataclass(frozen=True)
class Sqrt(ATenOp, UnaryOps):
    pass

@dataclass(frozen=True)
class Bitcast(ATenOp, UnaryOps):
    pass

@dataclass(frozen=True)
class Not(ATenOp, UnaryOps):
    """
    Logical not if the X is a boolean
    otherwise lognot ~x
    """
    pass
### BinaryOps
@dataclass(frozen=True)
class Add(ATenOp, BinaryOps):
    """
    OUT = Add(X, Y)
    """
    @classmethod
    def from_ast_expr(cls):
        pass

@dataclass(frozen=True)
class Mul(ATenOp, BinaryOps):
    """
    OUT = Mul(X, Y)
    """
    @classmethod
    def from_ast_expr(cls):
        pass

@dataclass(frozen=True)
class IDiv(ATenOp, BinaryOps):
    pass

@dataclass(frozen=True)
class And(ATenOp, BinaryOps):
    pass

@dataclass(frozen=True)
class Or(ATenOp, BinaryOps):
    pass

@dataclass(frozen=True)
class And(ATenOp, BinaryOps):
    pass

@dataclass(frozen=True)
class Xor(ATenOp, BinaryOps):
    pass

@dataclass(frozen=True)
class Max(ATenOp, BinaryOps):
    pass

@dataclass(frozen=True)
class Mod(ATenOp, BinaryOps):
    pass

@dataclass(frozen=True)
class Neq(ATenOp, BinaryOps):
    pass

@dataclass(frozen=True)
class Lt(ATenOp, BinaryOps):
    pass
### TernaryOps
@dataclass(frozen=True)
class Where(ATenOp, TernaryOps):
    pass

### Allocation
@dataclass(frozen=True)
class Const(ATenOp):
    value: Union[int, float, str] = 0.0
    @staticmethod
    def new(value: Union[int, float, str], dtype: DType):
        return Const(args=[], value=value, T=ATenOpType(shape=[], dtype=dtype))

@dataclass(frozen=True)
class Allocate(ATenOp):
    """
    Allocate(S1, S2, S3, ...)
    """
    @staticmethod
    def new(shape: List[Any], dtype: DType):
        return Allocate([], T=ATenOpType.from_shape(shape, dtype))

@dataclass(frozen=True)
class View(ATenOp):
    """
    View(X, T=T_New)
    """
    @staticmethod
    def new(tensor: ATenOp, view: ATenOpType):
        pass
## == JIT =====================================================================
@dataclass(frozen=True)
class Reduce(ATenOp):
    """
    OUT = Reduce(A, B, op=BinaryOps)
    """
    op: BinaryOps = Add
    @classmethod
    def from_ast_expr(cls):
        pass

@dataclass(frozen=True)
class Store(ATenOp):
    pass
## ControlFlow
@dataclass(frozen=True)
class Range(ATenOp):
    pass

@dataclass(frozen=True)
class Loop(ATenOp):
    pass

@dataclass(frozen=True)
class When(ATenOp):
    pass

@dataclass(frozen=True)
class Progn(ATenOp):
    pass
## == ScheduleOps ============================================================
@dataclass(frozen=True)
class Polyhedral(ATenOp):
    pass

def Var():
    pass

# e.g.:
# a = T.Var("A[m n]", float32)
# P.stmt("...")[a]
