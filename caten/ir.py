from __future__ import annotations

import dataclasses
import itertools
import math
import operator
import weakref
from dataclasses import dataclass
from typing import Any, Dict, Union

from .dtype import DType, index


class ATenOpMetaclass(type):
    cache: Dict[tuple, weakref.ReferenceType[ATenOp]] = {}
    @staticmethod
    def _freeze(x: Any) -> Any:
        if isinstance(x, ATenOp): return x
        if dataclasses.is_dataclass(x):
            return (type(x),) + tuple((f.name, ATenOpMetaclass._freeze(getattr(x, f.name))) for f in dataclasses.fields(x))
        if isinstance(x, (list, tuple)):
            return tuple(ATenOpMetaclass._freeze(i) for i in x)
        if isinstance(x, dict):
            return tuple(sorted((k, ATenOpMetaclass._freeze(v)) for k, v in x.items()))
        return x
    def __call__(cls, args: tuple[ATenOp, ...] | list[ATenOp], T: "ATenOpType | None" = None, **kwargs: Any) -> ATenOp:
        T = cls.verify(tuple(args), T, **kwargs) # type: ignore
        wret = ATenOpMetaclass.cache.get(key:=(cls, tuple(args), ATenOpMetaclass._freeze(T), ATenOpMetaclass._freeze(kwargs)), None)
        if wret is not None and (ret:=wret()) is not None: return ret.simplify()
        ATenOpMetaclass.cache[key] = weakref.ref(created:=super().__call__(tuple(args), T=T, **kwargs))
        return created.simplify()

@dataclass(frozen=True)
class ATenAxis():
    size: ATenOp
    stride: ATenOp
    offset: ATenOp
    incf: ATenOp
    def index(self, i: ATenOp) -> ATenOp:
        assert i.T is not None and i.T.dtype == index, "ATenAxis.index: range index should be type of index."
        return Mul((self.stride, Add((Mul((i, self.incf)), self.offset))))

def _const(val: Any, dtype: DType=index) -> ATenOp:
    if isinstance(val, Const): return val
    else: return Const.new(val, dtype)
            
@dataclass(frozen=True)
class ATenOpType():
    axes: tuple[ATenAxis, ...]
    dtype: DType
    offset: Union[ATenOp, None] = None
    is_ptr: bool = False # TODO: for vectorize?
    def index(self, indices: tuple[ATenOp, ...]) -> Any:
        assert self.ndim == len(indices)
        total = itertools.accumulate([b.index(a) for (a, b) in zip(indices, self.axes, strict=True)], lambda a, b: Add((a, b)), initial=Const.new(0, index)) # type: ignore
        if self.offset: total = Add((total, self.offset)) # type: ignore
        return total
    @property
    def ndim(self) -> int: return len(self.axes)
    @staticmethod
    def from_shape(shape: tuple[Any, ...], dtype: DType) -> ATenOpType:
        def _mul(a: Any, b: Any) -> Any: return Mul((_const(a), _const(b)))
        strides = tuple(itertools.accumulate(reversed(shape[1:]), _mul, initial=_const(1)))[::-1]
        return ATenOpType(
            axes=tuple([ATenAxis(size=_const(size), stride=_const(stride), offset=_const(0), incf=_const(1)) for (size, stride) in zip(shape, strides, strict=True)]),
            dtype=dtype,
        )

@dataclass(frozen=True)
class ATenOp(metaclass=ATenOpMetaclass):
    args: tuple[ATenOp, ...]
    T: Union[ATenOpType, None] = None # this should be provided via T=... option, or inferred via verify method. 
    @property
    def predecessors(self) -> tuple[ATenOp, ...]:
        return tuple(self.args) + (tuple(*[tuple((axis.size, axis.stride, axis.offset, axis.incf)) for axis in self.T.axes]) + () if self.T is not None else ()) + ((self.T.offset,) if self.T and self.T.offset is not None else ())
    
    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: Union[None, ATenOpType], **kwargs: Any) -> ATenOpType:
        raise NotImplementedError("Not implemented")

    def simplify(self) -> ATenOp:
        from caten.simplifier import simplifier
        return simplifier.simplify(self)

    def deepwalk(self) -> None:
        pass

    def viz(self) -> None:
        pass

    @property
    def item(self) -> Union[int, float, ATenOp]:
        # Returns scalar value if self is constant folded
        if isinstance(self, Const) and isinstance(self.value, (int, float)):
            return self.value
        else: return self
    # Mixin for computing shapes (required by reshape, etc)
    # TODO: Use same semantic of broadcast as tensor
    def __add__(self, other: Any) -> ATenOp: return Add((self, _const(other)))
    def __radd__(self, other: Any) -> ATenOp: return Add((_const(other), self))
    def __mul__(self, other: Any) -> ATenOp: return Mul((self, _const(other)))
    def __rmul__(self, other: Any) -> ATenOp: return Mul((_const(other), self))
    # note: do not try to overload __eq__ since it is need to compute hash
    @staticmethod
    def eql(a: Union[int, float, ATenOp], b: Union[int, float, ATenOp]) -> bool:
        """
        Compare two scalars (Python numbers or ATenOp scalars) for equality.
        """
        if isinstance(a, (int, float)) and isinstance(b, (int, float)): return (a == b)
        assert isinstance(a, ATenOp) and a.T is not None
        dtype = a.T.dtype if isinstance(a, ATenOp) else b.T.dtype # A or B is asserted to have a dtype
        a, b = _const(a, dtype=dtype), _const(b, dtype=dtype)
        # Note(hikettei): this comparison highly depends on whether they are constant folded.
        # plus, cannot verify the equivalence of A*B and B*A
        return a == b
    @staticmethod
    def equals(a: tuple[Union[int, float, ATenOp], ...], b: tuple[Union[int, float, ATenOp], ...]) -> bool:
        """
        Compare two lists element-wise using `ATenOp.eql`
        """
        if not len(a) == len(b): return False
        for ai, bi in zip(a, b, strict=True):
            if not ATenOp.eql(ai, bi): return False
        return True
## == Tensor Graph ============================================================
class UnaryOps():
    # ops whose first argument is returned dtype
    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: Union[None, ATenOpType], **kwargs: Any) -> ATenOpType:
        assert len(args) == 1, f"UnaryOp {cls.__name__} takes one argument, getting {args}"
        assert args[0].T is not None
        return args[0].T
class BinaryOps():
    # ops whose first argument is returned dtype
    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: Union[None, ATenOpType], **kwargs: Any) -> ATenOpType:
        assert len(args) == 2, f"BinaryOp {cls.__name__} takes two argument, getting {args}"
        assert args[0].T is not None
        return args[0].T
class TernaryOps():
    # ops whose first argument is returned dtype
    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: Union[None, ATenOpType], **kwargs: Any) -> ATenOpType:
        assert len(args) == 3, f"TernaryOp {cls.__name__} takes three argument, getting {args}"
        assert args[0].T is not None
        return args[0].T
class ViewOps():
    # ops whose return dtypes are explicitly provided via T option
    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: Union[None, ATenOpType], **kwargs: Any) -> ATenOpType:
        assert T is not None, f"Cannot create {cls.__name__} without providing T"
        return T
### UnaryOps
@dataclass(frozen=True)
class Neg(UnaryOps, ATenOp):
    """
    OUT = -X
    """
    python_op = lambda x: -x

@dataclass(frozen=True)
class Recip(UnaryOps, ATenOp):
    """
    OUT = 1/X
    """
    python_op = lambda x: 1/x

@dataclass(frozen=True)
class Sin(UnaryOps, ATenOp):
    """
    OUT = sin(X)
    """
    python_op = math.sin

@dataclass(frozen=True)
class Exp2(UnaryOps, ATenOp):
    """
    OUT = exp2(X)
    """
    python_op = math.exp2

@dataclass(frozen=True)
class Log2(UnaryOps, ATenOp):
    """
    OUT = log2(X)
    """
    python_op = math.log2

@dataclass(frozen=True)
class Sqrt(UnaryOps, ATenOp):
    """
    OUT = sqrt(X)
    """
    python_op = math.sqrt

@dataclass(frozen=True)
class Bitcast(UnaryOps, ATenOp):
    pass

@dataclass(frozen=True)
class Not(UnaryOps, ATenOp):
    """
    Logical not if the X is a boolean
    otherwise lognot ~x
    """
### BinaryOps
@dataclass(frozen=True)
class Add(BinaryOps, ATenOp):
    """
    OUT = Add(X, Y)
    """
    python_op = operator.add

@dataclass(frozen=True)
class Mul(BinaryOps, ATenOp):
    """
    OUT = Mul(X, Y)
    """
    python_op = operator.mul

@dataclass(frozen=True)
class IDiv(BinaryOps, ATenOp):
    """
    OUT = A // B
    """
    python_op = operator.floordiv

@dataclass(frozen=True)
class And(BinaryOps, ATenOp):
    pass

@dataclass(frozen=True)
class Or(BinaryOps, ATenOp):
    pass

@dataclass(frozen=True)
class Xor(BinaryOps, ATenOp):
    pass

@dataclass(frozen=True)
class Max(BinaryOps, ATenOp):
    python_op = max

@dataclass(frozen=True)
class Mod(BinaryOps, ATenOp):
    python_op = operator.mod

@dataclass(frozen=True)
class Neq(BinaryOps, ATenOp):
    python_op = operator.ne

@dataclass(frozen=True)
class Lt(BinaryOps, ATenOp):
    python_op = operator.lt
### TernaryOps
@dataclass(frozen=True)
class Where(TernaryOps, ATenOp):
    python_op = lambda a, b, c: b if a else c

### Allocation
@dataclass(frozen=True)
class Const(ViewOps, ATenOp):
    value: Union[int, float, str, bool] = 0.0
    @staticmethod
    def new(value: Union[int, float, str, bool], dtype: DType) -> Const:
        return Const(args=(), value=value, T=ATenOpType(axes=(), dtype=dtype))

@dataclass(frozen=True)
class Allocate(ViewOps, ATenOp):
    """
    Allocate(S1, S2, S3, ...)
    """
    @staticmethod
    def new(shape: tuple[Any, ...], dtype: DType) -> Allocate:
        return Allocate((), T=ATenOpType.from_shape(shape, dtype))

@dataclass(frozen=True)
class View(ViewOps, ATenOp):
    """
    View(X, T=T_New)
    """
    # This is the definition of view
    @staticmethod
    def reshape(tensor: ATenOp, shape: tuple[ATenOp, ...]) -> View:
        assert tensor.T is not None
        return View((tensor,), T=ATenOpType.from_shape(shape, tensor.T.dtype))

    @staticmethod
    def permute(tensor: ATenOp, order: tuple[int, ...]) -> View:
        assert tensor.T is not None
        return View((tensor,), T=ATenOpType(
            axes=tuple([tensor.T.axes[i] for i in order]),
            dtype=tensor.T.dtype,
            offset=tensor.T.offset,
            is_ptr=tensor.T.is_ptr
        ))

    @staticmethod
    def expand(tensor: ATenOp, shape: tuple[Union[int, ATenOp], ...]) -> View:
        assert tensor.T is not None
        def _expand(old_axis: ATenAxis, new_size: int | float | ATenOp) -> ATenAxis:
            if ATenOp.eql(old_axis.size, new_size): return old_axis
            else:
                assert ATenOp.eql(old_axis.size, 1), f"The axis to expand should be evaluated to 1, getting {old_axis}" # Fix: old_axis -> old_axis.size
                return ATenAxis(size=_const(new_size), stride=Const.new(0, index), offset=Const.new(0, index), incf=Const.new(1, index))
        return View((tensor,), T=ATenOpType(
            axes=tuple([_expand(old_axis, new_size) for (old_axis, new_size) in zip(tensor.T.axes, shape, strict=True)]),
            dtype=tensor.T.dtype,
            offset=tensor.T.offset,
            is_ptr=tensor.T.is_ptr
        ))
## == JIT =====================================================================
@dataclass(frozen=True)
class Reduce(ATenOp):
    """
    OUT = Reduce(A, B, op=BinaryOps)
    """
    op: type[BinaryOps] = Add
    @classmethod
    def from_ast_expr(cls) -> None:
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

def Var() -> None:
    pass

# e.g.:
# a = T.Var("A[m n]", float32)
# P.stmt("...")[a]
