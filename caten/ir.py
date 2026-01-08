from __future__ import annotations

import dataclasses
import itertools
import math
import operator
import weakref
from dataclasses import dataclass, replace
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
    def range(self) -> Range: return Range((self.size, ))
    def aff(self) -> Aff: return Aff((self.stride, self.range(), self.offset, self.incf))
    def index(self) -> ATenOp: return self.aff().index()

def _const(val: Any, dtype: DType=index) -> ATenOp:
    if isinstance(val, Const): return val
    else: return Const.new(val, dtype)

@dataclass(frozen=True)
class ATenOpType():
    axes: tuple[ATenAxis, ...]
    dtype: DType
    offset: Union[ATenOp, None] = None
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
        raise NotImplementedError(f"verify is not implemented for {cls.__name__}")
        
    def simplify(self) -> ATenOp:
        from caten.simplifier import simplifier
        return simplifier.simplify(self)

    def schedule(self):
        def _explore(item: ATenOp):
            return item.lower(tuple([_explore(arg) for arg in item.args]))
        return _explore(self)
    
    def viz(self) -> str:
        from caten.viz import render
        return render(self)
    
    def dot(self) -> str:
        from caten.viz import get_jupyter_graphviz, to_dot
        return get_jupyter_graphviz(to_dot(self))

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
        dtype = a.T.dtype if isinstance(a, ATenOp) else b.T.dtype # type: ignore
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
    
    def lower(self, args: tuple[ATenOp, ...]) -> ATenOp:
        return replace(self, args=args)
## == Tensor Graph ============================================================
class TensorOps():
    def lower(self, args: tuple[ATenOp, ...]) -> ATenOp:
        new_args = []
        for arg in args:
            if isinstance(arg, Memory) or isinstance(arg, LexFence):
                new_args.append(arg.domain())
            else:
                new_args.append(arg)
        out = replace(self, args=tuple(new_args))
        assert out.T is not None
        tmp = Memory.defglobal([arg.size for arg in self.T.axes], self.T.dtype, tmp=True)
        return LexFence.sync(tmp, Store.new(Load.from_tensor(tmp), out))
# UnaryOps verifier: check dtypes/shapes of arguments
class UnaryOps(TensorOps):
    # ops whose first argument is returned dtype
    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: Union[None, ATenOpType], **kwargs: Any) -> ATenOpType:
        assert len(args) == 1, f"UnaryOp {cls.__name__} takes one argument, getting {args}"
        assert args[0].T is not None
        return args[0].T
class BinaryOps(TensorOps):
    # ops whose first argument is returned dtype
    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: Union[None, ATenOpType], **kwargs: Any) -> ATenOpType:
        assert len(args) == 2, f"BinaryOp {cls.__name__} takes two argument, getting {args}"
        assert args[0].T is not None
        return args[0].T
class TernaryOps(TensorOps):
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
class Bitcast(ViewOps, ATenOp):
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
    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: Union[None, ATenOpType], **kwargs: Any) -> ATenOpType:
        assert len(args) == 3, f"TernaryOp {cls.__name__} takes three argument, getting {args}"
        assert args[1].T is not None
        return args[1].T

@dataclass(frozen=True)
class Const(ViewOps, ATenOp):
    value: Union[int, float, str, bool] = 0.0
    @staticmethod
    def new(value: Union[int, float, str, bool, ATenOp], dtype: DType) -> Const:
        assert isinstance(value, (int, float, str, bool, ATenOp)), f"{value} should be int/float/str/bool"
        if isinstance(value, ATenOp):
            return value
        else:
            return Const(args=(), value=value, T=ATenOpType(axes=(), dtype=dtype))

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
        ))

    @staticmethod
    def expand(tensor: ATenOp, shape: tuple[Union[int, ATenOp], ...]) -> View:
        assert tensor.T is not None
        def _expand(old_axis: ATenAxis, new_size: int | float | ATenOp) -> ATenAxis:
            if ATenOp.eql(old_axis.size, new_size): return old_axis
            else:
                assert ATenOp.eql(old_axis.size, 1), f"The axis to expand should be evaluated to 1, getting {old_axis.size}"
                return ATenAxis(size=_const(new_size), stride=Const.new(0, index), offset=Const.new(0, index), incf=Const.new(1, index))
        return View((tensor,), T=ATenOpType(
            axes=tuple([_expand(old_axis, new_size) for (old_axis, new_size) in zip(tensor.T.axes, shape, strict=True)]),
            dtype=tensor.T.dtype,
            offset=tensor.T.offset,
        ))
    # TODO
    # def contiguous
@dataclass(frozen=True)
class Reduce(ViewOps, ATenOp):
    """
    OUT = Reduce(A, B, op=BinaryOps)
    """
    op: type[BinaryOps] = Add
## == ScheduleOps ============================================================
### Array access graph constrainted via only affine functions, sorted by lex order (for symbolic shape)
@dataclass(frozen=True)
class Range(ATenOp):
    """
    OUT = Range(SIZE)
    OUT is the range of [0, SIZE)
    """
    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: Union[None, ATenOpType], **kwargs: Any) -> ATenOpType:
        assert len(args) == 1 and args[0].T is not None, "Range is defined as: Range(SIZE)"
        assert args[0].T.ndim == 0, "Range: SIZE should be given as a scalar"
        assert args[0].T.dtype == index, "Range: SIZE should be type of index"
        return ATenOpType(axes=tuple(), dtype=index, offset=_const(0, index))

@dataclass(frozen=True)
class Aff(ATenOp):
    """
    OUT = Aff(Stride, Range, Offset, Incx)
    which is the equivalent to `Stride(Incx*Range+Offset)` in ir.INDEX
    In lexorder, [Range] -> { Stmt[Incx*Range+Offset] }
                                    Strideth dim
    """
    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: Union[None, ATenOpType], **kwargs: Any) -> ATenOpType:
        assert len(args) == 4, "Aff is defined as: Aff(Stride, Range, Offset, Incx)"
        assert all([arg.T is not None and arg.T.ndim == 0 and arg.T.dtype == index for arg in args]), "Aff: Stride/Range/Offset/Incx should be a scalar typed index"
        assert isinstance(args[1], Range), f"Aff: The second argument should be a Range, getting {args[1].__class__}"
        assert isinstance(args[3], Const), f"Aff: The fourth argument should be a constant, getting {args[3].__class__}"
        return ATenOpType(axes=tuple(), dtype=index, offset=_const(0, index))
    # def index(self) -> Index:
    #     return Index((self.args[0], Mul((self.args[0], Add((Mul((self.args[1], self.args[3])), self.args[2])))),))
@dataclass(frozen=True)
class Load(ATenOp):
    """
    X = Load(Memory | LexFence, Aff1, Aff2, ...)
    Access the (Aff1.index() + Aff2.index() + ...)th element of Array. The dependency is trackable by Polyhedral Compiler.
    """
    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: Union[None, ATenOpType], **kwargs: Any) -> ATenOpType:
        assert len(args) >= 2, "Load: the number of argument should be larger than two"
        assert isinstance(args[0], Memory) or isinstance(args[0], LexFence), "Load: Can only load element from Memory or Array."
        assert args[0].T is not None and args[0].T.ndim > 0, f"Load: the first argument should be array, getting scalar {args[0].__class__}"
        assert all([isinstance(arg, Aff) for arg in args[1:]]), "Load: the index should be specified by the list of Aff."
        return ATenOpType(axes=tuple(), dtype=args[0].T.dtype, offset=_const(0, index))

    @staticmethod
    def from_tensor(tensor: ATenOp) -> Load:
        assert tensor.T is not None
        return Load((tensor,) + tuple([axis.aff() for axis in tensor.T.axes]))
### Scheduling Graph
@dataclass(frozen=True)
class Memory(ViewOps, ATenOp):
    """
    Memory(ATenOp, level="global or local")
    """
    level: str = "global"
    tmp: bool = False
    @staticmethod
    def defglobal(shape: tuple[Any, ...], dtype: DType, tmp: bool=False) -> Memory:
        return Memory((), T=ATenOpType.from_shape(shape, dtype), level="global", tmp=tmp)

    @staticmethod
    def deflocal(shape: tuple[Any, ...], dtype: DType) -> Memory:
        return Memory((), T=ATenOpType.from_shape(shape, dtype), level="local", tmp=True)

    def domain(self) -> Load: return Load.from_tensor(self)

@dataclass(frozen=True)
class LexFence(ViewOps, ATenOp):
    """
    ```
    LexFence(Memory, Store) -> Tensor(Shaped)
    ```
    Wait until all schedule in store is executed.
    """
    @staticmethod
    def sync(memory: Memory, store: Store):
        assert memory.T is not None
        return LexFence((memory, store), T=ATenOpType.from_shape(tuple([s.size for s in memory.T.axes]), memory.T.dtype))

    def domain(self) -> Load: return Load.from_tensor(self)

@dataclass(frozen=True)
class Store(ATenOp):
    @staticmethod
    def new(dst: ATenOp, op: ATenOp):
        assert dst.T is not None
        return Store((dst, op), T=ATenOpType(axes=(), dtype=dst.T.dtype))
    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: Union[None, ATenOpType], **kwargs: Any) -> ATenOpType:
        return cls.T
# TODO: END or SINK after last STORE
# - TRANSFER = STORE(Memory(LOCAL), LOAD(Memory(GLOBAL), IDX))
# - Can have a polyhedron?
# - Can express loop fusion?
# 最終的に何がしたい？効率的にUnionAccessRelからFusionがしたい。
# - IDXにRangeが繋がってくってイメージ？
# - Fusion = IDXをReshapeすることで共通のRangeを参照すること？
# - Advanced Symbolic Graph Processing System
# STORE(AREF(X, IDX(ijk)), AREF(X(IDX(ij))))
# ↑ Can express reduce, softmax, and so on finally
# Interop w/ Symbolic+Polyhedral!
# Real time lowering
# With the help of Polyhedral Compiler
# Refactor: Const val is not ATenOp
# TODO: Schedule --> Runtime
# e.g.:
# a = T.Var("A[m n]", float32)
# P.stmt("...")[a]
