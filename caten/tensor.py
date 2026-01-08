from __future__ import annotations

import math
import os
from abc import ABCMeta, abstractmethod
from typing import Any, Callable, Tuple, Union

import caten.ir as ir
from caten.helpers import align_left, argfix, prod

from .dtype import DType, default_float, floats, index, integers

TOperand = Union[ir.ATenOp, int, float] 
## Backend Abstraction
DEVICE_TO_TENSOR = {}
def get_backend() -> str: return os.environ.get("BACKEND", "CPU")
## Tensor annotation for jit/aot shape check
class ATenSpec:
    """
    C.Tensor[M, N] -> ATenSpec(M N)
    """
    def __init__(self, shape: Tuple[Any, ...]):
        self.shape: tuple[Union[int, str], ...] = shape
    def __repr__(self) -> str: return f"ATenSpec{self.shape}"

class ATen:
    op: ir.ATenOp # ATen is just a wrapper for ATenOp
    def __init__(self, *args: Any, op: Union[None, ir.ATenOp]=None, dtype:DType=default_float):
        self.op = (op or ir.Memory.defglobal(tuple(args[0]), dtype)).schedule()
    @staticmethod
    def register(device_id: str, cls: Any) -> None:
        DEVICE_TO_TENSOR[device_id] = cls
    @classmethod
    def from_shape(cls, shape: tuple[int|ir.ATenOp, ...], dtype: DType=default_float) -> ATen: return Tensor(op=ir.Memory.defglobal(shape, dtype))
    @classmethod
    def const(cls, obj: Any, dtype: DType=index) -> ir.Const:
        match obj:
            case int(): assert dtype in integers
            case float(): assert dtype in floats
            case str(): pass
            case _: raise TypeError(f"ATen.const: Only integer or float objects can become constant! getting {obj}")
        return ir.Const.new(obj, dtype)
    @staticmethod
    def wrap_const(obj: Union[ATen, ir.ATenOp, float, int], dtype: DType = index) -> ir.ATenOp:
        """
        Ensures obj is a constant of dtype
        """
        if isinstance(obj, ATen):
            assert obj.dtype == dtype # todo: decent error msg
            return obj.op
        elif isinstance(obj, ir.ATenOp):
            assert obj.T is not None and obj.T.dtype == dtype # todo: decent error msg
            return obj
        else:
            return ATen.const(obj, dtype=dtype)
    
    @staticmethod
    def unwrap(obj: ATen|TOperand) -> TOperand:
        return obj.op if isinstance(obj, ATen) else obj

    def forward(self, op: Callable, args: tuple[ir.ATenOp, ...], **kwargs: Any) -> Tensor: return Tensor(op=op(args, **kwargs))
    def __class_getitem__(cls, item: Union[Any, Tuple[Any, ...]]) -> ATenSpec: return ATenSpec(item)
    def viz(self) -> str: return self.op.viz()
    def dot(self) -> str: return self.op.dot()
    def __repr__(self) -> str:
        shape = [s.item for s in self.shape] # if expr, render!
        return f"{self.__class__.__name__}<shape={shape}, dtype={self.dtype}>"
    
    @property
    def shape(self) -> tuple[ir.ATenOp, ...]:
        assert self.op.T is not None
        return tuple([x.size for x in self.op.T.axes])
    @property
    def strides(self) -> tuple[ir.ATenOp, ...]:
        assert self.op.T is not None
        return tuple([x.stride for x in self.op.T.axes])
    @property
    def dtype(self) -> DType:
        assert self.op.T is not None
        return self.op.T.dtype
    @property
    def ndim(self) -> int: return len(self.shape)
    def _resolve_dim(self, dim: int, *, extra: bool = False) -> int:
        total = self.ndim + int(extra)
        if not -max(1, total) <= dim <= max(1, total) - 1:
            raise IndexError(f"{dim=} out of range {[-max(1, total), max(1, total) - 1]}")
        return dim + total if dim < 0 else dim
    # ref: https://github.com/tinygrad/tinygrad/blob/master/tinygrad/mixin/movement.py#L58
    def _broadcast_to(self, new_shape: tuple[ir.ATenOp, ...]) -> ATen:
        """
        Implements Numpy-Semantic Broadcasting operation
        """
        if ir.ATenOp.equals(self.shape, new_shape): return self
        if self.ndim > len(new_shape):
            raise ValueError(f"cannot broadcast tensor to fewer dimensions. shape={self.shape} to {new_shape}")
        shape, _ = align_left(self.shape, new_shape)
        if not all(ir.ATenOp.eql(s, ns) or ir.ATenOp.eql(s, 1) for s, ns in zip(shape, new_shape, strict=True)):
            raise ValueError(f"cannot broadcast {self.shape} to {new_shape=}")
        reshaped = self.reshape(shape)
        ret = Tensor(op=ir.View.expand(reshaped.op, new_shape))
        return reshaped if ir.ATenOp.equals(ret.shape, reshaped.shape) else ret

    def reshape(self, shape: tuple[Union[int, ir.ATenOp], ...], *args: Any) -> ATen:
        new_shape = tuple([s if s is not None else self.shape[i] for i, s in enumerate(argfix(shape, *args))])
        if (c := new_shape.count(-1)) > 1:
            raise RuntimeError(f"only one dimension can be inferred using -1, getting {new_shape}")
        if c: new_shape = tuple([-prod(self.shape) // prod(new_shape) if ir.ATenOp.eql(s, -1) else s for s in new_shape]) # type: ignore
        if not ir.ATenOp.eql(prod(self.shape), prod(new_shape)):
            raise ValueError(f"size mismatch, can't reshape ({self.shape}) -> ({new_shape})")
        ret = Tensor(op=ir.View.reshape(self.op, tuple([ATen.wrap_const(s, dtype=index) for s in new_shape])))
        return self if ir.ATenOp.equals(ret.shape, self.shape) else ret
    
    def shrink(self, arg: tuple[tuple[int, int] | None, ...]) -> Tensor:
        raise NotImplementedError("shrink todo")

    def permute(self, order: tuple[int, ...], *args: Any) -> ATen:
        order_arg = tuple(self._resolve_dim(x) for x in argfix(order, *args))
        if sorted(order_arg) != list(range(self.ndim)):
            raise RuntimeError(f"order is not a valid permutation, getting {order_arg}")
        return Tensor(op=ir.View.permute(self.op, order_arg)) if order_arg != tuple(range(self.ndim)) else self

    def expand(self, shape: tuple[Union[int, ir.ATenOp], ...], *args: Any) -> ATen:
        new_shape = tuple(from_ if ir.ATenOp.eql(to, -1) or to is None else to for from_, to in zip(*(align_left(self.shape, argfix(shape, *args))), strict=True))
        return self._broadcast_to(tuple([ATen.wrap_const(s, dtype=index) for s in new_shape]))

    def _broadcasted(self, y_:ATen|TOperand, reverse:bool=False) -> tuple[ir.ATenOp, ir.ATenOp]:
        x: ATen = self
        y: ATen = Tensor(op=ATen.wrap_const(y_, x.dtype))
        assert isinstance(x, ATen) and isinstance(y, ATen)
        if x.dtype != y.dtype:
            raise TypeError("Cannot add x and y (dtypes mismatch, todo)")
        if reverse: x, y = y, x
        # compute the output shape
        def _broadcast_shape(*shapes:tuple[int|ir.ATenOp, ...]) -> tuple[ir.ATenOp, ...]:
            def smax(a: int|ir.ATenOp, b: int|ir.ATenOp) -> ir.ATenOp:
                if ir.ATenOp.eql(a, 1): return ir._const(b, index)
                elif ir.ATenOp.eql(b, 1): return ir._const(a, index)
                else:
                    assert ir.ATenOp.eql(a, b), f"Cannot broadcast two shape: {a} vs {b}"
                    return ir._const(a, index) # a != b is asserted here?
            return tuple(smax(*nth_dim_sizes) for nth_dim_sizes in zip(*align_left(*shapes), strict=True))
        out_shape = _broadcast_shape(x.shape, y.shape)
        return x._broadcast_to(out_shape).op, y._broadcast_to(out_shape).op
    # TODO:
    # - reduce option
    # - ir.Add.new (or binop) can have reduce option
    def add(self, other: ATen|TOperand, reverse:bool=False) -> Tensor: return self.forward(ir.Add, self._broadcasted(other, reverse=reverse))
    def sub(self, other: ATen|TOperand, reverse:bool=False) -> Tensor:
        x: ATen = self
        y: ATen = Tensor(op=ATen.wrap_const(other, x.dtype))
        if reverse: x, y = y, x
        return x.add(y.neg())
    def mul(self, other: ATen|TOperand, reverse:bool=False) -> Tensor: return self.forward(ir.Mul, self._broadcasted(other, reverse=reverse))
    def idiv(self, other: ATen|TOperand, reverse:bool=False) -> Tensor: return self.forward(ir.IDiv, self._broadcasted(other, reverse=reverse))
    def div(self, other: ATen|TOperand, reverse:bool=False) -> Tensor:
        x: ATen = self
        y: ATen = Tensor(op=ATen.wrap_const(other, x.dtype))
        if reverse: x, y = y, x
        return x.mul(y.recip())

    def max(self, other: ATen|TOperand, reverse:bool=False) -> Tensor: return self.forward(ir.Max, self._broadcasted(other, reverse=reverse))
    def mod(self, other: ATen|TOperand, reverse:bool=False) -> Tensor: return self.forward(ir.Mod, self._broadcasted(other, reverse=reverse))
    def ne(self, other: ATen|TOperand, reverse:bool=False) -> Tensor: return self.forward(ir.Neq, self._broadcasted(other, reverse=reverse))
    def lt(self, other: ATen|TOperand, reverse:bool=False) -> Tensor: return self.forward(ir.Lt, self._broadcasted(other, reverse=reverse))

    def bitwise_and(self, other: ATen|TOperand, reverse:bool=False) -> Tensor: return self.forward(ir.And, self._broadcasted(other, reverse=reverse))
    def bitwise_or(self, other: ATen|TOperand, reverse:bool=False) -> Tensor: return self.forward(ir.Or, self._broadcasted(other, reverse=reverse))
    def bitwise_xor(self, other: ATen|TOperand, reverse:bool=False) -> Tensor: return self.forward(ir.Xor, self._broadcasted(other, reverse=reverse))

    def where(self, x: ATen|TOperand, y: ATen|TOperand) -> Tensor:
        dtype = self.dtype
        if isinstance(x, Tensor): dtype = x.dtype
        elif isinstance(y, Tensor): dtype = y.dtype
        
        x_: ATen = Tensor(op=ATen.wrap_const(x, dtype))
        y_: ATen = Tensor(op=ATen.wrap_const(y, dtype))
        return self.forward(ir.Where, (self.op, x_._broadcast_to(self.shape).op, y_._broadcast_to(self.shape).op))

    def __add__(self, other: ATen|TOperand) -> Tensor: return self.add(other)
    def __radd__(self, other: ATen|TOperand) -> Tensor: return self.add(other, reverse=True)
    def __sub__(self, other: ATen|TOperand) -> Tensor: return self.sub(other)
    def __rsub__(self, other: ATen|TOperand) -> Tensor: return self.sub(other, reverse=True)
    def __mul__(self, other: ATen|TOperand) -> Tensor: return self.mul(other)
    def __rmul__(self, other: ATen|TOperand) -> Tensor: return self.mul(other, reverse=True)
    def __floordiv__(self, other: ATen|TOperand) -> Tensor: return self.idiv(other) 
    def __truediv__(self, other: ATen|TOperand) -> Tensor: return self.div(other)
    def __rtruediv__(self, other: ATen|TOperand) -> Tensor: return self.div(other, reverse=True)
    def __mod__(self, other: ATen|TOperand) -> Tensor: return self.mod(other)
    def __rmod__(self, other: ATen|TOperand) -> Tensor: return self.mod(other, reverse=True)

    def __lt__(self, other: ATen|TOperand) -> Tensor: return self.lt(other)
    def __gt__(self, other: ATen|TOperand) -> Tensor: return Tensor(op=ATen.wrap_const(other, self.dtype)).lt(self)
    def __ne__(self, other: ATen|TOperand) -> Tensor: return self.ne(other) # type: ignore[override]
    def __matmul__(self, other: ATen|TOperand) -> Tensor: return self.matmul(other)

    def neg(self) -> Tensor: return self.forward(ir.Neg, (self.op,))
    def recip(self) -> Tensor: return self.forward(ir.Recip, (self.op,))
    def sin(self) -> Tensor: return self.forward(ir.Sin, (self.op,))
    def cos(self) -> Tensor: return (self + Tensor.const(math.pi / 2, dtype=self.dtype)).sin()
    def exp2(self) -> Tensor: return self.forward(ir.Exp2, (self.op,))
    def log2(self) -> Tensor: return self.forward(ir.Log2, (self.op,))
    def sqrt(self) -> Tensor: return self.forward(ir.Sqrt, (self.op,))

    def reduce(self, axis: int | tuple[int, ...] | None = None, keepdim: bool = False, op: Callable = ir.Add) -> Tensor:
        # TODO: initial elements
        assert self.op.T is not None
        axes = tuple(range(self.ndim)) if axis is None else (tuple(axis) if isinstance(axis, (tuple, list)) else (axis,))
        axes = tuple(self._resolve_dim(x) for x in axes)
        reduce_axes, out_shape = [], []
        for i in range(self.ndim):
            if i in axes:
                reduce_axes.append(1)
            else:
                reduce_axes.append(self.op.T.axes[i].size)
                out_shape.append(self.op.T.axes[i].size)
        out = ir.Memory.defglobal(reduce_axes, dtype=self.op.T.dtype, tmp=True)
        out = ir.View.expand(out, tuple([arg.size for arg in self.op.T.axes]))
        return self.forward(ir.Reduce, (out, self.op), bop=op, axis=axes, keepdim=keepdim)
        
    def sum(self, axis: int | tuple[int, ...] | None = None, keepdim: bool = False) -> Tensor:
        return self.reduce(axis=axis, keepdim=keepdim, op=ir.Add)

    def matmul(self, other: ATen|TOperand) -> Tensor:
        x: ATen = self
        y: ATen = Tensor(op=ATen.wrap_const(other, x.dtype))
        if x.ndim < 1 or y.ndim < 1: raise ValueError("matmul requires at least 1D tensors")
        x_shape = x.shape
        x_expanded = x.reshape(x_shape[:-1] + (1, x_shape[-1]))
        y_permuted = y.permute(tuple(range(y.ndim-2)) + (y.ndim-1, y.ndim-2))
        y_expanded = y_permuted.reshape(y_permuted.shape[:-2] + (1,) + y_permuted.shape[-2:])
        return x_expanded.mul(y_expanded).sum(axis=-1)

class TensorImpl(ATen, metaclass=ABCMeta):
    @abstractmethod
    def allocate(self) -> None: ...
    @abstractmethod
    def free(self) -> None: ...
    @abstractmethod
    def compile(self) -> None: ...
    @staticmethod
    @abstractmethod
    def render(op: Any) -> None: ...

class Tensor(ATen):
    def __new__(cls: Any, *args: Any, **kwargs: Any) -> Any:
        impl = DEVICE_TO_TENSOR.get(get_backend())
        if impl is None: raise ValueError(f"Unknown BACKEND={get_backend()}")
        return impl(*args, **kwargs)
## == [Symbolic] ==============================================================
def Placeholder() -> None: ...
def Local() -> None: ...
def Vars(contents: str, dtype:DType=index) -> tuple[ir.ATenOp, ...]:
    """
    Declares a list of placeholders
    e.g.: M, N, K = C.vars("M, N, K")
    """
    return tuple([Tensor.const(char, dtype=dtype) for char in contents.replace(" ", "").split(",")])

## == [Loop-For Style Frontend IR Specs] ======================================
def kernel(get_kernel: bool = False) -> Callable:
    def decorator(func: Callable) -> Callable:
        return func
    return decorator
# 今我々が考えるべきこと
# Symbolic Array: Add more simplification rules and tests for it, e.g.: A*1 is A
# C.range/C.when/C.kernel/C.global/C.local Construction
# how can we construct a polyhedrla model?
# then how do we fuse them? without searching and fast
# how to generate polyhedral model from tensor ops?
# rangeify -> range/when ==> polyhedral model
# with C.range(10, 10):
# with C.when(10, 10)
# Then, how to fuse?
# TODO: relocated to ir.py (and rename ir.py -> air(abstraction ir).py?)
# TODO: with style constructor

# Lowered IR Design 3 Requirements:
# - Smart polyhedral model integration (can fuse!)
# - Rangifyみたく，Lowerされた/されてないOpを一緒に計算したい。
# - TopologicalSortするとTopDownである, ASTもTopDownである？
# - Reduction Semantic
# - When(Filter) Semantic
# TensorOp
# X2[0..3, 0..3] = sin(X1[0..3, 0..3))
# i = C.range(3)
# j = C.range(3)
# x3[i, j] = cos(x2[i, j])
# cond = C.when(i < j)
# 
# What would be smth like intersection(TensorOp, KernelGraph)
# TensorOp: DAG, TopDown, every variable has a shape
# KernelGraph: BottomUp+TopDown,
# Then, early rangify should be doable!
# IRの理論をきちんと構築してからコードを書こう
# Workload:
# - [ ] Perfect IR for tensor
#   - [ ] Polyhedral Loop Fusion
class Range():
    # ctx: contextlib.contextmanager
    # __with__ will append ...
    pass

class When(): # its root should be always Range!
    pass

class LocalVar():
    pass
