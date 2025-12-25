from __future__ import annotations

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
        self.op = op or ir.Allocate.new(tuple(args[0]), dtype)

    @staticmethod
    def register(device_id: str, cls: Any) -> None:
        DEVICE_TO_TENSOR[device_id] = cls
    @classmethod
    def from_shape(cls, shape: tuple[int|ir.ATenOp, ...], dtype: DType=default_float) -> ATen: return Tensor(op=ir.Allocate.new(shape, dtype))
    @classmethod
    def const(cls, obj: Any, dtype: DType=index) -> ir.Const:
        match obj:
            case int(): assert dtype in integers
            case float(): assert dtype in floats
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
        ret = Tensor(op=ir.View.expand(self.op, new_shape))
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
                    assert ir.ATenOp.eql(a, b)
                    return ir._const(a, index) # a != b is asserted here?
            return tuple(smax(*nth_dim_sizes) for nth_dim_sizes in zip(*align_left(*shapes), strict=True))
        out_shape = _broadcast_shape(x.shape, y.shape)
        return x._broadcast_to(out_shape).op, y._broadcast_to(out_shape).op
    # TODO:
    # - reduce option
    # - ir.Add.new (or binop) can have reduce option
    def add(self, other: ATen|TOperand, reverse:bool=False) -> Tensor: return self.forward(ir.Add, self._broadcasted(other, reverse=reverse))
    def mul(self, other: ATen|TOperand, reverse:bool=False) -> Tensor: return self.forward(ir.Mul, self._broadcasted(other, reverse=reverse))
    def idiv(self, other: ATen|TOperand, reverse:bool=False) -> Tensor: return self.forward(ir.IDiv, self._broadcasted(other, reverse=reverse))
    # def __eq__(self, other: Any): pass
    # def __neq__(self, other: Any): pass
    def __add__(self, other: ATen|TOperand) -> Tensor: return self.add(other)
    def __radd__(self, other: ATen|TOperand) -> Tensor: return self.add(other, reverse=True)
    def __mul__(self, other: ATen|TOperand) -> Tensor: return self.mul(other)
    def __rmul__(self, other: ATen|TOperand) -> Tensor: return self.mul(other, reverse=True)
    def __floordiv__(self, other: ATen|TOperand) -> Tensor: return self.idiv(other) 
    def neg(self) -> Tensor: return self.forward(ir.Neg, (self.op,))
    def sin(self) -> Tensor: return self.forward(ir.Sin, (self.op,))
    def cos(self) -> Tensor: return self.forward(ir.Sin, ((self + Tensor.const(0.0, dtype=self.dtype)).op,))

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
## == [Loop-For Style Frontend IR Specs] ======================================
def kernel(get_kernel: bool = False) -> Callable:
    def decorator(func: Callable) -> Callable:
        return func
    return decorator
# how to generate polyhedral model from tensor ops?
# rangeify -> range/when ==> polyhedral model
# with C.range(10, 10):
# with C.when(10, 10)
class Range():
    pass

class When():
    pass

class LocalVar():
    pass
