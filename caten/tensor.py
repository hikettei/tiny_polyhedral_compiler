from __future__ import annotations

import os
from abc import ABCMeta, abstractmethod
from typing import Any, Callable, ClassVar, List, Self, Tuple, Union

import caten.ir as ir
from caten.helpers import align_left, argfix, prod

from .dtype import DType, default_float, floats, index, integers

## Backend Abstraction
DEVICE_TO_TENSOR = {}
def get_backend(): return os.environ.get("BACKEND", "CPU")
## Tensor annotation for jit/aot shape check
class ATenSpec:
    """
    C.Tensor[M, N] -> ATenSpec(M N)
    """
    def __init__(self, shape: Tuple[Any, ...]):
        self.shape: List[Union[int, str]] = shape
    def __repr__(self) -> str: return f"ATenSpec{self.shape}"
## Tensor compiler core
class ATen:
    op: ir.ATenOp # ATen is just a wrapper for ATenOp
    @classmethod
    def from_shape(cls, shape: List[ir.ATenOp], dtype: DType=default_float): return Tensor(op=ir.Allocate.new(shape, dtype))
    @classmethod
    def const(cls, obj: Any, dtype: DType=index):
        match obj:
            case int(): assert dtype in integers
            case float(): assert dtype in floats
            case _: raise TypeError(f"ATen.const: Only integer or float objects can become constant! getting {obj}")
        return ir.Const.new(obj, dtype)
    def forward(self, op: Callable, *args: List, **kwargs) -> ATen: return Tensor(op=op(*args, **kwargs))
    def __class_getitem__(cls, item: Union[Any, Tuple[Any, ...]]) -> ATenSpec: return ATenSpec(item)
    def __repr__(self) -> str:
        # TODO: Display Shape, realized buffer, etc.
        return f"{self.__class__.__name__}<{self.op}>"
    @property
    def dtype(self): return self.op.T.dtype
    @staticmethod
    def wrap_const(obj: Any, dtype: DType = index):
        """
        Ensures obj is a constant of dtype
        """
        if isinstance(obj, ATen):
            assert obj.dtype == dtype # todo: decent error msg
            return obj
        else:
            return ATen.const(obj, dtype=dtype)
    @staticmethod
    def top(f: Callable[Any, ATen]):
        """
        Declares the given function as toplevel tensor operation.
        """
        # TODO: Toplevel in helpers.py
        return f
## movement ops mixin
class ATenMovements():
    @property
    def shape(self) -> List[ATen]: return [x.size for x in self.op.T.axes]
    @property
    def strides(self) -> List[ATen]: return [x.stride for x in self.op.T.axes]
    @property
    def ndim(self) -> int: return len(self.shape)
    def _resolve_dim(self, dim: int, *, extra: bool = False) -> int:
        total = self.ndim + int(extra)
        if not -max(1, total) <= dim <= max(1, total) - 1:
            raise IndexError(f"{dim=} out of range {[-max(1, total), max(1, total) - 1]}")
        return dim + total if dim < 0 else dim
    # ref: https://github.com/tinygrad/tinygrad/blob/master/tinygrad/mixin/movement.py#L58
    def _broadcast_to(self, new_shape: List[ATen]) -> Self:
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
    @ATen.top
    def reshape(self, shape, *args) -> Self:
        new_shape = tuple([s if s is not None else self.shape[i] for i, s in enumerate(argfix(shape, *args))])
        if (c := new_shape.count(-1)) > 1:
            raise RuntimeError(f"only one dimension can be inferred using -1, getting {new_shape}")
        if c: new_shape = tuple([-prod(self.shape) // prod(new_shape) if ir.ATenOp.eql(s, -1) else s for s in new_shape])
        if not ir.ATenOp.eql(prod(self.shape), prod(new_shape)):
            raise ValueError(f"size mismatch, can't reshape ({self.shape}) -> ({new_shape})")
        ret = Tensor(op=ir.View.reshape(self.op, [ATen.wrap_const(s, dtype=index) for s in new_shape]))
        return self if ir.ATenOp.equals(ret.shape, self.shape) else ret
    @ATen.top
    def shrink(self, arg: tuple[tuple[int, int] | None, ...]) -> Self:
        raise NotImplementedError("shrink todo")
    @ATen.top
    def permute(self, order, *args) -> Self:
        order_arg = tuple(self._resolve_dim(x) for x in argfix(order, *args))
        if sorted(order_arg) != list(range(self.ndim)):
            raise RuntimeError(f"order is not a valid permutation, getting {order_arg}")
        return Tensor(op=ir.View.permute(self.op, order_arg)) if order_arg != tuple(range(self.ndim)) else self
    @ATen.top
    def expand(self, shape, *args) -> Self:
        new_shape = tuple(from_ if ir.ATenOp.eql(to, -1) or to is None else to for from_, to in zip(*(align_left(self.shape, argfix(shape, *args))), strict=True))
        return self._broadcast_to([ATen.wrap_const(s, dtype=index) for s in new_shape])

## arithmetic mixin
class ATenArith():
    def _broadcasted(self, y:Tensor|int|float, reverse:bool=False) -> tuple[Tensor, Tensor]:
        x: ATen = self
        if not isinstance(y, Tensor):
            y = Tensor.const(y, dtype=x.dtype)
        if x.dtype != y.dtype:
            raise TypeError("Cannot add x and y (dtypes mismatch, todo)")
        if reverse: x, y = y, x
        # compute the output shape
        def _broadcast_shape(*shapes:tuple[int, ...]) -> tuple[int, ...]:
            def smax(a, b):
                if ir.ATenOp.eql(a, 1): return b
                elif ir.ATenOp.eql(b, 1): return a
                else:
                    assert ir.ATenOp.eql(a, b)
                    return a # a != b is asserted here?
            return tuple(0 if 0 in nth_dim_sizes else smax(nth_dim_sizes) for nth_dim_sizes in zip(*align_left(*shapes), strict=True))
        out_shape = _broadcast_shape(x.shape, y.shape)
        return x._broadcast_to(out_shape), y._broadcast_to(out_shape)
    # TODO:
    # - reduce option
    # - ir.Add.new (or binop) can have reduce option
    @ATen.top
    def add(self, other, reverse:bool=False): return self.forward(ir.Add, tuple(self._broadcasted(self, other, reverse=reverse)))
    @ATen.top
    def mul(self, other, reverse:bool=False): return self.forward(ir.Mul, tuple(self._broadcasted(self, other, reverse=reverse)))
    def __eq__(self, other: Any):
        print("A")
        pass
    def __neq__(self, other: Any):
        print("B")
        pass
    def __add__(self, other: Any): return self.add(other)
    def __radd__(self, other: Any): return self.add(other, reverse=True)
    def __mul__(self, other: Any): return self.mul(other)
    def __rmul__(self, other: Any): return self.mul(other, reverse=True)
## math mixin
class ATenMath():
    @ATen.top
    def sin(self: ATen): return self.forward(ir.Sin, self)
    @ATen.top
    def cos(self: ATen): return self.forward(ir.Sin, self + Tensor.const(0.0, dtype=self.dtype))
## nn ops mixin
class ATenNN():
    pass
## linalg ops mixin
class ATenLinalg():
    pass
## facet mixin
class Facet():
    # Facet is device transfer abstraction: A.to("CUDA")
    # TODO: with tensor.facet("CUDA") as tensor: ...
    pass
## abstraction over backends
class ATenBase(ATen, ATenMovements, ATenArith,ATenMath, ATenNN, ATenLinalg, metaclass=ABCMeta):
    def __init__(self, *args, op=None):
        self.op = op
        
    ## == AbstractionLayer
    @staticmethod
    def register(device_id: str, cls: ClassVar):
        DEVICE_TO_TENSOR[device_id] = cls
    
    @abstractmethod
    def allocate(self):
        pass

    @abstractmethod
    def free(self):
        pass

    @abstractmethod
    def compile(self):
        pass

    @staticmethod
    @abstractmethod
    def render(op):
        pass

class Tensor(ATenBase):
    def __new__(cls, *args, **kwargs):
        impl = DEVICE_TO_TENSOR.get(get_backend())
        if impl is None: raise ValueError(f"Unknown BACKEND={get_backend()}")
        return impl(*args, **kwargs)
## == [Loop-For Style Frontend IR Specs] ======================================
def kernel(get_kernel: bool = False) -> Callable:
    def decorator(func: Callable) -> Callable:
        pass
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
