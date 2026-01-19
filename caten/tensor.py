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
        self.op = (op or ir.Memory.defglobal(tuple(args[0]), dtype)).lower()[0]
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
            assert obj.T[0] is not None and obj.T[0].dtype == dtype # todo: decent error msg
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
        assert self.op.T[0] is not None
        return tuple([x.size for x in self.op.T[0].axes])
    @property
    def strides(self) -> tuple[ir.ATenOp, ...]:
        assert self.op.T[0] is not None
        return tuple([x.stride for x in self.op.T[0].axes])
    @property
    def dtype(self) -> DType:
        assert self.op.T[0] is not None
        return self.op.T[0].dtype
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
    
    def shrink(self, arg: tuple[tuple[int, int] | None, ...]) -> ATen:
        """
        Select a sub-region of the tensor.
        
        arg: tuple of (start, end) pairs or None for each dimension.
             None means keep the full dimension.
             (start, end) selects elements from start to end (exclusive).
        
        Example:
            x.shrink(((0, 5), None, (2, 4)))  # Select [0:5, :, 2:4]
        """
        if len(arg) != self.ndim:
            raise ValueError(f"shrink arg length {len(arg)} != ndim {self.ndim}")
        # Normalize: None -> (0, size)
        bounds: list[tuple[int, int] | None] = []
        for b, s in zip(arg, self.shape, strict=True):
            if b is None:
                size = s.item if hasattr(s, 'item') else s
                bounds.append((0, size))
            else:
                bounds.append(b)
        return Tensor(op=ir.View.shrink(self.op, tuple(bounds)))
    
    def repeat(self, repeats: tuple[int, ...], *args: Any) -> ATen:
        """
        Repeat tensor along each dimension.
        
        repeats: number of repetitions for each dimension.
        
        Example:
            x.repeat((2, 3))  # [A, B] -> [A*2, B*3]
        
        Implementation: reshape to interleave 1s, expand, reshape back.
        """
        repeats_arg = argfix(repeats, *args)
        if len(repeats_arg) != self.ndim:
            raise ValueError(f"repeat arg length {len(repeats_arg)} != ndim {self.ndim}")
        
        # Insert 1s: [A, B, C] -> [A, 1, B, 1, C, 1]
        interleaved_shape: list[Any] = []
        for s in self.shape:
            interleaved_shape.extend([s, 1])
        x = self.reshape(tuple(interleaved_shape))
        
        # Expand the 1s: [A, 1, B, 1, C, 1] -> [A, r0, B, r1, C, r2]
        expand_shape: list[Any] = []
        for s, r in zip(self.shape, repeats_arg, strict=True):
            expand_shape.extend([s, r])
        x = x.expand(tuple(expand_shape))
        
        # Merge: [A, r0, B, r1, C, r2] -> [A*r0, B*r1, C*r2]
        final_shape = tuple(
            ir.Mul((s, ATen.wrap_const(r, index))) if r != 1 else s
            for s, r in zip(self.shape, repeats_arg, strict=True)
        )
        return x.reshape(final_shape)

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

    def maximum(self, other: ATen|TOperand, reverse:bool=False) -> Tensor: return self.forward(ir.Max, self._broadcasted(other, reverse=reverse))
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

    def _reduce(self, axis: int | tuple[int, ...] | None = None, keepdim: bool = False, op: Callable|None = ir.Add, initial_value: float = 0.0) -> Tensor:
        # TODO: initial elements
        assert self.op.T[0] is not None
        axes = tuple(range(self.ndim)) if axis is None else (tuple(axis) if isinstance(axis, (tuple, list)) else (axis,))
        axes = tuple(self._resolve_dim(x) for x in axes)
        reduce_axes, out_shape = [], []
        for i in range(self.ndim):
            if i in axes:
                reduce_axes.append(1)
            else:
                reduce_axes.append(self.op.T[0].axes[i].size)
                out_shape.append(self.op.T[0].axes[i].size)
        out = ir.Memory.defglobal(reduce_axes, dtype=self.op.T[0].dtype, tmp=True)
        out = ir.View.expand(out, tuple([arg.size for arg in self.op.T[0].axes]))
        out = ir.Reduce((out, ir._const(initial_value, self.dtype)), keepdim=False, axis=tuple(), bop=None) # out = initial_value
        return self.forward(ir.Reduce, (out, self.op), bop=op, axis=axes, keepdim=keepdim)

    def sum(self, axis: int | tuple[int, ...] | None = None, keepdim: bool = False) -> Tensor:
        return self._reduce(axis=axis, keepdim=keepdim, op=ir.Add, initial_value=0.0)

    def max(self, axis: int | tuple[int, ...] | None = None, keepdim: bool = False) -> Tensor:
        """Max reduction along axis."""
        return self._reduce(axis=axis, keepdim=keepdim, op=ir.Max, initial_value=float("-inf"))

    def exp(self) -> Tensor:
        """Exponential: e^x = 2^(x * log2(e))"""
        log2_e = math.log2(math.e)
        return (self * Tensor.const(log2_e, dtype=self.dtype)).exp2()

    def log(self) -> Tensor:
        """Natural logarithm: ln(x) = log2(x) / log2(e)"""
        log2_e = math.log2(math.e)
        return self.log2() / Tensor.const(log2_e, dtype=self.dtype)

    def softmax(self, axis: int = -1) -> Tensor:
        """
        Softmax along the specified axis.

        softmax(x, dim) = exp(x - max(x, dim)) / sum(exp(x - max(x, dim)), dim)

        This is numerically stable because we subtract the max before exponentiating.

        The computation is structured as:
            for i in range(batch):
                max_val = -inf
                for j in range(dim):      # reduction
                    max_val = max(max_val, x[i,j])
                sum_exp = 0.0
                for j in range(dim):      # reduction
                    sum_exp += exp(x[i,j] - max_val)
                for j in range(dim):      # parallel
                    out[i,j] = exp(x[i,j] - max_val) / sum_exp
        """
        axis = self._resolve_dim(axis)

        # Step 1: Compute max along axis (for numerical stability)
        max_val = self.max(axis=axis, keepdim=True)

        # Step 2: Subtract max and compute exp
        shifted = self - max_val
        exp_shifted = shifted.exp()

        # Step 3: Sum of exponentials along axis
        sum_exp = exp_shifted.sum(axis=axis, keepdim=True)

        # Step 4: Normalize
        return exp_shifted / sum_exp

    def pool2d(self, kernel_size: int | tuple[int, int], stride: int | tuple[int, int] | None = None, op: str = "max") -> Tensor:
        """
        2D pooling operation.
        
        Args:
            kernel_size: Size of pooling window (k or (kh, kw))
            stride: Stride of pooling (default: kernel_size)
            op: 'max' or 'avg'
        
        Input shape: [N, C, H, W]
        Output shape: [N, C, H//stride_h, W//stride_w]
        
        Implementation:
            1. Reshape [N, C, H, W] -> [N, C, H//kh, kh, W//kw, kw]
            2. Reduce over (kh, kw) dimensions with max or avg
        """
        if self.ndim != 4:
            raise ValueError(f"pool2d expects 4D input [N,C,H,W], got {self.ndim}D")
        
        kh, kw = (kernel_size, kernel_size) if isinstance(kernel_size, int) else kernel_size
        sh, sw = (stride, stride) if isinstance(stride, int) else (stride if stride else (kh, kw))
        
        N, C, H, W = [s.item for s in self.shape]
        
        if H % sh != 0 or W % sw != 0:
            raise ValueError(f"Input size ({H}, {W}) not divisible by stride ({sh}, {sw})")
        
        # For simplicity, require stride == kernel_size (non-overlapping)
        if sh != kh or sw != kw:
            raise NotImplementedError("Currently only stride == kernel_size is supported")
        
        H_out, W_out = H // kh, W // kw
        
        # Reshape: [N, C, H, W] -> [N, C, H_out, kh, W_out, kw]
        x = self.reshape((N, C, H_out, kh, W_out, kw))
        # Permute: -> [N, C, H_out, W_out, kh, kw]
        x = x.permute((0, 1, 2, 4, 3, 5))
        
        # Reduce over last two dimensions (kh, kw)
        if op == "max":
            return x.max(axis=(-2, -1))
        elif op == "avg":
            return x.sum(axis=(-2, -1)) / Tensor.const(float(kh * kw), dtype=self.dtype)
        else:
            raise ValueError(f"Unknown pooling op: {op}")

    def unfold(self, kernel_size: tuple[int, ...], stride: tuple[int, ...] | int = 1, dilation: tuple[int, ...] | int = 1) -> ATen:
        """
        Extract sliding local blocks from the last len(kernel_size) dimensions.
        
        Input: [..., *spatial_dims]
        Output: [..., *output_spatial_dims, *kernel_size]
        
        For 2D with input [N, C, H, W] and kernel_size [KH, KW]:
        Output: [N, C, H_out, W_out, KH, KW]
        
        This is the im2col operation used for convolution.
        Based on tinygrad's _pool implementation.
        """
        k_ = kernel_size
        ndim_k = len(k_)
        
        # Normalize stride and dilation to tuples
        s_ = (stride,) * ndim_k if isinstance(stride, int) else stride
        d_ = (dilation,) * ndim_k if isinstance(dilation, int) else dilation
        
        assert len(s_) == len(d_) == ndim_k
        
        # noop_ indices (batch dims), i_ (spatial input sizes)
        noop_len = self.ndim - ndim_k
        i_ = [s.item for s in self.shape[-ndim_k:]]  # spatial sizes
        noop1_ = [s for s in self.shape[:noop_len]]  # batch shape
        
        # Output spatial sizes: o = ceil((i - d*(k-1)) / s)
        o_ = [math.ceil((i - d * (k - 1)) / s) for i, d, k, s in zip(i_, d_, k_, s_)]
        
        # Step 1: repeat spatial dims
        # repeat factors: [1]*noop_len + [ceil(k*(i+d)/i) for k,i,d in zip(k_,i_,d_)]
        repeat_factors = [1] * noop_len + [math.ceil(k * (i + d) / i) for k, i, d in zip(k_, i_, d_)]
        xup = self.repeat(tuple(repeat_factors))
        
        # Step 2: shrink to [(0, k*(i+d)) for k,i,d in zip(k_,i_,d_)]
        shrink_bounds: list[tuple[int, int] | None] = [None] * noop_len + [(0, k * (i + d)) for k, i, d in zip(k_, i_, d_)]
        xup = xup.shrink(tuple(shrink_bounds))
        
        # Step 3: reshape to noop1_ + flatten((k, i+d) for k,i,d in zip(k_,i_,d_))
        reshape1: list[Any] = list(noop1_)
        for k, i, d in zip(k_, i_, d_):
            reshape1.extend([k, i + d])
        xup = xup.reshape(tuple(reshape1))
        
        # Step 4: shrink to noop_ + flatten(((0,k), (0,o*s)) for k,o,s in zip(k_,o_,s_))
        shrink2: list[tuple[int, int] | None] = [None] * noop_len
        for k, o, s in zip(k_, o_, s_):
            shrink2.extend([(0, k), (0, o * s)])
        xup = xup.shrink(tuple(shrink2))
        
        # Step 5: reshape to noop1_ + flatten((k,o,s) for k,o,s in zip(k_,o_,s_))
        reshape2: list[Any] = list(noop1_)
        for k, o, s in zip(k_, o_, s_):
            reshape2.extend([k, o, s])
        xup = xup.reshape(tuple(reshape2))
        
        # Step 6: shrink to noop_ + flatten(((0,k), (0,o), (0,1)) for k,o in zip(k_,o_))
        shrink3: list[tuple[int, int] | None] = [None] * noop_len
        for k, o in zip(k_, o_):
            shrink3.extend([(0, k), (0, o), (0, 1)])
        xup = xup.shrink(tuple(shrink3))
        
        # Step 7: reshape to noop1_ + flatten((k,o) for k,o in zip(k_,o_))
        reshape3: list[Any] = list(noop1_)
        for k, o in zip(k_, o_):
            reshape3.extend([k, o])
        xup = xup.reshape(tuple(reshape3))
        
        # Step 8: permute to [..., *o_, *k_]
        # Current: [..., k0, o0, k1, o1, ...]
        # Target:  [..., o0, o1, ..., k0, k1, ...]
        perm = list(range(noop_len))
        # Add o indices (odd positions after noop_len)
        for idx in range(ndim_k):
            perm.append(noop_len + idx * 2 + 1)
        # Add k indices (even positions after noop_len)
        for idx in range(ndim_k):
            perm.append(noop_len + idx * 2)
        
        return xup.permute(tuple(perm))

    def conv2d(self, weight: ATen, stride: int | tuple[int, int] = 1, padding: int | tuple[int, int] = 0, groups: int = 1, dilation: int | tuple[int, int] = 1) -> Tensor:
        """
        2D convolution using im2col + matmul approach.
        
        Args:
            weight: Convolution kernel [C_out, C_in/groups, KH, KW]
            stride: Convolution stride
            padding: Zero padding (not implemented yet)
            groups: Number of groups for grouped convolution
            dilation: Dilation factor
        
        Input shape: [N, C_in, H, W]
        Output shape: [N, C_out, H_out, W_out]
        
        Implementation:
            1. unfold -> [N, C_in, H_out, W_out, KH, KW]
            2. reshape -> [N, H_out*W_out, C_in*KH*KW]  
            3. weight reshape -> [C_out, C_in*KH*KW]
            4. matmul -> [N, H_out*W_out, C_out]
            5. permute + reshape -> [N, C_out, H_out, W_out]
        """
        if self.ndim != 4:
            raise ValueError(f"conv2d expects 4D input [N,C,H,W], got {self.ndim}D")
        if weight.ndim != 4:
            raise ValueError(f"conv2d expects 4D weight [Cout,Cin,KH,KW], got {weight.ndim}D")
        if padding != 0:
            raise NotImplementedError("Padding not implemented yet")
        if groups != 1:
            raise NotImplementedError("Grouped convolution not implemented yet")
        
        sh, sw = (stride, stride) if isinstance(stride, int) else stride
        dh, dw = (dilation, dilation) if isinstance(dilation, int) else dilation
        
        N, C_in, H, W = [s.item for s in self.shape]
        C_out, C_in_w, KH, KW = [s.item for s in weight.shape]
        
        if C_in != C_in_w:
            raise ValueError(f"Channel mismatch: input has {C_in}, weight expects {C_in_w}")
        
        # Output spatial dimensions
        H_out = (H - dh * (KH - 1) - 1) // sh + 1
        W_out = (W - dw * (KW - 1) - 1) // sw + 1
        
        # Step 1: unfold input -> [N, C_in, H_out, W_out, KH, KW]
        x = self.unfold((KH, KW), stride=(sh, sw), dilation=(dh, dw))
        
        # Step 2: permute to [N, H_out, W_out, C_in, KH, KW]
        x = x.permute((0, 2, 3, 1, 4, 5))
        
        # Step 3: reshape to [N, H_out*W_out, C_in*KH*KW]
        x = x.reshape((N, H_out * W_out, C_in * KH * KW))
        
        # Step 4: reshape weight to [C_out, C_in*KH*KW] then transpose for matmul
        w = weight.reshape((C_out, C_in * KH * KW))
        # w.T -> [C_in*KH*KW, C_out]
        w = w.permute((1, 0))
        
        # Step 5: matmul [N, H_out*W_out, C_in*KH*KW] @ [C_in*KH*KW, C_out]
        # -> [N, H_out*W_out, C_out]
        y = x.matmul(w)
        
        # Step 6: reshape to [N, H_out, W_out, C_out]
        y = y.reshape((N, H_out, W_out, C_out))
        
        # Step 7: permute to [N, C_out, H_out, W_out]
        return y.permute((0, 3, 1, 2))

    def matmul(self, other: ATen|TOperand) -> Tensor:
        """
        Matrix multiplication.
        
        For A[..., M, K] @ B[..., K, N] -> C[..., M, N]
        """
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
