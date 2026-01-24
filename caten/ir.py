from __future__ import annotations

import dataclasses
import functools
import itertools
import math
import operator
import weakref
from dataclasses import dataclass, field, replace
from typing import Any, Dict, FrozenSet, List, Mapping, Optional, Sequence, Set, Union

import caten.dtype as dtype

from .dtype import DType, index


class ATenOpMetaclass(type):
    cache: Dict[tuple, weakref.ReferenceType[ATenOp]] = {}
    @staticmethod
    def _freeze(x: Any) -> Any:
        if isinstance(x, ATenOp): return x
        if dataclasses.is_dataclass(x):
            return (type(x),) + tuple((f.name, ATenOpMetaclass._freeze(getattr(x, f.name))) for f in dataclasses.fields(x) if f.name not in ["args"])
        if isinstance(x, (list, tuple)):
            return tuple(ATenOpMetaclass._freeze(i) for i in x)
        if isinstance(x, dict):
            return tuple(sorted((k, ATenOpMetaclass._freeze(v)) for k, v in x.items()))
        return x
    @staticmethod
    def _check_struct(cls_name: str, args: tuple) -> None:
        """Structural constraints: Polyhedron→MemoryOf only, Range→Band only."""
        for arg in args:
            t = type(arg).__name__
            if t == "Polyhedron" and cls_name != "MemoryOf":
                raise TypeError(f"{cls_name}: Polyhedron can only be referenced by MemoryOf")
            if t == "Range" and cls_name != "Band":
                raise TypeError(f"{cls_name}: Range can only be referenced by Band")
    
    def __call__(cls, args: tuple[ATenOp, ...] | list[ATenOp], T: "tuple[ATenOpType|None, ...]" = (None), **kwargs: Any) -> ATenOp:
        args = tuple(args)
        ATenOpMetaclass._check_struct(cls.__name__, args)
        T = cls.verify(args, T, **kwargs) # type: ignore
        wret = ATenOpMetaclass.cache.get(key:=(cls, args, ATenOpMetaclass._freeze(T), ATenOpMetaclass._freeze(kwargs)), None)
        if wret is not None and (ret:=wret()) is not None: return ret.simplify()
        ATenOpMetaclass.cache[key] = weakref.ref(created:=super().__call__(args, T=T, **kwargs))
        return created.simplify()

@dataclass(frozen=True)
class ATenAxis():
    size: ATenOp
    stride: ATenOp
    offset: ATenOp
    incf: ATenOp
    def range(self) -> "Range": return Range((self.size,))
    def aff(self, band: "Band", dim: int) -> "Aff":
        assert 0 <= dim < len(band.args), "Band"
        return Aff((self.stride, Dim((band,), dim=dim), self.offset, self.incf))
    def index(self, band: "Band", dim: int) -> ATenOp:
        assert 0 <= dim < len(band.args), f"Band dim {dim} out of range [0, {len(band.args)})"
        return self.stride * (Dim((band,), dim=dim) * self.incf + self.offset)

def _const(val: Any, dtype: DType=index) -> ATenOp:
    if isinstance(val, Const): return val
    else: return Const.new(val, dtype)

@dataclass(frozen=True)
class ATenOpType():
    axes: tuple[ATenAxis, ...]
    dtype: DType
    offset: Union[ATenOp, None] = None
    def band(self) -> "Band": return Band(tuple([x.range() for x in self.axes]))
    def index(self, band: Band) -> Any:
        assert self.ndim == len(band.args)
        total = itertools.accumulate([b.index(band, a) for (a, b) in zip(range(0, self.ndim), self.axes, strict=True)], lambda a, b: Add((a, b)), initial=Const.new(0, index)) # type: ignore
        if self.offset: total = Add((total, self.offset)) # type: ignore
        return total
    @property
    def ndim(self) -> int: return len(self.axes)
    @property
    def shape(self) -> tuple[ATenOp, ...]: return [x.size for x in self.axes]
    @staticmethod
    def from_shape(shape: tuple[Any, ...], dtype: DType) -> ATenOpType:
        if len(shape) == 0: return ATenOpType(axes=(), dtype=dtype)
        def _mul(a: Any, b: Any) -> Any: return Mul((_const(a), _const(b)))
        strides = tuple(itertools.accumulate(reversed(shape[1:]), _mul, initial=_const(1)))[::-1]
        # TODO: This design is OK? in real, expand should set the stride=0
        # Size-1 dimensions get stride=0 for broadcast semantics
        axes = []
        for size, stride in zip(shape, strides, strict=True):
            stride = 0 if ATenOp.eql(size, 1) else stride
            axes.append(ATenAxis(size=_const(size), stride=_const(stride), offset=_const(0), incf=_const(1)))
        return ATenOpType(axes=tuple(axes), dtype=dtype)

@dataclass(frozen=True)
class ATenOp(metaclass=ATenOpMetaclass):
    args: tuple[ATenOp, ...]
    T: tuple[Union[ATenOpType, None], ...] = () # this should be provided via T=... option, or inferred via verify method.
    @property
    def predecessors(self) -> tuple[ATenOp, ...]:
        outputs = tuple(self.args)
        for t in self.T:
            outputs += (tuple(*[tuple((axis.size, axis.stride, axis.offset, axis.incf)) for axis in t.axes]) + () if t is not None else ()) + ((t.offset,) if t and t.offset is not None else ())
        return outputs
    
    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: tuple[Union[None, ATenOpType], ...], **kwargs: Any) -> tuple[ATenOpType, ...]:
        raise NotImplementedError(f"verify is not implemented for {cls.__name__}")

    def render(self) -> str:
        from caten.runtime.cpu import CPUTensor
        return CPUTensor.render(self)

    def simplify(self) -> ATenOp:
        from caten.simplifier import simplifier
        return simplifier.simplify(self)
    
    def viz(self) -> str:
        from caten.viz import render
        return render(self)
    
    def dot(self) -> str:
        from caten.viz import get_jupyter_graphviz, to_dot
        return get_jupyter_graphviz(to_dot(self))

    @property
    def item(self) -> Union[int, float, str, ATenOp]:
        # Returns scalar value if self is constant folded
        if isinstance(self, Const) and isinstance(self.value, (int, float, str)):
            return self.value
        else: return self
    # Mixin for computing shapes (required by reshape, etc)
    # TODO: Use same semantic of broadcast as tensor
    def __add__(self, other: Any) -> ATenOp: return Add((self, _const(other)))
    def __radd__(self, other: Any) -> ATenOp: return Add((_const(other), self))
    def __mul__(self, other: Any) -> ATenOp: return Mul((self, _const(other)))
    def __rmul__(self, other: Any) -> ATenOp: return Mul((_const(other), self))
    def __neg__(self) -> ATenOp: return Neg((self,))
    # note: do not try to overload __eq__ since it is need to compute hash
    @staticmethod
    def eql(a: Union[int, float, ATenOp], b: Union[int, float, ATenOp]) -> bool:
        """
        Compare two scalars (Python numbers or ATenOp scalars) for equality.
        """
        if isinstance(a, (int, float)) and isinstance(b, (int, float)): return (a == b)
        dtype = a.T[0].dtype if isinstance(a, ATenOp) else b.T[0].dtype # type: ignore
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
    
    def lower(self) -> tuple[ATenOp, ...]: return (self,)
## == Tensor Graph ============================================================
class TensorOps():
    def lower(self) -> tuple[ATenOp, ...]:
        """
        If TensorGraph (access relations are constrainted by View) is detected,
        rewrite them into ScheduleGraph (access relations are constrainted by Band/AccessMap)
        For example:
          Add(A[0:10, 0:10], B[0:10, 0:10])
        will be transformed into:
        band = ir.band()
        out  = Memory() 
        MemoryOf(
          Polyhedron(Dim(band, dim=0), Dim(band, dim=1),
               out,
               Store(out, Add(Load(A, AccessMap(band. ,,,)), Load(B, AccessMap(band, ...))))),
          nth=0
        )
        
        Note: In Unary/Binary/TernaryOps, A, B, C produce the equivalent band space, because shapes are equal.
        """
        if all([x.T[0].ndim == 0 for x in self.args]) is True:
            return (self,) # the graph is lowered, returning myself
        else:
            # we can use: all x.ndim, shape, are equal here.
            band = self.args[0].T[0].band()
            args = []
            for arg in self.args:
                lowered = arg.lower()
                assert len(lowered) == 1, "Tensor graph should not produce multiple outputs!"
                args.append(Load.from_tensor(lowered[0], band))
            # Note: out is keep viewed? or contiguous?
            r0 = replace(self, args=tuple(args)) # note: __call__ will update the output
            assert r0.T[0] is not None and r0.T[0].ndim == 0
            out = Memory.defglobal([arg.size for arg in self.T[0].axes], self.T[0].dtype, tmp=True)
            # Run r0 over band.all_dimensions(), with access relations defined by Load.from_tensor
            # returning out
            instance = Polyhedron.schedule(band.all_dimensions(), (out,), Store.new(Load.from_tensor(out, band), r0))
            return (MemoryOf((instance,), nth=0, T=(out.T[0],)),) # the output become contiguous array!
# UnaryOps verifier: check dtypes/shapes of arguments
class UnaryOps(TensorOps):
    # ops whose first argument is returned dtype
    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: tuple[Union[None, ATenOpType], ...], **kwargs: Any) -> tuple[ATenOpType, ...]:
        assert len(args) == 1, f"UnaryOp {cls.__name__} takes one argument, getting {args}"
        assert args[0].T[0] is not None
        return args[0].T
class BinaryOps(TensorOps):
    # ops whose first argument is returned dtype
    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: tuple[Union[None, ATenOpType], ...], **kwargs: Any) -> tuple[ATenOpType, ...]:
        assert len(args) == 2, f"BinaryOp {cls.__name__} takes two argument, getting {args}"
        assert args[0].T is not None
        assert ATenOp.equals(args[0].T[0].shape, args[1].T[0].shape), "BinaryOps: Detected shape mismatch."
        
        return args[0].T
class TernaryOps(TensorOps):
    # ops whose first argument is returned dtype
    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: tuple[Union[None, ATenOpType], ...], **kwargs: Any) -> tuple[ATenOpType, ...]:
        assert len(args) == 3,f"TernaryOp {cls.__name__} takes three argument, getting {args}"
        assert args[0].T is not None
        assert ATenOp.equals(args[0].T[0].shape, args[1].T[0].shape), "TernaryOps: Detected shape mismatch."
        assert ATenOp.equals(args[1].T[0].shape, args[2].T[0].shape), "TernaryOps: Detected shape mismatch."
        return args[0].T
class ViewOps():
    # ops whose return dtypes are explicitly provided via T option
    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: tuple[Union[None, ATenOpType], ...], **kwargs: Any) -> tuple[ATenOpType, ...]:
        assert T[0] is not None, f"Cannot create {cls.__name__} without providing T"
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
    python_op = lambda x, y: x and y if isinstance(x, bool) and isinstance(y, bool) else x & y

@dataclass(frozen=True)
class Or(BinaryOps, ATenOp):
    python_op = lambda x, y: x or y if isinstance(x, bool) and isinstance(y, bool) else x | y

@dataclass(frozen=True)
class Xor(BinaryOps, ATenOp):
    python_op = lambda x, y: x ^ y

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
    def verify(cls, args: tuple[ATenOp, ...], T: tuple[Union[None, ATenOpType], ...], **kwargs: Any) -> tuple[ATenOpType, ...]:
        assert len(args) == 3, f"TernaryOp {cls.__name__} takes three argument, getting {args}"
        assert args[1].T[0] is not None
        assert ATenOp.equals(args[0].T[0].shape, args[1].T[0].shape), "TernaryOps: Detected shape mismatch."
        assert ATenOp.equals(args[1].T[0].shape, args[2].T[0].shape), "TernaryOps: Detected shape mismatch."
        return args[1].T # extend the result's shape

@dataclass(frozen=True)
class Const(ViewOps, ATenOp):
    value: Union[int, float, str, bool] = 0.0
    @staticmethod
    def new(value: Union[int, float, str, bool, ATenOp], dtype: DType) -> Const:
        assert isinstance(value, (int, float, str, bool, ATenOp)), f"{value} should be int/float/str/bool"
        if isinstance(value, ATenOp): return value
        else: return Const(args=(), value=value, T=(ATenOpType(axes=(), dtype=dtype),))

@dataclass(frozen=True)
class View(ViewOps, ATenOp):
    """
    View(X, T=T_New) - General Tensor Transformation
    Semantics:
    ==========
    View(X, T) ≡ X.contiguous(T)
    
    Every View operation explicitly represents a copy to a new contiguous
    buffer with the specified shape/layout. This is the "contiguous model":
    - Source tensor X may have any strided layout
    - Output is always contiguous (row-major) with shape from T
    """
    @staticmethod
    def reshape(tensor: ATenOp, shape: tuple[ATenOp, ...]) -> View:
        assert tensor.T[0] is not None
        return View((tensor,), T=(ATenOpType.from_shape(shape, tensor.T[0].dtype,),))

    @staticmethod
    def permute(tensor: ATenOp, order: tuple[int, ...]) -> View:
        assert tensor.T[0] is not None
        return View((tensor,), T=(ATenOpType(
            axes=tuple([tensor.T[0].axes[i] for i in order]),
            dtype=tensor.T[0].dtype,
            offset=tensor.T[0].offset,
        ),))

    @staticmethod
    def expand(tensor: ATenOp, shape: tuple[Union[int, ATenOp], ...]) -> View:
        assert tensor.T[0] is not None
        def _expand(old_axis: ATenAxis, new_size: int | float | ATenOp) -> ATenAxis:
            if ATenOp.eql(old_axis.size, new_size): return old_axis
            else:
                assert ATenOp.eql(old_axis.size, 1), f"The axis to expand should be evaluated to 1, getting {old_axis.size}"
                return ATenAxis(size=_const(new_size), stride=Const.new(0, index), offset=Const.new(0, index), incf=Const.new(1, index))
        return View((tensor,), T=(ATenOpType(
            axes=tuple([_expand(old_axis, new_size) for (old_axis, new_size) in zip(tensor.T[0].axes, shape, strict=True)]),
            dtype=tensor.T[0].dtype,
            offset=tensor.T[0].offset,
        ),))

    @staticmethod
    def shrink(tensor: ATenOp, bounds: tuple[tuple[int | ATenOp, int | ATenOp] | None, ...]) -> View:
        """
        Shrink tensor by selecting a sub-region along each axis.
        
        bounds: tuple of (start, end) pairs or None for each dimension.
                None means keep the full dimension.
                (start, end) selects elements from start to end (exclusive).
        
        The result is a strided view with adjusted offset.
        """
        assert tensor.T[0] is not None
        assert len(bounds) == len(tensor.T[0].axes), f"bounds length {len(bounds)} != ndim {len(tensor.T[0].axes)}"
        
        new_axes = []
        # Compute additional offset from shrinking
        offset_contrib: ATenOp = _const(0)
        
        for axis, bound in zip(tensor.T[0].axes, bounds, strict=True):
            if bound is None:
                # Keep full dimension
                new_axes.append(axis)
            else:
                start, end = bound
                start_const = _const(start) if isinstance(start, int) else start
                end_const = _const(end) if isinstance(end, int) else end
                new_size = Add((end_const, Neg((start_const,))))
                # Offset contribution: start * stride
                offset_contrib = Add((offset_contrib, Mul((start_const, axis.stride))))
                new_axes.append(ATenAxis(
                    size=new_size,
                    stride=axis.stride,
                    offset=axis.offset,
                    incf=axis.incf,
                ))
        
        # Combine with existing offset
        total_offset = tensor.T[0].offset
        if total_offset is not None:
            total_offset = Add((total_offset, offset_contrib))
        else:
            total_offset = offset_contrib
        
        return View((tensor,), T=(ATenOpType(
            axes=tuple(new_axes),
            dtype=tensor.T[0].dtype,
            offset=total_offset,
        ),))
    
    def lower(self) -> tuple[ATenOp, ...]:
        """Lower View to Sync that copies to contiguous buffer."""
        # Y = View(X, T=(T_New,))
        band = self.T[0].band()
        lowered = self.args[0].lower()
        assert len(lowered) == 1, "Tensor graph should not produce multiple outputs!"
        src = Load.from_tensor(lowered[0], band, T=self.T[0])
        dst = Memory.defglobal([arg.size for arg in self.T[0].axes], self.T[0].dtype, tmp=True)
        mv = Store.new(Load.from_tensor(dst, band), src)
        instance = Polyhedron.schedule(band.all_dimensions(), (dst,), mv)
        return (MemoryOf((instance,), nth=0),)

# MetaOps: Something like a macro in CatenIR
class MetaOps(): pass

@dataclass(frozen=True)
class Reduce(MetaOps, ATenOp):
    """
    OUT = Reduce(A, B, op=BinaryOps)
    Reduces tensor along specified axes using the binary operation.
    Example:
        Reduce((A, B), bop=Add, axis=(2,), )  # Sum reduction over axis 2
        Reduce((A, B), bop=Max, axis=(1,), )  # Max reduction over axis 1
    """
    bop: Union[type[BinaryOps], None] = Add # If None, just move
    axis: tuple[int, ...] = ()
    keepdim: bool = False
    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: tuple[Union[None, ATenOpType], ...], **kwargs: Any) -> tuple[ATenOpType, ...]:
        tensor = args[0]
        assert len(args) == 2
        assert tensor.T[0] is not None
        new_axes = []
        for dim, i in enumerate(tensor.T[0].axes):
            if dim in kwargs["axis"]:
                if kwargs["keepdim"]:
                    new_axes.append(ATenAxis(size=_const(1, index), stride=_const(1, index), offset=_const(0, index), incf=_const(0, index)))
            else:
                new_axes.append(i)
        return (ATenOpType(axes=tuple(new_axes), dtype=tensor.T[0].dtype, offset=tensor.T[0].offset,),)

    def lower(self) -> tuple[ATenOp, ...]:
        assert len(self.args) == 2
        band = self.args[0].T[0].band()

        a, b = tuple([x.lower()[0] for x in self.args])
        a, b = [Load.from_tensor(a, band), Load.from_tensor(b, band)]
        # note: set bop=None to just filling by values.
        reduced = self.bop((a, b)) if self.bop is not None else b
        instance = Polyhedron.schedule(band.all_dimensions(), (a, ), Store.new(a, reduced))
        # Use self.T (reduce output type) instead of instance.T (buffer type)
        return (MemoryOf((instance,), nth=0, T=self.T),)

@dataclass(frozen=True)
class Einsum(MetaOps, ATenOp): pass

## == ScheduleOps = ============================================================
class ScheduleOps():
    """
    Ops for scheduling.
    """
### Array access graph constrained via only affine functions, sorted by lex order (for symbolic shape)
@dataclass(frozen=True)
class Range(ScheduleOps, ATenOp):
    """
    Range(SIZE) represents the half-open interval [0, SIZE).
    - SIZE should be a scalar typed tensor.
    - Band is the only user for the Range.
    """
    name: Union[str, None] = None
    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: tuple[Union[None, ATenOpType], ...], **kwargs: Any) -> tuple[ATenOpType, ...]:
        assert len(args) == 1 and args[0].T[0] is not None, "Range is defined as: Range(SIZE)"
        assert args[0].T[0].ndim == 0, "Range: SIZE should be given as a scalar"
        assert args[0].T[0].dtype == index, "Range: SIZE should be type of index"
        return (ATenOpType(axes=tuple(), dtype=index, offset=_const(0, index)),)

    @property
    def size(self) -> ATenOp: return self.args[0]
    def named(self, name: str) -> Range: return Range(self.args, name=name)
    def rename(self, mapping: Mapping[str, str]) -> Range:
        if self.name in mapping: return self.named(mapping.get(self.name))
        else: return self

@dataclass(frozen=True)
class Band(ScheduleOps, ATenOp):
    """
    Band(Range1, Range2, ...) binds multiple ranges as an iteration space.
    
    A Band is an ordered list of Ranges that defines the loop nest:
    - Band(Range(M), Range(N)) represents: for i in [0,M): for j in [0,N):

    Use Dim(domain, dim=k) to extract the k-th Range.
    Example:
        domain = Band(Range(10), Range(20))
        # Represents: for i0 in [0,10): for i1 in [0,20):
    """
    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: tuple[Union[None, ATenOpType], ...], **kwargs: Any) -> tuple[ATenOpType, ...]:
        assert len(args) > 0, "Band requires at least one Range"
        for i, arg in enumerate(args):
            assert isinstance(arg, Range), f"Band arg[{i}] must be Range, got {type(arg).__name__}"
        return (ATenOpType(axes=tuple(), dtype=index, offset=_const(0, index)),)
    @property
    def ndim(self) -> int: return len(self.args)
    @property
    def ranges(self) -> tuple[Range, ...]: return tuple(r for r in self.args)
    def all_dimensions(self) -> tuple[ATenOp, ...]:
        return tuple([Dim((self,), dim=i) for i in range(self.ndim)])
    @property
    def shape(self) -> tuple[ATenOp, ...]: return tuple(r.size for r in self.ranges)
    def rename(self, mapping: Mapping[str, str]) -> Band:
        return Band(tuple(x.rename(mapping) for x in self.args))
    # TODO: Implement reshape
    # - Semantics: They returns "a new band" for the size.
    # def tile(self):
    # TODO: Range with name?

@dataclass(frozen=True)
class Dim(ScheduleOps, ATenOp):
    """
    Dim(Band, dim=k) extracts the k-th Range from a Band.
    This is how you reference a specific loop variable within an iteration space.
    
    Example:
        domain = Band(Range(10), Range(20))
        i = Dim(domain, dim=0)  # References the i-loop [0,10)
        j = Dim(domain, dim=1)  # References the j-loop [0,20)
    """
    dim: int = 0
    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: tuple[Union[None, ATenOpType], ...], **kwargs: Any) -> tuple[ATenOpType, ...]:
        assert len(args) == 1, "Dim requires exactly one Band argument"
        assert isinstance(args[0], Band), f"Dim arg must be Band, got {type(args[0]).__name__}"
        assert "dim" in kwargs, "dim is required."
        dim = kwargs.get("dim")
        assert 0 <= dim < args[0].ndim, f"Dim {dim} out of range for Band with {args[0].ndim} dims"
        return (ATenOpType(axes=tuple(), dtype=index, offset=_const(0, index)), )

    @property
    def domain(self) -> Band: return self.args[0]
    @property
    def range(self) -> Range: return self.domain.args[self.dim]
    @property
    def ndim(self) -> ATenOp: return len(self.domain.args)
    def rename(self, mapping: Mapping[str, str]) -> Dim:
        return Dim((self.args[0].rename(mapping),), dim=self.dim)

### Polyhedral Compiler Primitives
@dataclass(frozen=True)
class Aff(ScheduleOps, ATenOp):
    """
    Aff(Stride, Dim, Offset, Incf)
    Equivalent to: Stride * (Incf * Dim + Offset)
    In polyhedral notation: [i] -> { Stmt[Stride * (Incf * i + Offset)] }
    
    Args:
        Stride: Coefficient for this dimension's contribution to address
        Dim: The loop variable (Dim node referencing a Band)
        Offset: Constant offset added before scaling
        Incf: Increment factor (usually 1)
    
    Example:
        domain = Band(Range(10), Range(20))
        # Access pattern for A[i*20 + j]:
        Aff(20, Dim(domain, dim=0), 0, 1)  # 20 * i
        Aff(1, Dim(domain, dim=1), 0, 1)   # 1 * j
    """
    @property
    def stride(self) -> ATenOp: return self.args[0]
    @property
    def dim(self) -> ATenOp: return self.args[1]
    @property
    def offset(self) -> ATenOp: return self.args[2]
    @property
    def incf(self) -> ATenOp: return self.args[3]
    def ax_b(self) -> tuple[ATenOp, ATenOp]:
        # a=stride*incf (incremental), b=stride*offset(offset)
        # Aff = a*self.dim+b
        return self.stride*self.incf, self.stride*self.offset
    def index(self) -> ATenOp:
        a, b = self.ax_b()
        # For symbolic Affs (_cst dimension), skip a*dim since dim is always 0
        if isinstance(self.dim, Dim) and self.dim.args[0].args[0].name == "_cst":
            return b
        return a * self.dim + b
    @staticmethod
    def _cst_dim() -> Dim:
        """Create a dummy constant dimension for symbolic Affs."""
        return Dim((Band((Range((_const(1, index),), name="_cst"),)),), dim=0)
    
    @staticmethod
    def var(name: str, flip: bool = False) -> Aff:
        """Create a variable Aff: name (or -name if flip=True).
        
        Example:
            Aff.var("i")       # represents variable i
            Aff.var("i", True) # represents -i
        """
        cst = Aff._cst_dim()
        return Aff((_const(1, index), cst, Const.new(name, index), _const(-1 if flip else 1, index),))
    
    @staticmethod
    def term(coef: int, varname: str) -> Aff:
        """Create an affine term: coef * varname.
        
        Example:
            Aff.term(2, "i")   # represents 2*i
            Aff.term(-3, "j")  # represents -3*j
        """
        cst = Aff._cst_dim()
        return Aff((_const(coef, index), cst, Const.new(varname, index), _const(1, index),))
    
    @staticmethod
    def const(value: int) -> Aff:
        """Create a constant Aff: value.
        
        Example:
            Aff.const(5)   # represents constant 5
            Aff.const(-3)  # represents constant -3
        """
        cst = Aff._cst_dim()
        # stride=1, offset=value, incf=0 => 1 * (0 * dim + value) = value
        return Aff((_const(1, index), cst, _const(value, index), _const(0, index),))
    
    @staticmethod
    def lin(coef: int, varname: str, const: int = 0) -> tuple[Aff, ...]:
        """Create linear expression: coef * varname + const.
        
        Returns a tuple of Affs suitable for use in constraints/BasicMap.
        
        Example:
            Aff.lin(2, "i", 3)   # (2*i, 3) representing 2*i + 3
            Aff.lin(1, "j")      # (j,) representing just j
            Aff.lin(0, "i", 5)   # (5,) just a constant
        """
        if coef == 0:
            return (Aff.const(const),) if const != 0 else ()
        if const == 0:
            return (Aff.term(coef, varname),)
        return (Aff.term(coef, varname), Aff.const(const))
    
    def rename(self, mapping: Mapping[str, str]) -> Aff:
        # For symbolic Affs, the variable name is stored in offset (as a string Const)
        new_offset = self.offset
        offset_val = self.offset.item
        if isinstance(offset_val, str) and offset_val in mapping:
            new_offset = Const.new(mapping[offset_val], self.offset.T[0].dtype)
        return Aff((self.stride, self.dim.rename(mapping), new_offset, self.incf))
    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: tuple[Union[None, ATenOpType], ...], **kwargs: Any) -> tuple[ATenOpType, ...]:
        assert len(args) == 4, "Aff is defined as: Aff(Stride, Dim, Offset, Incf)"
        stride, dim_node, offset, incf = args
        assert stride.T[0] is not None and stride.T[0].ndim == 0 and stride.T[0].dtype == index, \
            "Aff: Stride should be a scalar index"
        assert isinstance(dim_node, Dim), \
            f"Aff: Second argument should be Dim, got {type(dim_node).__name__}"
        assert offset.T[0] is not None and offset.T[0].ndim == 0 and offset.T[0].dtype == index, \
            "Aff: Offset should be a scalar index"
        assert incf.T[0] is not None and incf.T[0].ndim == 0 and incf.T[0].dtype == index, \
            "Aff: Incf should be a scalar index"
        return (ATenOpType(axes=tuple(), dtype=index, offset=_const(0, index)), )

@dataclass(frozen=True)
class Constraint(ScheduleOps, ViewOps, ATenOp):
    """
    Equality constraint: sum of Affs == 0
    
    Constraint(aff1, aff2, ...) represents: aff1 + aff2 + ... = 0
    
    Aff Structure for Symbolic Variables (created by Aff.term/var/const):
    ====================================================================
    For Aff.term(coef, "varname"):
        stride=coef, dim=dummy, offset="varname"(str), incf=1
        Expression: coef * (1 * 0 + "varname") = coef * varname
        
    For Aff.const(value):
        stride=1, dim=dummy, offset=value(int), incf=0
        Expression: 1 * (0 * dim + value) = value
        
    So for symbolic Affs:
        - Variable term: offset is str, incf != 0, coefficient = stride * incf
        - Constant term: incf == 0, constant = stride * offset
    """
    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: tuple[Union[None, ATenOpType], ...], **kwargs: Any) -> tuple[ATenOpType, ...]:
        assert all(isinstance(x, Aff) for x in args), "Constraint requires Aff arguments"
        return (ATenOpType(axes=(), dtype=dtype.bool, offset=_const(0, index)), )
    
    def get_coefficient_of(self, varname: str) -> ATenOp:
        """Get the total coefficient of a variable as an ATenOp.

        For symbolic Affs, the variable name is stored in offset (as a string Const).
        The coefficient is stride * incf.
        """
        terms: List[ATenOp] = []
        for aff in self.args:
            offset_val = aff.offset.item
            # Check if this Aff represents the target variable
            if isinstance(offset_val, str) and offset_val == varname:
                # Coefficient = stride * incf (as computation graph)
                terms.append(aff.stride * aff.incf)
        if not terms:
            return _const(0, index)
        return functools.reduce(lambda a, b: Add((a, b)), terms)

    def get_constant(self) -> ATenOp:
        """Get the constant term as an ATenOp.

        For symbolic Affs, constant terms have incf == 0.
        The constant value is stride * offset.
        """
        terms: List[ATenOp] = []
        for aff in self.args:
            # Constant term: incf == 0
            if ATenOp.eql(aff.incf, 0):
                # Constant = stride * offset (as computation graph)
                terms.append(aff.stride * aff.offset)
        if not terms:
            return _const(0, index)
        return functools.reduce(lambda a, b: Add((a, b)), terms)
    
    def variables(self) -> FrozenSet[str]:
        """Get all variable names (where offset is str and incf != 0)."""
        result: Set[str] = set()
        for aff in self.args:
            offset_val = aff.offset.item
            incf_simplified = aff.incf.simplify()
            if isinstance(offset_val, str) and not ATenOp.eql(incf_simplified, 0):
                result.add(offset_val)
        return frozenset(result)
    
    def without_var(self, varname: str) -> tuple[Aff, ...]:
        """Return Affs excluding the specified variable."""
        return tuple(aff for aff in self.args 
                     if not (isinstance(aff.offset.item, str) and aff.offset.item == varname))
    
    def substitute(self, varname: str, solution: tuple[Aff, ...]) -> "Constraint":
        """Substitute variable with solution Affs, scaling by the variable's coefficient.
        
        Uses computation graphs for coefficient scaling.
        """
        coef = self.get_coefficient_of(varname).simplify()
        if ATenOp.eql(coef, 0):
            return self
        remaining = list(self.without_var(varname))
        for aff in solution:
            # Scale stride by coefficient: new_stride = stride * coef (computation graph)
            new_stride = aff.stride * coef
            remaining.append(Aff((new_stride, aff.dim, aff.offset, aff.incf)))
        return Constraint(tuple(remaining))
    
    def is_trivial(self) -> bool:
        """Check if constraint is 0 = 0 (all coefficients sum to 0 and constant is 0)."""
        for var in self.variables():
            coef = self.get_coefficient_of(var).simplify()
            if not ATenOp.eql(coef, 0):
                return False
        const = self.get_constant().simplify()
        return ATenOp.eql(const, 0)
    
    def is_contradiction(self) -> bool:
        """Check if constraint is unsatisfiable (no variables but non-zero constant)."""
        if len(self.variables()) != 0:
            return False
        const = self.get_constant().simplify()
        return not ATenOp.eql(const, 0)
    
    def rename(self, mapping: Mapping[str, str]) -> "Constraint":
        return Constraint(tuple(x.rename(mapping) for x in self.args))
    
    def __str__(self) -> str:
        if not self.args:
            return "0 = 0"
        total = functools.reduce(lambda a, b: Add((a, b)), [x.index() for x in self.args])
        return f"{total.render()} = 0"
    
    @staticmethod
    def fourier_motzkin(constraints: List["Constraint"], vars_to_elim: Sequence[str]) -> List["Constraint"]:
        """
        Eliminate variables via Fourier-Motzkin style substitution.
        
        For each variable to eliminate:
        1. Find constraint where variable has coefficient ±1
        2. Solve for variable: if coef=1: var=-rest, if coef=-1: var=rest
        3. Substitute into remaining constraints
        4. Remove pivot constraint
        
        Only eliminates variables with coefficient ±1 (exact integer solution).
        Uses computation graphs for all arithmetic operations.
        """
        constraints = list(constraints)
        for var in vars_to_elim:
            pivot_idx: Optional[int] = None
            solution: Optional[tuple[Aff, ...]] = None
            for i, c in enumerate(constraints):
                coef = c.get_coefficient_of(var).simplify()
                if ATenOp.eql(coef, 1) or ATenOp.eql(coef, -1):
                    pivot_idx = i
                    rest = c.without_var(var)
                    if ATenOp.eql(coef, 1):
                        # var + rest = 0 => var = -rest (negate all rest terms via computation graph)
                        negated = []
                        for aff in rest:
                            # new_stride = -1 * stride (computation graph)
                            neg_stride = _const(-1, index) * aff.stride
                            negated.append(Aff((neg_stride, aff.dim, aff.offset, aff.incf)))
                        solution = tuple(negated)
                    else:  # coef == -1
                        # -var + rest = 0 => var = rest
                        solution = rest
                    break
            if pivot_idx is None or solution is None:
                continue
            constraints.pop(pivot_idx)
            constraints = [c.substitute(var, solution) for c in constraints]
        return [c for c in constraints if not c.is_trivial()]

@dataclass(frozen=True)
class BasicMap(ScheduleOps, ViewOps, ATenOp):
    """
    ```
    BasicMap(*constraints, dom_vars=list, rng_vars=list)
    ```
    An affine relation from domain to range, constrained by equalities.
    Represents: { dom_name[*dom_vars] -> rng_name[*rng_vars] : *constraints }
    Example:
        { S[gid0, gid1, gid2] -> [addr] : addr = 1500*gid0 + 30*gid1 + gid2 }
    The constraints are stored as a list of Constraint objects.
    """
    dom_vars: tuple[str, ...] = field(default_factory=list)
    rng_vars: tuple[str, ...] = field(default_factory=list)
    dom_name: str = "S"
    rng_name: str = ""
    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: tuple[Union[None, ATenOpType], ...], **kwargs: Any) -> tuple[ATenOpType, ...]:
        """Verify AccessMap structure."""
        assert all([isinstance(x, Constraint) for x in args]), "BasicMap is only constrainted by Constraint."
        return (ATenOpType(axes=(), dtype=index, offset=_const(0, index)), )

    @staticmethod
    def from_affine(dom_vars: tuple[str, ...], rng_vars: tuple[str, ...], rng_exprs: tuple[tuple[Aff, ...], ...], dom_name: str = "S", rng_name: str = "") -> BasicMap:
        if not len(rng_vars) == len(rng_exprs):
            raise ValueError("rng_vars and rng_exprs length mismatch")
        constraints: List[Constraint] = []
        for rv, ex in zip(rng_vars, rng_exprs, strict=True):
            constraints.append(Constraint(ex + (Aff.var(rv, flip=True),)))
        return BasicMap(tuple(constraints), dom_vars=dom_vars, rng_vars=rng_vars,
                        dom_name=dom_name, rng_name=rng_name,
                        T=(ATenOpType(axes=(), dtype=index),))

    @staticmethod
    def define(
        dom: tuple[str, ...],
        mapping: dict[str, tuple[Aff, ...]],
        dom_name: str = "S",
        rng_name: str = ""
    ) -> BasicMap:
        """Create a BasicMap with a cleaner dict-based API.
        
        Args:
            dom: Domain variable names (e.g., ("i", "j"))
            mapping: Dict mapping range variables to their affine expressions.
                     Each value is a tuple of Affs that are summed.
            dom_name: Name of the domain space (default "S")
            rng_name: Name of the range space (default "")
        
        Example:
            # Create map: S[i, j] -> T[x, y] where x = 2*i + j, y = j + 5
            BasicMap.define(
                dom=("i", "j"),
                mapping={
                    "x": Aff.lin(2, "i") + Aff.lin(1, "j"),  # 2*i + j
                    "y": Aff.lin(1, "j", 5),                 # j + 5
                },
                dom_name="S",
                rng_name="T"
            )
            
            # Simpler example: S[i] -> [addr] where addr = 10*i + 3
            BasicMap.define(
                dom=("i",),
                mapping={"addr": Aff.lin(10, "i", 3)},
            )
        """
        rng_vars = tuple(mapping.keys())
        rng_exprs = tuple(mapping.values())
        return BasicMap.from_affine(dom, rng_vars, rng_exprs, dom_name, rng_name)

    @staticmethod
    def from_tensor_type(band: Band, T: ATenOpType) -> BasicMap:
        if T.ndim == 0: return BasicMap((), T=(ATenOpType(axes=(), dtype=T.dtype),))
        affs: tuple[Aff, ...] = tuple(axis.aff(band, dim) for dim, axis in enumerate(T.axes))
        return BasicMap.from_affine(
            tuple([f"gid_{i}" for i in range(len(affs))]),
            ("addr",), (affs,),
            dom_name="S",
            rng_name="",
        )
    # TODO: def index
    def all_variables(self) -> FrozenSet[str]:
        vars_set: Set[str] = set(self.dom_vars) | set(self.rng_vars)
        for c in self.args: vars_set |= self.variables()
        return frozenset(vars_set)

    def rename_vars(self, mapping: Mapping[str, str]) -> BasicMap:
        new_dom = tuple(mapping.get(v, v) for v in self.dom_vars)
        new_rng = tuple(mapping.get(v, v) for v in self.rng_vars)
        new_cons = tuple(c.rename(mapping) for c in self.args)
        return BasicMap(new_cons, dom_vars=new_dom, rng_vars=new_rng, dom_name=self.dom_name, rng_name=self.rng_name)

    def reverse(self) -> BasicMap:
        return BasicMap(self.args, dom_vars=self.rng_vars, rng_vars=self.dom_vars, dom_name=self.rng_name or "S", rng_name=self.dom_name)

    def is_empty(self) -> bool:
        """Check if this BasicMap has no solutions (is unsatisfiable).
        
        TODO: Implement proper satisfiability checking. Currently always returns False
        (assumes all maps are non-empty). A proper implementation would check if
        the constraints are contradictory.
        """
        return False

    # [TODO] lru_cache
    def apply_range(self, other: BasicMap) -> BasicMap:
        if len(self.rng_vars) != len(other.dom_vars):
            return ValueError(
                f"Range/Domain arity mismatch: {len(self.rng_vars)} vs {len(other.dom_vars)}")
        intermidate = tuple(f"__m{i}" for i in range(len(self.rng_vars)))
        self_renamed = self.rename_vars(
            {v: m for v, m in zip(self.rng_vars, intermidate, strict=True)}
        )
        other_renamed = other.rename_vars(
            {v: m for v, m in zip(other.dom_vars, intermidate, strict=True)}
        )
        all_csts = list(self_renamed.args) + list(other_renamed.args)
        final_constraints = Constraint.fourier_motzkin(all_csts, intermidate)
        return BasicMap(
            tuple(final_constraints),
            dom_vars=self_renamed.dom_vars,
            rng_vars=other_renamed.rng_vars,
            dom_name=self_renamed.dom_name,
            rng_name=other_renamed.rng_name
        )
    # apply_domain is equivalent to self.reverse().apply_range(other.reverse())?
    def apply_domain(self, other: BasicMap) -> BasicMap:
        return self.reverse().apply_range(other.reverse()).reverse()
    
    def __str__(self) -> str:
        dom, rng = ", ".join(self.dom_vars), ", ".join(self.rng_vars)
        dom_str, rng_str = f"{self.dom_name}[{dom}]", f"{self.rng_name}[{rng}]"
        if self.args:
            cons_str = " and ".join(str(c) for c in self.args)
            return f"{{ {dom_str} -> {rng_str} : {cons_str} }}"
        else:
            return f"{{ {dom_str} -> {rng_str} }}"

@dataclass(frozen=True)
class UnionMap(ScheduleOps, ViewOps, ATenOp):
    """Union of multiple BasicMaps. { map1; map2; ...}"""
    def __or__(self, other: UnionMap) -> UnionMap: return UnionMap(self.args + other.args)
    def reverse(self) -> UnionMap: return UnionMap([m.reverse() for m in self.args])
    def is_empty(self) -> bool: return all(m.is_empty() for m in self.args) if self.args else True
    def apply_range(self, other: UnionMap) -> UnionMap:
        result: List[BasicMap] = []
        for m1 in self.args:
            for m2 in other.args:
                if not (composed:=m1.apply_range(m2)).is_empty():
                    result.append(composed)
        return UnionMap(result)
    def apply_domain(self, other: UnionMap) -> UnionMap:
        result: List[BasicMap] = []
        for m1 in self.args:
            for m2 in other.args:
                if not (composed:=m1.apply_domain(m2)).is_empty():
                    result.append(composed)
        return UnionMap(result)
    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: tuple[Union[None, ATenOpType], ...], **kwargs: Any) -> tuple[ATenOpType, ...]:
        """Verify AccessMap structure."""
        assert all([isinstance(x, BasicMap) for x in args]), "UnionMap: all args should be type of BasicMap"
        return (ATenOpType(axes=(), dtype=index, offset=_const(0, index)), )

    def __str__(self) -> str:
        return "<UnionMap: { " + " ; ".join(str(m)[2:-2] for m in self.args) + " }>"
## == Read/Write access in the polyhedral model ==========================================
@dataclass(frozen=True)
class Load(ScheduleOps, ATenOp):
    """
    Load(Memory | MemoryOf, AccessMap)
    Access AccessMapth element of Memory or MemoryOf
    """
    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: tuple[Union[None, ATenOpType], ...], **kwargs: Any) -> tuple[ATenOpType, ...]:
        assert len(args) == 2, f"Load definition is Load(Memory | MemoryOf, AccessMap), getting args={len(args)}"
        assert isinstance(args[0], Memory) or isinstance(args[0], MemoryOf), f"Load definition is Load(Memory | MemoryOf, AccessMap), getting first argument = {type(args[0])}"
        assert isinstance(args[1], BasicMap) and len(args[1].rng_vars), "Load: The first argument should be BasicMap where len(rng_vars) == 1"
        assert args[0].T[0] is not None and args[0].T[0].ndim > 0, f"Load: the first argument should be array, getting scalar {args[0].__class__}"
        # Create scalar from array
        return (ATenOpType(axes=tuple(), dtype=args[0].T[0].dtype, offset=_const(0, index)),)

    @staticmethod
    def from_tensor(tensor: ATenOp, band: "Band", T: "ATenOpType | None" = None) -> ATenOp:
        """Create a Load from a tensor using Band/Dim structure.
        When tensor is a Polyhedron, reuses its Band to ensure fused kernels
        share the same Band structure.
        """
        dtype = T or tensor.T[0]
        assert dtype is not None
        if dtype.ndim == 0: return tensor
        if isinstance(tensor, Const): return tensor
        # Create Affs with Dim references
        am = BasicMap.from_tensor_type(band, dtype)
        return Load((tensor, am))

@dataclass(frozen=True)
class Store(ScheduleOps, ATenOp):
    """
    Store(dst, src) - Store src value into dst location.
    dst is typically a Load (with Aff indices), src is the computed value.
    """
    @staticmethod
    def new(dst: ATenOp, op: ATenOp) -> "Store":
        assert dst.T[0] is not None
        return Store((dst, op), T=(ATenOpType(axes=(), dtype=dst.T[0].dtype),))

    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: tuple[Union[None, ATenOpType], ...], **kwargs: Any) -> tuple[ATenOpType, ...]:
        assert len(args) == 2, "Store takes (dst, src)"
        assert args[0].T[0] is not None and args[1].T[0] is not None
        assert args[0].T[0].ndim == 0 and args[1].T[0].ndim == 0, "Store can only take scalar values!"
        return (ATenOpType(axes=(), dtype=args[0].T[0].dtype), )
## ==========================================================================
## Memory Allocation Model
@dataclass(frozen=True)
class Memory(ScheduleOps, ViewOps, ATenOp):
    """Memory(ATenOp). A root of memory allocation."""
    level: str = "global" # mark as local for prefetch/accumlator
    tmp: bool = False     # set to True if this Memory allocation can be removed by fusion.
    @staticmethod
    def defglobal(shape: tuple[Any, ...], dtype: DType, tmp: bool=False) -> Memory:
        return Memory((), T=(ATenOpType.from_shape(shape, dtype),), level="global", tmp=tmp)

    @staticmethod
    def deflocal(shape: tuple[Any, ...], dtype: DType) -> Memory:
        return Memory((), T=(ATenOpType.from_shape(shape, dtype),), level="local", tmp=True)

@dataclass(frozen=True)
class MemoryOf(ScheduleOps, ViewOps, ATenOp):
    """
    MemoryOf(Polyhedron, nth=int) retries the result of `Polyhedron` node tensor. (artifact of Polyhedron)
    """
    nth: int = 0
    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: tuple[Union[None, ATenOpType], ...] | None, **kwargs: Any) -> tuple[ATenOpType, ...]:
        assert len(args) == 1, "MemoryOf requires exactly one Polyhedron argument"
        assert isinstance(args[0], Polyhedron), f"MemoryOf arg must be Polyhedron, got {type(args[0]).__name__}"
        assert "nth" in kwargs, "MemoryOf requires nth argument."
        nth = kwargs.get("nth")
        assert 0 <= nth < len(args[0].T), f"MemoryOf(Polyhedron, nth={nth}) out of range for Polyhedron with {1+len(args[0].T_rest)} outputs"
        # If T is explicitly provided (e.g., from Reduce.lower()), use it
        # Otherwise fall back to Polyhedron's output type
        if T is not None and T[0] is not None:
            return T
        return (args[0].T[nth],)

@dataclass(frozen=True)
class Polyhedron(ScheduleOps, ViewOps, ATenOp):
    """
    ```
    Polyhedron(UnionMap, UnionMap, OP)
            ^         ^
         reads      writes
    ```
    Polyhedron constructs an integer polyhedral shaped by the domain as it named.
    - Each lattice point builds the graph via read and write union maps.
    e.g.:
    Polyhedron(..., ..., ...)

    +--+
    |  | (TODO: 3D AA)
    +--+
    
    Polyhedron iterates op over area constrainted by Dim1, Dim2, ... until they reaches end.
    - Assuming OP produces Tensor1, Tensor2 as a result.
    - Returns (Tensor1, Tensor2, ...) as output
      - This can be only retrived by MemoryOf(Polyhedron, nth=int)
    - MemoryOf is the only user of PolyhedronNode.

    For example, elementwise reduction is represented as:
        band = Band(Range(10), Range(10))
        Polyhedron(Dim(band, 0), Dim(band, 1), out_mem, Store(...))

    For example, gemm with k-reduction is represented as:
        outer_domain = Band(Range(M), Range(N))
        inner_domain = Band(Range(K))
        res = Polyhedron.schedule(
          Dim(outer_domain, 0), Dim(outer_domain, 1), # schedule
          C,                                          # output
          Store(Load(C, AccessMap(Band, Aff(...), Aff(...)))
                Polyhedron.schedule(
                  Dim(inner_domain, 0),
                  acc,
                  Store(acc, Add(Load(acc), Mul(...))))))
        MemoryOf(res, nth=0) # Final Output!

    Run(Dim1, Dim2, ..., Tensor1, Tensor2, ..., OP) is rendered as:
    ```
    {stack_heap}
    declare tensor1;
    declare tensor2;
    loop for dim1.range, dim2.range:
      op // Graph follows recursively ...
    ```
    When the graph is HOGE, Loop is separated
    ```
    (3.) create new loop in stack_heap <----
    declare tensor1;                       |
    declare tensor2;                       |
    loop for dim1.range, dim2.range:       |
      // (1.) Trying to render Run(...)    |
      // (2.) But no room to insert here ---
    ```
    """
    n_outs: int = 0
    root: bool = True
    @staticmethod
    def explore_predecessors(roots: tuple[ATenOp, ...]) -> tuple[tuple[Polyhedron, ...], tuple[ATenOp, ...], tuple[AccessMap, ...], tuple[AccessMap, ...]]:
        """Extract all Polyhedron nodes that roots depend on."""
        seen: set[int] = set()
        body: list[ATenOp] = []
        parents: list[Polyhedron] = []
        reads: list[AccessMap] = []
        writes: list[AccessMap] = []
        def _explore(node: ATenOp, read: bool=True) -> None:
            if id(node) in seen: return
            seen.add(id(node))
            match node:
                case Polyhedron():
                    parents.append(node)
                    return
                case BasicMap():
                    if read: reads.append(node)
                    else:    writes.append(node)
            body.append(node)
            if isinstance(node, Store):
                assert len(node.args) == 2 and read is True, "Currently WaW (write-after-write) dependency is not supported"
                # Note: how to support waw
                # for a in range(10):
                #   a[i] = 10
                #   a[i] = 20
                _explore(node.args[0], read=False)
                _explore(node.args[1], read=True)
            else:
                for arg in node.args: _explore(arg, read=read)
        for root in roots: _explore(root, read=True)
        return tuple(parents), tuple(body), tuple(reads), tuple(writes)

    @staticmethod
    def schedule(dims: tuple[Dim, ...], outs: tuple[ATenOp, ...], op: ATenOp) -> Polyhedron:
        parents, body, R, W = Polyhedron.explore_predecessors((op,))
        assert all([o in body for o in outs]), f"Cannot schedule missing vars for {outs}"
        for item in body: pass
        # TODO: Doing some assertions
        # like every user sharing the same band
        instance = Polyhedron((UnionMap(R), UnionMap(W), op), n_outs=len(outs), T=tuple([o.T[0] for o in outs]))
        # triggers fusion
        for p in parents: instance += p
        return instance

    # [TODO]
    # - find_parent_endrangesの範囲ないの全てのノードが，Polyhedronと同じBandを持っているかverifyする
    # - Rename Polyhedron -> Polyhedra
    def __add__(self, predecessor: Polyhedron) -> Polyhedron:
        """
        Triggers the loop fusion.
        Return Polyhedron when:
        - Fused     => Fused Polyhedron Instance
        - Non-Fused => self but separated with the node Separate (or smth)
        """
        # 1. AccessMapを削除
        # 2. IRを適切に定義するところを実装
        # 3. Domain+Domainの計算を実装
        R: UnionMap = self.args[0]
        W: UnionMap = self.args[1]
        print(R)
        print(W)
        # [TODO]
        # - This should be lru_cache d
        D: UnionMap = R.apply_range(W.reverse())
        print("Fusion Triggered")
        print(D)
        # TODO
        # - ir.pyをEGraphで実装したい！
        # - apply_range, reverse, apply_domain,
        # - 計算グラフ中に常にR.apply_range(W.reverse())が存在するようにしてもいい。
        # TODO:
        # - Set root=True if non-fusible
        # - otherwise root=False
        return self

    def search(self):
        # TODO: Beam Search Trigger.
        pass
