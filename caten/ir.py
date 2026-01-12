from __future__ import annotations

import dataclasses
import itertools
import math
import operator
import weakref
from dataclasses import dataclass, replace
from typing import Any, Dict, Union

import caten.aff as A

from .dtype import DType, index


class ATenOpMetaclass(type):
    cache: Dict[tuple, weakref.ReferenceType[ATenOp]] = {}
    @staticmethod
    def _freeze(x: Any) -> Any:
        if isinstance(x, ATenOp): return x
        # Handle aff.py classes by hash (they have custom __hash__)
        if hasattr(x, "__module__") and "aff" in str(x.__module__):
            return (type(x).__name__, hash(x))
        if dataclasses.is_dataclass(x):
            return (type(x),) + tuple((f.name, ATenOpMetaclass._freeze(getattr(x, f.name))) for f in dataclasses.fields(x) if f.name not in ["args"])
        if isinstance(x, (list, tuple)):
            return tuple(ATenOpMetaclass._freeze(i) for i in x)
        if isinstance(x, dict):
            return tuple(sorted((k, ATenOpMetaclass._freeze(v)) for k, v in x.items()))
        return x
    @staticmethod
    def _check_struct(cls_name: str, args: tuple) -> None:
        """Structural constraints: Exec→MemoryOf only, Range→Band only."""
        for arg in args:
            t = type(arg).__name__
            if t == "Exec" and cls_name != "MemoryOf":
                raise TypeError(f"{cls_name}: Exec can only be referenced by MemoryOf")
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
        assert 0 <= dim < len(band.args), f"Band"
        return Aff((self.stride, Dim((band,), dim=dim), self.offset, self.incf))
    def index(self, band: "Band", dim: int) -> ATenOp:
        assert 0 <= dim < len(band.args), f"Band"
        # TODO: Add(Mul(...))
        raise NotImplementedError("Not ready index")

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
            if ATenOp.eql(size, 1):
                axes.append(ATenAxis(size=_const(size), stride=_const(0), offset=_const(0), incf=_const(1)))
            else:
                axes.append(ATenAxis(size=_const(size), stride=_const(stride), offset=_const(0), incf=_const(1)))
        return ATenOpType(
            axes=tuple(axes),
            dtype=dtype,
        )

@dataclass(frozen=True)
class ATenOp(metaclass=ATenOpMetaclass):
    args: tuple[ATenOp, ...]
    T: tuple[Union[ATenOpType, None], ...] = () # this should be provided via T=... option, or inferred via verify method.
    # TODO:
    # expected_users
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
        band = A.band()
        out  = Memory() 
        MemoryOf(
          Exec(Dim(band, dim=0), Dim(band, dim=1),
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
            instance = Exec.schedule(band.all_dimensions(), (out,), Store.new(Load.from_tensor(out, band), r0))
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
        assert ATenOp.equals(args[0].T[0].shape, args[1].T[0].shape), f"BinaryOps: Detected shape mismatch."
        
        return args[0].T
class TernaryOps(TensorOps):
    # ops whose first argument is returned dtype
    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: tuple[Union[None, ATenOpType], ...], **kwargs: Any) -> tuple[ATenOpType, ...]:
        assert len(args) == 3,f"TernaryOp {cls.__name__} takes three argument, getting {args}"
        assert args[0].T is not None
        assert ATenOp.equals(args[0].T[0].shape, args[1].T[0].shape), f"TernaryOps: Detected shape mismatch."
        assert ATenOp.equals(args[1].T[0].shape, args[2].T[0].shape), f"TernaryOps: Detected shape mismatch."
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
        assert ATenOp.equals(args[0].T[0].shape, args[1].T[0].shape), f"TernaryOps: Detected shape mismatch."
        assert ATenOp.equals(args[1].T[0].shape, args[2].T[0].shape), f"TernaryOps: Detected shape mismatch."
        return args[1].T # extend the result's shape

@dataclass(frozen=True)
class Const(ViewOps, ATenOp):
    value: Union[int, float, str, bool] = 0.0
    @staticmethod
    def new(value: Union[int, float, str, bool, ATenOp], dtype: DType) -> Const:
        assert isinstance(value, (int, float, str, bool, ATenOp)), f"{value} should be int/float/str/bool"
        if isinstance(value, ATenOp):
            return value
        else:
            return Const(args=(), value=value, T=(ATenOpType(axes=(), dtype=dtype),))

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

    # todo: check it
    def get_source_access_map(self) -> "AccessMap":
        """Get the AccessMap for reading from the source tensor."""
        assert self.args[0].T[0] is not None
        # ???
        return AccessMap.from_tensor_type(self.args[0].T[0])
    # todo: check it
    def get_output_access_map(self) -> "AccessMap":
        """Get the AccessMap for writing to the output (contiguous)."""
        assert self.T[0] is not None
        return AccessMap.from_tensor_type(self.T[0])
    
    def lower(self) -> tuple[ATenOp, ...]:
        """Lower View to Sync that copies to contiguous buffer."""
        # Y = View(X, T=(T_New,))
        band = self.T[0].band()
        lowered = self.args[0].lower()
        assert len(lowered) == 1, "Tensor graph should not produce multiple outputs!"
        src = Load.from_tensor(lowered[0], band, T=self.T[0])
        dst = Memory.defglobal([arg.size for arg in self.T[0].axes], self.T[0].dtype, tmp=True)
        mv = Store.new(Load.from_tensor(dst, band), src)
        instance = Exec.schedule(band.all_dimensions(), (dst,), mv)
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
                    new_axes.append(ATenAxis(
                        size=_const(1, index),
                        stride=_const(1, index),
                        offset=_const(0, index),
                        incf=_const(0, index)
                    ))
            else:
                new_axes.append(i)
        return (ATenOpType(axes=tuple(new_axes), dtype=tensor.T[0].dtype, offset=tensor.T[0].offset,),)

    def lower(self) -> tuple[ATenOp, ...]:
        assert len(self.args) == 2
        band = self.args[0].T[0].band()

        a, b = tuple([x.lower()[0] for x in self.args])
        a, b = [Load.from_tensor(a, band), Load.from_tensor(b, band)]
        # initially reduce is not fused.
        reduced = self.bop((a, b)) if self.bop is not None else b
        instance = Exec.schedule(band.all_dimensions(), (a, ), Store.new(a, reduced))
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
    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: tuple[Union[None, ATenOpType], ...], **kwargs: Any) -> tuple[ATenOpType, ...]:
        assert len(args) == 1 and args[0].T[0] is not None, "Range is defined as: Range(SIZE)"
        assert args[0].T[0].ndim == 0, "Range: SIZE should be given as a scalar"
        assert args[0].T[0].dtype == index, "Range: SIZE should be type of index"
        return (ATenOpType(axes=tuple(), dtype=index, offset=_const(0, index)),)

    @property
    def size(self) -> ATenOp: return self.args[0]

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
    # TODO:
    # - Unsqueeze
    # - Squeeze
    # - Reshape

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
        assert "dim" in kwargs, f"dim is required."
        dim = kwargs.get("dim")
        assert 0 <= dim < args[0].ndim, f"Dim {dim} out of range for Band with {args[0].ndim} dims"
        return (ATenOpType(axes=tuple(), dtype=index, offset=_const(0, index)), )

    @property
    def domain(self) -> Band: return self.args[0]
    @property
    def range(self) -> Range: return self.domain.args[self.dim]
    @property
    def ndim(self) -> ATenOp: return len(self.domain.args)

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
class AccessMap(ScheduleOps, ATenOp):
    """
    AccessMap(Band, Aff1, Aff2, ...) represents an affine access pattern.
    equivalent to the following BasicMap.
    { Band -> [Aff1+Aff2+...] }
    Example - Row-major 2D access:
    ==============================
    For out[i,j] where shapes are [M, N]:
        d = Band(Range(M, dim=0), Range(N, dim=1))
        Load(out,
          AccessMap(
              d,
              Aff(N, Dim(d, dim=0), 1)
              Aff(1, Dim(d, dim=1), 1)))
    """
    n_ranges: int=0
    @property
    def ranges(self) -> tuple[ATenOp, ...]: return self.args[:self.n_ranges]
    @property
    def affs(self) -> tuple[ATenOp, ...]: return self.args[self.n_ranges:]
    @property
    def dims(self) -> tuple[int, ...]: return tuple(range(self.n_ranges))
    @property
    def domain_shape(self) -> tuple[ATenOp, ...]: return tuple(r.size for r in self.ranges if isinstance(r, Range))

    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: tuple[Union[None, ATenOpType], ...], **kwargs: Any) -> tuple[ATenOpType, ...]:
        """Verify AccessMap structure."""
        assert "n_ranges" in kwargs
        n_ranges = kwargs.get("n_ranges")
        assert n_ranges >= 0, "AccessMap: n_ranges must be non-negative"
        assert len(args) >= n_ranges, "AccessMap: not enough arguments for n_ranges"
        
        # Verify first n_ranges are Range nodes
        for i in range(n_ranges):
            assert isinstance(args[i], Dim), \
                f"AccessMap: arg[{i}] should be Dim, got {type(args[i]).__name__}"
        
        # Verify remaining are scalar index expressions (Aff or arithmetic)
        for i in range(n_ranges, len(args)):
            assert args[i].T[0] is not None and args[i].T[0].ndim == 0, \
                f"AccessMap: arg[{i}] should be scalar index expression"
        
        return (ATenOpType(axes=(), dtype=index, offset=_const(0, index)), )

    @staticmethod
    def from_tensor_type(band: Band, T: ATenOpType) -> "AccessMap":
        if T.ndim == 0: return AccessMap((), T=(ATenOpType(axes=(), dtype=T.dtype),), n_ranges=0)
        affs: list[Aff] = [axis.aff(band, dim) for dim, axis in enumerate(T.axes)]
        return AccessMap(
            band.all_dimensions() + tuple(affs),
            T=(ATenOpType(axes=(), dtype=T.dtype),),
            n_ranges=len(band.ranges)
        )

    # not checkd
    def domain_equals(self, other: "AccessMap") -> bool:
        """
        Check if two AccessMaps have identical iteration domains.
        
        This is the fundamental fusion check: two kernels can be fused
        iff they iterate over the same domain.
        
        Compares Ranges by position and size (Range no longer has dim attribute).
        """
        if not isinstance(other, AccessMap):
            return False
        if self.n_ranges != other.n_ranges:
            return False
        
        for r1, r2 in zip(self.ranges, other.ranges, strict=True):
            if not isinstance(r1, Range) or not isinstance(r2, Range):
                return False
            # Compare sizes (positions are implicit by order)
            if not ATenOp.eql(r1.size, r2.size):
                return False
        
        return True

    # not checkd
    def linear_address(self) -> ATenOp:
        """Compute linear memory address by summing Aff contributions."""
        addr: ATenOp = _const(0)
        for aff in self.affs:
            addr = Add((addr, aff))
        return addr

    # not checked
    def to_basic_map(self) -> "A.BasicMap":
        """Convert to BasicMap for polyhedral analysis."""
        dom_vars = tuple(f"gid{d}" for d in self.dims)
        addr_expr = A.AffExpr.zero()
        
        for aff in self.affs:
            if not isinstance(aff, Aff):
                continue
            stride, range_node, offset, incf = aff.args
            if not isinstance(range_node, Range):
                continue
            
            gid_var = f"gid{range_node.dim}"
            s = stride.item if hasattr(stride, "item") else stride
            o = offset.item if hasattr(offset, "item") else offset
            i = incf.item if hasattr(incf, "item") else incf
            
            if isinstance(s, (int, float)) and isinstance(i, (int, float)):
                coeff = int(s * i)
                const = int(s * o) if isinstance(o, (int, float)) else 0
                addr_expr = addr_expr + A.AffExpr({gid_var: coeff}, const)
        
        return A.BasicMap.from_access(dom_vars, addr_expr, dom_name="S")
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
        assert args[0].T[0] is not None and args[0].T[0].ndim > 0, f"Load: the first argument should be array, getting scalar {args[0].__class__}"
        # Create scalar from array
        return (ATenOpType(axes=tuple(), dtype=args[0].T[0].dtype, offset=_const(0, index)),)

    @staticmethod
    def from_tensor(tensor: ATenOp, band: "Band", T: "ATenOpType | None" = None) -> ATenOp:
        """Create a Load from a tensor using Band/Dim structure.
        When tensor is a Exec, reuses its Band to ensure fused kernels
        share the same Band structure.
        """
        dtype = T or tensor.T[0]
        assert dtype is not None
        if dtype.ndim == 0: return tensor
        if isinstance(tensor, Const): return tensor
        # Create Affs with Dim references
        am = AccessMap.from_tensor_type(band, dtype)
        return Load((tensor, am))

    def get_access_map(self) -> "AccessMap":
        """
        Extract AccessMap from this Load's indices.
        
        Collects Dim nodes from Aff indices to extract the Band,
        and uses the Aff nodes as the access pattern.
        """
        affs: list[ATenOp] = []
        domain: Union[Band, None] = None
        
        for idx in self.args[1:]:
            if isinstance(idx, Aff):
                # Extract Band from Dim node
                dim_node = idx.args[1]
                if isinstance(dim_node, Dim) and domain is None:
                    domain = dim_node.domain
                affs.append(idx)
            else:
                affs.append(idx)
        
        if domain is None:
            # Fallback: no proper Aff nodes found
            return AccessMap((), T=(ATenOpType(axes=(), dtype=self.args[0].T.dtype if self.args[0].T else index),), n_ranges=0)
        
        ranges = list(domain.ranges)
        return AccessMap(
            tuple(ranges) + tuple(affs),
            T=(ATenOpType(axes=(), dtype=self.args[0].T.dtype if self.args[0].T else index),),
            n_ranges=len(ranges)
        )

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
        assert args[0].T[0].ndim == 0 and args[1].T[0].ndim == 0, f"Store can only take scalar values!"
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
    MemoryOf(Exec, nth=int) retries the result of `Exec` node tensor. (artifact of Exec)
    """
    nth: int = 0
    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: tuple[Union[None, ATenOpType], ...] | None, **kwargs: Any) -> tuple[ATenOpType, ...]:
        assert len(args) == 1, "MemoryOf requires exactly one Exec argument"
        assert isinstance(args[0], Exec), f"MemoryOf arg must be Exec, got {type(args[0]).__name__}"
        assert "nth" in kwargs, "MemoryOf requires nth argument."
        nth = kwargs.get("nth")
        assert 0 <= nth < len(args[0].T), f"MemoryOf(Exec, nth={nth}) out of range for Exec with {1+len(args[0].T_rest)} outputs"
        # If T is explicitly provided (e.g., from Reduce.lower()), use it
        # Otherwise fall back to Exec's output type
        if T is not None and T[0] is not None:
            return T
        return (args[0].T[nth],)
        return (args[0].T[nth],)
## Execute Instance
@dataclass(frozen=True)
class Exec(ScheduleOps, ViewOps, ATenOp):
    """
    ```
    Exec(Dim1, Dim2, ..., Tensor1, Tensor2, ..., OP)
           n_dims             n_out
    len(args) = n_dims + n_out + 1
    ```
    Exec iterates op over area constrainted by Dim1, Dim2, ... until they reaches end.
    - Assuming OP produces Tensor1, Tensor2 as a result.
    - Returns (Tensor1, Tensor2, ...) as output
      - This can be only retrived by MemoryOf(Exec, nth=int)
    - MemoryOf is the only user of ExecNode.

    For example, elementwise reduction is represented as:
        band = Band(Range(10), Range(10))
        Exec(Dim(band, 0), Dim(band, 1), out_mem, Store(...))

    For example, gemm with k-reduction is represented as:
        outer_domain = Band(Range(M), Range(N))
        inner_domain = Band(Range(K))
        res = Run(
          Dim(outer_domain, 0), Dim(outer_domain, 1), # schedule
          C,                                          # output
          Store(Load(C, AccessMap(Band, Aff(...), Aff(...)))
                Run(
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

    ## Scheduling
    The loop is separated when:
    - 
    Overapするまで分割しちゃダメだよね。
    - わかりやすい条件式で言い表すと？
    TODO: class Domain/EndDomainを実装する？Execでやる？
    Kernel is separated when:
    Loop Fusion is doable when:
    TODO
    """
    n_dims: int = 0
    n_out: int = 0
    
    @property
    def dim_nodes(self) -> tuple[Dim, ...]:
        """Get all Dim nodes (iteration space references)."""
        return tuple(d for d in self.args[:self.n_dims] if isinstance(d, Dim))

    @property
    def domain(self) -> Union[Band, None]:
        """Get the shared Band (all Dims should reference the same Band)."""
        dims = self.dim_nodes
        if not dims: return
        return dims[0].domain

    @property
    def ranges(self) -> tuple[Range, ...]:
        """Get all Range nodes from the Band."""
        domain = self.domain
        if domain is None: return ()
        return domain.ranges

    @property
    def output(self) -> ATenOp:
        """Get the output memory."""
        return self.args[self.n_dims]

    @property
    def body(self) -> ATenOp:
        """Get the body computation (Store node)."""
        return self.args[-1]

    @property
    def dims(self) -> tuple[int, ...]:
        """Get dimension indices from Dim nodes."""
        return tuple(d.dim for d in self.dim_nodes)
    
    def get_iteration_domain(self) -> "AccessMap":
        """
        Get the iteration domain as an AccessMap (ranges only, no access pattern).
        
        This represents the loop bounds without specifying how memory is accessed.
        Useful for checking if two Execs can be fused (same iteration domain).
        """
        ranges = tuple(r for r in self.ranges if isinstance(r, Range))
        return AccessMap(
            ranges,
            T=(ATenOpType(axes=(), dtype=index),),
            n_ranges=len(ranges)
        )

    def collect_load_access_maps(self) -> "list[tuple[Load, AccessMap]]":
        """
        Collect all (Load, AccessMap) pairs from the body.
        
        Returns list of (load_node, access_map) for fusion analysis.
        The access maps can be compared to check if loads can be fused.
        """
        seen: set[int] = set()
        result: list[tuple[Load, AccessMap]] = []

        def _collect(node: ATenOp) -> None:
            if id(node) in seen:
                return
            seen.add(id(node))
            if isinstance(node, Load) and not isinstance(node.args[0], Exec):
                # Skip loads from Exec (those are kernel boundaries)
                result.append((node, node.get_access_map()))
            if isinstance(node, Exec):
                return  # Don't recurse into nested Execs
            if hasattr(node, "args"):
                for arg in node.args:
                    _collect(arg)

        _collect(self.body)
        return result

    def can_fuse_with(self, other: "Exec") -> bool:
        """
        Check if this Exec can be fused with another.
        
        Fusion requires identical iteration domains (same Ranges).
        """
        return self.get_iteration_domain().domain_equals(other.get_iteration_domain())

    def load_sources(self) -> "list[Exec]":
        """
        Get Execs that are Load sources (require separate kernels).

        Loop separation condition:
        - Load(Exec, ...) means the Exec is a data source
        - These must be computed as separate kernels before this one
        - Execs appearing directly in computation (like reduction) are inline

        Used by renderers (CPU, CUDA, etc.) to determine kernel boundaries.
        """
        seen: set[int] = set()
        sources: list[Exec] = []

        def _find(node: ATenOp) -> None:
            if id(node) in seen:
                return
            seen.add(id(node))
            if isinstance(node, Load) and isinstance(node.args[0], Exec):
                sources.append(node.args[0])
            if hasattr(node, "args"):
                for arg in node.args:
                    _find(arg)

        _find(self.body)
        return sources

    @staticmethod
    def schedule(dims: tuple[Dim, ...], outs: tuple[ATenOp, ...], op: ATenOp) -> Exec:
        instance = Exec(dims + outs + tuple([op]), n_dims=len(dims), n_out=len(outs), T=tuple([o.T[0] for o in outs]))
        # TODO: FUsion
        return instance
    
    @staticmethod
    def sync(
        output: "Memory",
        body: "Store",
    ) -> "Exec":
        """
        Create a Exec by synchronizing output with a computation body.

        Extracts the Band from Dim nodes in the body, then builds:
        args = (dim1, dim2, ..., output, body)

        Each Dim references the shared Band.

        Complexity: O(n) for traversal
        """
        # Find Band and used dims from Dim nodes in the body
        seen: set[int] = set()
        found_domain: Union[Band, None] = None
        used_dims: set[int] = set()  # Track which dim indices are actually used

        def _collect_domain(node: ATenOp) -> None:
            nonlocal found_domain
            if id(node) in seen:
                return
            seen.add(id(node))
            
            if isinstance(node, Dim):
                if found_domain is None:
                    found_domain = node.domain
                used_dims.add(node.dim)  # Track this dim as used
                return  # Don't need to go deeper
            
            if isinstance(node, Exec):
                return  # Don't collect from nested Execs
            
            if hasattr(node, "args"):
                for arg in node.args:
                    _collect_domain(arg)

        _collect_domain(body)

        # Create Dim nodes only for USED dimensions (sorted to maintain order)
        if found_domain is not None and used_dims:
            sorted_dims = sorted(used_dims)
            dims = tuple(Dim((found_domain,), dim=d) for d in sorted_dims)
        else:
            dims = ()

        # Build args: (dims..., output, body)
        args = dims + (output, body)

        assert output.T is not None
        T = ATenOpType.from_shape(
            tuple(s.size for s in output.T.axes),
            output.T.dtype
        )

        sync_node = Exec(
            args,
            T=T,
            n_dims=len(dims),
        )

        # Try to fuse with parent Execs
        parents = sync_node._find_parent_endranges()
        for p in parents:
            sync_node = sync_node._fuse(p)

        return sync_node

    def _find_parent_endranges(self) -> "list[Exec]":
        """Find all Exec nodes that this computation depends on. O(n)"""
        seen: set[int] = set()
        parents: list[Exec] = []

        def _explore(node: ATenOp) -> None:
            if id(node) in seen:
                return
            seen.add(id(node))
            if isinstance(node, Exec) and node is not self:
                parents.append(node)
                return
            if hasattr(node, "args"):
                for arg in node.args:
                    _explore(arg)

        _explore(self.body)
        return parents

    def _load_to_basic_map(self, load: "Load") -> "A.BasicMap":
        """Convert Load node to BasicMap for dependency analysis. O(d)"""
        dom_vars = tuple(f"gid{d}" for d in self.dims)
        addr_expr = A.AffExpr.zero()

        for aff_node in load.args[1:]:
            if not isinstance(aff_node, Aff):
                continue
            stride, dim_node, offset, incf = aff_node.args
            
            # Get dimension index from Dim node (new IR structure)
            if isinstance(dim_node, Dim):
                dim_idx = dim_node.dim
            else:
                continue

            gid_var = f"gid{dim_idx}"
            s = stride.item if hasattr(stride, "item") else stride
            o = offset.item if hasattr(offset, "item") else offset
            i = incf.item if hasattr(incf, "item") else incf

            if isinstance(s, (int, float)) and isinstance(i, (int, float)):
                coeff = int(s * i)
                const = int(s * o) if isinstance(o, (int, float)) else 0
                addr_expr = addr_expr + A.AffExpr({gid_var: coeff}, const)
            else:
                addr_expr = addr_expr + A.AffExpr({gid_var: A._coeff_mul(s, i)}, A._coeff_mul(s, o))

        return A.BasicMap.from_access(dom_vars, addr_expr, dom_name="S")

    def _collect_access_maps(self) -> tuple["list[A.BasicMap]", "list[A.BasicMap]"]:
        """Collect (read_maps, write_maps) from body. O(n)"""
        reads, writes = [], []
        seen: set[int] = set()

        def collect(node: ATenOp, is_write: bool = False) -> None:
            if id(node) in seen:
                return
            seen.add(id(node))
            if isinstance(node, Load):
                try:
                    m = self._load_to_basic_map(node)
                    (writes if is_write else reads).append(m)
                except Exception:
                    pass
            if isinstance(node, Exec):
                return
            if hasattr(node, "args"):
                for arg in node.args:
                    collect(arg, is_write)

        if isinstance(self.body, Store):
            collect(self.body.args[0], is_write=True)
            collect(self.body.args[1], is_write=False)
        return reads, writes

    def _fuse(self, producer: "Exec") -> "Exec":
        """
        Unified fusion via polyhedral analysis (aff.py).

        Uses attempt_fusion() from aff.py to analyze RAW dependencies
        via BasicMap composition. Falls back to shape-based analysis.
        """
        # Try polyhedral analysis first (handles tiled fusion like Conv+Pool)
        subst = self._find_subst_polyhedral(producer)

        # Fall back to shape-based analysis
        if subst is None:
            subst = self._find_subst(producer)

        if subst is None:
            return self
        return self._apply_fusion(producer, subst)

    def _find_subst_polyhedral(self, producer: "Exec") -> "dict[int, ATenOp] | None":
        """
        Find morphism using polyhedral analysis from aff.py.

        Calls A.attempt_fusion() which computes RAW dependencies:
        RAW = producer_write.apply_range(consumer_read.reverse())
        """
        prod_reads, prod_writes = producer._collect_access_maps()
        cons_reads, cons_writes = self._collect_access_maps()

        if not prod_writes:
            return None

        result = A.attempt_fusion(
            A.UnionMap.from_maps(prod_writes),
            A.UnionMap.from_maps(prod_reads),
            A.UnionMap.from_maps(cons_writes),
            A.UnionMap.from_maps(cons_reads),
        )

        if not result.success:
            return None

        cons_domain = self.domain
        if cons_domain is None:
            return None

        # Tiled fusion (Conv+Pool): build morphism from tiling_info
        if result.fusion_type == "tiled" and result.tiling_info is not None:
            return self._morphism_from_tiling(producer, result.tiling_info, cons_domain)

        # Perfect fusion: identity morphism
        if result.fusion_type == "perfect":
            morphism: dict[int, ATenOp] = {}
            for d in producer.dims:
                if d < cons_domain.ndim:
                    morphism[d] = Dim((cons_domain,), dim=d)
            return morphism if morphism else None

        return None

    def _morphism_from_tiling(
        self,
        producer: "Exec",
        tiling_info: "A.TiledFusionInfo",
        cons_domain: "Band"
    ) -> "dict[int, ATenOp] | None":
        """Build morphism from TiledFusionInfo for Conv+Pool style fusion."""
        morphism: dict[int, ATenOp] = {}

        # Map gid variable names to consumer dimension indices
        cons_name_to_dim: dict[str, int] = {f"gid{i}": i for i in range(cons_domain.ndim)}

        for pvar, (tile_size, rvar) in tiling_info.tile_dims.items():
            if not pvar.startswith("gid") or not pvar[3:].isdigit():
                continue
            pdim = int(pvar[3:])

            if tiling_info.constraint is None:
                continue

            # Find scaled consumer variable from constraint
            p_coeff = tiling_info.constraint.expr.coeff_of(pvar)
            if not isinstance(p_coeff, int) or p_coeff == 0:
                continue

            scaled_var = None
            for var in tiling_info.constraint.expr.variables():
                if var in (pvar, rvar):
                    continue
                c = tiling_info.constraint.expr.coeff_of(var)
                if isinstance(c, int) and abs(c) == abs(p_coeff * tile_size):
                    scaled_var = var
                    break

            if scaled_var is None or scaled_var not in cons_name_to_dim or rvar not in cons_name_to_dim:
                continue

            # morphism[pdim] = tile_size * Dim(scaled) + Dim(red)
            scaled_dim = cons_name_to_dim[scaled_var]
            red_dim = cons_name_to_dim[rvar]
            scaled_ref = Dim((cons_domain,), dim=scaled_dim)
            red_ref = Dim((cons_domain,), dim=red_dim)
            morphism[pdim] = Add((Mul((_const(tile_size), scaled_ref)), red_ref))

        # Identity for shared dims
        for svar in tiling_info.shared_dims:
            if svar in cons_name_to_dim and svar.startswith("gid") and svar[3:].isdigit():
                dim_idx = cons_name_to_dim[svar]
                pdim = int(svar[3:])
                if pdim not in morphism:
                    morphism[pdim] = Dim((cons_domain,), dim=dim_idx)

        return morphism if morphism else None
    def _find_subst(self, producer: "Exec") -> "dict[int, ATenOp] | None":
        """
        Find iteration space morphism: producer_dims → consumer_dims.

        Returns dict mapping producer position → IR expression over consumer Dim nodes,
        or None if fusion is not possible.

        Algorithm (using position-based indexing since Range no longer has dim):
        1. Compare producer's OUTPUT shape with consumer's iteration
        2. Handle broadcast (size 1) and reduction (output smaller than iteration)
        3. Use identity mapping when shapes match
        4. Use linear decomposition for reshape
        """
        # Use position-based indexing
        prod_sizes = {i: r.size for i, r in enumerate(producer.ranges) if isinstance(r, Range)}
        cons_sizes = {i: r.size for i, r in enumerate(self.ranges) if isinstance(r, Range)}

        # Build position -> Range mapping for consumer
        cons_pos_to_range: dict[int, Range] = {}
        for i, rng in enumerate(self.ranges):
            if isinstance(rng, Range):
                cons_pos_to_range[i] = rng

        # Get integer sizes (bail on symbolic for now)
        def get_int_sizes(sizes: dict[int, ATenOp]) -> dict[int, int] | None:
            result = {}
            for d, s in sizes.items():
                if isinstance(s, Const) and isinstance(s.value, int):
                    result[d] = s.value
                else:
                    return None
            return result

        prod_int = get_int_sizes(prod_sizes)
        cons_int = get_int_sizes(cons_sizes)

        # Also get producer's OUTPUT shape (may differ from iteration due to reduction)
        prod_out_sizes: dict[int, int] = {}
        if producer.output.T and producer.output.T.axes:
            for i, ax in enumerate(producer.output.T.axes):
                if isinstance(ax.size, Const) and isinstance(ax.size.value, int):
                    prod_out_sizes[i] = ax.size.value

        if prod_int is None or cons_int is None:
            return None

        producer_dims = set(prod_int.keys())
        consumer_dims = set(cons_int.keys())

        # Check if dims match with broadcast handling (size 1 matches any size)
        def sizes_compatible(prod_size: int, cons_size: int) -> bool:
            return prod_size == cons_size or prod_size == 1

        # Case: Consumer matches producer's OUTPUT shape (e.g., after reduction)
        if prod_out_sizes and set(prod_out_sizes.keys()) == consumer_dims:
            if all(sizes_compatible(prod_out_sizes.get(d, 1), cons_int[d]) for d in consumer_dims):
                subst: dict[int, ATenOp] = {}
                for d in producer_dims:
                    if d in consumer_dims and d in cons_pos_to_range:
                        if prod_int[d] == 1 and cons_int[d] != 1:
                            subst[d] = _const(0)
                        elif prod_int[d] == cons_int[d]:
                            subst[d] = cons_pos_to_range[d]
                        # else: Producer iterates more (reduction dim) - skip
                if subst:
                    return subst

        if producer_dims == consumer_dims:
            # Same dims - check if sizes match (with broadcast)
            if all(sizes_compatible(prod_int[d], cons_int[d]) for d in consumer_dims):
                subst = {}
                for d in producer_dims:
                    if prod_int[d] == 1 and cons_int[d] != 1:
                        subst[d] = _const(0)  # Broadcast: always index 0
                    else:
                        subst[d] = cons_pos_to_range[d]
                return subst

        if consumer_dims < producer_dims:
            # Reduce case: producer iterates more, consumer is inner loop
            if consumer_dims == producer_dims & consumer_dims:
                if all(prod_int.get(d) == cons_int.get(d) for d in consumer_dims):
                    return {d: cons_pos_to_range[d] for d in producer_dims if d in cons_pos_to_range}

        # Check: same total elements (reshape case)
        prod_total = 1
        for v in prod_int.values():
            prod_total *= v
        cons_total = 1
        for v in cons_int.values():
            cons_total *= v

        if prod_total != cons_total:
            return None

        # Build linear IR expression from consumer Range nodes (row-major order)
        sorted_cons = sorted(cons_int.keys())
        cons_strides: list[int] = []
        stride = 1
        for d in reversed(sorted_cons):
            cons_strides.insert(0, stride)
            stride *= cons_int[d]

        # linear = Σ cons_stride[d] * Range(d)
        linear: ATenOp = _const(0)
        for d, s in zip(sorted_cons, cons_strides):
            rng = cons_pos_to_range[d]
            if s == 1:
                linear = Add((linear, rng))
            else:
                linear = Add((linear, Mul((rng, _const(s)))))

        # Decompose linear into producer dims (row-major order)
        sorted_prod = sorted(prod_int.keys())
        prod_strides: list[int] = []
        stride = 1
        for d in reversed(sorted_prod):
            prod_strides.insert(0, stride)
            stride *= prod_int[d]

        subst: dict[int, ATenOp] = {}
        remaining = linear
        for d, s in zip(sorted_prod, prod_strides):
            if s == 1:
                subst[d] = remaining
            else:
                subst[d] = IDiv((remaining, _const(s)))
                remaining = Mod((remaining, _const(s)))

        return subst

    def _extract_substitution(
        self,
        result: "A.FusionResult",
        producer: "Exec"
    ) -> "dict[int, A.AffExpr] | None":
        """
        Extract dim -> expr substitution from fusion result.

        For RAW constraint like: 128*h - 512*hp - 128*rh = 0
        Solve for producer vars to get: h = 4*hp + rh

        Returns dict mapping producer dim -> consumer AffExpr, or None if unsolvable.
        """
        subst: dict[int, A.AffExpr] = {}
        producer_dims = set(producer.dims)
        consumer_dims = set(self.dims)

        # Identity for shared dims
        for d in producer_dims & consumer_dims:
            subst[d] = A.AffExpr.var(f"gid{d}")

        # Handle tiled fusion
        if result.tiling_info:
            for pvar, (tile_size, rvar) in result.tiling_info.tile_dims.items():
                if pvar.startswith("gid"):
                    try:
                        pdim = int(pvar[3:])
                    except ValueError:
                        continue

                    # Find scaled consumer var from constraint
                    if result.tiling_info.constraint:
                        expr = result.tiling_info.constraint.expr
                        p_coeff = expr.coeff_of(pvar)
                        if isinstance(p_coeff, int) and p_coeff != 0:
                            for var in expr.variables():
                                if var in (pvar, rvar):
                                    continue
                                c = expr.coeff_of(var)
                                if isinstance(c, int) and abs(c) == abs(p_coeff * tile_size):
                                    # pdim = tile_size * var + rvar
                                    subst[pdim] = tile_size * A.AffExpr.var(var) + A.AffExpr.var(rvar)
                                    break
            return subst if subst else None

        # Handle perfect/partial: solve from RAW constraint
        if result.dep_info.raw.maps:
            raw_map = result.dep_info.raw.maps[0]
            for pvar in raw_map.dom_vars:
                if not pvar.startswith("gid"):
                    continue
                try:
                    pdim = int(pvar[3:])
                except ValueError:
                    continue

                if pdim in subst:
                    continue

                # Try to solve constraint for this var
                for constraint in raw_map.constraints:
                    sol = A._try_solve_for(constraint, pvar)
                    if sol is not None:
                        # Verify solution uses only consumer vars
                        sol_vars = sol.variables()
                        if all(v.startswith("gid") and int(v[3:]) in consumer_dims for v in sol_vars if v.startswith("gid")):
                            subst[pdim] = sol
                            break

        return subst if subst else None

    def _apply_fusion(
        self,
        producer: "Exec",
        subst: "dict[int, ATenOp]"
    ) -> "Exec":
        """
        Apply fusion by transforming producer and inlining.

        In DAG, Exec itself is the output reference.
        Replace Load(producer) with transformed computation.
        """
        producer_comp = producer.body.args[1] if isinstance(producer.body, Store) else producer.body

        # Transform producer's computation
        transformed = self._preimage(producer_comp, subst)

        # Inline into consumer: replace Load(producer) with producer's computation
        def inline(node: ATenOp) -> ATenOp:
            if isinstance(node, Load):
                if node.args[0] is producer:
                    return transformed
            if isinstance(node, Exec):
                return node
            if hasattr(node, "args") and node.args:
                new_args = tuple(inline(arg) for arg in node.args)
                if new_args != node.args:
                    return replace(node, args=new_args)
            return node

        new_body = inline(self.body)

        return Exec(
            self.dim_nodes + (self.output, new_body),
            T=self.T,
            n_dims=self.n_dims,
        )

    def _preimage(self, node: ATenOp, subst: "dict[int, ATenOp]") -> ATenOp:
        """
        Apply preimage transform: replace Dim/Aff nodes using substitution.

        For Dim(Band, dim=d), if d in subst, replace with subst[d].
        For Aff with Dim(Band, dim=d), if d in subst, expand to scalar expression.
        subst maps producer dim positions to IR expressions over consumer's Dim nodes.
        """
        # Direct Dim replacement
        if isinstance(node, Dim) and node.dim in subst:
            return subst[node.dim]

        if isinstance(node, Aff):
            stride, dim_node, offset, incf = node.args
            if isinstance(dim_node, Dim) and dim_node.dim in subst:
                ir_expr = subst[dim_node.dim]
                # Aff computes: stride * (incf * dim + offset)
                # With substitution: stride * (incf * ir_expr + offset)
                scaled = Mul((incf, ir_expr)) if not ATenOp.eql(incf, _const(1)) else ir_expr
                shifted = Add((scaled, offset)) if not ATenOp.eql(offset, _const(0)) else scaled
                result = Mul((stride, shifted)) if not ATenOp.eql(stride, _const(1)) else shifted
                return result
            return node

        if isinstance(node, Load):
            # Transform indices recursively
            new_indices = [self._preimage(a, subst) for a in node.args[1:]]
            # If any index changed, rebuild Load
            if new_indices != list(node.args[1:]):
                # Sum all indices for scalar access
                if any(not isinstance(idx, Aff) for idx in new_indices):
                    total: ATenOp = _const(0)
                    for idx in new_indices:
                        total = Add((total, idx))
                    return Load((node.args[0], total), T=node.T)
                return Load((node.args[0],) + tuple(new_indices))
            return node

        if isinstance(node, (Exec, Memory)):
            return node

        if hasattr(node, "args") and node.args:
            new_args = tuple(self._preimage(a, subst) for a in node.args)
            if new_args != node.args:
                return replace(node, args=new_args)

        return node
