from __future__ import annotations

import dataclasses
import itertools
import math
import operator
import weakref
from dataclasses import dataclass, replace
from typing import Any, Dict, Union

from .dtype import DType, index
import caten.aff as A

class ATenOpMetaclass(type):
    cache: Dict[tuple, weakref.ReferenceType[ATenOp]] = {}
    @staticmethod
    def _freeze(x: Any) -> Any:
        if isinstance(x, ATenOp): return x
        # Handle aff.py classes by hash (they have custom __hash__)
        if hasattr(x, '__module__') and 'aff' in str(x.__module__):
            return (type(x).__name__, hash(x))
        if dataclasses.is_dataclass(x):
            return (type(x),) + tuple((f.name, ATenOpMetaclass._freeze(getattr(x, f.name))) for f in dataclasses.fields(x) if f.name not in ["args"])
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
    def range(self, dim: int) -> Range: return Range((self.size, ), dim=dim)
    def aff(self, dim: int) -> Aff: return Aff((self.stride, self.range(dim), self.offset, self.incf))

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
        if len(shape) == 0:
            return ATenOpType(axes=(), dtype=dtype)
        def _mul(a: Any, b: Any) -> Any: return Mul((_const(a), _const(b)))
        strides = tuple(itertools.accumulate(reversed(shape[1:]), _mul, initial=_const(1)))[::-1]
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
    T: Union[ATenOpType, None] = None # this should be provided via T=... option, or inferred via verify method.
    @property
    def predecessors(self) -> tuple[ATenOp, ...]:
        return tuple(self.args) + (tuple(*[tuple((axis.size, axis.stride, axis.offset, axis.incf)) for axis in self.T.axes]) + () if self.T is not None else ()) + ((self.T.offset,) if self.T and self.T.offset is not None else ())
    
    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: Union[None, ATenOpType], **kwargs: Any) -> ATenOpType:
        raise NotImplementedError(f"verify is not implemented for {cls.__name__}")

    def render_isl(self) -> str:
        from caten.runtime.cpu import CPUTensor
        # TODO: ISLRenderer
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
    
    def lower(self) -> ATenOp: return self
## == Tensor Graph ============================================================
class TensorOps():
    def lower(self) -> ATenOp:
        new_args, is_domain = [], False
        for arg in self.args:
            if arg.T.ndim > 0:
                is_domain = True
                new_args.append(Load.from_tensor(arg.lower()))
            else:
                new_args.append(arg)
        out = replace(self, args=tuple(new_args))
        if is_domain is False: return out
        assert out.T is not None
        tmp = Memory.defglobal([arg.size for arg in self.T.axes], self.T.dtype, tmp=True)
        # This is where "Loop Fusion" occurs?
        # - The Goal here is to create Compute Bound Graph.
        # - Fusion is doable?
        # - How to rewrite graph to fuse them?
        # 1. Use pattern matcher to label all LexFence
        # 1. Construct dependency graph
        # 2. analyse access dependencies
        # 3. tile if needed
        # 4. fuse if possible
        return EndRange.sync(tmp, Store.new(Load.from_tensor(tmp), out))
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
    View always materializes data contiguously. Each View operation creates
    an EndRange that copies data to a new contiguous buffer. Fusion via _fuse
    can merge consecutive Views into a single kernel.
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
    
    def lower(self):
        tmp = Memory.defglobal([arg.size for arg in self.T.axes], self.T.dtype, tmp=True)
        return EndRange.sync(tmp, Store.new(Load.from_tensor(tmp), Load.from_tensor(self.args[0], T=self.T)))

@dataclass(frozen=True)
class Reduce(ATenOp):
    """
    OUT = Reduce(A, B, op=BinaryOps)

    Reduces tensor along specified axes using the binary operation.

    Example:
        Reduce((A, init), bop=Add, axis=(2,))  # Sum reduction over axis 2
        Reduce((A, init), bop=Max, axis=(1,))  # Max reduction over axis 1
    """
    bop: type[BinaryOps] = Add
    axis: tuple[int, ...] = ()
    keepdim: bool = False

    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: Union[None, ATenOpType], **kwargs: Any) -> ATenOpType:
        tensor = args[0]
        assert len(args) == 2
        assert tensor.T is not None
        new_axes = []
        for dim, i in enumerate(tensor.T.axes):
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
        return ATenOpType(
            axes=tuple(new_axes),
            dtype=tensor.T.dtype,
            offset=tensor.T.offset,
        )

    def lower(self) -> ATenOp:
        """
        Lower Reduce to nested EndRange with proper reduction semantics.

        For sum reduction over axis k:
        ```c
        for (int i = 0; i < M; i++) {
            for (int j = 0; j < N; j++) {
                float acc = 0.0f;
                for (int k = 0; k < K; k++) {
                    acc += x[i,j,k];
                }
                out[i,j] = acc;
            }
        }
        ```

        Represented as:
            EndRange(
                Range(M, dim=0), Range(N, dim=1),
                out,
                Store(Load(out, ...),
                    EndRange(
                        Range(K, dim=2),
                        acc,  # Memory(0.0)
                        Store(Load(acc), Add(Load(acc), x[i,j,k])))))
        """
        # args[0] is the expanded output view, args[1] is the input tensor
        out_view = self.args[0]
        input_tensor = self.args[1].lower()

        # Get underlying memory from output view
        def find_memory(node: ATenOp) -> Memory:
            if isinstance(node, Memory):
                return node
            if isinstance(node, View):
                return find_memory(node.args[0])
            raise ValueError(f"Cannot find Memory in {node}")

        out_memory = find_memory(out_view)
        input_T = input_tensor.T
        assert input_T is not None

        # Determine initial value based on operation
        if self.bop == Add:
            init_val = 0.0
        elif self.bop == Max:
            init_val = float('-inf')
        else:
            init_val = 0.0

        # Create scalar accumulator for reduction
        acc = Memory.deflocal((), input_T.dtype)
        acc_init = Const.new(init_val, input_T.dtype)

        # Build ranges for reduction dimensions
        reduce_ranges: list[Range] = []
        for dim in self.axis:
            if dim < len(input_T.axes):
                reduce_ranges.append(input_T.axes[dim].range(dim))

        # Inner body: acc = reduce_op(acc, input[...])
        input_load = Load.from_tensor(input_tensor)
        acc_load = Load.from_tensor(acc)
        reduction_expr = self.bop((acc_load, input_load))
        inner_store = Store.new(acc_load, reduction_expr)

        # Build inner EndRange for reduction loop
        inner_args = tuple(reduce_ranges) + (acc, inner_store)
        inner_endrange = EndRange(
            inner_args,
            T=ATenOpType(axes=(), dtype=input_T.dtype),
            n_ranges=len(reduce_ranges),
        )
        # Try to fuse inner EndRange with its load sources (e.g., Mul)
        for src in inner_endrange.load_sources():
            inner_endrange = inner_endrange._fuse(src)

        # Outer body: out[i,j] = inner_endrange (which produces the reduced acc)
        out_load = Load.from_tensor(out_memory, T=out_view.T)
        outer_store = Store.new(out_load, inner_endrange)

        # Build outer EndRange for parallel dimensions
        return EndRange.sync(out_memory, outer_store)
## == EndRangeOps ============================================================
### Array access graph constrained via only affine functions, sorted by lex order (for symbolic shape)
@dataclass(frozen=True)
class Range(ATenOp):
    """
    OUT = Range(SIZE)
    OUT is the range of [0, SIZE)
    """
    dim: int = 0
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
        # incf can be Const or any scalar index expression (e.g., Mul for tiled fusion)
        return ATenOpType(axes=tuple(), dtype=index, offset=_const(0, index))
    def as_aff_str(self) -> str: return self.render_isl()

@dataclass(frozen=True)
class Load(ATenOp):
    """
    X = Load(Memory | EndRange, idx1, idx2, ...)
    Access the (idx1 + idx2 + ...)th element of Array.
    Indices can be Aff nodes or scalar index expressions (for fused reshape).
    """
    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: Union[None, ATenOpType], **kwargs: Any) -> ATenOpType:
        assert len(args) >= 2, "Load: the number of argument should be larger than two"
        assert isinstance(args[0], Memory) or isinstance(args[0], EndRange), "Load: Can only load element from Memory or Array."
        assert args[0].T is not None and args[0].T.ndim > 0, f"Load: the first argument should be array, getting scalar {args[0].__class__}"
        # Indices can be Aff or any scalar index expression
        assert all([arg.T is not None and arg.T.ndim == 0 for arg in args[1:]]), "Load: indices should be scalar expressions."
        return ATenOpType(axes=tuple(), dtype=args[0].T.dtype, offset=_const(0, index))

    @staticmethod
    def from_tensor(tensor: ATenOp, T: "ATenOpType | None" = None) -> ATenOp:
        """Create a Load from a tensor, or return scalar directly if 0-dim."""
        dtype = T or tensor.T
        assert dtype is not None
        if dtype.ndim == 0: return tensor
        if isinstance(tensor, Const): return tensor
        return Load((tensor,) + tuple([axis.aff(dim) for dim, axis in enumerate(dtype.axes)]))

### Scheduling Graph
@dataclass(frozen=True)
class Memory(ViewOps, ATenOp):
    """Memory(ATenOp, level="global or local")"""
    level: str = "global"
    tmp: bool = False
    @staticmethod
    def defglobal(shape: tuple[Any, ...], dtype: DType, tmp: bool=False) -> Memory:
        return Memory((), T=ATenOpType.from_shape(shape, dtype), level="global", tmp=tmp)

    @staticmethod
    def deflocal(shape: tuple[Any, ...], dtype: DType) -> Memory:
        return Memory((), T=ATenOpType.from_shape(shape, dtype), level="local", tmp=True)

@dataclass(frozen=True)
class EndRange(ViewOps, ATenOp):
    """
    EndRange represents a loop nest with ranges, output, and body.

    Structure:
    ==========
    args = (range1, range2, ..., output, body)

    Where:
    - args[0:n_ranges]: Range nodes - iteration space
    - args[n_ranges]: Memory node - the output array
    - args[-1]: Body (Store or nested EndRange)

    Example - Element-wise sin:
    ===========================
    ```c
    float out[10*10];
    for (int i0 = 0; i0 < 10; i0++) {
        for (int i1 = 0; i1 < 10; i1++) {
            out[i0*10 + i1] = sinf(in[i0*10 + i1]);
        }
    }
    ```
    Represented as:
        EndRange(Range(10, dim=0), Range(10, dim=1), out_mem, Store(...))

    Example - GEMM with k-reduction (nested EndRange):
    ==================================================
    ```c
    float C[M*N];
    for (int i = 0; i < M; i++) {
        for (int j = 0; j < N; j++) {
            float acc = 0.0f;
            for (int k = 0; k < K; k++) {
                acc += A[i*K+k] * B[k*N+j];
            }
            C[i*N+j] = acc;
        }
    }
    ```
    Represented as:
        EndRange(
            Range(M, dim=0), Range(N, dim=1),
            C,
            Store(Load(C, ...),
                EndRange(
                    Range(K, dim=2),
                    acc,  # Memory(0.0)
                    Store(acc, Add(Load(acc), Mul(A[i,k], B[k,j]))))))
    """
    n_ranges: int = 0

    @property
    def ranges(self) -> tuple[ATenOp, ...]:
        """Get all Range nodes (iteration space)."""
        return self.args[:self.n_ranges]

    @property
    def output(self) -> ATenOp:
        """Get the output memory."""
        return self.args[self.n_ranges]

    @property
    def body(self) -> ATenOp:
        """Get the body computation (Store node)."""
        return self.args[-1]

    @property
    def dims(self) -> tuple[int, ...]:
        """Get dimension indices from ranges."""
        return tuple(r.dim for r in self.ranges if isinstance(r, Range))

    def load_sources(self) -> "list[EndRange]":
        """
        Get EndRanges that are Load sources (require separate kernels).

        Loop separation condition:
        - Load(EndRange, ...) means the EndRange is a data source
        - These must be computed as separate kernels before this one
        - EndRanges appearing directly in computation (like reduction) are inline

        Used by renderers (CPU, CUDA, etc.) to determine kernel boundaries.
        """
        seen: set[int] = set()
        sources: list[EndRange] = []

        def _find(node: ATenOp) -> None:
            if id(node) in seen:
                return
            seen.add(id(node))
            if isinstance(node, Load) and isinstance(node.args[0], EndRange):
                sources.append(node.args[0])
            if hasattr(node, 'args'):
                for arg in node.args:
                    _find(arg)

        _find(self.body)
        return sources

    @staticmethod
    def sync(
        output: "Memory",
        body: "Store",
    ) -> "EndRange":
        """
        Create an EndRange by synchronizing output with a computation body.

        Builds args = (ranges..., output, body)

        Complexity: O(n) for collection, O(d log d) for sorting ranges
        """
        # Collect all Range nodes from the body (not nested EndRanges) - O(n)
        seen: set[int] = set()
        dim2range: dict[int, Range] = {}

        def _collect_ranges(node: ATenOp) -> None:
            if id(node) in seen:
                return
            seen.add(id(node))
            if isinstance(node, Range):
                if node.dim in dim2range:
                    assert ATenOp.eql(dim2range[node.dim].args[0], node.args[0]), \
                        f"Conflicting Range sizes for dim {node.dim}"
                dim2range[node.dim] = node
            elif isinstance(node, EndRange):
                return  # Don't collect ranges from nested EndRanges
            if hasattr(node, 'args'):
                for arg in node.args:
                    _collect_ranges(arg)

        _collect_ranges(body)

        # Sort ranges by dimension - O(d log d)
        sorted_dims = sorted(dim2range.keys())
        ranges = tuple(dim2range[d] for d in sorted_dims)

        # Build args: (ranges..., output, body)
        args = ranges + (output, body)

        assert output.T is not None
        T = ATenOpType.from_shape(
            tuple(s.size for s in output.T.axes),
            output.T.dtype
        )

        endrange = EndRange(
            args,
            T=T,
            n_ranges=len(ranges),
        )

        # Try to fuse with parent EndRanges
        parents = endrange._find_parent_endranges()
        for p in parents:
            endrange = endrange._fuse(p)

        return endrange

    def _find_parent_endranges(self) -> "list[EndRange]":
        """Find all EndRange nodes that this computation depends on. O(n)"""
        seen: set[int] = set()
        parents: list[EndRange] = []

        def _explore(node: ATenOp) -> None:
            if id(node) in seen:
                return
            seen.add(id(node))
            if isinstance(node, EndRange) and node is not self:
                parents.append(node)
                return
            if hasattr(node, 'args'):
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
            stride, range_node, offset, incf = aff_node.args
            if not isinstance(range_node, Range):
                continue

            gid_var = f"gid{range_node.dim}"
            s = stride.item if hasattr(stride, 'item') else stride
            o = offset.item if hasattr(offset, 'item') else offset
            i = incf.item if hasattr(incf, 'item') else incf

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
            if isinstance(node, EndRange):
                return
            if hasattr(node, 'args'):
                for arg in node.args:
                    collect(arg, is_write)

        if isinstance(self.body, Store):
            collect(self.body.args[0], is_write=True)
            collect(self.body.args[1], is_write=False)
        return reads, writes

    def _fuse(self, producer: "EndRange") -> "EndRange":
        """
        Unified fusion via iteration space morphism.

        All fusion cases reduce to: find a mapping from producer dims to consumer dims
        such that the linear address spaces align. This handles:
        - Element-wise: identity mapping
        - Reduce inner: producer dims ⊃ consumer dims, extras reference outer scope
        - Reshape: same total elements, decompose via div/mod
        - Permute/broadcast: size-based matching
        """
        subst = self._find_subst(producer)
        if subst is None:
            return self
        return self._apply_fusion(producer, subst)

    def _find_subst(self, producer: "EndRange") -> "dict[int, ATenOp] | None":
        """
        Find iteration space morphism: producer_dims → consumer_dims.

        Returns dict mapping producer dim → IR expression over consumer Range nodes,
        or None if fusion is not possible.

        Algorithm:
        1. Compare producer's OUTPUT shape (not iteration shape) with consumer's iteration
        2. Handle broadcast (size 1) and reduction (output smaller than iteration)
        3. Use identity mapping when shapes match
        4. Use linear decomposition for reshape
        """
        prod_sizes = {r.dim: r.args[0] for r in producer.ranges if isinstance(r, Range)}
        cons_sizes = {r.dim: r.args[0] for r in self.ranges if isinstance(r, Range)}

        # Build dim -> Range mapping for consumer
        cons_dim_to_range: dict[int, Range] = {}
        for rng in self.ranges:
            if isinstance(rng, Range):
                cons_dim_to_range[rng.dim] = rng

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
        # Producer iterates [20, 50, 30] but outputs [20, 50, 1]
        # Consumer iterates [20, 50, 1] - should fuse
        if prod_out_sizes and set(prod_out_sizes.keys()) == consumer_dims:
            if all(sizes_compatible(prod_out_sizes.get(d, 1), cons_int[d]) for d in consumer_dims):
                # Map consumer dims to producer's output dims
                subst: dict[int, ATenOp] = {}
                for d in producer_dims:
                    if d in consumer_dims and d in cons_dim_to_range:
                        if prod_int[d] == 1 and cons_int[d] != 1:
                            subst[d] = _const(0)
                        elif prod_int[d] == cons_int[d]:
                            subst[d] = cons_dim_to_range[d]
                        else:
                            # Producer iterates more than consumer (reduction dim)
                            # Don't substitute - keep producer's Range for reduction
                            pass
                    # Dims not in consumer stay as-is (use producer's Range)
                if subst:
                    return subst

        if producer_dims == consumer_dims:
            # Same dims - check if sizes match (with broadcast)
            if all(sizes_compatible(prod_int[d], cons_int[d]) for d in consumer_dims):
                # For broadcast dims (size 1), use constant 0; otherwise identity
                subst = {}
                for d in producer_dims:
                    if prod_int[d] == 1 and cons_int[d] != 1:
                        subst[d] = _const(0)  # Broadcast: always index 0
                    else:
                        subst[d] = cons_dim_to_range[d]
                return subst

        if consumer_dims < producer_dims:
            # Reduce case: producer iterates more, consumer is inner loop
            # All producer dims must have matching consumer dims
            if consumer_dims == producer_dims & consumer_dims:
                if all(prod_int.get(d) == cons_int.get(d) for d in consumer_dims):
                    # Identity for shared dims, extras reference outer scope
                    return {d: cons_dim_to_range[d] for d in producer_dims if d in cons_dim_to_range}

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
            rng = cons_dim_to_range[d]
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
        producer: "EndRange"
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
        producer: "EndRange",
        subst: "dict[int, ATenOp]"
    ) -> "EndRange":
        """
        Apply fusion by transforming producer and inlining.

        In DAG, EndRange itself is the output reference.
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
            if isinstance(node, EndRange):
                return node
            if hasattr(node, 'args') and node.args:
                new_args = tuple(inline(arg) for arg in node.args)
                if new_args != node.args:
                    return replace(node, args=new_args)
            return node

        new_body = inline(self.body)

        return EndRange(
            self.ranges + (self.output, new_body),
            T=self.T,
            n_ranges=self.n_ranges,
        )

    def _preimage(self, node: ATenOp, subst: "dict[int, ATenOp]") -> ATenOp:
        """
        Apply preimage transform: replace Range/Aff nodes using substitution.

        For Range(dim=d), if d in subst, replace with subst[d].
        For Aff with Range(dim=d), if d in subst, expand to scalar expression.
        subst maps producer dims to IR expressions over consumer's Range nodes.
        """
        # Direct Range replacement (for already-transformed expressions)
        if isinstance(node, Range) and node.dim in subst:
            return subst[node.dim]

        if isinstance(node, Aff):
            stride, range_node, offset, incf = node.args
            if isinstance(range_node, Range) and range_node.dim in subst:
                ir_expr = subst[range_node.dim]
                # Aff computes: stride * (incf * range + offset)
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

        if isinstance(node, (EndRange, Memory)):
            return node

        if hasattr(node, 'args') and node.args:
            new_args = tuple(self._preimage(a, subst) for a in node.args)
            if new_args != node.args:
                return replace(node, args=new_args)

        return node

@dataclass(frozen=True)
class Store(ATenOp):
    """
    Store(dst, src) - Store src value into dst location.
    dst is typically a Load (with Aff indices), src is the computed value.
    """
    @staticmethod
    def new(dst: ATenOp, op: ATenOp) -> "Store":
        assert dst.T is not None
        return Store((dst, op), T=ATenOpType(axes=(), dtype=dst.T.dtype))

    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: Union[None, ATenOpType], **kwargs: Any) -> ATenOpType:
        assert len(args) == 2, "Store takes (dst, src)"
        assert args[0].T is not None
        return ATenOpType(axes=(), dtype=args[0].T.dtype)
