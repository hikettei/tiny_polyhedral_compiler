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
                if kwargs["keepdim"]: new_axes.append(ATenAxis(size=_const(1, index), stride=_const(1, index), offset=_const(0, index), incf=_const(0, index)))
            else:
                new_axes.append(i)
        return ATenOpType(
            axes=tuple(new_axes),
            dtype=tensor.T.dtype,
            offset=tensor.T.offset,
        )
    def lower(self) -> ATenOp:
        x, y = self.args[0].lower(), self.args[1].lower()
        return EndRange.sync(x, Store.new(x, self.bop((x, y)))) # todo: wait only reduced dims
## == EndRangeOps ============================================================
### Array access graph constrainted via only affine functions, sorted by lex order (for symbolic shape)
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
        assert isinstance(args[3], Const), f"Aff: The fourth argument should be a constant, getting {args[3].__class__}"
        return ATenOpType(axes=tuple(), dtype=index, offset=_const(0, index))
    def as_aff_str(self) -> str: return self.render_isl()

@dataclass(frozen=True)
class Load(ATenOp):
    """
    X = Load(Memory | EndRange, Aff1, Aff2, ...)
    Access the (Aff1.index() + Aff2.index() + ...)th element of Array. The dependency is trackable by Polyhedral Compiler.
    """
    @classmethod
    def verify(cls, args: tuple[ATenOp, ...], T: Union[None, ATenOpType], **kwargs: Any) -> ATenOpType:
        assert len(args) >= 2, "Load: the number of argument should be larger than two"
        assert isinstance(args[0], Memory) or isinstance(args[0], EndRange), "Load: Can only load element from Memory or Array."
        assert args[0].T is not None and args[0].T.ndim > 0, f"Load: the first argument should be array, getting scalar {args[0].__class__}"
        assert all([isinstance(arg, Aff) for arg in args[1:]]), "Load: the index should be specified by the list of Aff."
        return ATenOpType(axes=tuple(), dtype=args[0].T.dtype, offset=_const(0, index))

    @staticmethod
    def from_tensor(tensor: ATenOp, T: Optional[ATenOpType] = None) -> Load:
        dtype = T or tensor.T
        assert dtype is not None
        if dtype.ndim == 0: return tensor
        return Load((tensor,) + tuple([axis.aff(dim) for dim, axis in enumerate(dtype.axes)]))

    def as_union_map_str(self):
        # [TODO] How to write strides
        # [TODO] Symbolics
        doms = ", ".join([f"gid{aff.args[1].dim}" for aff in self.args[1:]])
        return f"S[{doms}] -> [" + "+".join([arg.as_aff_str() for arg in self.args[1:]]) + "]"

    def to_basic_map(self, dom_dim: int) -> A.BasicMap:
        """
        Convert Load access pattern to a BasicMap for dependency analysis.

        Returns a map: { S[gid0, ..., gidN] -> [addr] : addr = linearized_address }
        """
        dom_vars = tuple(f"gid{i}" for i in range(dom_dim))
        addr_expr = A.AffExpr.zero()

        for aff_node in self.args[1:]:  # Skip first arg (Memory)
            if not isinstance(aff_node, Aff):
                continue

            stride, range_node, offset, incf = aff_node.args

            # Get dimension from Range
            if isinstance(range_node, Range):
                dim = range_node.dim
                gid_var = f"gid{dim}"
            else:
                continue

            # Extract coefficient values (handle both concrete and symbolic)
            stride_val = stride.item if hasattr(stride, 'item') else stride
            offset_val = offset.item if hasattr(offset, 'item') else offset
            incf_val = incf.item if hasattr(incf, 'item') else incf

            # Build affine term: stride * (incf * gid + offset)
            if isinstance(stride_val, (int, float)) and isinstance(incf_val, (int, float)):
                # Concrete case
                coeff = int(stride_val * incf_val)
                const_contrib = int(stride_val * offset_val) if isinstance(offset_val, (int, float)) else 0
                term = A.AffExpr({gid_var: coeff}, 0)
                if const_contrib != 0:
                    term = term + const_contrib
            else:
                # Symbolic case - use ATenOp as coefficient
                coeff = A._coeff_mul(stride_val, incf_val)
                const_contrib = A._coeff_mul(stride_val, offset_val)
                term = A.AffExpr({gid_var: coeff}, 0)
                if not A._coeff_is_zero(const_contrib):
                    term = term + const_contrib

            addr_expr = addr_expr + term

        return A.BasicMap.from_access(dom_vars, addr_expr, dom_name="S")
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

@dataclass(frozen=True)
class EndRange(ViewOps, ATenOp):
    """
    ```
    EndRange(Memory, Store, range1, range2, ...) -> Tensor(Shaped)
    ```
    Proceeds to the next when range1 == max(range1) and range2 == max(range2) and ...

    Fusion Analysis:
    ================
    When two EndRanges are candidates for fusion, we analyze their access patterns:
    - Read/Write access relations are extracted as BasicMaps
    - Dependency analysis computes RAW/WAR/WAW relations
    - Fusion is legal if dependencies preserve sequential semantics
    """
    dims: tuple[int, ...] = ()
    reads: str = ""
    writes: str = ""
    # New fields for structured access relations
    read_maps: tuple = ()   # tuple[A.BasicMap, ...]
    write_maps: tuple = ()  # tuple[A.BasicMap, ...]

    @staticmethod
    def sync(memory: Memory, store: Store, axis: tuple[int, ...]=()): # rename: sync -> barrier?
        """
        Synchronize a memory store operation, collecting access patterns for fusion analysis.

        This method:
        1. Explores the IR graph to find all Load operations
        2. Extracts their access patterns as BasicMaps
        3. Builds dependency information for fusion decisions
        """
        assert memory.T is not None
        seen: dict[int, bool] = {}
        dim2range: dict[int, Range] = {}
        reads_str: list[str] = []
        writes_str: list[str] = []
        read_maps: list[A.BasicMap] = []
        write_maps: list[A.BasicMap] = []

        def _explore(item: ATenOp, write: bool = False) -> None:
            if id(item) in seen:
                return
            seen[id(item)] = True

            if isinstance(item, Range):
                if item.dim in dim2range:
                    assert ATenOp.eql(
                        (sz1 := dim2range[item.dim].args[0]),
                        (sz2 := item.args[0])
                    ), f"Cannot create schedule: {sz1} vs {sz2}"
                dim2range[item.dim] = item
                return

            if isinstance(item, EndRange):
                return

            if isinstance(item, Load):
                # Collect string representation (legacy)
                if write:
                    writes_str.append(item.as_union_map_str())
                else:
                    reads_str.append(item.as_union_map_str())

                # Collect BasicMap (new structured representation)
                dom_dim = max(len(dim2range), len(item.args) - 1)
                try:
                    basic_map = item.to_basic_map(dom_dim)
                    if write:
                        write_maps.append(basic_map)
                    else:
                        read_maps.append(basic_map)
                except Exception:
                    pass  # Skip if map construction fails

            for arg in item.args:
                _explore(arg, write=write)

        # Explore write side (destination)
        _explore(store.args[0], write=True)
        # Explore read side (source expression)
        _explore(store.args[1], write=False)

        reads_union_map = "{ " + "; ".join(reads_str) + " }"
        writes_union_map = "{ " + "; ".join(writes_str) + " }"

        assert sorted(tuple(dim2range.keys())) == list(range(0, len(dim2range.keys())))

        endrange = EndRange(
            (memory, store),
            T=ATenOpType.from_shape(tuple([s.size for s in memory.T.axes]), memory.T.dtype),
            dims=tuple(sorted(tuple(dim2range.keys()))),
            reads=reads_union_map,
            writes=writes_union_map,
            read_maps=tuple(read_maps),
            write_maps=tuple(write_maps),
        )

        # Try to fuse with parent EndRanges
        parents = endrange.get_parent_groups()
        for p in parents:
            endrange = endrange.fuse(p)

        return endrange

    def get_parent_groups(self) -> list:
        """Find all EndRange nodes that this statement depends on."""
        seen: dict[int, bool] = {}
        parents: list = []

        def _explore(item: ATenOp) -> None:
            if id(item) in seen:
                return
            seen[id(item)] = True
            if isinstance(item, EndRange):
                parents.append(item)
                return
            for arg in item.args:
                _explore(arg)

        for arg in self.args:
            _explore(arg)
        return parents

    def fuse(self, other: EndRange) -> EndRange:
        """
        Attempt to fuse this EndRange with another (typically a producer).

        Fusion Algorithm (Unified for all fusion types):
        =================================================
        1. Compute dependency relation: producer_iter -> consumer_iter
        2. Extract variable substitution from the relation's constraint
        3. Apply preimage to transform producer's iteration space
        4. Use compute_at to inline transformed producer into consumer

        The key insight is that all fusion types differ only in the substitution:
        - Perfect: identity (gid0 -> gid0)
        - Tiled: scaled + offset (h -> 4*hp + rh)
        - Partial: subset of dimensions fused
        """
        # Build UnionMaps from BasicMaps
        self_reads = A.UnionMap.from_maps(list(self.read_maps))
        self_writes = A.UnionMap.from_maps(list(self.write_maps))
        other_reads = A.UnionMap.from_maps(list(other.read_maps))
        other_writes = A.UnionMap.from_maps(list(other.write_maps))

        # Attempt fusion with full analysis
        fusion_result = A.attempt_fusion(
            other_writes, other_reads,  # Producer (other)
            self_writes, self_reads,    # Consumer (self)
        )

        if not fusion_result.success:
            return self

        # Extract substitution from RAW dependency
        # The constraint tells us how producer vars map to consumer vars
        subst = self._extract_substitution(fusion_result)
        if subst is None:
            return self

        # Transform producer using preimage and inline via compute_at
        transformed_producer = other.preimage(subst, self)
        return self.compute_at(transformed_producer, other)

    def _extract_substitution(self, fusion_result: A.FusionResult) -> dict[str, A.AffExpr] | None:
        """
        Extract variable substitution from fusion result.

        For a constraint like: 128*h - 512*hp - 128*rh = 0
        We derive: h = 4*hp + rh

        Returns dict mapping producer vars to expressions in consumer vars.
        """
        if not fusion_result.dep_info.raw.maps:
            return {}

        raw_map = fusion_result.dep_info.raw.maps[0]
        producer_vars = set(raw_map.dom_vars)
        consumer_vars = set(raw_map.rng_vars)

        subst: dict[str, A.AffExpr] = {}

        # For shared variables (same name), identity mapping
        shared = producer_vars & consumer_vars
        for v in shared:
            subst[v] = A.AffExpr.var(v)

        # For tiled fusion, use tiling info
        if fusion_result.tiling_info:
            tinfo = fusion_result.tiling_info
            # For each tiled producer dim, compute: p = tile_size * q + r
            # where q is the scaled consumer var and r is the reduction var
            for pvar, (tile_size, rvar) in tinfo.tile_dims.items():
                # Find the scaled variable q (has coefficient tile_size * coeff(p))
                if raw_map.constraints:
                    expr = raw_map.constraints[0].expr
                    p_coeff = expr.coeff_of(pvar)
                    if isinstance(p_coeff, int) and p_coeff != 0:
                        # Find q with coefficient = tile_size * p_coeff
                        for cvar in consumer_vars - producer_vars:
                            c_coeff = expr.coeff_of(cvar)
                            if isinstance(c_coeff, int) and abs(c_coeff) == abs(p_coeff * tile_size):
                                # p = tile_size * q + r
                                subst[pvar] = tile_size * A.AffExpr.var(cvar) + A.AffExpr.var(rvar)
                                break
        else:
            # For perfect/partial fusion, try to solve the constraint
            if raw_map.constraints:
                constraint = raw_map.constraints[0]
                for pvar in producer_vars - consumer_vars:
                    sol = A._try_solve_for(constraint, pvar)
                    if sol is not None:
                        # Check that solution only uses consumer vars
                        if sol.variables() <= consumer_vars:
                            subst[pvar] = sol

        # If we couldn't find substitutions for all producer-only vars,
        # it might still be valid if they cancel out
        return subst

    def preimage(self, subst: dict[str, A.AffExpr], consumer: EndRange) -> EndRange:
        """
        Transform this EndRange's iteration space using the substitution.

        This rewrites all Aff nodes to use consumer's iteration variables.

        Args:
            subst: Mapping from producer vars (gid0, gid1, ...) to expressions
                   in consumer vars
            consumer: The consumer EndRange (provides the new iteration space)

        Returns:
            New EndRange with transformed access patterns
        """
        if not subst:
            return self

        # Build mapping from old Range dims to new expressions
        dim_to_expr: dict[int, A.AffExpr] = {}
        for var, expr in subst.items():
            if var.startswith("gid"):
                try:
                    dim = int(var[3:])
                    dim_to_expr[dim] = expr
                except ValueError:
                    pass

        # Transform the Store node's access patterns
        def transform_node(node: ATenOp) -> ATenOp:
            if isinstance(node, Aff):
                stride, range_node, offset, incf = node.args
                if isinstance(range_node, Range):
                    dim = range_node.dim
                    if dim in dim_to_expr:
                        # Transform this Aff using the substitution
                        new_aff = self._transform_aff(node, dim_to_expr[dim], consumer)
                        if new_aff is not None:
                            return new_aff
                return node

            if isinstance(node, Load):
                # Transform each Aff in the Load
                new_args = [node.args[0]]  # Keep Memory reference
                for arg in node.args[1:]:
                    new_args.append(transform_node(arg))
                return Load(tuple(new_args))

            if isinstance(node, Store):
                new_dst = transform_node(node.args[0])
                new_src = transform_node(node.args[1])
                return Store((new_dst, new_src), T=node.T)

            if isinstance(node, EndRange):
                return node  # Don't recurse into other EndRanges

            if isinstance(node, Memory):
                return node

            # For other ops, recursively transform children
            if hasattr(node, 'args') and node.args:
                new_args = tuple(transform_node(arg) for arg in node.args)
                if new_args != node.args:
                    return replace(node, args=new_args)
            return node

        # Transform the computation
        new_store = transform_node(self.args[1])

        # Create new EndRange with consumer's dimensions
        return EndRange(
            (self.args[0], new_store),  # Keep memory, use transformed store
            T=self.T,
            dims=consumer.dims,  # Use consumer's loop dimensions
            reads=self.reads,
            writes=self.writes,
            read_maps=self.read_maps,
            write_maps=self.write_maps,
        )

    def _transform_aff(self, aff: Aff, expr: A.AffExpr, consumer: EndRange) -> Aff | None:
        """
        Transform a single Aff node using the affine expression.

        If expr = c0*v0 + c1*v1 + ... + const, we expand to multiple Aff nodes
        summed together, or create a composite Aff.
        """
        stride, range_node, offset, incf = aff.args

        # For simple case: expr = c * var + const
        if len(expr.coeff) == 1:
            var, coeff = next(iter(expr.coeff.items()))
            if var.startswith("gid"):
                try:
                    new_dim = int(var[3:])
                    # Find the Range for this dimension in consumer
                    for cdim in consumer.dims:
                        if cdim == new_dim:
                            # Create new Aff with transformed range
                            new_size = consumer._get_dim_size(cdim)
                            if new_size is not None:
                                new_range = Range((new_size,), dim=new_dim)
                                new_offset = _const(expr.const) if expr.const != 0 else offset
                                new_incf = Mul((incf, _const(coeff))) if coeff != 1 else incf
                                return Aff((stride, new_range, new_offset, new_incf))
                except ValueError:
                    pass

        # For complex expressions (tiled), we need to generate multiple Aff nodes
        # This happens with expr = tile_size * q + r
        # We create: stride * (tile_size * q + r) = stride*tile_size*q + stride*r
        # But this should be handled by expanding into separate Load terms
        return None

    def _get_dim_size(self, dim: int) -> ATenOp | None:
        """Get the size of a dimension from this EndRange's iteration space."""
        # Look through the IR to find the Range with this dim
        def find_range(node: ATenOp) -> ATenOp | None:
            if isinstance(node, Range) and node.dim == dim:
                return node.args[0]  # Return the size
            if isinstance(node, EndRange):
                return None
            if hasattr(node, 'args'):
                for arg in node.args:
                    result = find_range(arg)
                    if result is not None:
                        return result
            return None

        for arg in self.args:
            result = find_range(arg)
            if result is not None:
                return result
        return None

    def compute_at(self, producer: EndRange, original_producer: EndRange) -> EndRange:
        """
        Inline the (transformed) producer into this consumer.

        This eliminates the intermediate buffer and creates a single fused kernel.

        Args:
            producer: The transformed producer EndRange (with preimage applied)
            original_producer: The original producer (to identify what to replace)

        Returns:
            New EndRange with inlined computation
        """
        # The producer's Store contains the computation to inline
        # We need to replace Load(producer_memory) with producer's computation

        producer_memory = original_producer.args[0]  # Memory node
        producer_computation = producer.args[1].args[1]  # Store's source expression

        def inline_producer(node: ATenOp) -> ATenOp:
            """Replace loads from producer's memory with producer's computation."""
            if isinstance(node, Load):
                if node.args[0] is producer_memory or node.args[0] is original_producer:
                    # This Load reads from the producer - inline the computation
                    return producer_computation

            if isinstance(node, EndRange):
                # Don't recurse into other EndRanges (except if it's the producer)
                if node is original_producer:
                    # Skip the producer - it's being inlined
                    return node
                return node

            # Recursively process children
            if hasattr(node, 'args') and node.args:
                new_args = tuple(inline_producer(arg) for arg in node.args)
                if new_args != node.args:
                    return replace(node, args=new_args)
            return node

        # Transform the consumer's store
        new_store = inline_producer(self.args[1])

        # Merge read maps (consumer reads what producer reads)
        merged_read_maps = tuple(list(producer.read_maps) + [
            m for m in self.read_maps
            if m not in producer.write_maps  # Exclude reads of intermediate
        ])

        return EndRange(
            (self.args[0], new_store),
            T=self.T,
            dims=self.dims,
            reads=self.reads,
            writes=self.writes,
            read_maps=merged_read_maps,
            write_maps=self.write_maps,
        )

    def get_dependency_info(self, other: EndRange) -> A.DependencyInfo:
        """
        Get detailed dependency information between this and another EndRange.

        Useful for external analysis and scheduling decisions.
        """
        self_reads = A.UnionMap.from_maps(list(self.read_maps))
        self_writes = A.UnionMap.from_maps(list(self.write_maps))
        other_reads = A.UnionMap.from_maps(list(other.read_maps))
        other_writes = A.UnionMap.from_maps(list(other.write_maps))

        return A.analyze_dependencies(
            other_writes, other_reads,
            self_writes, self_reads,
        )

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
# TODO: Update Viz
# TODO: EndRange --> Runtime
# TODO: class View
# [TODO]
# 1. Fence
# 2. View
# - Priority:
#  - Semanticを強固にする，View/Reduceの取り扱い
#  - Where is Fuse
#  - 基本的には，EndRangeがある地点でKernelを分割するイメージ
#  - EndRangeに対して，Reshape (= Tile) が可能であるかどうかが知りたい
#  - LexFebce.syncした時点でRaW/WaW/WaRを解析してもいい
#  - LeafだけをTileして，あとはType Inferenceで自動で再構築したい。
# e.g.:
# a = T.Var("A[m n]", float32)
# P.stmt("...")[a]
