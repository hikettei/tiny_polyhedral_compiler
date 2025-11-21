from __future__ import annotations

import inspect
from enum import Enum, auto
from typing import Any, Callable, Dict, List, Optional, Tuple, Union

# --- Op Categories ---

class UnaryOps(Enum):
    NEG = auto()
    RECIP = auto()
    SIN = auto()
    EXP2 = auto()
    LOG2 = auto()
    SQRT = auto()
    NOT = auto()
    CAST = auto()

class BinaryOps(Enum):
    ADD = auto()
    MUL = auto()
    IDIV = auto()
    AND = auto()
    OR = auto()
    XOR = auto()
    MAX = auto()
    MOD = auto()
    
    # Comparison
    NEQ = auto()
    LT = auto()

class TernaryOps(Enum):
    WHERE = auto() # Select

class MemoryOps(Enum):
    LOAD = auto()
    STORE = auto()

class ControlOps(Enum):
    RANGE = auto() # Loop
    IF = auto()    # Conditional

class MetaOps(Enum):
    CONST = auto()
    VAR = auto() # Symbolic Variable
    PLACEHOLDER = auto() # Function Argument
    DIRECTIVE = auto() # Generic Directive Node

# Union type for type hinting
OpType = Union[UnaryOps, BinaryOps, TernaryOps, MemoryOps, ControlOps, MetaOps]

class Node:
    """
    IR Node.
    """
    __slots__ = ("op", "src", "arg", "name", "_hash", "shape", "dtype")
    
    def __init__(self, op: OpType, src: Tuple[Node, ...], arg: Any = None, name: Optional[str] = None):
        self.op = op
        self.src = src
        self.arg = arg # Can hold subgraphs for Range/If, or values for Const
        self.name = name if name is not None else ""
        self.shape: Optional[Tuple[int, ...]] = None
        self.dtype: Optional[Any] = None
        self._hash: Optional[int] = None

    def __repr__(self) -> str:
        if self.op == MetaOps.CONST:
            return f"Const({self.arg})"
        if self.op == MetaOps.VAR:
            return f"Var({self.arg})"
        if self.op == MetaOps.PLACEHOLDER:
            return f"Arg({self.name})"
        if self.op == ControlOps.RANGE:
            return f"Range(iter={self.name}, ...)"
        src_str = ", ".join([s.name or str(i) for i, s in enumerate(self.src)])
        return f"{self.op.name}({src_str})"

    # Ops for easy graph building in tracing
    def __add__(self, other: Any) -> Node: return _binop(BinaryOps.ADD, self, other)
    def __radd__(self, other: Any) -> Node: return _binop(BinaryOps.ADD, other, self)
    def __sub__(self, other: Any) -> Node: return _binop(BinaryOps.ADD, self, _unop(UnaryOps.NEG, other))
    def __rsub__(self, other: Any) -> Node: return _binop(BinaryOps.ADD, other, _unop(UnaryOps.NEG, self))
    def __mul__(self, other: Any) -> Node: return _binop(BinaryOps.MUL, self, other)
    def __rmul__(self, other: Any) -> Node: return _binop(BinaryOps.MUL, other, self)

def _to_node(obj: Any) -> Node:
    if isinstance(obj, Node):
        return obj
    return Node(MetaOps.CONST, (), arg=obj)

def _binop(op: OpType, a: Any, b: Any) -> Node:
    from .trace import get_builder
    node = Node(op, (_to_node(a), _to_node(b)))
    get_builder().push(node)
    return node

def _unop(op: OpType, a: Any) -> Node:
    from .trace import get_builder
    node = Node(op, (_to_node(a),))
    get_builder().push(node)
    return node

# --- Pattern Matcher ---

class UPat:
    def __init__(self, op: Union[OpType, Tuple[OpType, ...], None] = None, name: Optional[str] = None, src: Optional[Tuple[UPat, ...]] = None, arg: Any = None):
        self.op = (op,) if isinstance(op, (UnaryOps, BinaryOps, TernaryOps, MemoryOps, ControlOps, MetaOps)) else op
        self.name = name
        self.src = src
        self.arg = arg

    def match(self, node: Node, ctx: Dict[str, Node]) -> bool:
        if self.op is not None and node.op not in self.op:
            return False
        if self.arg is not None and node.arg != self.arg:
            return False
        if self.src is not None:
            if len(node.src) != len(self.src):
                return False
            if not all(p.match(n, ctx) for p, n in zip(self.src, node.src, strict=True)):
                return False
        
        if self.name:
            ctx[self.name] = node
        return True

    @staticmethod
    def var(name: str) -> 'UPat':
        return UPat(name=name)

class PatternMatcher:
    def __init__(self, patterns: List[Tuple[UPat, Callable]]):
        self.patterns = patterns

    def rewrite(self, node: Node, ctx_obj: Any = None) -> Any:
        for pat, func in self.patterns:
            match_ctx: Dict[str, Node] = {}
            if pat.match(node, match_ctx):
                sig = inspect.signature(func)
                args = []
                for name in sig.parameters:
                    if name == "ctx":
                        args.append(ctx_obj) # Context passed from caller (e.g. GradientContext)
                    elif name in match_ctx:
                        args.append(match_ctx[name])
                    else:
                        args.append(None) # Fallback
                
                return func(*args)
        return None
