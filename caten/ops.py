from __future__ import annotations

from enum import Enum, auto
from typing import Any, Optional, Tuple


class OpType(Enum):
    # --- Arithmetic / Logic ---
    NEG = auto()
    RECIP = auto()
    SIN = auto()
    EXP2 = auto()
    LOG2 = auto()
    SQRT = auto()
    NOT = auto()
    CAST = auto()
    
    ADD = auto()
    MUL = auto()
    IDIV = auto()
    AND = auto()
    OR = auto()
    XOR = auto()
    MAX = auto()
    MOD = auto()
    
    NEQ = auto()
    LT = auto()
    
    WHERE = auto()
    
    # --- Memory ---
    LOAD = auto()
    STORE = auto()
    
    # --- Terminals ---
    CONST = auto()
    VAR = auto() # Symbolic Variable
    PLACEHOLDER = auto() # Function Argument
    
    # --- Control Flow / Structure ---
    RANGE = auto() # Loop
    IF = auto()    # Conditional
    
    # --- Directives ---
    DIRECTIVE = auto() # Generic Directive Node

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
        if self.op == OpType.CONST:
            return f"Const({self.arg})"
        if self.op == OpType.VAR:
            return f"Var({self.arg})"
        if self.op == OpType.PLACEHOLDER:
            return f"Arg({self.name})"
        if self.op == OpType.RANGE:
            return f"Range(iter={self.name}, ...)"
        src_str = ", ".join([s.name or str(i) for i, s in enumerate(self.src)])
        return f"{self.op.name}({src_str})"

    # Ops for easy graph building in tracing
    def __add__(self, other: Any) -> Node: return _binop(OpType.ADD, self, other)
    def __radd__(self, other: Any) -> Node: return _binop(OpType.ADD, other, self)
    def __sub__(self, other: Any) -> Node: return _binop(OpType.ADD, self, _unop(OpType.NEG, other))
    def __rsub__(self, other: Any) -> Node: return _binop(OpType.ADD, other, _unop(OpType.NEG, self))
    def __mul__(self, other: Any) -> Node: return _binop(OpType.MUL, self, other)
    def __rmul__(self, other: Any) -> Node: return _binop(OpType.MUL, other, self)

def _to_node(obj: Any) -> Node:
    if isinstance(obj, Node):
        return obj
    return Node(OpType.CONST, (), arg=obj)

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
