from __future__ import annotations

from typing import Any, Optional, Tuple, Union

from .ops import Node, OpType
from .trace import get_builder


class TensorSpec:
    def __init__(self, shape: Tuple[Any, ...], dtype: Any = None):
        self.shape = shape
        self.dtype = dtype
    def __repr__(self) -> str: return f"TensorSpec({self.shape}, {self.dtype})"

class Tensor:
    """
    Frontend Tensor object.
    Operates as a wrapper around an IR Node.
    """
    def __init__(self, *args: Any, node: Optional[Node] = None, shape: Optional[Tuple[Any, ...]] = None, dtype: Any = None, name: Optional[str] = None):
        if node is not None:
            self.node = node
            self.shape = shape
            self.dtype = dtype
        elif len(args) > 0 and isinstance(args[0], Node):
            self.node = args[0]
            self.shape = shape
            self.dtype = dtype
        else:
            # User instantiation: Tensor(10, 10, dtype=f32, name="A")
            self.shape = args
            self.dtype = dtype
            # If name is not provided, generate a temp name? Or allow None?
            # Node constructor defaults name to ""
            self.node = Node(OpType.PLACEHOLDER, (), arg=None, name=name)

    def __class_getitem__(cls, item: Union[Any, Tuple[Any, ...]]) -> TensorSpec:
        if not isinstance(item, tuple):
            item = (item,)
        # Usage: C.Tensor[10, 10] -> TensorSpec((10, 10))
        return TensorSpec(item)

    def __repr__(self) -> str:
        return f"Tensor<{self.node.name or 'tmp'}>"

    # Proxy Ops to Node
    def __add__(self, other: Any) -> Tensor: return self._op(other, lambda a, b: a + b)
    def __mul__(self, other: Any) -> Tensor: return self._op(other, lambda a, b: a * b)
    def __sub__(self, other: Any) -> Tensor: return self._op(other, lambda a, b: a - b)
    def __neg__(self) -> Tensor: 
        from .ops import _unop
        return Tensor(node=_unop(OpType.NEG, self.node))

    def _op(self, other: Any, func: Any) -> Tensor:
        other_node = other.node if isinstance(other, Tensor) else other
        res_node = func(self.node, other_node)
        return Tensor(node=res_node)

    def __getitem__(self, idx: Any) -> Tensor:
        # Load op
        from .ops import Node, OpType
        # idx normalization logic needed
        node = Node(OpType.LOAD, (self.node,), arg=idx)
        get_builder().push(node)
        return Tensor(node=node)

    def __setitem__(self, idx: Any, value: Any) -> None:
        # Store op
        from .ops import Node, OpType
        val_node = value.node if isinstance(value, Tensor) else value
        node = Node(OpType.STORE, (self.node, _to_node(val_node)), arg=idx)
        get_builder().push(node)

def _to_node(obj: Any) -> Any:
    from .ops import _to_node as ops_to_node
    return ops_to_node(obj)

# DTypes (kept for backward compatibility / explicit usage if needed)
class DType:
    def __init__(self, name: str): self.name = name
    def __repr__(self) -> str: return self.name

float32 = DType("float32")
int32 = DType("int32")
f32 = float32
i32 = int32
