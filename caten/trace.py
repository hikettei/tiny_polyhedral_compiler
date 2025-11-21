from __future__ import annotations

import contextvars
from typing import Any, List, Optional, Set

from .ops import Node, OpType


class GraphBuilder:
    def __init__(self) -> None:
        # The root block of the graph
        self.root_block: List[Node] = []
        # Stack of active blocks (lists of nodes) where new nodes are appended
        self.block_stack: List[List[Node]] = [self.root_block]
        # Inputs to the kernel
        self.inputs: List[Node] = []

    def push(self, node: Node) -> None:
        """Push a node to the current active block."""
        self.block_stack[-1].append(node)

    def push_block(self) -> List[Node]:
        """Start a new scope (block) and return it."""
        new_block: List[Node] = []
        self.block_stack.append(new_block)
        return new_block

    def pop_block(self) -> List[Node]:
        """End the current scope and return the block."""
        if len(self.block_stack) <= 1:
            raise RuntimeError("Cannot pop root block")
        return self.block_stack.pop()

    def register_input(self, node: Node) -> None:
        self.inputs.append(node)
        self.push(node)

    def reset(self) -> None:
        self.root_block = []
        self.block_stack = [self.root_block]
        self.inputs = []

    def resolve_graph(self, outputs: List[Node]) -> List[Node]:
        """
        Returns the list of nodes reachable from outputs, topologically sorted.
        Since we build the graph sequentially, the root_block is already largely sorted,
        but this filters out unused nodes.
        
        NOTE: For control flow (Range/If), the Node itself contains the sub-block.
        So we just need to traverse dependencies.
        """
        visited: Set[Node] = set()
        topo: List[Node] = []

        def visit(n: Node) -> None:
            if n in visited:
                return
            visited.add(n)
            # Visit children
            for s in n.src:
                visit(s)
            # Special handling for control flow nodes that have sub-graphs in 'arg'
            if n.op in (OpType.RANGE, OpType.IF):
                 # n.arg is the sub-block (List[Node])
                 # We need to visit nodes inside the block to mark them as used?
                 # Actually, if the Range node is used, its body is implicitly used.
                 # However, nodes INSIDE the body might depend on outside nodes.
                 if isinstance(n.arg, list):
                     for sub_n in n.arg:
                         visit(sub_n)
            topo.append(n)

        for o in outputs:
            visit(o)
        
        return topo

_builder_ctx: contextvars.ContextVar[Optional[GraphBuilder]] = contextvars.ContextVar("builder", default=None)

def get_builder() -> GraphBuilder:
    b = _builder_ctx.get()
    if b is None:
        b = GraphBuilder()
        _builder_ctx.set(b)
    return b

def set_builder(b: GraphBuilder) -> Any:
    return _builder_ctx.set(b)