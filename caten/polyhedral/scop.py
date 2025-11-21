from __future__ import annotations

from typing import Any, Callable, Dict, List, Set, Tuple

from caten.ops import Node, OpType


class ScopStatementInfo:
    def __init__(self, name: str, domain: str, iter_names: List[str]):
        self.name = name
        self.domain = domain
        self.iter_names = iter_names

class Scop:
    def __init__(self) -> None:
        self.statements: List[ScopStatementInfo] = []
        self.params: Set[str] = set()

class Computation:
    def __init__(self) -> None:
        # Body returns a Node (graph), not a string.
        # The mapping maps iterator names (str) to Target-specific expressions (Node or str or Any)
        self.bodies: Dict[str, Callable[[Dict[str, Any]], Node]] = {}

    def add(self, name: str, body: Callable[[Dict[str, Any]], Node]) -> None:
        self.bodies[name] = body

def build_scop(graph: List[Node]) -> Tuple[Scop, Computation]:
    scop = Scop()
    comp = Computation()
    _traverse(graph, [], scop, comp)
    return scop, comp

def _traverse(nodes: List[Node], loop_stack: List[Tuple[str, str, str]], scop: Scop, comp: Computation) -> None:
    for node in nodes:
        if node.op == OpType.RANGE:
            iter_sym, args, body = node.arg
            start, stop = "0", "0"
            
            def _fmt(x: Any) -> str:
                if hasattr(x, "name"):
                    scop.params.add(x.name)
                    return x.name
                return str(x)

            if len(args) == 1:
                stop = _fmt(args[0])
            else:
                start = _fmt(args[0])
                stop = _fmt(args[1])
            
            loop_stack.append((iter_sym.name, start, stop))
            _traverse(body, loop_stack, scop, comp)
            loop_stack.pop()
            
        elif node.op == OpType.STORE:
            stmt_id = f"S_{len(scop.statements)}"
            iters = [loop[0] for loop in loop_stack]
            params_list = sorted(list(scop.params))
            params_str = f"[{', '.join(params_list)}]" if params_list else ""
            iter_str = ", ".join(iters)
            constraints = [f"{loop[1]} <= {loop[0]} < {loop[2]}" for loop in loop_stack]
            const_str = " and ".join(constraints)
            
            if not iters:
                domain_str = f"{params_str} -> {{ {stmt_id}[] : }}"
            else:
                domain_str = f"{params_str} -> {{ {stmt_id}[{iter_str}] : {const_str} }}"
            
            scop.statements.append(ScopStatementInfo(stmt_id, domain_str, iters))
            
            body_lambda = _create_body_lambda(node)
            comp.add(stmt_id, body_lambda)

def _create_body_lambda(store_node: Node) -> Callable[[Dict[str, Any]], Node]:
    def impl(mapping: Dict[str, Any]) -> Node:
        # Return a new Node tree with iterators replaced by mapping values
        return _replace_node(store_node, mapping)
    return impl

def _replace_node(node: Node, mapping: Dict[str, Any]) -> Node:
    # If VAR/Symbol matches mapping, return mapped value (which should be a Node or leaf)
    if node.op == OpType.VAR:
        if node.arg.name in mapping:
            val = mapping[node.arg.name]
            if isinstance(val, Node):
                return val
            # If mapped value is not a Node (e.g. string from ISL AST), wrap it?
            # Ideally Renderer passes Nodes or Atoms.
            # Let's assume Renderer constructs Nodes for loop indices.
            return _to_node(val)
        return node
    
    if node.op == OpType.CONST:
        return node
    
    if node.op == OpType.PLACEHOLDER:
        return node

    # Recursively replace children
    # LOAD arg is index (tuple/scalar). We need to replace symbols inside it.
    if node.op == OpType.LOAD:
        new_src = tuple(_replace_node(s, mapping) for s in node.src)
        new_arg = _replace_index(node.arg, mapping)
        return Node(node.op, new_src, arg=new_arg, name=node.name)
    
    if node.op == OpType.STORE:
        new_src = tuple(_replace_node(s, mapping) for s in node.src)
        new_arg = _replace_index(node.arg, mapping)
        return Node(node.op, new_src, arg=new_arg, name=node.name)

    # Generic traversal
    new_src = tuple(_replace_node(s, mapping) for s in node.src)
    # arg might need replacement if it holds symbols? 
    # For standard ops, arg is usually None or primitive.
    return Node(node.op, new_src, arg=node.arg, name=node.name)

def _replace_index(idx: Any, mapping: Dict[str, Any]) -> Any:
    if isinstance(idx, tuple):
        return tuple(_replace_val(x, mapping) for x in idx)
    return _replace_val(idx, mapping)

def _replace_val(val: Any, mapping: Dict[str, Any]) -> Any:
    # val could be Symbol, int, or Expr
    if hasattr(val, "name"):
        if val.name in mapping:
            return mapping[val.name]
    return val

def _to_node(obj: Any) -> Node:
    if isinstance(obj, Node):
        return obj
    return Node(OpType.CONST, (), arg=obj)