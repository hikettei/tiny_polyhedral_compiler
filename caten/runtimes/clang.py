from typing import Any, List

from ..ops import Node, OpType
from ..polyhedral.schedule import PolyhedralSchedule
from ..polyhedral.scop import build_scop
from ..runtime import CompiledKernel, Runtime


class ClangKernel(CompiledKernel):
    def __init__(self, source: str):
        self.source = source
    
    def __call__(self, *args: Any, **kwargs: Any) -> Any:
        print("--- Generated C Code ---")
        print(self.source)
        print("------------------------")
        return None

class ClangRenderer:
    """
    Renders Caten Ops Graph to C code string.
    Does not depend on ISL AST directly.
    """
    def __init__(self) -> None:
        pass

    def render(self, graph: List[Node]) -> str:
        lines: List[str] = []
        self._render_block(graph, lines, 0)
        return "\n".join(lines)

    def _render_block(self, nodes: List[Node], lines: List[str], indent: int) -> None:
        prefix = "  " * indent
        for node in nodes:
            if node.op == OpType.RANGE:
                # arg = (iter_sym, bounds, body)
                iter_sym, bounds, body = node.arg
                
                if len(bounds) == 3: # Polyhedral Loop: (init, cond, inc)
                    init, cond, inc = bounds
                    lines.append(f"{prefix}for (int {iter_sym} = {init}; {cond}; {iter_sym} += {inc}) {{")
                elif len(bounds) == 2: # Simple Range: (start, stop)
                    start, stop = bounds
                    lines.append(f"{prefix}for (int {iter_sym} = {start}; {iter_sym} < {stop}; ++{iter_sym}) {{")
                else:
                    # Handle 1 arg or other cases if needed
                    stop = bounds[0]
                    lines.append(f"{prefix}for (int {iter_sym} = 0; {iter_sym} < {stop}; ++{iter_sym}) {{")

                self._render_block(body, lines, indent + 1)
                lines.append(f"{prefix}}}")
                
            elif node.op == OpType.IF:
                cond, then_block, else_block = node.arg
                lines.append(f"{prefix}if ({cond}) {{")
                self._render_block(then_block, lines, indent + 1)
                if else_block:
                    lines.append(f"{prefix}}} else {{")
                    self._render_block(else_block, lines, indent + 1)
                lines.append(f"{prefix}}}")
                
            elif node.op in (OpType.STORE, OpType.LOAD, OpType.ADD, OpType.MUL): 
                # Top level expression (usually STORE)
                code = self._render_node_tree(node)
                lines.append(f"{prefix}{code};")
            
            elif node.op == OpType.DIRECTIVE:
                lines.append(f"{prefix}// Directive: {node.arg}")
                
            else:
                pass # Skip placeholders etc

    def _render_node_tree(self, node: Node) -> str:
        if node.op == OpType.CONST:
            return str(node.arg)
        if node.op == OpType.VAR:
            return str(node.arg)
        
        if node.op == OpType.LOAD:
            src = node.src[0]
            idx = node.arg
            idx_str = self._render_index(idx)
            return f"{src.name}{idx_str}"
            
        if node.op == OpType.STORE:
            dest = node.src[0]
            val = node.src[1]
            idx = node.arg
            idx_str = self._render_index(idx)
            val_str = self._render_node_tree(val)
            return f"{dest.name}{idx_str} = {val_str}"
        
        if node.op == OpType.ADD:
            return f"({self._render_node_tree(node.src[0])} + {self._render_node_tree(node.src[1])})"
        if node.op == OpType.MUL:
            return f"({self._render_node_tree(node.src[0])} * {self._render_node_tree(node.src[1])})"
        
        return f"/* Unhandled Op: {node.op} */"

    def _render_index(self, idx: Any) -> str:
        indices = []
        if isinstance(idx, tuple):
            for i in idx:
                indices.append(self._render_val(i))
        else:
            indices.append(self._render_val(idx))
        return "".join(f"[{s}]" for s in indices)

    def _render_val(self, val: Any) -> str:
        if isinstance(val, Node):
            return self._render_node_tree(val)
        if hasattr(val, "name"):
            return val.name
        return str(val)


class ClangRuntime(Runtime):
    def compile(self, graph_nodes: List[Node], input_placeholders: List[Node]) -> CompiledKernel:
        # 1. Build Scop and Computation
        scop, comp = build_scop(graph_nodes)
        
        # 2. Schedule
        sched = PolyhedralSchedule(scop)
        
        # 3. Convert scheduled AST to Caten Ops Graph (with Control Flow)
        ops_graph = sched.to_graph(comp)
        
        if not ops_graph:
            return ClangKernel("// Empty Kernel")
            
        # 4. Render Ops Graph to C Code
        renderer = ClangRenderer()
        body_code = renderer.render(ops_graph)
        
        src = [
            "// Polyhedral Generated Kernel (Ops Graph Renderer)",
            "#include <math.h>",
            "#include <stdio.h>",
            "",
            "void kernel() {",
            body_code,
            "}"
        ]
        return ClangKernel("\n".join(src))