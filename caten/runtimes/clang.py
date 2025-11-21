from abc import ABC, abstractmethod
from typing import Any, List

import caten.isl as I

from ..kernel import Symbol
from ..ops import Node, OpType
from ..polyhedral.ast_visitor import ASTVisitor
from ..polyhedral.schedule import PolyhedralSchedule
from ..polyhedral.scop import Computation, Scop, build_scop


class CompiledKernel(ABC):
    @abstractmethod
    def __call__(self, *args: Any, **kwargs: Any) -> Any: pass

class Runtime(ABC):
    @abstractmethod
    def compile(self, graph_nodes: List[Node], input_placeholders: List[Node]) -> CompiledKernel: pass

class ClangKernel(CompiledKernel):
    def __init__(self, source: str):
        self.source = source
    
    def __call__(self, *args: Any, **kwargs: Any) -> Any:
        print("--- Generated C Code ---")
        print(self.source)
        print("------------------------")
        return None

class ClangRenderer(ASTVisitor):
    def __init__(self, scop: Scop, comp: Computation):
        self.scop = scop
        self.comp = comp
        self.lines: List[str] = []
        self.indent = 0
        self.stmt_map = {s.name: s for s in scop.statements}

    def _emit(self, s: str):
        self.lines.append("  " * self.indent + s)

    def _print_isl_expr(self, expr: I.ASTExpr) -> str:
        # Create a temporary printer for each expression
        p = I.Printer.alloc_str()
        p.request_inplace()
        p = p.set_output_format(I.ISL_FORMAT_C)
        p.request_inplace()
        p = p.print_ast_expr(expr)
        return p.get_str()

    def get_code(self) -> str:
        return "\n".join(self.lines)

    # --- Visitor Methods ---

    def visit_block(self, node: I.ASTNode):
        # block children list
        child_list = node.block_get_children()
        # child_list is an ASTNodeList
        n = child_list.n_ast_node()
        for i in range(n):
            child = child_list.get_ast_node(i)
            self.visit(child)

    def visit_for(self, node: I.ASTNode):
        # Extract loop info
        iterator = node.for_get_iterator()
        init = node.for_get_init()
        cond = node.for_get_cond()
        inc = node.for_get_inc()
        
        iter_str = self._print_isl_expr(iterator)
        init_str = self._print_isl_expr(init)
        cond_str = self._print_isl_expr(cond)
        inc_val = self._print_isl_expr(inc)
        
        # Construct C loop
        self._emit(f"for (int {iter_str} = {init_str}; {cond_str}; {iter_str} += {inc_val}) {{")
        self.indent += 1
        self.visit(node.for_get_body())
        self.indent -= 1
        self._emit("}")

    def visit_if(self, node: I.ASTNode):
        cond = node.if_get_cond()
        cond_str = self._print_isl_expr(cond)
        
        self._emit(f"if ({cond_str}) {{")
        self.indent += 1
        self.visit(node.if_get_then())
        self.indent -= 1
        
        if node.if_has_else():
            self._emit("} else {")
            self.indent += 1
            self.visit(node.if_get_else())
            self.indent -= 1
            
        self._emit("}")

    def visit_user(self, node: I.ASTNode):
        # User node contains an expression which is a call: S_0(c0, c1)
        expr = node.user_get_expr()
        # op = expr.get_op_type() # Unused
        
        # arg 0 is the function ID (S_0)
        func_id = expr.get_op_arg(0)
        stmt_name = func_id.get_id().name()
        
        # Remaining args are arguments
        n_args = expr.get_op_n_arg()
        args = []
        for i in range(1, n_args):
            arg_expr = expr.get_op_arg(i)
            arg_str = self._print_isl_expr(arg_expr)
            # Wrap in VAR node so replacement logic treats it as a value
            args.append(Node(OpType.VAR, (), arg=Symbol(arg_str)))

        stmt_info = self.stmt_map.get(stmt_name)
        body_func = self.comp.bodies.get(stmt_name)
        
        if stmt_info and body_func:
            if len(args) == len(stmt_info.iter_names):
                # Create mapping: iter_name -> Node(VAR, "c0")
                mapping = dict(zip(stmt_info.iter_names, args, strict=True))
                
                # Invoke lambda to get the computation Graph (Node tree)
                comp_graph_node = body_func(mapping)
                
                # Render this graph to C string
                code_str = self._render_node_tree(comp_graph_node)
                self._emit(code_str + ";")
            else:
                self._emit(f"// Error: Arg mismatch for {stmt_name}")
        else:
            self._emit(f"// Unknown statement: {stmt_name}")

    def visit_mark(self, node: I.ASTNode):
        # Example: #pragma omp parallel
        mark_id = node.mark_get_id()
        self._emit(f"// Mark: {mark_id.name()}")
        self.visit(node.mark_get_node())

    # --- Node Tree Renderer ---
    def _render_node_tree(self, node: Node) -> str:
        # Recursive renderer for computation graph Nodes
        if node.op == OpType.CONST:
            return str(node.arg)
        if node.op == OpType.VAR:
            return str(node.arg) # arg is Symbol or str
        
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
        # ... Add other ops ...
        
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
        ast_root = sched.get_ast()
        
        if not ast_root:
            return ClangKernel("// Empty Kernel")
            
        # 3. Render using ASTVisitor
        renderer = ClangRenderer(scop, comp)
        renderer.visit(ast_root)
        body_code = renderer.get_code()
        
        src = [
            "// Polyhedral Generated Kernel (AST Visitor)",
            "#include <math.h>",
            "#include <stdio.h>",
            "",
            "void kernel() {",
            body_code,
            "}"
        ]
        return ClangKernel("\n".join(src))