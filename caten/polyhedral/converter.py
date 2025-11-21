from typing import List

import caten.isl as I

from ..kernel import Symbol
from ..ops import Node, OpType
from .ast_visitor import ASTVisitor
from .scop import Computation, Scop


class AstToGraphConverter(ASTVisitor):
    def __init__(self, scop: Scop, comp: Computation):
        self.scop = scop
        self.comp = comp
        self.stmt_map = {s.name: s for s in scop.statements}
        self.graph: List[Node] = [] # Resulting flat list of nodes (top level)

    def convert(self, ast_root: I.ASTNode) -> List[Node]:
        self.graph = []
        if ast_root:
            self.visit(ast_root)
        return self.graph

    def _expr_to_symbol(self, expr: I.ASTExpr) -> Symbol:
        # Convert ISL expr to string symbol
        p = I.Printer.alloc_str()
        p.request_inplace()
        p = p.set_output_format(I.ISL_FORMAT_C)
        p.request_inplace()
        p = p.print_ast_expr(expr)
        return Symbol(p.get_str())

    def visit_block(self, node: I.ASTNode):
        child_list = node.block_get_children()
        n = child_list.n_ast_node()
        for i in range(n):
            child = child_list.get_ast_node(i)
            self.visit(child)

    def visit_for(self, node: I.ASTNode):
        iterator = node.for_get_iterator()
        init = node.for_get_init()
        cond = node.for_get_cond()
        inc = node.for_get_inc()
        
        iter_sym = self._expr_to_symbol(iterator)
        init_sym = self._expr_to_symbol(init)
        cond_sym = self._expr_to_symbol(cond)
        inc_sym = self._expr_to_symbol(inc)
        
        # Save current graph
        parent_graph = self.graph
        self.graph = [] # New block for body
        
        self.visit(node.for_get_body())
        
        body_block = self.graph
        self.graph = parent_graph
        
        # RANGE arg for polyhedral loop: (iter_sym, (init, cond, inc), body)
        # We distinguish this from simple range by the structure of args tuple
        range_node = Node(
            OpType.RANGE, 
            (), 
            arg=(iter_sym, (init_sym, cond_sym, inc_sym), body_block), 
            name=iter_sym.name
        )
        self.graph.append(range_node)

    def visit_if(self, node: I.ASTNode):
        cond = node.if_get_cond()
        cond_sym = self._expr_to_symbol(cond)
        
        parent_graph = self.graph
        self.graph = []
        self.visit(node.if_get_then())
        then_block = self.graph
        
        else_block = []
        if node.if_has_else():
            self.graph = []
            self.visit(node.if_get_else())
            else_block = self.graph
            
        self.graph = parent_graph
        
        if_node = Node(OpType.IF, (), arg=(cond_sym, then_block, else_block))
        self.graph.append(if_node)

    def visit_user(self, node: I.ASTNode):
        expr = node.user_get_expr()
        func_id = expr.get_op_arg(0)
        stmt_name = func_id.get_id().name()
        
        n_args = expr.get_op_n_arg()
        args = []
        for i in range(1, n_args):
            arg_expr = expr.get_op_arg(i)
            # Pass iterators as VAR nodes with symbol string
            args.append(Node(OpType.VAR, (), arg=self._expr_to_symbol(arg_expr)))
            
        stmt_info = self.stmt_map.get(stmt_name)
        body_func = self.comp.bodies.get(stmt_name)
        
        if stmt_info and body_func:
            if len(args) == len(stmt_info.iter_names):
                mapping = dict(zip(stmt_info.iter_names, args, strict=True))
                comp_graph_node = body_func(mapping)
                self.graph.append(comp_graph_node)
    
    def visit_mark(self, node: I.ASTNode):
        # Skip mark for now, or wrap in DIRECTIVE node
        self.visit(node.mark_get_node())
