import re
from abc import ABC, abstractmethod
from typing import TYPE_CHECKING, Any, Dict, List, Optional

import caten.isl as I

from .ops import Node, OpType

if TYPE_CHECKING:
    from .polyhedral.integration import PolyhedralModel
    from .tensor import Tensor

class Renderer(ABC):
    def __init__(self, model: "PolyhedralModel"):
        self.model = model
        
    @abstractmethod
    def render(self) -> str: pass

class CStyleLanguage(Renderer):
    """Mixin for C-like languages (C, CUDA, Metal)"""
    def _gen_expr(self, node: Any, mapping: Dict[str, str]) -> str:
        if node.op == OpType.CONST:
            return str(node.arg)
        elif node.op == OpType.LOAD:
            return self._gen_access(node.arg[0], node.arg[1], mapping)
        elif node.op == OpType.ADD:
            return f"({self._gen_expr(node.src[0], mapping)} + {self._gen_expr(node.src[1], mapping)})"
        elif node.op == OpType.MUL:
            return f"({self._gen_expr(node.src[0], mapping)} * {self._gen_expr(node.src[1], mapping)})"
        elif node.op == OpType.NEG:
            return f"-({self._gen_expr(node.src[0], mapping)})"
        return "..." # Fallback for unhandled ops

    def _gen_access(self, tensor: "Tensor", idx: Any, mapping: Dict[str, str]) -> str:
        indices: List[str] = []
        if isinstance(idx, tuple):
            for i in idx:
                indices.append(self._resolve(i, mapping))
        else:
            indices.append(self._resolve(idx, mapping))
            
        idx_str = "][".join(indices)
        return f"{tensor.name}[{idx_str}]"

    def _resolve(self, val: Any, mapping: Dict[str, str]) -> str:
        # Avoid circular import of Symbol
        if hasattr(val, "name") and type(val).__name__ == "Symbol":
            return mapping.get(val.name, str(val)) # Use str(val) if not in mapping
        return str(val)

class CRenderer(CStyleLanguage):
    def render(self) -> str:
        # Build AST from schedule
        build: I.ASTBuild = I.ASTBuild.from_context(self.model.schedule.get_domain().params())
        ast_node: I.ASTNode = build.node_from_schedule(self.model.schedule)
        
        # Print AST to C code string
        p: I.Printer = I.Printer.alloc_str()
        p.request_inplace()
        p = p.set_output_format(I.ISL_FORMAT_C)
        p.request_inplace()
        p = p.print_ast_node(ast_node)
        code: str = p.get_str()
        
        # Replace Statement calls with actual computation
        lines: List[str] = code.splitlines()
        new_lines: List[str] = []
        
        for line in lines:
            if "S_" in line and "(" in line and ");" in line:
                # Parse call "S_0(c0, c1);"
                match: Optional[re.Match] = re.search(r"(S_\d+)\s*\((.*?)\);", line)
                if match:
                    stmt_name: str = match.group(1)
                    args_str: str = match.group(2)
                    args: List[str] = [a.strip() for a in args_str.split(",")]
                    
                    idx: int = int(stmt_name.split("_")[1])
                    stmt_info: Dict[str, Any] = self.model.ctx.statements[idx]
                    
                    original_iters: List[str] = []
                    for scope in stmt_info["domain"]:
                        if scope.type == "loop":
                            original_iters.append(scope.var.name)
                            
                    mapping: Dict[str, str] = dict(zip(original_iters, args, strict=True)) # Mypy fix: Add strict=True
                    
                    comp: str = self._gen_computation(stmt_info, mapping)
                    indent: str = line[:line.find(stmt_name)]
                    new_lines.append(f"{indent}{comp}")
                    continue
            
            new_lines.append(line)
            
        return "\n".join(new_lines)

    def _gen_computation(self, stmt: Dict[str, Any], mapping: Dict[str, str]) -> str:
        target: "Tensor" = stmt["target"]
        idx: Any = stmt["index"]
        expr_node: Node = stmt["expr"]
        
        lhs: str = self._gen_access(target, idx, mapping)
        rhs: str = self._gen_expr(expr_node, mapping)
        
        return f"{lhs} = {rhs};"
