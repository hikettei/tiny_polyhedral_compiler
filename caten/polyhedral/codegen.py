from __future__ import annotations
from typing import Any, Optional, Dict, Callable, Type, ClassVar

from ctypes import CFUNCTYPE, c_void_p, cast
import base64
import json
import re
from caten.isl.specs.enums import isl_ast_node_type, isl_ast_expr_type, isl_ast_expr_op_type

import caten.isl as I

try:
    from pydantic import BaseModel
    try:
        from pydantic import ConfigDict
    except Exception:  # pragma: no cover - compatibility shim
        ConfigDict = None
except Exception:  # pragma: no cover - fallback when pydantic is unavailable
    class BaseModel:
        def __init__(self, **kwargs: Any) -> None:
            self.__dict__.update(kwargs)

        def model_dump_json(self) -> str:
            return json.dumps(self.__dict__, separators=(",", ":"))

        @classmethod
        def model_validate_json(cls, data: str) -> "BaseModel":
            return cls(**json.loads(data))

    ConfigDict = None

class DirectiveParams(BaseModel):
    if ConfigDict is not None:
        model_config = ConfigDict(extra="forbid")
    else:
        class Config:
            extra = "forbid"

class Directive:
    name: ClassVar[str] = "Directive"
    Params: ClassVar[Optional[Type[DirectiveParams]]] = None
    _registry: ClassVar[Dict[str, Type["Directive"]]] = {}

    def __init_subclass__(cls, **kwargs: Any) -> None:
        super().__init_subclass__(**kwargs)
        if getattr(cls, "name", None):
            Directive._registry[cls.name] = cls

    def __init__(self, **params: Any) -> None:
        if self.Params is None:
            self.params = None
        else:
            self.params = self.Params(**params)

    def dumps(self) -> str:
        payload = "" if self.params is None else self.params.model_dump_json()
        encoded = base64.urlsafe_b64encode(payload.encode("utf-8")).decode("ascii")
        return f"@Directive[{self.name}]<{encoded}>"

    @classmethod
    def loads(cls, encoded: str) -> Optional["Directive"]:
        match = re.match(r"^@Directive\\[([^\\]]+)\\]<([^>]*)>$", encoded)
        if not match:
            return None
        name, payload = match.group(1), match.group(2)
        target = cls._registry.get(name)
        if target is None:
            return None
        if payload:
            decoded = base64.urlsafe_b64decode(payload.encode("ascii")).decode("utf-8")
        else:
            decoded = ""
        if target.Params is None:
            return target()
        if decoded:
            params = target.Params.model_validate_json(decoded)
            if hasattr(params, "model_dump"):
                data = params.model_dump()
            elif hasattr(params, "dict"):
                data = params.dict()
            else:
                data = params.__dict__
            return target(**data)
        return target()

    def mark_id(self) -> "I.Id":
        return I.Id.alloc(self.dumps())

    def on_schedule(self, node: "I.ScheduleNode") -> "I.ScheduleNode":
        """
        Apply the directive at schedule-tree level.
        Default behavior inserts a mark node carrying the directive payload.
        """
        return node.insert_mark(self.mark_id())

    def on_ast_generation(self, node: "I.ASTNode", stmts: Dict[str, Callable]) -> "I.ASTNode":
        """
        Apply the directive at AST generation time.
        Default behavior strips the mark and returns its child node.
        """
        return node.mark_get_node()

# [TODO]
# Separate AST Generation and Rendering Process
# Use MLIR?
def schedule_to_ast(schedule: I.Schedule, stmts: Dict[str, Callable]):
    ast_node = I.ASTBuild.alloc().node_from_schedule(schedule)
    # Callback signature: isl_ast_node *(*fn)(__isl_take isl_ast_node *node, void *user)
    CALLBACK = CFUNCTYPE(c_void_p, c_void_p, c_void_p)
    def replace_cb(node_handle, user_data):
        node = I.ASTNode(node_handle)    
        if node.get_type() == isl_ast_node_type.ISL_AST_NODE_MARK:
            directive = Directive.loads(node.mark_get_id().name())
            if directive is not None:
                new_node = directive.on_ast_generation(node, stmts)
                if new_node is None:
                    return node.mark_get_node().copy_handle()
                return new_node.copy_handle()
            return node.mark_get_node().copy_handle()
        if node.get_type() == isl_ast_node_type.ISL_AST_NODE_USER: 
            expr = node.user_get_expr()
            if expr.get_type() == isl_ast_expr_type.ISL_AST_EXPR_OP: 
                if expr.get_op_type() == isl_ast_expr_op_type.ISL_AST_EXPR_OP_CALL: 
                    call_id = expr.get_op_arg(0).id_get_id()
                    name = call_id.name()
                    if name in stmts:
                        func = stmts[name]
                        n_args = expr.get_op_n_arg()
                        # arg 0 is the function ID, actual args start from 1
                        args = [expr.get_op_arg(i) for i in range(1, n_args)]
                        new_node = func(*args)
                        return new_node.copy_handle()
        return node.copy_handle()
    c_cb = CALLBACK(replace_cb)
    cb_ptr = cast(c_cb, c_void_p)
    ast_node = ast_node.map_descendant_bottom_up(cb_ptr, None)
    return ast_node

def schedule_to_c(schedule: I.Schedule, stmts: Dict[str, Callable]):
    # Print to C
    p = I.Printer.alloc_str()
    p.request_inplace()
    p = p.set_output_format(4) # ISL_FORMAT_C   
    p.request_inplace()
    p = p.print_ast_node(schedule_to_ast(schedule, stmts))
    return p.get_str()
