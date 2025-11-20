from __future__ import annotations

from ctypes import c_char_p, c_int, c_void_p
from typing import TYPE_CHECKING, Any

from ..ffi import load_libisl
from ..func import ISLFunction
from ..mixin import ISLObjectMixin
from ..obj import ISLObject
from ..qualifier import Give, Keep, Param, Take
from ..registry import register_type
from .context import Context

if TYPE_CHECKING:
    from .ast_expr import ASTExpr
    from .ast_node_list import AstNodeList
    from .ast_print_options import ASTPrintOptions
    from .context import Context
    from .id import Id
    from .printer import Printer

_lib = load_libisl()

class ASTNode(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_ast_node_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_ast_node_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_ast_node_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_ast_node_free(handle)

    def __str__(self) -> str:
        return _isl_ast_node_to_str(self)

    def __repr__(self) -> str:
        return f"ASTNode({self.__str__()})"

    def get_ctx(self) -> "Context":
        return _isl_ast_node_get_ctx(self)

    def get_type(self) -> int:
        return _isl_ast_node_get_type(self)

    def for_get_iterator(self) -> "ASTExpr":
        return _isl_ast_node_for_get_iterator(self)

    def for_get_init(self) -> "ASTExpr":
        return _isl_ast_node_for_get_init(self)

    def for_get_cond(self) -> "ASTExpr":
        return _isl_ast_node_for_get_cond(self)

    def for_get_inc(self) -> "ASTExpr":
        return _isl_ast_node_for_get_inc(self)

    def for_get_body(self) -> "ASTNode":
        return _isl_ast_node_for_get_body(self)

    def for_is_degenerate(self) -> bool:
        return _isl_ast_node_for_is_degenerate(self)

    def if_get_cond(self) -> "ASTExpr":
        return _isl_ast_node_if_get_cond(self)

    def if_get_then_node(self) -> "ASTNode":
        return _isl_ast_node_if_get_then_node(self)

    def if_get_then(self) -> "ASTNode":
        return _isl_ast_node_if_get_then(self)

    def if_has_else_node(self) -> bool:
        return _isl_ast_node_if_has_else_node(self)

    def if_has_else(self) -> bool:
        return _isl_ast_node_if_has_else(self)

    def if_get_else_node(self) -> "ASTNode":
        return _isl_ast_node_if_get_else_node(self)

    def if_get_else(self) -> "ASTNode":
        return _isl_ast_node_if_get_else(self)

    def block_get_children(self) -> "AstNodeList":
        return _isl_ast_node_block_get_children(self)

    def mark_get_id(self) -> "Id":
        return _isl_ast_node_mark_get_id(self)

    def mark_get_node(self) -> "ASTNode":
        return _isl_ast_node_mark_get_node(self)

    def user_get_expr(self) -> "ASTExpr":
        return _isl_ast_node_user_get_expr(self)

    def foreach_descendant_top_down(self, fn: Any, user: Any = None) -> int:
        return _isl_ast_node_foreach_descendant_top_down(self, fn, user)

    def foreach_ast_expr_op_type(self, fn: Any, user: Any = None) -> int:
        return _isl_ast_node_foreach_ast_expr_op_type(self, fn, user)

    def foreach_ast_op_type(self, fn: Any, user: Any = None) -> int:
        return _isl_ast_node_foreach_ast_op_type(self, fn, user)

    def map_descendant_bottom_up(self, fn: Any, user: Any = None) -> "ASTNode":
        return _isl_ast_node_map_descendant_bottom_up(self, fn, user)

    def set_annotation(self, annotation: "Id") -> "ASTNode":
        return _isl_ast_node_set_annotation(self, annotation)

    def get_annotation(self) -> "Id":
        return _isl_ast_node_get_annotation(self)

    def to_C_str(self) -> str:
        return _isl_ast_node_to_C_str(self)

    def print_macros(self, p: "Printer") -> "Printer":
        return _isl_ast_node_print_macros(self, p)

    def print(self, p: "Printer", options: "ASTPrintOptions") -> "Printer":
        return _isl_ast_node_print(self, p, options)

    def for_print(self, p: "Printer", options: "ASTPrintOptions") -> "Printer":
        return _isl_ast_node_for_print(self, p, options)

    def if_print(self, p: "Printer", options: "ASTPrintOptions") -> "Printer":
        return _isl_ast_node_if_print(self, p, options)

    @classmethod
    def user_from_expr(cls, expr: "ASTExpr") -> "ASTNode":
        return _isl_ast_node_user_from_expr(expr)

    @classmethod
    def from_expr(cls, expr: "ASTExpr") -> "ASTNode":
        return _isl_ast_node_alloc_user(expr)

    @classmethod
    def block_from_children(cls, list: "AstNodeList") -> "ASTNode":
        return _isl_ast_node_block_from_children(list)


class ASTUserNode(ASTNode):
    pass
register_type("ASTUserNode", ASTUserNode)

register_type("ASTNode", ASTNode)

_isl_ast_node_get_ctx = ISLFunction.create(
    "isl_ast_node_get_ctx",
    Keep("ASTNode"),
    return_=Give("Context"),
    lib=_lib,
)

_isl_ast_node_get_type = ISLFunction.create(
    "isl_ast_node_get_type",
    Keep("ASTNode"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_ast_node_for_get_iterator = ISLFunction.create(
    "isl_ast_node_for_get_iterator",
    Keep("ASTNode"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_node_for_get_init = ISLFunction.create(
    "isl_ast_node_for_get_init",
    Keep("ASTNode"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_node_for_get_cond = ISLFunction.create(
    "isl_ast_node_for_get_cond",
    Keep("ASTNode"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_node_for_get_inc = ISLFunction.create(
    "isl_ast_node_for_get_inc",
    Keep("ASTNode"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_node_for_get_body = ISLFunction.create(
    "isl_ast_node_for_get_body",
    Keep("ASTNode"),
    return_=Give("ASTNode"),
    lib=_lib,
)

_isl_ast_node_for_is_degenerate = ISLFunction.create(
    "isl_ast_node_for_is_degenerate",
    Keep("ASTNode"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_ast_node_if_get_cond = ISLFunction.create(
    "isl_ast_node_if_get_cond",
    Keep("ASTNode"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_node_if_get_then_node = ISLFunction.create(
    "isl_ast_node_if_get_then_node",
    Keep("ASTNode"),
    return_=Give("ASTNode"),
    lib=_lib,
)

_isl_ast_node_if_get_then = ISLFunction.create(
    "isl_ast_node_if_get_then",
    Keep("ASTNode"),
    return_=Give("ASTNode"),
    lib=_lib,
)

_isl_ast_node_if_has_else_node = ISLFunction.create(
    "isl_ast_node_if_has_else_node",
    Keep("ASTNode"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_ast_node_if_has_else = ISLFunction.create(
    "isl_ast_node_if_has_else",
    Keep("ASTNode"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_ast_node_if_get_else_node = ISLFunction.create(
    "isl_ast_node_if_get_else_node",
    Keep("ASTNode"),
    return_=Give("ASTNode"),
    lib=_lib,
)

_isl_ast_node_if_get_else = ISLFunction.create(
    "isl_ast_node_if_get_else",
    Keep("ASTNode"),
    return_=Give("ASTNode"),
    lib=_lib,
)

_isl_ast_node_block_get_children = ISLFunction.create(
    "isl_ast_node_block_get_children",
    Keep("ASTNode"),
    return_=Give("AstNodeList"),
    lib=_lib,
)

_isl_ast_node_mark_get_id = ISLFunction.create(
    "isl_ast_node_mark_get_id",
    Keep("ASTNode"),
    return_=Give("Id"),
    lib=_lib,
)

_isl_ast_node_mark_get_node = ISLFunction.create(
    "isl_ast_node_mark_get_node",
    Keep("ASTNode"),
    return_=Give("ASTNode"),
    lib=_lib,
)

_isl_ast_node_user_get_expr = ISLFunction.create(
    "isl_ast_node_user_get_expr",
    Keep("ASTNode"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_node_foreach_descendant_top_down = ISLFunction.create(
    "isl_ast_node_foreach_descendant_top_down",
    Keep("ASTNode"),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_ast_node_foreach_ast_expr_op_type = ISLFunction.create(
    "isl_ast_node_foreach_ast_expr_op_type",
    Keep("ASTNode"),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_ast_node_foreach_ast_op_type = ISLFunction.create(
    "isl_ast_node_foreach_ast_op_type",
    Keep("ASTNode"),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_ast_node_copy = ISLFunction.create(
    "isl_ast_node_copy",
    Keep("ASTNode"),
    return_=Give("ASTNode"),
    lib=_lib,
)

_isl_ast_node_free = ISLFunction.create(
    "isl_ast_node_free",
    Take("ASTNode"),
    return_=Give("ASTNode"),
    lib=_lib,
)

_isl_ast_node_map_descendant_bottom_up = ISLFunction.create(
    "isl_ast_node_map_descendant_bottom_up",
    Take("ASTNode"),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Give("ASTNode"),
    lib=_lib,
)

_isl_ast_node_set_annotation = ISLFunction.create(
    "isl_ast_node_set_annotation",
    Take("ASTNode"),
    Take("Id"),
    return_=Give("ASTNode"),
    lib=_lib,
)

_isl_ast_node_get_annotation = ISLFunction.create(
    "isl_ast_node_get_annotation",
    Keep("ASTNode"),
    return_=Give("Id"),
    lib=_lib,
)

_isl_ast_node_to_str = ISLFunction.create(
    "isl_ast_node_to_str",
    Keep("ASTNode"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_ast_node_to_C_str = ISLFunction.create(
    "isl_ast_node_to_C_str",
    Keep("ASTNode"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_ast_node_print_macros = ISLFunction.create(
    "isl_ast_node_print_macros",
    Keep("ASTNode"),
    Take("Printer"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_ast_node_print = ISLFunction.create(
    "isl_ast_node_print",
    Keep("ASTNode"),
    Take("Printer"),
    Take("ASTPrintOptions"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_ast_node_for_print = ISLFunction.create(
    "isl_ast_node_for_print",
    Keep("ASTNode"),
    Take("Printer"),
    Take("ASTPrintOptions"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_ast_node_if_print = ISLFunction.create(
    "isl_ast_node_if_print",
    Keep("ASTNode"),
    Take("Printer"),
    Take("ASTPrintOptions"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_ast_node_user_from_expr = ISLFunction.create(
    "isl_ast_node_user_from_expr",
    Take("ASTExpr"),
    return_=Give("ASTNode"),
    lib=_lib,
)

_isl_ast_node_alloc_user = ISLFunction.create(
    "isl_ast_node_alloc_user",
    Take("ASTExpr"),
    return_=Give("ASTNode"),
    lib=_lib,
)

_isl_ast_node_block_from_children = ISLFunction.create(
    "isl_ast_node_block_from_children",
    Take("AstNodeList"),
    return_=Give("ASTNode"),
    lib=_lib,
)

_isl_ast_node_read_from_str = ISLFunction.create(
    "isl_ast_node_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("ASTNode"),
    lib=_lib,
)
