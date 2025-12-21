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
from .enums import _ISL_AST_EXPR_OP_TYPE_MAP, _ISL_AST_EXPR_TYPE_MAP

if TYPE_CHECKING:
    from .ast_expr_list import ASTExprList
    from .context import Context
    from .id import Id
    from .val import Val

_lib = load_libisl()

# handle is an integer, to prevent the confusion vs val, use I.expr instead of I.ASTExpr
def expr(id_or_val: Any) -> "ASTExpr":
    if isinstance(id_or_val, int):
        from .val import Val
        return ASTExpr.from_val(Val.int_from_si(id_or_val))
    elif isinstance(id_or_val, str):
        from .id import Id
        return ASTExpr.from_id(Id(id_or_val))
    else:
        raise TypeError("I.expr can only accept int or str")

class ASTExpr(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        super().__init__(handle_or_spec)

    def get_type_name(self) -> str:
        """Helper to get the string name of the expr type."""
        return _ISL_AST_EXPR_TYPE_MAP.get(self.get_type(), "unknown")

    def op_get_type_name(self) -> str:
        """Helper to get the string name of the expr op type."""
        return _ISL_AST_EXPR_OP_TYPE_MAP.get(self.op_get_type(), "unknown")

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_ast_expr_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_ast_expr_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_ast_expr_free(handle)

    def __str__(self) -> str:
        return _isl_ast_expr_to_str(self)

    def __repr__(self) -> str:
        return f"ASTExpr({self.__str__()})"

    def get_ctx(self) -> "Context":
        return _isl_ast_expr_get_ctx(self)

    def get_type(self) -> int:
        return _isl_ast_expr_get_type(self)

    def op_get_type(self) -> int:
        return _isl_ast_expr_op_get_type(self)

    def get_op_type(self) -> int:
        return _isl_ast_expr_get_op_type(self)

    def op_get_n_arg(self) -> int:
        return _isl_ast_expr_op_get_n_arg(self)

    def get_op_n_arg(self) -> int:
        return _isl_ast_expr_get_op_n_arg(self)

    def op_get_arg(self, pos: int) -> "ASTExpr":
        return _isl_ast_expr_op_get_arg(self, pos)

    def get_op_arg(self, pos: int) -> "ASTExpr":
        return _isl_ast_expr_get_op_arg(self, pos)

    def foreach_ast_expr_op_type(self, fn: Any, user: Any, user_: Any = None) -> int:
        return _isl_ast_expr_foreach_ast_expr_op_type(self, fn, user, user_)

    def foreach_ast_op_type(self, fn: Any, user: Any, user_: Any = None) -> int:
        return _isl_ast_expr_foreach_ast_op_type(self, fn, user, user_)

    def id_get_id(self) -> "Id":
        return _isl_ast_expr_id_get_id(self)

    def get_id(self) -> "Id":
        return _isl_ast_expr_get_id(self)

    def int_get_val(self) -> "Val":
        return _isl_ast_expr_int_get_val(self)

    def get_val(self) -> "Val":
        return _isl_ast_expr_get_val(self)

    def is_equal(self, expr2: "ASTExpr") -> bool:
        return _isl_ast_expr_is_equal(self, expr2)

    @classmethod
    def from_val(cls, v: "Val") -> "ASTExpr":
        return _isl_ast_expr_from_val(v)

    @classmethod
    def from_id(cls, id: "Id") -> "ASTExpr":
        return _isl_ast_expr_from_id(id)

    def neg(self) -> "ASTExpr":
        return _isl_ast_expr_neg(self)

    def address_of(self) -> "ASTExpr":
        return _isl_ast_expr_address_of(self)

    def add(self, expr2: "ASTExpr") -> "ASTExpr":
        return _isl_ast_expr_add(self, expr2)

    def sub(self, expr2: "ASTExpr") -> "ASTExpr":
        return _isl_ast_expr_sub(self, expr2)

    def mul(self, expr2: "ASTExpr") -> "ASTExpr":
        return _isl_ast_expr_mul(self, expr2)

    def div(self, expr2: "ASTExpr") -> "ASTExpr":
        return _isl_ast_expr_div(self, expr2)
    
    def max(self, expr2: "ASTExpr") -> "ASTExpr":
        return expr("max").call(self, expr2)
    
    def min(self, expr2: "ASTExpr") -> "ASTExpr":
        return expr("min").call(self, expr2)

    def pdiv_q(self, expr2: "ASTExpr") -> "ASTExpr":
        return _isl_ast_expr_pdiv_q(self, expr2)

    def pdiv_r(self, expr2: "ASTExpr") -> "ASTExpr":
        return _isl_ast_expr_pdiv_r(self, expr2)

    def and_(self, expr2: "ASTExpr") -> "ASTExpr":
        return _isl_ast_expr_and(self, expr2)

    def and_then(self, expr2: "ASTExpr") -> "ASTExpr":
        return _isl_ast_expr_and_then(self, expr2)

    def or_(self, expr2: "ASTExpr") -> "ASTExpr":
        return _isl_ast_expr_or(self, expr2)

    def or_else(self, expr2: "ASTExpr") -> "ASTExpr":
        return _isl_ast_expr_or_else(self, expr2)

    def eq(self, expr2: "ASTExpr") -> "ASTExpr":
        return _isl_ast_expr_eq(self, expr2)

    def le(self, expr2: "ASTExpr") -> "ASTExpr":
        return _isl_ast_expr_le(self, expr2)

    def lt(self, expr2: "ASTExpr") -> "ASTExpr":
        return _isl_ast_expr_lt(self, expr2)

    def ge(self, expr2: "ASTExpr") -> "ASTExpr":
        return _isl_ast_expr_ge(self, expr2)

    def gt(self, expr2: "ASTExpr") -> "ASTExpr":
        return _isl_ast_expr_gt(self, expr2)

    def set_op_arg(self, pos: int, arg: "ASTExpr") -> "ASTExpr":
        return _isl_ast_expr_set_op_arg(self, pos, arg)

    def substitute_ids(self, id2expr: "IdToAstExpr") -> "ASTExpr":
        return _isl_ast_expr_substitute_ids(self, id2expr)

    def call(self, *args: List["ASTExpr"]) -> "ASTExpr":
        from .ast_expr_list import AstExprList
        return _isl_ast_expr_call(self, AstExprList.from_exprs(args))

    def access(self, *indices: List["ASTExpr"]) -> "ASTExpr":
        from .ast_expr_list import AstExprList
        return _isl_ast_expr_access(self, AstExprList.from_exprs(indices))

    def assign(self, expr2: "ASTExpr") -> "ASTNode":
        """
        Creates an assignment node: self = expr2.
        Since ISL AST does not natively support assignment statements, 
        we represent this as a call to a function named "=".
        """
        from .ast_node import ASTNode
        from .id import Id
        
        # Create a call expression: =(self, expr2)
        # Use Id.alloc to force creating an ID named "=", avoiding parser error
        eq_id = Id.alloc("assign")
        eq_expr = ASTExpr.from_id(eq_id)
        call_expr = eq_expr.call(self, expr2)
        
        # Wrap it in a User Node
        return ASTNode.user_from_expr(call_expr)

    def to_C_str(self) -> str:
        return _isl_ast_expr_to_C_str(self)

    def print_macros(self, p: "Printer") -> "Printer":
        return _isl_ast_expr_print_macros(self, p)


register_type("ASTExpr", ASTExpr)

_isl_ast_expr_get_ctx = ISLFunction.create(
    "isl_ast_expr_get_ctx",
    Keep("ASTExpr"),
    return_=Give("Context"),
    lib=_lib,
)

_isl_ast_expr_get_type = ISLFunction.create(
    "isl_ast_expr_get_type",
    Keep("ASTExpr"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_ast_expr_op_get_type = ISLFunction.create(
    "isl_ast_expr_op_get_type",
    Keep("ASTExpr"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_ast_expr_get_op_type = ISLFunction.create(
    "isl_ast_expr_get_op_type",
    Keep("ASTExpr"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_ast_expr_op_get_n_arg = ISLFunction.create(
    "isl_ast_expr_op_get_n_arg",
    Keep("ASTExpr"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_ast_expr_get_op_n_arg = ISLFunction.create(
    "isl_ast_expr_get_op_n_arg",
    Keep("ASTExpr"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_ast_expr_op_get_arg = ISLFunction.create(
    "isl_ast_expr_op_get_arg",
    Keep("ASTExpr"),
    Param(int, ctype=c_int),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_expr_get_op_arg = ISLFunction.create(
    "isl_ast_expr_get_op_arg",
    Keep("ASTExpr"),
    Param(int, ctype=c_int),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_expr_foreach_ast_expr_op_type = ISLFunction.create(
    "isl_ast_expr_foreach_ast_expr_op_type",
    Keep("ASTExpr"),
    Param(None, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_ast_expr_foreach_ast_op_type = ISLFunction.create(
    "isl_ast_expr_foreach_ast_op_type",
    Keep("ASTExpr"),
    Param(None, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_ast_expr_id_get_id = ISLFunction.create(
    "isl_ast_expr_id_get_id",
    Keep("ASTExpr"),
    return_=Give("Id"),
    lib=_lib,
)

_isl_ast_expr_get_id = ISLFunction.create(
    "isl_ast_expr_get_id",
    Keep("ASTExpr"),
    return_=Give("Id"),
    lib=_lib,
)

_isl_ast_expr_int_get_val = ISLFunction.create(
    "isl_ast_expr_int_get_val",
    Keep("ASTExpr"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_ast_expr_get_val = ISLFunction.create(
    "isl_ast_expr_get_val",
    Keep("ASTExpr"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_ast_expr_is_equal = ISLFunction.create(
    "isl_ast_expr_is_equal",
    Keep("ASTExpr"),
    Keep("ASTExpr"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_ast_expr_copy = ISLFunction.create(
    "isl_ast_expr_copy",
    Keep("ASTExpr"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_expr_free = ISLFunction.create(
    "isl_ast_expr_free",
    Take("ASTExpr"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_expr_from_val = ISLFunction.create(
    "isl_ast_expr_from_val",
    Take("Val"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_expr_from_id = ISLFunction.create(
    "isl_ast_expr_from_id",
    Take("Id"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_expr_neg = ISLFunction.create(
    "isl_ast_expr_neg",
    Take("ASTExpr"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_expr_address_of = ISLFunction.create(
    "isl_ast_expr_address_of",
    Take("ASTExpr"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_expr_add = ISLFunction.create(
    "isl_ast_expr_add",
    Take("ASTExpr"),
    Take("ASTExpr"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_expr_sub = ISLFunction.create(
    "isl_ast_expr_sub",
    Take("ASTExpr"),
    Take("ASTExpr"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_expr_mul = ISLFunction.create(
    "isl_ast_expr_mul",
    Take("ASTExpr"),
    Take("ASTExpr"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_expr_div = ISLFunction.create(
    "isl_ast_expr_div",
    Take("ASTExpr"),
    Take("ASTExpr"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_expr_pdiv_q = ISLFunction.create(
    "isl_ast_expr_pdiv_q",
    Take("ASTExpr"),
    Take("ASTExpr"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_expr_pdiv_r = ISLFunction.create(
    "isl_ast_expr_pdiv_r",
    Take("ASTExpr"),
    Take("ASTExpr"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_expr_and = ISLFunction.create(
    "isl_ast_expr_and",
    Take("ASTExpr"),
    Take("ASTExpr"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_expr_and_then = ISLFunction.create(
    "isl_ast_expr_and_then",
    Take("ASTExpr"),
    Take("ASTExpr"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_expr_or = ISLFunction.create(
    "isl_ast_expr_or",
    Take("ASTExpr"),
    Take("ASTExpr"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_expr_or_else = ISLFunction.create(
    "isl_ast_expr_or_else",
    Take("ASTExpr"),
    Take("ASTExpr"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_expr_eq = ISLFunction.create(
    "isl_ast_expr_eq",
    Take("ASTExpr"),
    Take("ASTExpr"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_expr_le = ISLFunction.create(
    "isl_ast_expr_le",
    Take("ASTExpr"),
    Take("ASTExpr"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_expr_lt = ISLFunction.create(
    "isl_ast_expr_lt",
    Take("ASTExpr"),
    Take("ASTExpr"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_expr_ge = ISLFunction.create(
    "isl_ast_expr_ge",
    Take("ASTExpr"),
    Take("ASTExpr"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_expr_gt = ISLFunction.create(
    "isl_ast_expr_gt",
    Take("ASTExpr"),
    Take("ASTExpr"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_expr_set_op_arg = ISLFunction.create(
    "isl_ast_expr_set_op_arg",
    Take("ASTExpr"),
    Param(int, ctype=c_int),
    Take("ASTExpr"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_expr_substitute_ids = ISLFunction.create(
    "isl_ast_expr_substitute_ids",
    Take("ASTExpr"),
    Take("IdToAstExpr"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_expr_to_str = ISLFunction.create(
    "isl_ast_expr_to_str",
    Keep("ASTExpr"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_ast_expr_to_C_str = ISLFunction.create(
    "isl_ast_expr_to_C_str",
    Keep("ASTExpr"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_ast_expr_print_macros = ISLFunction.create(
    "isl_ast_expr_print_macros",
    Keep("ASTExpr"),
    Take("Printer"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_ast_expr_read_from_str = ISLFunction.create(
    "isl_ast_expr_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_expr_access = ISLFunction.create(
    "isl_ast_expr_access",
    Take("ASTExpr"),
    Take("AstExprList"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_expr_call = ISLFunction.create(
    "isl_ast_expr_call",
    Take("ASTExpr"),
    Take("AstExprList"),
    return_=Give("ASTExpr"),
    lib=_lib,
)
