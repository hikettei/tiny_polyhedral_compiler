from __future__ import annotations

from ctypes import c_char_p, c_int
from typing import TYPE_CHECKING, Any, List

from ..ffi import load_libisl
from ..func import ISLFunction
from ..mixin import ISLObjectMixin
from ..obj import ISLObject
from ..qualifier import Give, Keep, Param, Take
from ..registry import register_type
from .context import Context

if TYPE_CHECKING:
    from .ast_expr import ASTExpr
    from .context import Context

_lib = load_libisl()

class AstExprList(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_ast_expr_list_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_ast_expr_list_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_ast_expr_list_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_ast_expr_list_free(handle)

    @classmethod
    def from_exprs(cls, expr_list: List[ASTExpr]) -> AstExprList:
        handle = _isl_ast_expr_list_alloc(0)
        for expr in expr_list:
            handle = _isl_ast_expr_list_add(handle, expr)
        return handle

register_type("AstExprList", AstExprList)

_isl_ast_expr_list_read_from_str = ISLFunction.create(
    "isl_ast_expr_list_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("AstExprList"),
    lib=_lib,
)

_isl_ast_expr_list_add = ISLFunction.create(
    "isl_ast_expr_list_add",
    Take("AstExprList"),
    Take("ASTExpr"),
    return_=Give("AstExprList"),
    lib=_lib,
)

_isl_ast_expr_list_copy = ISLFunction.create(
    "isl_ast_expr_list_copy",
    Keep("AstExprList"),
    return_=Give("AstExprList"),
    lib=_lib,
)

_isl_ast_expr_list_free = ISLFunction.create(
    "isl_ast_expr_list_free",
    Take("AstExprList"),
    return_=Give("AstExprList"),
    lib=_lib,
)

_isl_ast_expr_list_alloc = ISLFunction.create(
    "isl_ast_expr_list_alloc",
    Context(),
    Param(int, ctype=c_int),
    return_=Give("AstExprList"),
    lib=_lib,
)
