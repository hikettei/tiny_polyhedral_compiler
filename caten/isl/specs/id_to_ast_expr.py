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
    from .context import Context
    from .id import Id

_lib = load_libisl()

class IdToAstExpr(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_id_to_ast_expr_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_id_to_ast_expr_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_id_to_ast_expr_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_id_to_ast_expr_free(handle)

    def __str__(self) -> str:
        return _isl_id_to_ast_expr_to_str(self)

    def __repr__(self) -> str:
        return f"IdToAstExpr({self.__str__()})"

    def get_ctx(self) -> "Context":
        return _isl_id_to_ast_expr_get_ctx(self)

    @classmethod
    def alloc(cls, min_size: int) -> "IdToAstExpr":
        return _isl_id_to_ast_expr_alloc(min_size)

    def has(self, key: "Id") -> bool:
        return _isl_id_to_ast_expr_has(self, key)

    def get(self, key: "Id") -> "ASTExpr":
        return _isl_id_to_ast_expr_get(self, key)

    def foreach(self, fn: Any, val: "ASTExpr", user: Any, user_: Any = None) -> int:
        return _isl_id_to_ast_expr_foreach(self, fn, val, user, user_)

    def every(self, test: Any, val: "ASTExpr", user: Any, user_: Any = None) -> bool:
        return _isl_id_to_ast_expr_every(self, test, val, user, user_)

    def set(self, key: "Id", val: "ASTExpr") -> "IdToAstExpr":
        return _isl_id_to_ast_expr_set(self, key, val)

    def drop(self, key: "Id") -> "IdToAstExpr":
        return _isl_id_to_ast_expr_drop(self, key)

    def is_equal(self, id2expr2: "IdToAstExpr") -> bool:
        return _isl_id_to_ast_expr_is_equal(self, id2expr2)


register_type("IdToAstExpr", IdToAstExpr)

_isl_id_to_ast_expr_get_ctx = ISLFunction.create(
    "isl_id_to_ast_expr_get_ctx",
    Keep("IdToAstExpr"),
    return_=Give("Context"),
    lib=_lib,
)

_isl_id_to_ast_expr_alloc = ISLFunction.create(
    "isl_id_to_ast_expr_alloc",
    Context(),
    Param(int, ctype=c_int),
    return_=Give("IdToAstExpr"),
    lib=_lib,
)

_isl_id_to_ast_expr_copy = ISLFunction.create(
    "isl_id_to_ast_expr_copy",
    Keep("IdToAstExpr"),
    return_=Give("IdToAstExpr"),
    lib=_lib,
)

_isl_id_to_ast_expr_free = ISLFunction.create(
    "isl_id_to_ast_expr_free",
    Take("IdToAstExpr"),
    return_=Give("IdToAstExpr"),
    lib=_lib,
)

_isl_id_to_ast_expr_has = ISLFunction.create(
    "isl_id_to_ast_expr_has",
    Keep("IdToAstExpr"),
    Keep("Id"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_id_to_ast_expr_get = ISLFunction.create(
    "isl_id_to_ast_expr_get",
    Keep("IdToAstExpr"),
    Take("Id"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_id_to_ast_expr_foreach = ISLFunction.create(
    "isl_id_to_ast_expr_foreach",
    Keep("IdToAstExpr"),
    Param(None, ctype=c_void_p),
    Take("ASTExpr"),
    Param(Any, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_id_to_ast_expr_every = ISLFunction.create(
    "isl_id_to_ast_expr_every",
    Keep("IdToAstExpr"),
    Param(None, ctype=c_void_p),
    Keep("ASTExpr"),
    Param(Any, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_id_to_ast_expr_set = ISLFunction.create(
    "isl_id_to_ast_expr_set",
    Take("IdToAstExpr"),
    Take("Id"),
    Take("ASTExpr"),
    return_=Give("IdToAstExpr"),
    lib=_lib,
)

_isl_id_to_ast_expr_drop = ISLFunction.create(
    "isl_id_to_ast_expr_drop",
    Take("IdToAstExpr"),
    Take("Id"),
    return_=Give("IdToAstExpr"),
    lib=_lib,
)

_isl_id_to_ast_expr_is_equal = ISLFunction.create(
    "isl_id_to_ast_expr_is_equal",
    Take("IdToAstExpr"),
    Take("IdToAstExpr"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_id_to_ast_expr_to_str = ISLFunction.create(
    "isl_id_to_ast_expr_to_str",
    Keep("IdToAstExpr"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_id_to_ast_expr_read_from_str = ISLFunction.create(
    "isl_id_to_ast_expr_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("IdToAstExpr"),
    lib=_lib,
)
