from __future__ import annotations

from ctypes import c_char_p, c_void_p
from typing import TYPE_CHECKING, Any

from ..ffi import load_libisl
from ..func import ISLFunction
from ..mixin import ISLObjectMixin
from ..obj import ISLObject
from ..qualifier import Give, Keep, Param, Take
from ..registry import register_type
from .context import Context

if TYPE_CHECKING:
    from .ast_node import ASTNode
    from .context import Context

_lib = load_libisl()

class ASTPrintOptions(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_ast_print_options_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_ast_print_options_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_ast_print_options_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_ast_print_options_free(handle)

    @classmethod
    def alloc(cls) -> "ASTPrintOptions":
        return _isl_ast_print_options_alloc()

    def set_print_user(self, print_user: Any, options: "ASTPrintOptions", node: "ASTNode", user: Any, user_: Any = None) -> "ASTPrintOptions":
        return _isl_ast_print_options_set_print_user(self, print_user, options, node, user, user_)

    def set_print_for(self, print_for: Any, options: "ASTPrintOptions", node: "ASTNode", user: Any, user_: Any = None) -> "ASTPrintOptions":
        return _isl_ast_print_options_set_print_for(self, print_for, options, node, user, user_)


register_type("ASTPrintOptions", ASTPrintOptions)

_isl_ast_print_options_alloc = ISLFunction.create(
    "isl_ast_print_options_alloc",
    Context(),
    return_=Give("ASTPrintOptions"),
    lib=_lib,
)

_isl_ast_print_options_copy = ISLFunction.create(
    "isl_ast_print_options_copy",
    Keep("ASTPrintOptions"),
    return_=Give("ASTPrintOptions"),
    lib=_lib,
)

_isl_ast_print_options_free = ISLFunction.create(
    "isl_ast_print_options_free",
    Take("ASTPrintOptions"),
    return_=Give("ASTPrintOptions"),
    lib=_lib,
)

_isl_ast_print_options_set_print_user = ISLFunction.create(
    "isl_ast_print_options_set_print_user",
    Take("ASTPrintOptions"),
    Param(None, ctype=c_void_p),
    Take("ASTPrintOptions"),
    Keep("ASTNode"),
    Param(Any, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    return_=Give("ASTPrintOptions"),
    lib=_lib,
)

_isl_ast_print_options_set_print_for = ISLFunction.create(
    "isl_ast_print_options_set_print_for",
    Take("ASTPrintOptions"),
    Param(None, ctype=c_void_p),
    Take("ASTPrintOptions"),
    Keep("ASTNode"),
    Param(Any, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    return_=Give("ASTPrintOptions"),
    lib=_lib,
)

_isl_ast_print_options_read_from_str = ISLFunction.create(
    "isl_ast_print_options_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("ASTPrintOptions"),
    lib=_lib,
)
