from __future__ import annotations

from ctypes import (
    c_char_p,
)
from typing import TYPE_CHECKING, Any

from ..ffi import load_libisl
from ..func import ISLFunction
from ..mixin import ISLObjectMixin
from ..obj import ISLObject
from ..qualifier import Give, Param, Take
from ..registry import register_type
from .context import Context

if TYPE_CHECKING:
    from .context import Context

_lib = load_libisl()

class AstNodeList(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_ast_node_list_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_ast_node_list_read_from_str(spec)



    @classmethod
    def from_node(cls, node: "ASTNode") -> "AstNodeList":
        return _isl_ast_node_list_from_ast_node(node)


register_type("AstNodeList", AstNodeList)

_isl_ast_node_list_read_from_str = ISLFunction.create(
    "isl_ast_node_list_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("AstNodeList"),
    lib=_lib,
)

_isl_ast_node_list_from_ast_node = ISLFunction.create(
    "isl_ast_node_list_from_ast_node",
    Take("ASTNode"),
    return_=Give("AstNodeList"),
    lib=_lib,
)
