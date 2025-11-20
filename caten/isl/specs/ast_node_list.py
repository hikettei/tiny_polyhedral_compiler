from __future__ import annotations

from typing import TYPE_CHECKING, Any

from ..ffi import load_libisl
from ..func import ISLFunction
from ..mixin import ISLObjectMixin
from ..obj import ISLObject
from ..qualifier import Give, Take
from ..registry import register_type

if TYPE_CHECKING:
    from .ast_node import ASTNode

_lib = load_libisl()

class AstNodeList(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle: Any = None) -> None:
        super().__init__(handle)



    @classmethod
    def from_node(cls, node: "ASTNode") -> "AstNodeList":
        return _isl_ast_node_list_from_ast_node(node)


register_type("AstNodeList", AstNodeList)

_isl_ast_node_list_from_ast_node = ISLFunction.create(
    "isl_ast_node_list_from_ast_node",
    Take("ASTNode"),
    return_=Give("AstNodeList"),
    lib=_lib,
)
