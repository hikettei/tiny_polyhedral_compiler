from __future__ import annotations

from ctypes import c_int
from typing import TYPE_CHECKING, Any

from ..ffi import load_libisl
from ..func import ISLFunction
from ..mixin import ISLObjectMixin
from ..obj import ISLObject
from ..qualifier import Give, Keep, Param, Take
from ..registry import register_type

if TYPE_CHECKING:
    from .ast_node import ASTNode

_lib = load_libisl()

class AstNodeList(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any = None) -> None:
        super().__init__(handle_or_spec)

    def copy_handle(self) -> Any:
        raise NotImplementedError(f"{type(self).__name__} does not support copy.")

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_ast_node_list_free(handle)

    @classmethod
    def from_node(cls, node: "ASTNode") -> "AstNodeList":
        return _isl_ast_node_list_from_ast_node(node)

    def n_ast_node(self) -> int:
        return _isl_ast_node_list_n_ast_node(self)

    def get_ast_node(self, index: int) -> "ASTNode":
        return _isl_ast_node_list_get_ast_node(self, index)


register_type("AstNodeList", AstNodeList)

_isl_ast_node_list_free = ISLFunction.create(
    "isl_ast_node_list_free",
    Take("AstNodeList"),
    return_=Give("AstNodeList"),
    lib=_lib,
)

_isl_ast_node_list_from_ast_node = ISLFunction.create(
    "isl_ast_node_list_from_ast_node",
    Take("ASTNode"),
    return_=Give("AstNodeList"),
    lib=_lib,
)

_isl_ast_node_list_n_ast_node = ISLFunction.create(
    "isl_ast_node_list_n_ast_node",
    Keep("AstNodeList"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_ast_node_list_get_ast_node = ISLFunction.create(
    "isl_ast_node_list_get_ast_node",
    Keep("AstNodeList"),
    Param(int, ctype=c_int),
    return_=Give("ASTNode"),
    lib=_lib,
)