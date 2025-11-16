from __future__ import annotations

from ctypes import c_int, c_void_p

from ..ffi import FfiPointer, load_libisl
from ..func import ISLFunction
from ..obj import ISLObject
from ..qualifier import Give, Keep, Null, Param, Take
from .val import Val

_lib = load_libisl()


class ASTExpr(ISLObject):
    __slots__ = ()

    @classmethod
    def from_val(cls, val: Val) -> "ASTExpr":
        return _isl_ast_expr_from_val(val)

    def copy_handle(self) -> FfiPointer:
        return _isl_ast_expr_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_ast_expr_free(handle)

    def get_type(self) -> int:
        return _isl_ast_expr_get_type(self)

    def __str__(self) -> str:  # pragma: no cover
        return _isl_ast_expr_to_str(self)

    def __repr__(self) -> str:  # pragma: no cover
        return f"ASTExpr({self.__str__()})"


class ASTNode(ISLObject):
    __slots__ = ()

    def copy_handle(self) -> FfiPointer:
        return _isl_ast_node_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_ast_node_free(handle)

    def __str__(self) -> str:  # pragma: no cover
        return _isl_ast_node_to_str(self)

    def __repr__(self) -> str:  # pragma: no cover
        return f"ASTNode({self.__str__()})"


class ASTBuild(ISLObject):
    __slots__ = ()

    def copy_handle(self) -> FfiPointer:
        return _isl_ast_build_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_ast_build_free(handle)


_isl_ast_expr_from_val = ISLFunction.create(
    _lib.isl_ast_expr_from_val,
    Take(Val),
    return_=Give(ASTExpr),
    lib=_lib,
)

_isl_ast_expr_copy = ISLFunction.create(
    _lib.isl_ast_expr_copy,
    Keep(ASTExpr),
    return_=Give(ASTExpr),
    lib=_lib,
)

_isl_ast_expr_free = ISLFunction.create(
    _lib.isl_ast_expr_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

_isl_ast_expr_get_type = ISLFunction.create(
    _lib.isl_ast_expr_get_type,
    Keep(ASTExpr),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_ast_expr_to_str = ISLFunction.create(
    _lib.isl_ast_expr_to_str,
    Keep(ASTExpr),
    return_=Param(str),
    lib=_lib,
)

_isl_ast_node_copy = ISLFunction.create(
    _lib.isl_ast_node_copy,
    Keep(ASTNode),
    return_=Give(ASTNode),
    lib=_lib,
)

_isl_ast_node_free = ISLFunction.create(
    _lib.isl_ast_node_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

_isl_ast_node_to_str = ISLFunction.create(
    _lib.isl_ast_node_to_str,
    Keep(ASTNode),
    return_=Param(str),
    lib=_lib,
)

_isl_ast_build_copy = ISLFunction.create(
    _lib.isl_ast_build_copy,
    Keep(ASTBuild),
    return_=Give(ASTBuild),
    lib=_lib,
)

_isl_ast_build_free = ISLFunction.create(
    _lib.isl_ast_build_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

__all__ = ["ASTExpr", "ASTNode", "ASTBuild"]
