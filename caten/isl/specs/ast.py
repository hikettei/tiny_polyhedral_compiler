from __future__ import annotations

from ctypes import c_int, c_void_p

from ..ffi import FfiPointer, load_libisl
from ..func import ISLFunction
from ..obj import ISLObject
from ..qualifier import Give, Keep, Null, Param, Take
from .context import Context
from .id import Id
from .val import Val

_lib = load_libisl()


class AstExprList(ISLObject):
    __slots__ = ()

    @classmethod
    def alloc(cls, min_size: int = 0) -> "AstExprList":
        return _isl_ast_expr_list_alloc(min_size)

    @classmethod
    def from_expr(cls, expr: ASTExpr) -> "AstExprList":
        return _isl_ast_expr_list_from_ast_expr(expr)

    def add(self, expr: ASTExpr) -> "AstExprList":
        return _isl_ast_expr_list_add(self, expr)

    def n_ast_expr(self) -> int:
        return _isl_ast_expr_list_n_ast_expr(self)

    def get(self, pos: int) -> ASTExpr:
        return _isl_ast_expr_list_get_ast_expr(self, pos)

    def copy_handle(self) -> FfiPointer:
        return _isl_ast_expr_list_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_ast_expr_list_free(handle)


class AstNodeList(ISLObject):
    __slots__ = ()

    @classmethod
    def alloc(cls, ctx: Context, min_size: int = 0) -> "AstNodeList":
        return _isl_ast_node_list_alloc(ctx, min_size)

    @classmethod
    def from_node(cls, node: "ASTNode") -> "AstNodeList":
        return _isl_ast_node_list_from_ast_node(node)

    def add(self, node: "ASTNode") -> "AstNodeList":
        return _isl_ast_node_list_add(self, node)

    def n_ast_node(self) -> int:
        return _isl_ast_node_list_n_ast_node(self)

    def get(self, pos: int) -> "ASTNode":
        return _isl_ast_node_list_get_ast_node(self, pos)

    def copy_handle(self) -> FfiPointer:
        return _isl_ast_node_list_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_ast_node_list_free(handle)


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

    def op_type(self) -> int:
        return _isl_ast_expr_get_op_type(self)

    def op_n_arg(self) -> int:
        return _isl_ast_expr_get_op_n_arg(self)

    def op_arg(self, pos: int) -> "ASTExpr":
        return _isl_ast_expr_get_op_arg(self, pos)

    def id(self) -> Id:
        return _isl_ast_expr_get_id(self)

    def val(self) -> Val:
        return _isl_ast_expr_get_val(self)

    def access(self, indices: AstExprList) -> "ASTExpr":
        return _isl_ast_expr_access(self, indices)

    def call(self, args: AstExprList) -> "ASTExpr":
        return _isl_ast_expr_call(self, args)

    def substitute_ids(self, mapping: object) -> "ASTExpr":
        # TODO: id_to_ast_expr symbols are unavailable in the linked libisl.
        raise NotImplementedError("isl_id_to_ast_expr* symbols unavailable in linked libisl")

    def print_macros(self, printer: "Printer") -> "Printer":
        return _isl_ast_expr_print_macros(self, printer)

    def __str__(self) -> str:  # pragma: no cover
        return _isl_ast_expr_to_str(self)

    def __repr__(self) -> str:  # pragma: no cover
        return f"ASTExpr({self.__str__()})"

    def context(self) -> Context:
        return _isl_ast_expr_get_ctx(self)

    @classmethod
    def from_id(cls, id: Id) -> "ASTExpr":
        return _isl_ast_expr_from_id(id)

    def neg(self) -> "ASTExpr":
        return _isl_ast_expr_neg(self)

    def add(self, other: "ASTExpr") -> "ASTExpr":
        return _isl_ast_expr_add(self, other)

    def sub(self, other: "ASTExpr") -> "ASTExpr":
        return _isl_ast_expr_sub(self, other)

    def mul(self, other: "ASTExpr") -> "ASTExpr":
        return _isl_ast_expr_mul(self, other)

    def div(self, other: "ASTExpr") -> "ASTExpr":
        return _isl_ast_expr_div(self, other)

    def pdiv_q(self, other: "ASTExpr") -> "ASTExpr":
        return _isl_ast_expr_pdiv_q(self, other)

    def pdiv_r(self, other: "ASTExpr") -> "ASTExpr":
        return _isl_ast_expr_pdiv_r(self, other)

    def logical_and(self, other: "ASTExpr") -> "ASTExpr":
        return _isl_ast_expr_and(self, other)

    def logical_and_then(self, other: "ASTExpr") -> "ASTExpr":
        return _isl_ast_expr_and_then(self, other)

    def logical_or(self, other: "ASTExpr") -> "ASTExpr":
        return _isl_ast_expr_or(self, other)

    def logical_or_else(self, other: "ASTExpr") -> "ASTExpr":
        return _isl_ast_expr_or_else(self, other)

    def eq(self, other: "ASTExpr") -> "ASTExpr":
        return _isl_ast_expr_eq(self, other)

    def lt(self, other: "ASTExpr") -> "ASTExpr":
        return _isl_ast_expr_lt(self, other)

    def le(self, other: "ASTExpr") -> "ASTExpr":
        return _isl_ast_expr_le(self, other)

    def gt(self, other: "ASTExpr") -> "ASTExpr":
        return _isl_ast_expr_gt(self, other)

    def ge(self, other: "ASTExpr") -> "ASTExpr":
        return _isl_ast_expr_ge(self, other)

    def address_of(self) -> "ASTExpr":
        return _isl_ast_expr_address_of(self)

    def is_equal(self, other: "ASTExpr") -> bool:
        return _isl_ast_expr_is_equal(self, other)

    def to_c_str(self) -> str:
        return _isl_ast_expr_to_C_str(self)


class ASTOpExpr(ASTExpr):
    __slots__ = ()


class ASTIdExpr(ASTExpr):
    __slots__ = ()


class ASTIntExpr(ASTExpr):
    __slots__ = ()


class ASTOpAnd(ASTOpExpr):
    __slots__ = ()


class ASTOpAndThen(ASTOpExpr):
    __slots__ = ()


class ASTOpOr(ASTOpExpr):
    __slots__ = ()


class ASTOpOrElse(ASTOpExpr):
    __slots__ = ()


class ASTOpMax(ASTOpExpr):
    __slots__ = ()


class ASTOpMin(ASTOpExpr):
    __slots__ = ()


class ASTOpMinus(ASTOpExpr):
    __slots__ = ()


class ASTOpAdd(ASTOpExpr):
    __slots__ = ()


class ASTOpSub(ASTOpExpr):
    __slots__ = ()


class ASTOpMul(ASTOpExpr):
    __slots__ = ()


class ASTOpDiv(ASTOpExpr):
    __slots__ = ()


class ASTOpFDivQ(ASTOpExpr):
    __slots__ = ()


class ASTOpPDivQ(ASTOpExpr):
    __slots__ = ()


class ASTOpPDivR(ASTOpExpr):
    __slots__ = ()


class ASTOpZDivR(ASTOpExpr):
    __slots__ = ()


class ASTOpCond(ASTOpExpr):
    __slots__ = ()


class ASTOpSelect(ASTOpExpr):
    __slots__ = ()


class ASTOpEq(ASTOpExpr):
    __slots__ = ()


class ASTOpLe(ASTOpExpr):
    __slots__ = ()


class ASTOpLt(ASTOpExpr):
    __slots__ = ()


class ASTOpGe(ASTOpExpr):
    __slots__ = ()


class ASTOpGt(ASTOpExpr):
    __slots__ = ()


class ASTOpCall(ASTOpExpr):
    __slots__ = ()


class ASTOpAccess(ASTOpExpr):
    __slots__ = ()


class ASTOpMember(ASTOpExpr):
    __slots__ = ()


class ASTOpAddressOf(ASTOpExpr):
    __slots__ = ()


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

    def get_type(self) -> int:
        return _isl_ast_node_get_type(self)

    def to_c_str(self) -> str:
        return _isl_ast_node_to_C_str(self)

    def for_iterator(self) -> ASTExpr:
        return _isl_ast_node_for_get_iterator(self)

    def for_init(self) -> ASTExpr:
        return _isl_ast_node_for_get_init(self)

    def for_cond(self) -> ASTExpr:
        return _isl_ast_node_for_get_cond(self)

    def for_inc(self) -> ASTExpr:
        return _isl_ast_node_for_get_inc(self)

    def for_body(self) -> "ASTNode":
        return _isl_ast_node_for_get_body(self)

    def for_is_degenerate(self) -> bool:
        return _isl_ast_node_for_is_degenerate(self)

    def if_cond(self) -> ASTExpr:
        return _isl_ast_node_if_get_cond(self)

    def if_then(self) -> "ASTNode":
        return _isl_ast_node_if_get_then_node(self)

    def if_has_else(self) -> bool:
        return _isl_ast_node_if_has_else_node(self)

    def if_else(self) -> "ASTNode":
        return _isl_ast_node_if_get_else_node(self)

    def mark_id(self) -> Id:
        return _isl_ast_node_mark_get_id(self)

    def mark_node(self) -> "ASTNode":
        return _isl_ast_node_mark_get_node(self)

    def user_expr(self) -> ASTExpr:
        return _isl_ast_node_user_get_expr(self)

    def context(self) -> Context:
        return _isl_ast_node_get_ctx(self)

    def set_annotation(self, annotation: Id) -> "ASTNode":
        return _isl_ast_node_set_annotation(self, annotation)

    def annotation(self) -> Id:
        return _isl_ast_node_get_annotation(self)

    def print(self, printer: "Printer", options: "AstPrintOptions") -> "Printer":
        return _isl_ast_node_print(self, printer, options)

    def print_macros(self, printer: "Printer") -> "Printer":
        return _isl_ast_node_print_macros(self, printer)

    def for_print(self, printer: "Printer", options: "AstPrintOptions") -> "Printer":
        return _isl_ast_node_for_print(self, printer, options)

    def if_print(self, printer: "Printer", options: "AstPrintOptions") -> "Printer":
        return _isl_ast_node_if_print(self, printer, options)

    def foreach_descendant_top_down(self, fn: object) -> None:
        # TODO: callback bridge not implemented yet.
        raise NotImplementedError("foreach_descendant_top_down callback bridge not implemented")

    def map_descendant_bottom_up(self, fn: object) -> "ASTNode":
        # TODO: callback bridge not implemented yet.
        raise NotImplementedError("map_descendant_bottom_up callback bridge not implemented")

    def foreach_ast_expr_op_type(self, fn: object) -> None:
        # TODO: callback bridge not implemented yet.
        raise NotImplementedError("foreach_ast_expr_op_type callback bridge not implemented")

    def foreach_ast_op_type(self, fn: object) -> None:
        # TODO: callback bridge not implemented yet.
        raise NotImplementedError("foreach_ast_op_type callback bridge not implemented")


class ASTForNode(ASTNode):
    __slots__ = ()


class ASTIfNode(ASTNode):
    __slots__ = ()

    def has_else(self) -> bool:
        return _isl_ast_node_if_has_else(self)

    def then_node(self) -> "ASTNode":
        return _isl_ast_node_if_get_then(self)

    def else_node(self) -> "ASTNode":
        return _isl_ast_node_if_get_else(self)


class ASTBlockNode(ASTNode):
    __slots__ = ()

    @classmethod
    def from_children(cls, nodes: AstNodeList) -> "ASTBlockNode":
        return _isl_ast_node_block_from_children(nodes)

    def children(self) -> AstNodeList:
        return _isl_ast_node_block_get_children(self)


class ASTMarkNode(ASTNode):
    __slots__ = ()


class ASTUserNode(ASTNode):
    __slots__ = ()

    @classmethod
    def from_expr(cls, expr: ASTExpr) -> "ASTUserNode":
        return _isl_ast_node_user_from_expr(expr)


class ASTBuild(ISLObject):
    __slots__ = ()

    def copy_handle(self) -> FfiPointer:
        return _isl_ast_build_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_ast_build_free(handle)


class Printer(ISLObject):
    """Minimal wrapper for ``isl_printer`` used in AST printing."""

    __slots__ = ()

    @classmethod
    def to_str(cls, ctx: Context) -> "Printer":
        return _isl_printer_to_str(ctx)

    def copy_handle(self) -> FfiPointer:
        # isl_printer_copy symbol is absent in this lib; copying is unsupported.
        raise NotImplementedError("isl_printer_copy not available")

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_printer_free(handle)

    def get_str(self) -> str:
        return _isl_printer_get_str(self)

    def set_output_format(self, fmt: int) -> "Printer":
        return _isl_printer_set_output_format(self, fmt)

    def print_ast_expr(self, expr: ASTExpr) -> "Printer":
        return _isl_printer_print_ast_expr(self, expr)

    def print_ast_node(self, node: ASTNode) -> "Printer":
        return _isl_printer_print_ast_node(self, node)


class AstPrintOptions(ISLObject):
    __slots__ = ()

    @classmethod
    def alloc(cls, ctx: Context) -> "AstPrintOptions":
        return _isl_ast_print_options_alloc(ctx)

    def copy_handle(self) -> FfiPointer:
        return _isl_ast_print_options_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_ast_print_options_free(handle)

    def set_print_user(self, fn: object) -> "AstPrintOptions":
        # TODO: callback bridge not implemented.
        raise NotImplementedError("ast_print_options_set_print_user not implemented")

    def set_print_for(self, fn: object) -> "AstPrintOptions":
        # TODO: callback bridge not implemented.
        raise NotImplementedError("ast_print_options_set_print_for not implemented")


_isl_ast_expr_from_val = ISLFunction.create(
    _lib.isl_ast_expr_from_val,
    Take(Val),
    return_=Give(ASTExpr),
    lib=_lib,
)

_isl_ast_expr_list_alloc = ISLFunction.create(
    _lib.isl_ast_expr_list_alloc,
    Context(),
    Param(int, ctype=c_int),
    return_=Give(AstExprList),
    lib=_lib,
)

_isl_ast_expr_list_from_ast_expr = ISLFunction.create(
    _lib.isl_ast_expr_list_from_ast_expr,
    Take(ASTExpr),
    return_=Give(AstExprList),
    lib=_lib,
)

_isl_ast_expr_list_copy = ISLFunction.create(
    _lib.isl_ast_expr_list_copy,
    Keep(AstExprList),
    return_=Give(AstExprList),
    lib=_lib,
)

_isl_ast_expr_list_free = ISLFunction.create(
    _lib.isl_ast_expr_list_free,
    Take(AstExprList),
    return_=Null(),
    lib=_lib,
)

_isl_ast_expr_list_add = ISLFunction.create(
    _lib.isl_ast_expr_list_add,
    Take(AstExprList),
    Take(ASTExpr),
    return_=Give(AstExprList),
    lib=_lib,
)

_isl_ast_expr_list_n_ast_expr = ISLFunction.create(
    _lib.isl_ast_expr_list_n_ast_expr,
    Keep(AstExprList),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_ast_expr_list_get_ast_expr = ISLFunction.create(
    _lib.isl_ast_expr_list_get_ast_expr,
    Keep(AstExprList),
    Param(int, ctype=c_int),
    return_=Give(ASTExpr),
    lib=_lib,
)

_isl_ast_node_list_alloc = ISLFunction.create(
    _lib.isl_ast_node_list_alloc,
    Context(),
    Param(int, ctype=c_int),
    return_=Give(AstNodeList),
    lib=_lib,
)

_isl_ast_node_list_from_ast_node = ISLFunction.create(
    _lib.isl_ast_node_list_from_ast_node,
    Take(ASTNode),
    return_=Give(AstNodeList),
    lib=_lib,
)

_isl_ast_node_list_copy = ISLFunction.create(
    _lib.isl_ast_node_list_copy,
    Keep(AstNodeList),
    return_=Give(AstNodeList),
    lib=_lib,
)

_isl_ast_node_list_free = ISLFunction.create(
    _lib.isl_ast_node_list_free,
    Take(AstNodeList),
    return_=Null(),
    lib=_lib,
)

_isl_ast_node_list_add = ISLFunction.create(
    _lib.isl_ast_node_list_add,
    Take(AstNodeList),
    Take(ASTNode),
    return_=Give(AstNodeList),
    lib=_lib,
)

_isl_ast_node_list_n_ast_node = ISLFunction.create(
    _lib.isl_ast_node_list_n_ast_node,
    Keep(AstNodeList),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_ast_node_list_get_ast_node = ISLFunction.create(
    _lib.isl_ast_node_list_get_ast_node,
    Keep(AstNodeList),
    Param(int, ctype=c_int),
    return_=Give(ASTNode),
    lib=_lib,
)

_isl_ast_node_block_from_children = ISLFunction.create(
    _lib.isl_ast_node_block_from_children,
    Take(AstNodeList),
    return_=Give(ASTNode),
    lib=_lib,
)

_isl_ast_node_block_get_children = ISLFunction.create(
    _lib.isl_ast_node_block_get_children,
    Keep(ASTNode),
    return_=Give(AstNodeList),
    lib=_lib,
)

_isl_ast_expr_access = ISLFunction.create(
    _lib.isl_ast_expr_access,
    Take(ASTExpr),
    Take(AstExprList),
    return_=Give(ASTExpr),
    lib=_lib,
)

_isl_ast_expr_call = ISLFunction.create(
    _lib.isl_ast_expr_call,
    Take(ASTExpr),
    Take(AstExprList),
    return_=Give(ASTExpr),
    lib=_lib,
)

_isl_ast_expr_from_id = ISLFunction.create(
    _lib.isl_ast_expr_from_id,
    Take(Id),
    return_=Give(ASTExpr),
    lib=_lib,
)

_isl_ast_expr_get_ctx = ISLFunction.create(
    _lib.isl_ast_expr_get_ctx,
    Keep(ASTExpr),
    return_=Keep(Context),
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

_isl_ast_expr_get_op_type = ISLFunction.create(
    _lib.isl_ast_expr_get_op_type,
    Keep(ASTExpr),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_ast_expr_op_get_type = ISLFunction.create(
    _lib.isl_ast_expr_op_get_type,
    Keep(ASTExpr),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_ast_expr_op_get_n_arg = ISLFunction.create(
    _lib.isl_ast_expr_op_get_n_arg,
    Keep(ASTExpr),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_ast_expr_get_op_n_arg = ISLFunction.create(
    _lib.isl_ast_expr_get_op_n_arg,
    Keep(ASTExpr),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_ast_expr_op_get_arg = ISLFunction.create(
    _lib.isl_ast_expr_op_get_arg,
    Keep(ASTExpr),
    Param(int, ctype=c_int),
    return_=Give(ASTExpr),
    lib=_lib,
)

_isl_ast_expr_get_op_arg = ISLFunction.create(
    _lib.isl_ast_expr_get_op_arg,
    Keep(ASTExpr),
    Param(int, ctype=c_int),
    return_=Give(ASTExpr),
    lib=_lib,
)

_isl_ast_expr_id_get_id = ISLFunction.create(
    _lib.isl_ast_expr_id_get_id,
    Keep(ASTExpr),
    return_=Give(Id),
    lib=_lib,
)

_isl_ast_expr_get_id = ISLFunction.create(
    _lib.isl_ast_expr_get_id,
    Keep(ASTExpr),
    return_=Give(Id),
    lib=_lib,
)

_isl_ast_expr_int_get_val = ISLFunction.create(
    _lib.isl_ast_expr_int_get_val,
    Keep(ASTExpr),
    return_=Give(Val),
    lib=_lib,
)

_isl_ast_expr_get_val = ISLFunction.create(
    _lib.isl_ast_expr_get_val,
    Keep(ASTExpr),
    return_=Give(Val),
    lib=_lib,
)

_isl_ast_expr_neg = ISLFunction.create(
    _lib.isl_ast_expr_neg,
    Take(ASTExpr),
    return_=Give(ASTExpr),
    lib=_lib,
)

_isl_ast_expr_add = ISLFunction.create(
    _lib.isl_ast_expr_add,
    Take(ASTExpr),
    Take(ASTExpr),
    return_=Give(ASTExpr),
    lib=_lib,
)

_isl_ast_expr_sub = ISLFunction.create(
    _lib.isl_ast_expr_sub,
    Take(ASTExpr),
    Take(ASTExpr),
    return_=Give(ASTExpr),
    lib=_lib,
)

_isl_ast_expr_mul = ISLFunction.create(
    _lib.isl_ast_expr_mul,
    Take(ASTExpr),
    Take(ASTExpr),
    return_=Give(ASTExpr),
    lib=_lib,
)

_isl_ast_expr_div = ISLFunction.create(
    _lib.isl_ast_expr_div,
    Take(ASTExpr),
    Take(ASTExpr),
    return_=Give(ASTExpr),
    lib=_lib,
)

_isl_ast_expr_pdiv_q = ISLFunction.create(
    _lib.isl_ast_expr_pdiv_q,
    Take(ASTExpr),
    Take(ASTExpr),
    return_=Give(ASTExpr),
    lib=_lib,
)

_isl_ast_expr_pdiv_r = ISLFunction.create(
    _lib.isl_ast_expr_pdiv_r,
    Take(ASTExpr),
    Take(ASTExpr),
    return_=Give(ASTExpr),
    lib=_lib,
)

_isl_ast_expr_and = ISLFunction.create(
    _lib.isl_ast_expr_and,
    Take(ASTExpr),
    Take(ASTExpr),
    return_=Give(ASTExpr),
    lib=_lib,
)

_isl_ast_expr_and_then = ISLFunction.create(
    _lib.isl_ast_expr_and_then,
    Take(ASTExpr),
    Take(ASTExpr),
    return_=Give(ASTExpr),
    lib=_lib,
)

_isl_ast_expr_or = ISLFunction.create(
    _lib.isl_ast_expr_or,
    Take(ASTExpr),
    Take(ASTExpr),
    return_=Give(ASTExpr),
    lib=_lib,
)

_isl_ast_expr_or_else = ISLFunction.create(
    _lib.isl_ast_expr_or_else,
    Take(ASTExpr),
    Take(ASTExpr),
    return_=Give(ASTExpr),
    lib=_lib,
)

_isl_ast_expr_eq = ISLFunction.create(
    _lib.isl_ast_expr_eq,
    Take(ASTExpr),
    Take(ASTExpr),
    return_=Give(ASTExpr),
    lib=_lib,
)

_isl_ast_expr_lt = ISLFunction.create(
    _lib.isl_ast_expr_lt,
    Take(ASTExpr),
    Take(ASTExpr),
    return_=Give(ASTExpr),
    lib=_lib,
)

_isl_ast_expr_le = ISLFunction.create(
    _lib.isl_ast_expr_le,
    Take(ASTExpr),
    Take(ASTExpr),
    return_=Give(ASTExpr),
    lib=_lib,
)

_isl_ast_expr_gt = ISLFunction.create(
    _lib.isl_ast_expr_gt,
    Take(ASTExpr),
    Take(ASTExpr),
    return_=Give(ASTExpr),
    lib=_lib,
)

_isl_ast_expr_ge = ISLFunction.create(
    _lib.isl_ast_expr_ge,
    Take(ASTExpr),
    Take(ASTExpr),
    return_=Give(ASTExpr),
    lib=_lib,
)

_isl_ast_expr_address_of = ISLFunction.create(
    _lib.isl_ast_expr_address_of,
    Take(ASTExpr),
    return_=Give(ASTExpr),
    lib=_lib,
)

_isl_ast_expr_is_equal = ISLFunction.create(
    _lib.isl_ast_expr_is_equal,
    Keep(ASTExpr),
    Keep(ASTExpr),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_ast_expr_to_C_str = ISLFunction.create(
    _lib.isl_ast_expr_to_C_str,
    Keep(ASTExpr),
    return_=Param(str),
    lib=_lib,
)

_isl_ast_expr_set_op_arg = ISLFunction.create(
    _lib.isl_ast_expr_set_op_arg,
    Take(ASTExpr),
    Param(int, ctype=c_int),
    Take(ASTExpr),
    return_=Give(ASTExpr),
    lib=_lib,
)

_isl_ast_expr_print_macros = ISLFunction.create(
    _lib.isl_ast_expr_print_macros,
    Take(ASTExpr),
    Take(lambda: Printer),  # type: ignore[arg-type]
    return_=Give(lambda: Printer),  # type: ignore[arg-type]
    lib=_lib,
)

_isl_ast_expr_op_type_print_macro = ISLFunction.create(
    _lib.isl_ast_expr_op_type_print_macro,
    Param(int, ctype=c_int),
    Take(lambda: Printer),  # type: ignore[arg-type]
    return_=Give(lambda: Printer),  # type: ignore[arg-type]
    lib=_lib,
)

_isl_ast_expr_op_type_set_print_name = ISLFunction.create(
    _lib.isl_ast_expr_op_type_set_print_name,
    Take(lambda: Printer),  # type: ignore[arg-type]
    Param(int, ctype=c_int),
    Param(str),
    return_=Give(lambda: Printer),  # type: ignore[arg-type]
    lib=_lib,
)

# printer bindings
_isl_printer_to_str = ISLFunction.create(
    _lib.isl_printer_to_str,
    Context(),
    return_=Give(lambda: Printer),  # type: ignore[arg-type]
    lib=_lib,
)

_isl_printer_free = ISLFunction.create(
    _lib.isl_printer_free,
    Take(lambda: Printer),  # type: ignore[arg-type]
    return_=Null(),
    lib=_lib,
)

_isl_printer_get_str = ISLFunction.create(
    _lib.isl_printer_get_str,
    Keep(lambda: Printer),  # type: ignore[arg-type]
    return_=Param(str),
    lib=_lib,
)

_isl_printer_set_output_format = ISLFunction.create(
    _lib.isl_printer_set_output_format,
    Take(lambda: Printer),  # type: ignore[arg-type]
    Param(int, ctype=c_int),
    return_=Give(lambda: Printer),  # type: ignore[arg-type]
    lib=_lib,
)

_isl_printer_print_ast_expr = ISLFunction.create(
    _lib.isl_printer_print_ast_expr,
    Take(lambda: Printer),  # type: ignore[arg-type]
    Keep(ASTExpr),
    return_=Give(lambda: Printer),  # type: ignore[arg-type]
    lib=_lib,
)

_isl_printer_print_ast_node = ISLFunction.create(
    _lib.isl_printer_print_ast_node,
    Take(lambda: Printer),  # type: ignore[arg-type]
    Keep(ASTNode),
    return_=Give(lambda: Printer),  # type: ignore[arg-type]
    lib=_lib,
)

_isl_ast_print_options_alloc = ISLFunction.create(
    _lib.isl_ast_print_options_alloc,
    Context(),
    return_=Give(lambda: AstPrintOptions),  # type: ignore[arg-type]
    lib=_lib,
)

_isl_ast_print_options_copy = ISLFunction.create(
    _lib.isl_ast_print_options_copy,
    Keep(lambda: AstPrintOptions),  # type: ignore[arg-type]
    return_=Give(lambda: AstPrintOptions),  # type: ignore[arg-type]
    lib=_lib,
)

_isl_ast_print_options_free = ISLFunction.create(
    _lib.isl_ast_print_options_free,
    Take(lambda: AstPrintOptions),  # type: ignore[arg-type]
    return_=Null(),
    lib=_lib,
)

_isl_ast_node_if_has_else = ISLFunction.create(
    _lib.isl_ast_node_if_has_else,
    Keep(ASTNode),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_ast_node_if_get_then = ISLFunction.create(
    _lib.isl_ast_node_if_get_then,
    Keep(ASTNode),
    return_=Give(ASTNode),
    lib=_lib,
)

_isl_ast_node_if_get_else = ISLFunction.create(
    _lib.isl_ast_node_if_get_else,
    Keep(ASTNode),
    return_=Give(ASTNode),
    lib=_lib,
)

_isl_ast_node_alloc_user = ISLFunction.create(
    _lib.isl_ast_node_alloc_user,
    Take(ASTExpr),
    return_=Give(ASTNode),
    lib=_lib,
)

_isl_ast_node_get_annotation = ISLFunction.create(
    _lib.isl_ast_node_get_annotation,
    Keep(ASTNode),
    return_=Give(Id),
    lib=_lib,
)

_isl_ast_node_set_annotation = ISLFunction.create(
    _lib.isl_ast_node_set_annotation,
    Take(ASTNode),
    Take(Id),
    return_=Give(ASTNode),
    lib=_lib,
)

_isl_ast_node_print = ISLFunction.create(
    _lib.isl_ast_node_print,
    Keep(ASTNode),
    Take(lambda: Printer),  # type: ignore[arg-type]
    Take(lambda: AstPrintOptions),  # type: ignore[arg-type]
    return_=Give(lambda: Printer),  # type: ignore[arg-type]
    lib=_lib,
)

_isl_ast_node_print_macros = ISLFunction.create(
    _lib.isl_ast_node_print_macros,
    Keep(ASTNode),
    Take(lambda: Printer),  # type: ignore[arg-type]
    return_=Give(lambda: Printer),  # type: ignore[arg-type]
    lib=_lib,
)

_isl_ast_node_for_print = ISLFunction.create(
    _lib.isl_ast_node_for_print,
    Keep(ASTNode),
    Take(lambda: Printer),  # type: ignore[arg-type]
    Take(lambda: AstPrintOptions),  # type: ignore[arg-type]
    return_=Give(lambda: Printer),  # type: ignore[arg-type]
    lib=_lib,
)

_isl_ast_node_if_print = ISLFunction.create(
    _lib.isl_ast_node_if_print,
    Keep(ASTNode),
    Take(lambda: Printer),  # type: ignore[arg-type]
    Take(lambda: AstPrintOptions),  # type: ignore[arg-type]
    return_=Give(lambda: Printer),  # type: ignore[arg-type]
    lib=_lib,
)

_isl_ast_node_user_from_expr = ISLFunction.create(
    _lib.isl_ast_node_user_from_expr,
    Take(ASTExpr),
    return_=Give(ASTNode),
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

_isl_ast_node_get_ctx = ISLFunction.create(
    _lib.isl_ast_node_get_ctx,
    Keep(ASTNode),
    return_=Keep(Context),
    lib=_lib,
)

_isl_ast_node_free = ISLFunction.create(
    _lib.isl_ast_node_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

_isl_ast_node_get_type = ISLFunction.create(
    _lib.isl_ast_node_get_type,
    Keep(ASTNode),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_ast_node_to_str = ISLFunction.create(
    _lib.isl_ast_node_to_str,
    Keep(ASTNode),
    return_=Param(str),
    lib=_lib,
)

_isl_ast_node_to_C_str = ISLFunction.create(
    _lib.isl_ast_node_to_C_str,
    Keep(ASTNode),
    return_=Param(str),
    lib=_lib,
)

_isl_ast_node_for_get_iterator = ISLFunction.create(
    _lib.isl_ast_node_for_get_iterator,
    Keep(ASTNode),
    return_=Give(ASTExpr),
    lib=_lib,
)

_isl_ast_node_for_get_init = ISLFunction.create(
    _lib.isl_ast_node_for_get_init,
    Keep(ASTNode),
    return_=Give(ASTExpr),
    lib=_lib,
)

_isl_ast_node_for_get_cond = ISLFunction.create(
    _lib.isl_ast_node_for_get_cond,
    Keep(ASTNode),
    return_=Give(ASTExpr),
    lib=_lib,
)

_isl_ast_node_for_get_inc = ISLFunction.create(
    _lib.isl_ast_node_for_get_inc,
    Keep(ASTNode),
    return_=Give(ASTExpr),
    lib=_lib,
)

_isl_ast_node_for_get_body = ISLFunction.create(
    _lib.isl_ast_node_for_get_body,
    Keep(ASTNode),
    return_=Give(ASTNode),
    lib=_lib,
)

_isl_ast_node_for_is_degenerate = ISLFunction.create(
    _lib.isl_ast_node_for_is_degenerate,
    Keep(ASTNode),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_ast_node_if_get_cond = ISLFunction.create(
    _lib.isl_ast_node_if_get_cond,
    Keep(ASTNode),
    return_=Give(ASTExpr),
    lib=_lib,
)

_isl_ast_node_if_get_then_node = ISLFunction.create(
    _lib.isl_ast_node_if_get_then_node,
    Keep(ASTNode),
    return_=Give(ASTNode),
    lib=_lib,
)

_isl_ast_node_if_has_else_node = ISLFunction.create(
    _lib.isl_ast_node_if_has_else_node,
    Keep(ASTNode),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_ast_node_if_get_else_node = ISLFunction.create(
    _lib.isl_ast_node_if_get_else_node,
    Keep(ASTNode),
    return_=Give(ASTNode),
    lib=_lib,
)

_isl_ast_node_mark_get_id = ISLFunction.create(
    _lib.isl_ast_node_mark_get_id,
    Keep(ASTNode),
    return_=Give(Id),
    lib=_lib,
)

_isl_ast_node_mark_get_node = ISLFunction.create(
    _lib.isl_ast_node_mark_get_node,
    Keep(ASTNode),
    return_=Give(ASTNode),
    lib=_lib,
)

_isl_ast_node_user_get_expr = ISLFunction.create(
    _lib.isl_ast_node_user_get_expr,
    Keep(ASTNode),
    return_=Give(ASTExpr),
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

__all__ = [
    "ASTExpr",
    "ASTOpExpr",
    "ASTIdExpr",
    "ASTIntExpr",
    "ASTOpAnd",
    "ASTOpAndThen",
    "ASTOpOr",
    "ASTOpOrElse",
    "ASTOpMax",
    "ASTOpMin",
    "ASTOpMinus",
    "ASTOpAdd",
    "ASTOpSub",
    "ASTOpMul",
    "ASTOpDiv",
    "ASTOpFDivQ",
    "ASTOpPDivQ",
    "ASTOpPDivR",
    "ASTOpZDivR",
    "ASTOpCond",
    "ASTOpSelect",
    "ASTOpEq",
    "ASTOpLe",
    "ASTOpLt",
    "ASTOpGe",
    "ASTOpGt",
    "ASTOpCall",
    "ASTOpAccess",
    "ASTOpMember",
    "ASTOpAddressOf",
    "AstExprList",
    "AstNodeList",
    "Printer",
    "AstPrintOptions",
    "ASTNode",
    "ASTForNode",
    "ASTIfNode",
    "ASTBlockNode",
    "ASTMarkNode",
    "ASTUserNode",
    "ASTBuild",
]
