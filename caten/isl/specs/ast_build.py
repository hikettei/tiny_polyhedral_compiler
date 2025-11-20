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
    from .ast_expr import ASTExpr
    from .ast_node import ASTNode
    from .context import Context
    from .id_list import IdList
    from .multi_pw_aff import MultiPwAff
    from .pw_aff import PwAff
    from .pw_multi_aff import PwMultiAff
    from .schedule import Schedule
    from .set import Set
    from .space import Space
    from .union_map import UnionMap

_lib = load_libisl()

class ASTBuild(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_ast_build_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_ast_build_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_ast_build_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_ast_build_free(handle)

    def get_ctx(self) -> "Context":
        return _isl_ast_build_get_ctx(self)

    @classmethod
    def alloc(cls) -> "ASTBuild":
        return _isl_ast_build_alloc()

    @classmethod
    def from_context(cls, set: "Set") -> "ASTBuild":
        return _isl_ast_build_from_context(set)

    def node_from_schedule(self, schedule: "Schedule") -> "ASTNode":
        return _isl_ast_build_node_from_schedule(self, schedule)

    def node_from_schedule_map(self, schedule: "UnionMap") -> "ASTNode":
        return _isl_ast_build_node_from_schedule_map(self, schedule)

    def expr_from_set(self, set: "Set") -> "ASTExpr":
        return _isl_ast_build_expr_from_set(self, set)

    def expr_from_pw_aff(self, pa: "PwAff") -> "ASTExpr":
        return _isl_ast_build_expr_from_pw_aff(self, pa)

    def access_from_pw_multi_aff(self, pma: "PwMultiAff") -> "ASTExpr":
        return _isl_ast_build_access_from_pw_multi_aff(self, pma)

    def access_from_multi_pw_aff(self, mpa: "MultiPwAff") -> "ASTExpr":
        return _isl_ast_build_access_from_multi_pw_aff(self, mpa)

    def call_from_pw_multi_aff(self, pma: "PwMultiAff") -> "ASTExpr":
        return _isl_ast_build_call_from_pw_multi_aff(self, pma)

    def call_from_multi_pw_aff(self, mpa: "MultiPwAff") -> "ASTExpr":
        return _isl_ast_build_call_from_multi_pw_aff(self, mpa)

    def set_options(self, options: "UnionMap") -> "ASTBuild":
        return _isl_ast_build_set_options(self, options)

    def set_iterators(self, iterators: "IdList") -> "ASTBuild":
        return _isl_ast_build_set_iterators(self, iterators)

    def set_create_leaf(self, fn: Any, user: Any, user_: Any = None) -> "ASTBuild":
        return _isl_ast_build_set_create_leaf(self, fn, user, user_)

    def set_at_each_domain(self, fn: Any, build: "ASTBuild", user: Any, user_: Any = None) -> "ASTBuild":
        return _isl_ast_build_set_at_each_domain(self, fn, build, user, user_)

    def set_before_each_for(self, fn: Any, user: Any, user_: Any = None) -> "ASTBuild":
        return _isl_ast_build_set_before_each_for(self, fn, user, user_)

    def set_after_each_for(self, fn: Any, build: "ASTBuild", user: Any, user_: Any = None) -> "ASTBuild":
        return _isl_ast_build_set_after_each_for(self, fn, build, user, user_)

    def set_before_each_mark(self, fn: Any, build: "ASTBuild", user: Any, user_: Any = None) -> "ASTBuild":
        return _isl_ast_build_set_before_each_mark(self, fn, build, user, user_)

    def set_after_each_mark(self, fn: Any, build: "ASTBuild", user: Any, user_: Any = None) -> "ASTBuild":
        return _isl_ast_build_set_after_each_mark(self, fn, build, user, user_)

    def get_schedule(self) -> "UnionMap":
        return _isl_ast_build_get_schedule(self)

    def get_schedule_space(self) -> "Space":
        return _isl_ast_build_get_schedule_space(self)

    def restrict(self, set: "Set") -> "ASTBuild":
        return _isl_ast_build_restrict(self, set)


register_type("ASTBuild", ASTBuild)

_isl_ast_build_get_ctx = ISLFunction.create(
    "isl_ast_build_get_ctx",
    Keep("ASTBuild"),
    return_=Give("Context"),
    lib=_lib,
)

_isl_ast_build_alloc = ISLFunction.create(
    "isl_ast_build_alloc",
    Context(),
    return_=Give("ASTBuild"),
    lib=_lib,
)

_isl_ast_build_from_context = ISLFunction.create(
    "isl_ast_build_from_context",
    Take("Set"),
    return_=Give("ASTBuild"),
    lib=_lib,
)

_isl_ast_build_copy = ISLFunction.create(
    "isl_ast_build_copy",
    Keep("ASTBuild"),
    return_=Give("ASTBuild"),
    lib=_lib,
)

_isl_ast_build_free = ISLFunction.create(
    "isl_ast_build_free",
    Take("ASTBuild"),
    return_=Give("ASTBuild"),
    lib=_lib,
)

_isl_ast_build_node_from_schedule = ISLFunction.create(
    "isl_ast_build_node_from_schedule",
    Keep("ASTBuild"),
    Take("Schedule"),
    return_=Give("ASTNode"),
    lib=_lib,
)

_isl_ast_build_node_from_schedule_map = ISLFunction.create(
    "isl_ast_build_node_from_schedule_map",
    Keep("ASTBuild"),
    Take("UnionMap"),
    return_=Give("ASTNode"),
    lib=_lib,
)

_isl_ast_build_expr_from_set = ISLFunction.create(
    "isl_ast_build_expr_from_set",
    Keep("ASTBuild"),
    Take("Set"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_build_expr_from_pw_aff = ISLFunction.create(
    "isl_ast_build_expr_from_pw_aff",
    Keep("ASTBuild"),
    Take("PwAff"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_build_access_from_pw_multi_aff = ISLFunction.create(
    "isl_ast_build_access_from_pw_multi_aff",
    Keep("ASTBuild"),
    Take("PwMultiAff"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_build_access_from_multi_pw_aff = ISLFunction.create(
    "isl_ast_build_access_from_multi_pw_aff",
    Keep("ASTBuild"),
    Take("MultiPwAff"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_build_call_from_pw_multi_aff = ISLFunction.create(
    "isl_ast_build_call_from_pw_multi_aff",
    Keep("ASTBuild"),
    Take("PwMultiAff"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_build_call_from_multi_pw_aff = ISLFunction.create(
    "isl_ast_build_call_from_multi_pw_aff",
    Keep("ASTBuild"),
    Take("MultiPwAff"),
    return_=Give("ASTExpr"),
    lib=_lib,
)

_isl_ast_build_set_options = ISLFunction.create(
    "isl_ast_build_set_options",
    Take("ASTBuild"),
    Take("UnionMap"),
    return_=Give("ASTBuild"),
    lib=_lib,
)

_isl_ast_build_set_iterators = ISLFunction.create(
    "isl_ast_build_set_iterators",
    Take("ASTBuild"),
    Take("IdList"),
    return_=Give("ASTBuild"),
    lib=_lib,
)

_isl_ast_build_set_create_leaf = ISLFunction.create(
    "isl_ast_build_set_create_leaf",
    Take("ASTBuild"),
    Param(None, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    return_=Give("ASTBuild"),
    lib=_lib,
)

_isl_ast_build_set_at_each_domain = ISLFunction.create(
    "isl_ast_build_set_at_each_domain",
    Take("ASTBuild"),
    Param(None, ctype=c_void_p),
    Keep("ASTBuild"),
    Param(Any, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    return_=Give("ASTBuild"),
    lib=_lib,
)

_isl_ast_build_set_before_each_for = ISLFunction.create(
    "isl_ast_build_set_before_each_for",
    Take("ASTBuild"),
    Param(None, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    return_=Give("ASTBuild"),
    lib=_lib,
)

_isl_ast_build_set_after_each_for = ISLFunction.create(
    "isl_ast_build_set_after_each_for",
    Take("ASTBuild"),
    Param(None, ctype=c_void_p),
    Keep("ASTBuild"),
    Param(Any, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    return_=Give("ASTBuild"),
    lib=_lib,
)

_isl_ast_build_set_before_each_mark = ISLFunction.create(
    "isl_ast_build_set_before_each_mark",
    Take("ASTBuild"),
    Param(None, ctype=c_void_p),
    Keep("ASTBuild"),
    Param(Any, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    return_=Give("ASTBuild"),
    lib=_lib,
)

_isl_ast_build_set_after_each_mark = ISLFunction.create(
    "isl_ast_build_set_after_each_mark",
    Take("ASTBuild"),
    Param(None, ctype=c_void_p),
    Keep("ASTBuild"),
    Param(Any, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    return_=Give("ASTBuild"),
    lib=_lib,
)

_isl_ast_build_get_schedule = ISLFunction.create(
    "isl_ast_build_get_schedule",
    Keep("ASTBuild"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_ast_build_get_schedule_space = ISLFunction.create(
    "isl_ast_build_get_schedule_space",
    Keep("ASTBuild"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_ast_build_restrict = ISLFunction.create(
    "isl_ast_build_restrict",
    Take("ASTBuild"),
    Take("Set"),
    return_=Give("ASTBuild"),
    lib=_lib,
)

_isl_ast_build_read_from_str = ISLFunction.create(
    "isl_ast_build_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("ASTBuild"),
    lib=_lib,
)
