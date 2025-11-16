from __future__ import annotations

from ctypes import c_int, c_void_p

from ..ffi import FfiPointer, load_libisl
from ..func import ISLFunction
from ..obj import ISLObject
from ..qualifier import Give, Keep, Null, Param, Take
from .context import Context

_lib = load_libisl()

class Set(ISLObject):
    """High-level wrapper around ``isl_set`` handles."""

    __slots__ = ()
    def __init__(self, handle_or_spec: str | FfiPointer) -> None:
        handle = None
        if isinstance(handle_or_spec, str):
            handle = _isl_set_read_from_str(handle_or_spec, return_raw_pointer=True)
        else:
            handle = handle_or_spec
        super().__init__(handle)

    @classmethod
    def from_str(cls, spec: str) -> "Set":
        return _isl_set_read_from_str(spec)

    def copy_handle(self) -> FfiPointer:
        return _isl_set_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_set_free(handle)

    @classmethod
    def from_union_set(cls, union: UnionSet) -> "Set":
        # UnionSet 依存の API は後続タスクで実装する
        raise NotImplementedError("UnionSet-dependent API is deferred")

    # -- Queries ---------------------------------------------------------
    def is_empty(self) -> bool:
        return _isl_set_is_empty(self)

    def is_equal(self, other: "Set") -> bool:
        return _isl_set_is_equal(self, other)

    def plain_is_empty(self) -> bool:
        return _isl_set_plain_is_empty(self)

    def plain_is_universe(self) -> bool:
        return _isl_set_plain_is_universe(self)

    def is_singleton(self) -> bool:
        return _isl_set_is_singleton(self)

    def involves_locals(self) -> bool:
        return _isl_set_involves_locals(self)

    def is_params(self) -> bool:
        return _isl_set_is_params(self)

    def is_wrapping(self) -> bool:
        return _isl_set_is_wrapping(self)

    def plain_is_equal(self, other: "Set") -> bool:
        return _isl_set_plain_is_equal(self, other)

    def plain_is_disjoint(self, other: "Set") -> bool:
        return _isl_set_plain_is_disjoint(self, other)

    def is_disjoint(self, other: "Set") -> bool:
        return _isl_set_is_disjoint(self, other)

    def is_subset(self, other: "Set") -> bool:
        return _isl_set_is_subset(self, other)

    def is_strict_subset(self, other: "Set") -> bool:
        return _isl_set_is_strict_subset(self, other)

    def plain_cmp(self, other: "Set") -> int:
        return _isl_set_plain_cmp(self, other)

    def tuple_dim(self) -> int:
        return _isl_set_tuple_dim(self)

    def has_tuple_id(self) -> bool:
        return _isl_set_has_tuple_id(self)

    def has_tuple_name(self) -> bool:
        return _isl_set_has_tuple_name(self)

    def get_tuple_name(self) -> str | None:
        return _isl_set_get_tuple_name(self)

    def reset_tuple_id(self) -> "Set":
        return _isl_set_reset_tuple_id(self)

    def reset_user(self) -> "Set":
        return _isl_set_reset_user(self)

    def compute_divs(self) -> "Set":
        return _isl_set_compute_divs(self)

    def remove_divs(self) -> "Set":
        return _isl_set_remove_divs(self)

    def remove_unknown_divs(self) -> "Set":
        return _isl_set_remove_unknown_divs(self)

    def make_disjoint(self) -> "Set":
        return _isl_set_make_disjoint(self)

    def n_basic_set(self) -> int:
        return _isl_set_n_basic_set(self)

    def complement(self) -> "Set":
        return _isl_set_complement(self)

    def wrapped_reverse(self) -> "Set":
        return _isl_set_wrapped_reverse(self)

    def project_out_all_params(self) -> "Set":
        return _isl_set_project_out_all_params(self)

    def params(self) -> "Set":
        return _isl_set_params(self)

    def from_params(self) -> "Set":
        return _isl_set_from_params(self)

    def coalesce(self) -> "Set":
        return _isl_set_coalesce(self)

    def detect_equalities(self) -> "Set":
        return _isl_set_detect_equalities(self)

    def remove_redundancies(self) -> "Set":
        return _isl_set_remove_redundancies(self)

    def flatten(self) -> "Set":
        return _isl_set_flatten(self)

    def lift(self) -> "Set":
        return _isl_set_lift(self)

    def drop_unused_params(self) -> "Set":
        return _isl_set_drop_unused_params(self)

    def neg(self) -> "Set":
        return _isl_set_neg(self)

    def union(self, other: "Set") -> "Set":
        return _isl_set_union(self, other)

    def intersect(self, other: "Set") -> "Set":
        return _isl_set_intersect(self, other)

    def subtract(self, other: "Set") -> "Set":
        return _isl_set_subtract(self, other)

    def intersect_params(self, other: "Set") -> "Set":
        return _isl_set_intersect_params(self, other)

    def intersect_factor_domain(self, other: "Set") -> "Set":
        return _isl_set_intersect_factor_domain(self, other)

    def intersect_factor_range(self, other: "Set") -> "Set":
        return _isl_set_intersect_factor_range(self, other)

    def product(self, other: "Set") -> "Set":
        return _isl_set_product(self, other)

    def flat_product(self, other: "Set") -> "Set":
        return _isl_set_flat_product(self, other)

    def gist(self, context: "Set") -> "Set":
        return _isl_set_gist(self, context)

    def gist_params(self, context: "Set") -> "Set":
        return _isl_set_gist_params(self, context)

    def sum(self, other: "Set") -> "Set":
        return _isl_set_sum(self, other)

    def partial_lexmin(self, dom: "Set") -> "Set":
        return _isl_set_partial_lexmin(self, dom, None)

    def partial_lexmax(self, dom: "Set") -> "Set":
        return _isl_set_partial_lexmax(self, dom, None)

    def lexmin(self) -> "Set":
        return _isl_set_lexmin(self)

    def lexmax(self) -> "Set":
        return _isl_set_lexmax(self)

    def add_constraint(self, constraint: Constraint) -> "Set":
        raise NotImplementedError("Constraint-dependent API is deferred")

    def __str__(self) -> str:  # pragma: no cover - trivial wrapper
        return _isl_set_to_str(self)

    def __repr__(self) -> str:  # pragma: no cover - human-friendly print
        return f"Set({self.__str__()})"

    def __eq__(self, other: object) -> bool:  # pragma: no cover - delegating
        if not isinstance(other, Set):
            return NotImplemented
        return self.is_equal(other)

_isl_set_read_from_str = ISLFunction.create(
    _lib.isl_set_read_from_str,
    Context(),
    Param(str),
    return_=Give(Set),
    lib=_lib,
)

_isl_set_copy = ISLFunction.create(
    _lib.isl_set_copy,
    Keep(Set),
    return_=Give(Set),
    lib=_lib,
)

_isl_set_free = ISLFunction.create(
    _lib.isl_set_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

_isl_set_to_str = ISLFunction.create(
    _lib.isl_set_to_str,
    Keep(Set),
    return_=Param(str),
    lib=_lib,
)

_isl_set_union = ISLFunction.create(
    _lib.isl_set_union,
    Take(Set),
    Take(Set),
    return_=Give(Set),
    lib=_lib,
)

_isl_set_intersect = ISLFunction.create(
    _lib.isl_set_intersect,
    Take(Set),
    Take(Set),
    return_=Give(Set),
    lib=_lib,
)

_isl_set_subtract = ISLFunction.create(
    _lib.isl_set_subtract,
    Take(Set),
    Take(Set),
    return_=Give(Set),
    lib=_lib,
)

_isl_set_is_empty = ISLFunction.create(
    _lib.isl_set_is_empty,
    Keep(Set),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_is_equal = ISLFunction.create(
    _lib.isl_set_is_equal,
    Keep(Set),
    Keep(Set),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_plain_is_empty = ISLFunction.create(
    _lib.isl_set_plain_is_empty,
    Keep(Set),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_plain_is_universe = ISLFunction.create(
    _lib.isl_set_plain_is_universe,
    Keep(Set),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_is_singleton = ISLFunction.create(
    _lib.isl_set_is_singleton,
    Keep(Set),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_involves_locals = ISLFunction.create(
    _lib.isl_set_involves_locals,
    Keep(Set),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_is_params = ISLFunction.create(
    _lib.isl_set_is_params,
    Keep(Set),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_is_wrapping = ISLFunction.create(
    _lib.isl_set_is_wrapping,
    Keep(Set),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_plain_is_equal = ISLFunction.create(
    _lib.isl_set_plain_is_equal,
    Keep(Set),
    Keep(Set),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_plain_is_disjoint = ISLFunction.create(
    _lib.isl_set_plain_is_disjoint,
    Keep(Set),
    Keep(Set),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_is_disjoint = ISLFunction.create(
    _lib.isl_set_is_disjoint,
    Keep(Set),
    Keep(Set),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_is_subset = ISLFunction.create(
    _lib.isl_set_is_subset,
    Keep(Set),
    Keep(Set),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_is_strict_subset = ISLFunction.create(
    _lib.isl_set_is_strict_subset,
    Keep(Set),
    Keep(Set),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_plain_cmp = ISLFunction.create(
    _lib.isl_set_plain_cmp,
    Keep(Set),
    Keep(Set),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_set_tuple_dim = ISLFunction.create(
    _lib.isl_set_tuple_dim,
    Keep(Set),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_set_reset_tuple_id = ISLFunction.create(
    _lib.isl_set_reset_tuple_id,
    Take(Set),
    return_=Give(Set),
    lib=_lib,
)

_isl_set_has_tuple_id = ISLFunction.create(
    _lib.isl_set_has_tuple_id,
    Keep(Set),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_has_tuple_name = ISLFunction.create(
    _lib.isl_set_has_tuple_name,
    Keep(Set),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_set_get_tuple_name = ISLFunction.create(
    _lib.isl_set_get_tuple_name,
    Keep(Set),
    return_=Param(str),
    lib=_lib,
)

_isl_set_reset_user = ISLFunction.create(
    _lib.isl_set_reset_user,
    Take(Set),
    return_=Give(Set),
    lib=_lib,
)

_isl_set_compute_divs = ISLFunction.create(
    _lib.isl_set_compute_divs,
    Take(Set),
    return_=Give(Set),
    lib=_lib,
)

_isl_set_remove_divs = ISLFunction.create(
    _lib.isl_set_remove_divs,
    Take(Set),
    return_=Give(Set),
    lib=_lib,
)

_isl_set_remove_unknown_divs = ISLFunction.create(
    _lib.isl_set_remove_unknown_divs,
    Take(Set),
    return_=Give(Set),
    lib=_lib,
)

_isl_set_make_disjoint = ISLFunction.create(
    _lib.isl_set_make_disjoint,
    Take(Set),
    return_=Give(Set),
    lib=_lib,
)

_isl_set_n_basic_set = ISLFunction.create(
    _lib.isl_set_n_basic_set,
    Keep(Set),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_set_complement = ISLFunction.create(
    _lib.isl_set_complement,
    Take(Set),
    return_=Give(Set),
    lib=_lib,
)

# isl_set_wrapped_reverse は古い libisl には存在しない場合があるため、存在チェックを行う
# if hasattr(_lib, "isl_set_wrapped_reverse"):
#     _isl_set_wrapped_reverse = ISLFunction.create(
#         _lib.isl_set_wrapped_reverse,
#         Take(Set),
#         return_=Give(Set),
#         lib=_lib,
#     )
# else:  # pragma: no cover - depends on libisl build
#     def _isl_set_wrapped_reverse(*args, **kwargs):
#         raise NotImplementedError("isl_set_wrapped_reverse not available in linked libisl")

_isl_set_project_out_all_params = ISLFunction.create(
    _lib.isl_set_project_out_all_params,
    Take(Set),
    return_=Give(Set),
    lib=_lib,
)

_isl_set_params = ISLFunction.create(
    _lib.isl_set_params,
    Take(Set),
    return_=Give(Set),
    lib=_lib,
)

_isl_set_from_params = ISLFunction.create(
    _lib.isl_set_from_params,
    Take(Set),
    return_=Give(Set),
    lib=_lib,
)

_isl_set_coalesce = ISLFunction.create(
    _lib.isl_set_coalesce,
    Take(Set),
    return_=Give(Set),
    lib=_lib,
)

_isl_set_detect_equalities = ISLFunction.create(
    _lib.isl_set_detect_equalities,
    Take(Set),
    return_=Give(Set),
    lib=_lib,
)

_isl_set_remove_redundancies = ISLFunction.create(
    _lib.isl_set_remove_redundancies,
    Take(Set),
    return_=Give(Set),
    lib=_lib,
)

_isl_set_flatten = ISLFunction.create(
    _lib.isl_set_flatten,
    Take(Set),
    return_=Give(Set),
    lib=_lib,
)

_isl_set_lift = ISLFunction.create(
    _lib.isl_set_lift,
    Take(Set),
    return_=Give(Set),
    lib=_lib,
)

_isl_set_drop_unused_params = ISLFunction.create(
    _lib.isl_set_drop_unused_params,
    Take(Set),
    return_=Give(Set),
    lib=_lib,
)

_isl_set_neg = ISLFunction.create(
    _lib.isl_set_neg,
    Take(Set),
    return_=Give(Set),
    lib=_lib,
)

_isl_set_intersect_params = ISLFunction.create(
    _lib.isl_set_intersect_params,
    Take(Set),
    Take(Set),
    return_=Give(Set),
    lib=_lib,
)

_isl_set_intersect_factor_domain = ISLFunction.create(
    _lib.isl_set_intersect_factor_domain,
    Take(Set),
    Take(Set),
    return_=Give(Set),
    lib=_lib,
)

_isl_set_intersect_factor_range = ISLFunction.create(
    _lib.isl_set_intersect_factor_range,
    Take(Set),
    Take(Set),
    return_=Give(Set),
    lib=_lib,
)

_isl_set_product = ISLFunction.create(
    _lib.isl_set_product,
    Take(Set),
    Take(Set),
    return_=Give(Set),
    lib=_lib,
)

_isl_set_flat_product = ISLFunction.create(
    _lib.isl_set_flat_product,
    Take(Set),
    Take(Set),
    return_=Give(Set),
    lib=_lib,
)

_isl_set_gist = ISLFunction.create(
    _lib.isl_set_gist,
    Take(Set),
    Take(Set),
    return_=Give(Set),
    lib=_lib,
)

_isl_set_gist_params = ISLFunction.create(
    _lib.isl_set_gist_params,
    Take(Set),
    Take(Set),
    return_=Give(Set),
    lib=_lib,
)

_isl_set_sum = ISLFunction.create(
    _lib.isl_set_sum,
    Take(Set),
    Take(Set),
    return_=Give(Set),
    lib=_lib,
)

_isl_set_partial_lexmin = ISLFunction.create(
    _lib.isl_set_partial_lexmin,
    Take(Set),
    Take(Set),
    Param(None, ctype=c_void_p),
    return_=Give(Set),
    lib=_lib,
)

_isl_set_partial_lexmax = ISLFunction.create(
    _lib.isl_set_partial_lexmax,
    Take(Set),
    Take(Set),
    Param(None, ctype=c_void_p),
    return_=Give(Set),
    lib=_lib,
)

_isl_set_lexmin = ISLFunction.create(
    _lib.isl_set_lexmin,
    Take(Set),
    return_=Give(Set),
    lib=_lib,
)

_isl_set_lexmax = ISLFunction.create(
    _lib.isl_set_lexmax,
    Take(Set),
    return_=Give(Set),
    lib=_lib,
)

__all__ = ["Set"]
