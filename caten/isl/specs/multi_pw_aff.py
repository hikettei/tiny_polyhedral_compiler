from __future__ import annotations

from ctypes import c_int, c_void_p

from ..ffi import FfiPointer, load_libisl
from ..func import ISLFunction
from ..obj import ISLObject
from ..qualifier import Give, Keep, Null, Param, Take
from .aff import Aff, MultiAff, PwAff, PwMultiAff
from .context import Context
from .id import Id
from .map import Map
from .multi_val import MultiVal
from .set import Set
from .space import Space
from .val import Val

_lib = load_libisl()


class MultiPwAff(ISLObject):
    """Wrapper around ``isl_multi_pw_aff``."""

    __slots__ = ()

    def __init__(self, handle_or_spec: FfiPointer | str) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_multi_pw_aff_read_from_str(handle_or_spec, return_raw_pointer=True)
        else:
            handle = handle_or_spec
        super().__init__(handle)

    @classmethod
    def from_str(cls, spec: str) -> "MultiPwAff":
        return _isl_multi_pw_aff_read_from_str(spec)

    def copy_handle(self) -> FfiPointer:
        return _isl_multi_pw_aff_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_multi_pw_aff_free(handle)

    def dim(self, dim_type: int) -> int:
        return _isl_multi_pw_aff_dim(self, dim_type)

    def size(self) -> int:
        return _isl_multi_pw_aff_size(self)

    def get_space(self) -> Space:
        return _isl_multi_pw_aff_get_space(self)

    def get_domain_space(self) -> Space:
        return _isl_multi_pw_aff_get_domain_space(self)

    def get_ctx(self) -> Context:
        return _isl_multi_pw_aff_get_ctx(self)

    # tuple/id/name
    def set_dim_id(self, dim_type: int, pos: int, id: Id) -> "MultiPwAff":
        return _isl_multi_pw_aff_set_dim_id(self, dim_type, pos, id)

    def set_dim_name(self, dim_type: int, pos: int, name: str) -> "MultiPwAff":
        return _isl_multi_pw_aff_set_dim_name(self, dim_type, pos, name)

    def get_dim_id(self, dim_type: int, pos: int) -> Id:
        return _isl_multi_pw_aff_get_dim_id(self, dim_type, pos)

    def find_dim_by_id(self, id: Id) -> int:
        return _isl_multi_pw_aff_find_dim_by_id(self, id)

    def find_dim_by_name(self, dim_type: int, name: str) -> int:
        return _isl_multi_pw_aff_find_dim_by_name(self, dim_type, name)

    def set_tuple_name(self, dim_type: int, name: str) -> "MultiPwAff":
        return _isl_multi_pw_aff_set_tuple_name(self, dim_type, name)

    def set_range_tuple_id(self, id: Id) -> "MultiPwAff":
        return _isl_multi_pw_aff_set_range_tuple_id(self, id)

    def reset_range_tuple_id(self) -> "MultiPwAff":
        return _isl_multi_pw_aff_reset_range_tuple_id(self)

    def reset_tuple_id(self, dim_type: int) -> "MultiPwAff":
        return _isl_multi_pw_aff_reset_tuple_id(self, dim_type)

    def has_range_tuple_id(self) -> bool:
        return _isl_multi_pw_aff_has_range_tuple_id(self)

    def get_range_tuple_id(self) -> Id:
        return _isl_multi_pw_aff_get_range_tuple_id(self)

    def has_tuple_id(self, dim_type: int) -> bool:
        return _isl_multi_pw_aff_has_tuple_id(self, dim_type)

    def get_tuple_id(self, dim_type: int) -> Id:
        return _isl_multi_pw_aff_get_tuple_id(self, dim_type)

    def reset_user(self) -> "MultiPwAff":
        return _isl_multi_pw_aff_reset_user(self)

    # construction
    @classmethod
    def zero(cls, space: Space) -> "MultiPwAff":
        return _isl_multi_pw_aff_zero(space)

    @classmethod
    def identity_on_domain_space(cls, space: Space) -> "MultiPwAff":
        return _isl_multi_pw_aff_identity_on_domain_space(space)

    @classmethod
    def identity(cls, space: Space) -> "MultiPwAff":
        return _isl_multi_pw_aff_identity(space)

    @classmethod
    def identity_multi_pw_aff(cls, mpa: "MultiPwAff") -> "MultiPwAff":
        return _isl_multi_pw_aff_identity_multi_pw_aff(mpa)

    @classmethod
    def from_pw_aff(cls, pa: PwAff) -> "MultiPwAff":
        return _isl_multi_pw_aff_from_pw_aff(pa)

    @classmethod
    def from_aff(cls, aff: Aff) -> "MultiPwAff":
        return _isl_multi_pw_aff_from_aff(aff)

    @classmethod
    def from_multi_aff(cls, ma: MultiAff) -> "MultiPwAff":
        return _isl_multi_pw_aff_from_multi_aff(ma)

    @classmethod
    def from_pw_multi_aff(cls, pma: PwMultiAff) -> "MultiPwAff":
        return _isl_multi_pw_aff_from_pw_multi_aff(pma)

    # conversions
    def as_map(self) -> Map:
        return _isl_multi_pw_aff_as_map(self)

    def as_set(self) -> Set:
        return _isl_multi_pw_aff_as_set(self)

    def range_product(self, other: "MultiPwAff") -> "MultiPwAff":
        return _isl_multi_pw_aff_range_product(self, other)

    def flat_range_product(self, other: "MultiPwAff") -> "MultiPwAff":
        return _isl_multi_pw_aff_flat_range_product(self, other)

    def product(self, other: "MultiPwAff") -> "MultiPwAff":
        return _isl_multi_pw_aff_product(self, other)

    def union_add(self, other: "MultiPwAff") -> "MultiPwAff":
        return _isl_multi_pw_aff_union_add(self, other)

    def flatten(self) -> "MultiPwAff":
        if _isl_multi_pw_aff_flatten is None:
            raise NotImplementedError("isl_multi_pw_aff_flatten not available in linked libisl")
        return _isl_multi_pw_aff_flatten(self)

    def from_range(self, set_: Set) -> "MultiPwAff":
        return _isl_multi_pw_aff_from_range(set_)

    def domain(self) -> Set:
        return _isl_multi_pw_aff_domain(self)

    def range(self) -> Set:
        if _isl_multi_pw_aff_range is None:
            raise NotImplementedError("isl_multi_pw_aff_range not available in linked libisl")
        return _isl_multi_pw_aff_range(self)

    def from_pw_aff_list(self, list_ptr: FfiPointer) -> "MultiPwAff":
        return _isl_multi_pw_aff_from_pw_aff_list(list_ptr)

    def set_pw_aff(self, pos: int, pa: PwAff) -> "MultiPwAff":
        return _isl_multi_pw_aff_set_pw_aff(self, pos, pa)

    def get_pw_aff(self, pos: int) -> PwAff:
        return _isl_multi_pw_aff_get_pw_aff(self, pos)

    def get_at(self, pos: int) -> PwAff:
        return _isl_multi_pw_aff_get_at(self, pos)

    def set_at(self, pos: int, pa: PwAff) -> "MultiPwAff":
        return _isl_multi_pw_aff_set_at(self, pos, pa)

    def get_list(self) -> object:
        return _isl_multi_pw_aff_get_list(self)

    def curry(self) -> "MultiPwAff":
        return _isl_multi_pw_aff_curry(self)

    def uncurry(self) -> "MultiPwAff":
        return _isl_multi_pw_aff_uncurry(self)

    def pullback_multi_aff(self, ma: "MultiAff") -> "MultiPwAff":
        return _isl_multi_pw_aff_pullback_multi_aff(self, ma)

    def pullback_pw_multi_aff(self, pma: PwMultiAff) -> "MultiPwAff":
        return _isl_multi_pw_aff_pullback_pw_multi_aff(self, pma)

    def pullback_multi_pw_aff(self, mpa: "MultiPwAff") -> "MultiPwAff":
        return _isl_multi_pw_aff_pullback_multi_pw_aff(self, mpa)

    def free_coefficients(self) -> "MultiPwAff":
        return _isl_multi_pw_aff_free_coefficients(self)

    def coalesce(self) -> "MultiPwAff":
        return _isl_multi_pw_aff_coalesce(self)

    def gist(self, context: Set) -> "MultiPwAff":
        return _isl_multi_pw_aff_gist(self, context)

    # predicates / properties
    def involves_dims(self, dim_type: int, first: int, n: int) -> bool:
        return _isl_multi_pw_aff_involves_dims(self, dim_type, first, n)

    def involves_param_id(self, id: Id) -> bool:
        return _isl_multi_pw_aff_involves_param_id(self, id)

    def involves_param_id_list(self, id_list: object) -> bool:
        return _isl_multi_pw_aff_involves_param_id_list(self, id_list)

    def range_is_wrapping(self) -> bool:
        return _isl_multi_pw_aff_range_is_wrapping(self)

    def is_cst(self) -> bool:
        return _isl_multi_pw_aff_is_cst(self)

    def involves_nan(self) -> bool:
        return _isl_multi_pw_aff_involves_nan(self)

    def plain_is_equal(self, other: "MultiPwAff") -> bool:
        return _isl_multi_pw_aff_plain_is_equal(self, other)

    def is_equal(self, other: "MultiPwAff") -> bool:
        return _isl_multi_pw_aff_is_equal(self, other)

    def domain_reverse(self) -> "MultiPwAff":
        return _isl_multi_pw_aff_domain_reverse(self)

    # binding / domain ops
    def bind_domain(self, multi_id: object) -> "MultiPwAff":
        return _isl_multi_pw_aff_bind_domain(self, multi_id)

    def bind_domain_wrapped_domain(self, multi_id: object) -> "MultiPwAff":
        return _isl_multi_pw_aff_bind_domain_wrapped_domain(self, multi_id)

    def bind(self, multi_id: object) -> Set:
        return _isl_multi_pw_aff_bind(self, multi_id)

    def project_domain_on_params(self) -> "MultiPwAff":
        return _isl_multi_pw_aff_project_domain_on_params(self)

    def unbind_params_insert_domain(self, multi_id: object) -> "MultiPwAff":
        return _isl_multi_pw_aff_unbind_params_insert_domain(self, multi_id)

    def insert_domain(self, space: Space) -> "MultiPwAff":
        return _isl_multi_pw_aff_insert_domain(self, space)

    # range / factor / splice
    def flatten_range(self) -> "MultiPwAff":
        return _isl_multi_pw_aff_flatten_range(self)

    def factor_range(self) -> "MultiPwAff":
        return _isl_multi_pw_aff_factor_range(self)

    def range_factor_domain(self) -> "MultiPwAff":
        return _isl_multi_pw_aff_range_factor_domain(self)

    def range_factor_range(self) -> "MultiPwAff":
        return _isl_multi_pw_aff_range_factor_range(self)

    def range_splice(self, pos: int, other: "MultiPwAff") -> "MultiPwAff":
        return _isl_multi_pw_aff_range_splice(self, pos, other)

    def splice(self, in_pos: int, out_pos: int, other: "MultiPwAff") -> "MultiPwAff":
        return _isl_multi_pw_aff_splice(self, in_pos, out_pos, other)

    # arithmetic
    def add(self, other: "MultiPwAff") -> "MultiPwAff":
        return _isl_multi_pw_aff_add(self, other)

    def add_constant_val(self, val: Val) -> "MultiPwAff":
        return _isl_multi_pw_aff_add_constant_val(self, val)

    def add_constant_multi_val(self, mv: MultiVal) -> "MultiPwAff":
        return _isl_multi_pw_aff_add_constant_multi_val(self, mv)

    def min(self, other: "MultiPwAff") -> "MultiPwAff":
        return _isl_multi_pw_aff_min(self, other)

    def max(self, other: "MultiPwAff") -> "MultiPwAff":
        return _isl_multi_pw_aff_max(self, other)

    def sub(self, other: "MultiPwAff") -> "MultiPwAff":
        return _isl_multi_pw_aff_sub(self, other)

    def scale_val(self, val: Val) -> "MultiPwAff":
        return _isl_multi_pw_aff_scale_val(self, val)

    def scale_down_val(self, val: Val) -> "MultiPwAff":
        return _isl_multi_pw_aff_scale_down_val(self, val)

    def scale_multi_val(self, mv: MultiVal) -> "MultiPwAff":
        return _isl_multi_pw_aff_scale_multi_val(self, mv)

    def scale_down_multi_val(self, mv: MultiVal) -> "MultiPwAff":
        return _isl_multi_pw_aff_scale_down_multi_val(self, mv)

    def mod_multi_val(self, mv: MultiVal) -> "MultiPwAff":
        return _isl_multi_pw_aff_mod_multi_val(self, mv)

    def min_multi_val(self) -> MultiVal:
        return _isl_multi_pw_aff_min_multi_val(self)

    def max_multi_val(self) -> MultiVal:
        return _isl_multi_pw_aff_max_multi_val(self)

    def neg(self) -> "MultiPwAff":
        return _isl_multi_pw_aff_neg(self)

    def add_dims(self, dim_type: int, n: int) -> "MultiPwAff":
        return _isl_multi_pw_aff_add_dims(self, dim_type, n)

    def insert_dims(self, dim_type: int, first: int, n: int) -> "MultiPwAff":
        return _isl_multi_pw_aff_insert_dims(self, dim_type, first, n)

    def move_dims(
        self, dst_type: int, dst_pos: int, src_type: int, src_pos: int, n: int
    ) -> "MultiPwAff":
        return _isl_multi_pw_aff_move_dims(self, dst_type, dst_pos, src_type, src_pos, n)

    def intersect_domain(self, domain: Set) -> "MultiPwAff":
        return _isl_multi_pw_aff_intersect_domain(self, domain)

    def intersect_params(self, params: Set) -> "MultiPwAff":
        return _isl_multi_pw_aff_intersect_params(self, params)

    # comparisons to Map
    def eq_map(self, other: "MultiPwAff") -> Map:
        return _isl_multi_pw_aff_eq_map(self, other)

    def lex_le_map(self, other: "MultiPwAff") -> Map:
        return _isl_multi_pw_aff_lex_le_map(self, other)

    def lex_lt_map(self, other: "MultiPwAff") -> Map:
        return _isl_multi_pw_aff_lex_lt_map(self, other)

    def lex_ge_map(self, other: "MultiPwAff") -> Map:
        return _isl_multi_pw_aff_lex_ge_map(self, other)

    def lex_gt_map(self, other: "MultiPwAff") -> Map:
        return _isl_multi_pw_aff_lex_gt_map(self, other)

    def gist_params(self, set_: Set) -> "MultiPwAff":
        return _isl_multi_pw_aff_gist_params(self, set_)


_isl_multi_pw_aff_read_from_str = ISLFunction.create(
    _lib.isl_multi_pw_aff_read_from_str,
    Context(),
    Param(str),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_copy = ISLFunction.create(
    _lib.isl_multi_pw_aff_copy,
    Keep(MultiPwAff),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_free = ISLFunction.create(
    _lib.isl_multi_pw_aff_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

_isl_multi_pw_aff_dim = ISLFunction.create(
    _lib.isl_multi_pw_aff_dim,
    Keep(MultiPwAff),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_multi_pw_aff_get_space = ISLFunction.create(
    _lib.isl_multi_pw_aff_get_space,
    Keep(MultiPwAff),
    return_=Give(Space),
    lib=_lib,
)

_isl_multi_pw_aff_get_domain_space = ISLFunction.create(
    _lib.isl_multi_pw_aff_get_domain_space,
    Keep(MultiPwAff),
    return_=Give(Space),
    lib=_lib,
)

_isl_multi_pw_aff_set_dim_id = ISLFunction.create(
    _lib.isl_multi_pw_aff_set_dim_id,
    Take(MultiPwAff),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Take(Id),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_get_dim_id = ISLFunction.create(
    _lib.isl_multi_pw_aff_get_dim_id,
    Keep(MultiPwAff),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Id),
    lib=_lib,
)

_isl_multi_pw_aff_find_dim_by_id = ISLFunction.create(
    _lib.isl_multi_pw_aff_find_dim_by_id,
    Keep(MultiPwAff),
    Keep(Id),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_multi_pw_aff_find_dim_by_name = ISLFunction.create(
    _lib.isl_multi_pw_aff_find_dim_by_name,
    Keep(MultiPwAff),
    Param(int, ctype=c_int),
    Param(str),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_multi_pw_aff_set_tuple_name = ISLFunction.create(
    _lib.isl_multi_pw_aff_set_tuple_name,
    Take(MultiPwAff),
    Param(int, ctype=c_int),
    Param(str),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_set_range_tuple_id = ISLFunction.create(
    _lib.isl_multi_pw_aff_set_range_tuple_id,
    Take(MultiPwAff),
    Take(Id),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_reset_range_tuple_id = ISLFunction.create(
    _lib.isl_multi_pw_aff_reset_range_tuple_id,
    Take(MultiPwAff),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_reset_tuple_id = ISLFunction.create(
    _lib.isl_multi_pw_aff_reset_tuple_id,
    Take(MultiPwAff),
    Param(int, ctype=c_int),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_has_range_tuple_id = ISLFunction.create(
    _lib.isl_multi_pw_aff_has_range_tuple_id,
    Keep(MultiPwAff),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_pw_aff_get_range_tuple_id = ISLFunction.create(
    _lib.isl_multi_pw_aff_get_range_tuple_id,
    Keep(MultiPwAff),
    return_=Give(Id),
    lib=_lib,
)

_isl_multi_pw_aff_has_tuple_id = ISLFunction.create(
    _lib.isl_multi_pw_aff_has_tuple_id,
    Keep(MultiPwAff),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_pw_aff_get_tuple_id = ISLFunction.create(
    _lib.isl_multi_pw_aff_get_tuple_id,
    Keep(MultiPwAff),
    Param(int, ctype=c_int),
    return_=Give(Id),
    lib=_lib,
)

_isl_multi_pw_aff_reset_user = ISLFunction.create(
    _lib.isl_multi_pw_aff_reset_user,
    Take(MultiPwAff),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_as_map = ISLFunction.create(
    _lib.isl_multi_pw_aff_as_map,
    Take(MultiPwAff),
    return_=Give(Map),
    lib=_lib,
)

_isl_multi_pw_aff_as_set = ISLFunction.create(
    _lib.isl_multi_pw_aff_as_set,
    Take(MultiPwAff),
    return_=Give(Set),
    lib=_lib,
)

_isl_multi_pw_aff_range_product = ISLFunction.create(
    _lib.isl_multi_pw_aff_range_product,
    Take(MultiPwAff),
    Take(MultiPwAff),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_flat_range_product = ISLFunction.create(
    _lib.isl_multi_pw_aff_flat_range_product,
    Take(MultiPwAff),
    Take(MultiPwAff),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_product = ISLFunction.create(
    _lib.isl_multi_pw_aff_product,
    Take(MultiPwAff),
    Take(MultiPwAff),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_union_add = ISLFunction.create(
    _lib.isl_multi_pw_aff_union_add,
    Take(MultiPwAff),
    Take(MultiPwAff),
    return_=Give(MultiPwAff),
    lib=_lib,
)

try:
    _isl_multi_pw_aff_flatten = ISLFunction.create(
        _lib.isl_multi_pw_aff_flatten,
        Take(MultiPwAff),
        return_=Give(MultiPwAff),
        lib=_lib,
    )
except AttributeError:
    _isl_multi_pw_aff_flatten = None  # type: ignore[assignment]

_isl_multi_pw_aff_from_range = ISLFunction.create(
    _lib.isl_multi_pw_aff_from_range,
    Take(Set),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_domain = ISLFunction.create(
    _lib.isl_multi_pw_aff_domain,
    Take(MultiPwAff),
    return_=Give(Set),
    lib=_lib,
)

try:
    _isl_multi_pw_aff_range = ISLFunction.create(
        _lib.isl_multi_pw_aff_range,
        Take(MultiPwAff),
        return_=Give(Set),
        lib=_lib,
    )
except AttributeError:
    _isl_multi_pw_aff_range = None  # type: ignore[assignment]

_isl_multi_pw_aff_from_pw_aff_list = ISLFunction.create(
    _lib.isl_multi_pw_aff_from_pw_aff_list,
    Param(None, ctype=c_void_p),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_set_pw_aff = ISLFunction.create(
    _lib.isl_multi_pw_aff_set_pw_aff,
    Take(MultiPwAff),
    Param(int, ctype=c_int),
    Take(PwAff),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_get_pw_aff = ISLFunction.create(
    _lib.isl_multi_pw_aff_get_pw_aff,
    Keep(MultiPwAff),
    Param(int, ctype=c_int),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_multi_pw_aff_curry = ISLFunction.create(
    _lib.isl_multi_pw_aff_curry,
    Take(MultiPwAff),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_uncurry = ISLFunction.create(
    _lib.isl_multi_pw_aff_uncurry,
    Take(MultiPwAff),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_pullback_multi_aff = ISLFunction.create(
    _lib.isl_multi_pw_aff_pullback_multi_aff,
    Take(MultiPwAff),
    Param(None, ctype=c_void_p),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_pullback_pw_multi_aff = ISLFunction.create(
    _lib.isl_multi_pw_aff_pullback_pw_multi_aff,
    Take(MultiPwAff),
    Take(PwMultiAff),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_pullback_multi_pw_aff = ISLFunction.create(
    _lib.isl_multi_pw_aff_pullback_multi_pw_aff,
    Take(MultiPwAff),
    Take(MultiPwAff),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_free_coefficients = ISLFunction.create(
    _lib.isl_multi_pw_aff_free_coefficients,
    Take(MultiPwAff),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_coalesce = ISLFunction.create(
    _lib.isl_multi_pw_aff_coalesce,
    Take(MultiPwAff),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_gist = ISLFunction.create(
    _lib.isl_multi_pw_aff_gist,
    Take(MultiPwAff),
    Take(Set),
    return_=Give(MultiPwAff),
    lib=_lib,
)

# Additional bindings
_isl_multi_pw_aff_get_ctx = ISLFunction.create(
    _lib.isl_multi_pw_aff_get_ctx,
    Keep(MultiPwAff),
    return_=Give(Context),
    lib=_lib,
)

_isl_multi_pw_aff_set_dim_name = ISLFunction.create(
    _lib.isl_multi_pw_aff_set_dim_name,
    Take(MultiPwAff),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(str),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_zero = ISLFunction.create(
    _lib.isl_multi_pw_aff_zero,
    Take(Space),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_identity_on_domain_space = ISLFunction.create(
    _lib.isl_multi_pw_aff_identity_on_domain_space,
    Take(Space),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_identity = ISLFunction.create(
    _lib.isl_multi_pw_aff_identity,
    Take(Space),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_identity_multi_pw_aff = ISLFunction.create(
    _lib.isl_multi_pw_aff_identity_multi_pw_aff,
    Take(MultiPwAff),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_from_pw_aff = ISLFunction.create(
    _lib.isl_multi_pw_aff_from_pw_aff,
    Take(PwAff),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_from_aff = ISLFunction.create(
    _lib.isl_multi_pw_aff_from_aff,
    Take(Aff),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_from_multi_aff = ISLFunction.create(
    _lib.isl_multi_pw_aff_from_multi_aff,
    Take(MultiAff),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_size = ISLFunction.create(
    _lib.isl_multi_pw_aff_size,
    Keep(MultiPwAff),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_multi_pw_aff_get_at = ISLFunction.create(
    _lib.isl_multi_pw_aff_get_at,
    Keep(MultiPwAff),
    Param(int, ctype=c_int),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_multi_pw_aff_set_at = ISLFunction.create(
    _lib.isl_multi_pw_aff_set_at,
    Take(MultiPwAff),
    Param(int, ctype=c_int),
    Take(PwAff),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_get_list = ISLFunction.create(
    _lib.isl_multi_pw_aff_get_list,
    Keep(MultiPwAff),
    return_=Param(None, ctype=c_void_p),
    lib=_lib,
)

_isl_multi_pw_aff_isa_multi_aff = ISLFunction.create(
    _lib.isl_multi_pw_aff_isa_multi_aff,
    Keep(MultiPwAff),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_pw_aff_as_multi_aff = ISLFunction.create(
    _lib.isl_multi_pw_aff_as_multi_aff,
    Take(MultiPwAff),
    return_=Give(MultiAff),
    lib=_lib,
)

_isl_multi_pw_aff_from_pw_multi_aff = ISLFunction.create(
    _lib.isl_multi_pw_aff_from_pw_multi_aff,
    Take(PwMultiAff),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_to_str = ISLFunction.create(
    _lib.isl_multi_pw_aff_to_str,
    Keep(MultiPwAff),
    return_=Param(str),
    lib=_lib,
)

_isl_multi_pw_aff_involves_dims = ISLFunction.create(
    _lib.isl_multi_pw_aff_involves_dims,
    Keep(MultiPwAff),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_pw_aff_involves_param_id = ISLFunction.create(
    _lib.isl_multi_pw_aff_involves_param_id,
    Keep(MultiPwAff),
    Keep(Id),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_pw_aff_involves_param_id_list = ISLFunction.create(
    _lib.isl_multi_pw_aff_involves_param_id_list,
    Keep(MultiPwAff),
    Param(None, ctype=c_void_p),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_pw_aff_range_is_wrapping = ISLFunction.create(
    _lib.isl_multi_pw_aff_range_is_wrapping,
    Keep(MultiPwAff),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_pw_aff_is_cst = ISLFunction.create(
    _lib.isl_multi_pw_aff_is_cst,
    Keep(MultiPwAff),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_pw_aff_involves_nan = ISLFunction.create(
    _lib.isl_multi_pw_aff_involves_nan,
    Keep(MultiPwAff),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_pw_aff_plain_is_equal = ISLFunction.create(
    _lib.isl_multi_pw_aff_plain_is_equal,
    Keep(MultiPwAff),
    Keep(MultiPwAff),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_pw_aff_is_equal = ISLFunction.create(
    _lib.isl_multi_pw_aff_is_equal,
    Keep(MultiPwAff),
    Keep(MultiPwAff),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_multi_pw_aff_domain_reverse = ISLFunction.create(
    _lib.isl_multi_pw_aff_domain_reverse,
    Take(MultiPwAff),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_bind_domain = ISLFunction.create(
    _lib.isl_multi_pw_aff_bind_domain,
    Take(MultiPwAff),
    Param(None, ctype=c_void_p),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_bind_domain_wrapped_domain = ISLFunction.create(
    _lib.isl_multi_pw_aff_bind_domain_wrapped_domain,
    Take(MultiPwAff),
    Param(None, ctype=c_void_p),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_bind = ISLFunction.create(
    _lib.isl_multi_pw_aff_bind,
    Take(MultiPwAff),
    Param(None, ctype=c_void_p),
    return_=Give(Set),
    lib=_lib,
)

_isl_multi_pw_aff_project_domain_on_params = ISLFunction.create(
    _lib.isl_multi_pw_aff_project_domain_on_params,
    Take(MultiPwAff),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_unbind_params_insert_domain = ISLFunction.create(
    _lib.isl_multi_pw_aff_unbind_params_insert_domain,
    Take(MultiPwAff),
    Param(None, ctype=c_void_p),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_insert_domain = ISLFunction.create(
    _lib.isl_multi_pw_aff_insert_domain,
    Take(MultiPwAff),
    Take(Space),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_min_multi_val = ISLFunction.create(
    _lib.isl_multi_pw_aff_min_multi_val,
    Take(MultiPwAff),
    return_=Give(MultiVal),
    lib=_lib,
)

_isl_multi_pw_aff_max_multi_val = ISLFunction.create(
    _lib.isl_multi_pw_aff_max_multi_val,
    Take(MultiPwAff),
    return_=Give(MultiVal),
    lib=_lib,
)

_isl_multi_pw_aff_flatten_range = ISLFunction.create(
    _lib.isl_multi_pw_aff_flatten_range,
    Take(MultiPwAff),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_neg = ISLFunction.create(
    _lib.isl_multi_pw_aff_neg,
    Take(MultiPwAff),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_insert_dims = ISLFunction.create(
    _lib.isl_multi_pw_aff_insert_dims,
    Take(MultiPwAff),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_add_dims = ISLFunction.create(
    _lib.isl_multi_pw_aff_add_dims,
    Take(MultiPwAff),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_move_dims = ISLFunction.create(
    _lib.isl_multi_pw_aff_move_dims,
    Take(MultiPwAff),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_intersect_domain = ISLFunction.create(
    _lib.isl_multi_pw_aff_intersect_domain,
    Take(MultiPwAff),
    Take(Set),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_intersect_params = ISLFunction.create(
    _lib.isl_multi_pw_aff_intersect_params,
    Take(MultiPwAff),
    Take(Set),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_eq_map = ISLFunction.create(
    _lib.isl_multi_pw_aff_eq_map,
    Take(MultiPwAff),
    Take(MultiPwAff),
    return_=Give(Map),
    lib=_lib,
)

_isl_multi_pw_aff_lex_le_map = ISLFunction.create(
    _lib.isl_multi_pw_aff_lex_le_map,
    Take(MultiPwAff),
    Take(MultiPwAff),
    return_=Give(Map),
    lib=_lib,
)

_isl_multi_pw_aff_lex_lt_map = ISLFunction.create(
    _lib.isl_multi_pw_aff_lex_lt_map,
    Take(MultiPwAff),
    Take(MultiPwAff),
    return_=Give(Map),
    lib=_lib,
)

_isl_multi_pw_aff_lex_ge_map = ISLFunction.create(
    _lib.isl_multi_pw_aff_lex_ge_map,
    Take(MultiPwAff),
    Take(MultiPwAff),
    return_=Give(Map),
    lib=_lib,
)

_isl_multi_pw_aff_lex_gt_map = ISLFunction.create(
    _lib.isl_multi_pw_aff_lex_gt_map,
    Take(MultiPwAff),
    Take(MultiPwAff),
    return_=Give(Map),
    lib=_lib,
)

_isl_multi_pw_aff_factor_range = ISLFunction.create(
    _lib.isl_multi_pw_aff_factor_range,
    Take(MultiPwAff),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_range_factor_domain = ISLFunction.create(
    _lib.isl_multi_pw_aff_range_factor_domain,
    Take(MultiPwAff),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_range_factor_range = ISLFunction.create(
    _lib.isl_multi_pw_aff_range_factor_range,
    Take(MultiPwAff),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_range_splice = ISLFunction.create(
    _lib.isl_multi_pw_aff_range_splice,
    Take(MultiPwAff),
    Param(int, ctype=c_int),
    Take(MultiPwAff),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_splice = ISLFunction.create(
    _lib.isl_multi_pw_aff_splice,
    Take(MultiPwAff),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Take(MultiPwAff),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_gist_params = ISLFunction.create(
    _lib.isl_multi_pw_aff_gist_params,
    Take(MultiPwAff),
    Take(Set),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_add = ISLFunction.create(
    _lib.isl_multi_pw_aff_add,
    Take(MultiPwAff),
    Take(MultiPwAff),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_add_constant_val = ISLFunction.create(
    _lib.isl_multi_pw_aff_add_constant_val,
    Take(MultiPwAff),
    Take(Val),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_add_constant_multi_val = ISLFunction.create(
    _lib.isl_multi_pw_aff_add_constant_multi_val,
    Take(MultiPwAff),
    Take(MultiVal),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_min = ISLFunction.create(
    _lib.isl_multi_pw_aff_min,
    Take(MultiPwAff),
    Take(MultiPwAff),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_max = ISLFunction.create(
    _lib.isl_multi_pw_aff_max,
    Take(MultiPwAff),
    Take(MultiPwAff),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_sub = ISLFunction.create(
    _lib.isl_multi_pw_aff_sub,
    Take(MultiPwAff),
    Take(MultiPwAff),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_scale_val = ISLFunction.create(
    _lib.isl_multi_pw_aff_scale_val,
    Take(MultiPwAff),
    Take(Val),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_scale_down_val = ISLFunction.create(
    _lib.isl_multi_pw_aff_scale_down_val,
    Take(MultiPwAff),
    Take(Val),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_mod_multi_val = ISLFunction.create(
    _lib.isl_multi_pw_aff_mod_multi_val,
    Take(MultiPwAff),
    Take(MultiVal),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_scale_multi_val = ISLFunction.create(
    _lib.isl_multi_pw_aff_scale_multi_val,
    Take(MultiPwAff),
    Take(MultiVal),
    return_=Give(MultiPwAff),
    lib=_lib,
)

_isl_multi_pw_aff_scale_down_multi_val = ISLFunction.create(
    _lib.isl_multi_pw_aff_scale_down_multi_val,
    Take(MultiPwAff),
    Take(MultiVal),
    return_=Give(MultiPwAff),
    lib=_lib,
)
__all__ = ["MultiPwAff"]
