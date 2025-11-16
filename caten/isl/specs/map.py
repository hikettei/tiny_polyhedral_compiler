from __future__ import annotations

from ctypes import c_int, c_void_p

from ..ffi import FfiPointer, load_libisl
from ..func import ISLFunction
from ..obj import ISLObject
from ..qualifier import Give, Keep, Null, Param, Take
from .aff import Aff, MultiAff, PwAff, PwMultiAff
from .basic_map import BasicMap
from .constraint import Constraint
from .context import Context
from .id import Id
from .set import Set
from .space import Space
from .val import Val

_lib = load_libisl()


class Map(ISLObject):
    """Wrapper around ``isl_map``."""

    __slots__ = ()

    def __init__(self, handle_or_spec: str | FfiPointer) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_map_read_from_str(handle_or_spec, return_raw_pointer=True)
        else:
            handle = handle_or_spec
        super().__init__(handle)

    @classmethod
    def from_str(cls, spec: str) -> "Map":
        return _isl_map_read_from_str(spec)

    @classmethod
    def empty(cls, space: Space) -> "Map":
        return _isl_map_empty(space)

    @classmethod
    def universe(cls, space: Space) -> "Map":
        return _isl_map_universe(space)

    @classmethod
    def from_basic_map(cls, bmap: BasicMap) -> "Map":
        return _isl_map_from_basic_map(bmap)

    def copy_handle(self) -> FfiPointer:
        return _isl_map_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_map_free(handle)

    # queries
    def dim(self, dim_type: int) -> int:
        return _isl_map_dim(self, dim_type)

    def is_empty(self) -> bool:
        return _isl_map_is_empty(self)

    def is_equal(self, other: "Map") -> bool:
        return _isl_map_is_equal(self, other)

    # transforms
    def add_constraint(self, constraint: Constraint) -> "Map":
        return _isl_map_add_constraint(self, constraint)

    def project_out(self, dim_type: int, first: int, n: int) -> "Map":
        return _isl_map_project_out(self, dim_type, first, n)

    def remove_divs(self) -> "Map":
        return _isl_map_remove_divs(self)

    def remove_unknown_divs(self) -> "Map":
        return _isl_map_remove_unknown_divs(self)

    def __str__(self) -> str:  # pragma: no cover
        return _isl_map_to_str(self)

    def __repr__(self) -> str:  # pragma: no cover
        return f"Map({self.__str__()})"

    # basic queries
    def get_space(self) -> Space:
        return _isl_map_get_space(self)

    def domain_tuple_dim(self) -> int:
        return _isl_map_domain_tuple_dim(self)

    def range_tuple_dim(self) -> int:
        return _isl_map_range_tuple_dim(self)

    # set/has/get tuple/dim ids
    def set_dim_id(self, dim_type: int, pos: int, id: "Id") -> "Map":
        return _isl_map_set_dim_id(self, dim_type, pos, id)

    def has_dim_id(self, dim_type: int, pos: int) -> bool:
        return _isl_map_has_dim_id(self, dim_type, pos)

    def get_dim_id(self, dim_type: int, pos: int) -> Id:
        return _isl_map_get_dim_id(self, dim_type, pos)

    def has_dim_name(self, dim_type: int, pos: int) -> bool:
        return _isl_map_has_dim_name(self, dim_type, pos)

    def get_dim_name(self, dim_type: int, pos: int) -> str:
        return _isl_map_get_dim_name(self, dim_type, pos)

    def find_dim_by_id(self, id: "Id") -> int:
        return _isl_map_find_dim_by_id(self, id)

    def find_dim_by_name(self, dim_type: int, name: str) -> int:
        return _isl_map_find_dim_by_name(self, dim_type, name)

    def set_domain_tuple_id(self, id: "Id") -> "Map":
        return _isl_map_set_domain_tuple_id(self, id)

    def set_range_tuple_id(self, id: "Id") -> "Map":
        return _isl_map_set_range_tuple_id(self, id)

    def set_tuple_id(self, dim_type: int, id: "Id") -> "Map":
        return _isl_map_set_tuple_id(self, dim_type, id)

    def reset_tuple_id(self, dim_type: int) -> "Map":
        return _isl_map_reset_tuple_id(self, dim_type)

    def has_domain_tuple_id(self) -> bool:
        return _isl_map_has_domain_tuple_id(self)

    def has_range_tuple_id(self) -> bool:
        return _isl_map_has_range_tuple_id(self)

    def has_tuple_id(self, dim_type: int) -> bool:
        return _isl_map_has_tuple_id(self, dim_type)

    def get_domain_tuple_id(self) -> "Id":
        return _isl_map_get_domain_tuple_id(self)

    def get_range_tuple_id(self) -> "Id":
        return _isl_map_get_range_tuple_id(self)

    def get_tuple_id(self, dim_type: int) -> "Id":
        return _isl_map_get_tuple_id(self, dim_type)

    # set/has tuple names
    def set_domain_tuple_name(self, name: str) -> "Map":
        raise NotImplementedError("isl_map_set_domain_tuple_name not available in linked libisl")

    def set_range_tuple_name(self, name: str) -> "Map":
        raise NotImplementedError("isl_map_set_range_tuple_name not available in linked libisl")

    def set_tuple_name(self, dim_type: int, name: str) -> "Map":
        raise NotImplementedError("isl_map_set_tuple_name not available in linked libisl")

    def get_domain_tuple_name(self) -> str:
        raise NotImplementedError("isl_map_get_domain_tuple_name not available in linked libisl")

    def get_range_tuple_name(self) -> str:
        raise NotImplementedError("isl_map_get_range_tuple_name not available in linked libisl")

    def get_tuple_name(self, dim_type: int) -> str:
        raise NotImplementedError("isl_map_get_tuple_name not available in linked libisl")

    # domain / range
    def domain(self) -> Set:
        return _isl_map_domain(self)

    def range(self) -> Set:
        return _isl_map_range(self)

    def intersect(self, other: "Map") -> "Map":
        return _isl_map_intersect(self, other)

    def union(self, other: "Map") -> "Map":
        return _isl_map_union(self, other)

    def subtract(self, other: "Map") -> "Map":
        return _isl_map_subtract(self, other)

    # extras
    def has_tuple_name(self) -> bool:
        return _isl_map_has_tuple_name(self)

    def reset_user(self) -> "Map":
        return _isl_map_reset_user(self)

    def nat_universe(self) -> "Map":
        return _isl_map_nat_universe(self)

    def identity(self) -> "Map":
        return _isl_map_identity(self)

    def lex_lt(self, other: "Map") -> "Map":
        return _isl_map_lex_lt(self, other)

    def lex_le(self, other: "Map") -> "Map":
        return _isl_map_lex_le(self, other)

    def lex_gt(self, other: "Map") -> "Map":
        return _isl_map_lex_gt(self, other)

    def lex_ge(self, other: "Map") -> "Map":
        return _isl_map_lex_ge(self, other)

    def lex_lt_first(self, other: "Map") -> "Map":
        return _isl_map_lex_lt_first(self, other)

    def lex_le_first(self, other: "Map") -> "Map":
        return _isl_map_lex_le_first(self, other)

    def lex_gt_first(self, other: "Map") -> "Map":
        return _isl_map_lex_gt_first(self, other)

    def lex_ge_first(self, other: "Map") -> "Map":
        return _isl_map_lex_ge_first(self, other)

    @classmethod
    def from_aff(cls, aff: "Aff") -> "Map":
        return _isl_map_from_aff(aff)

    @classmethod
    def from_multi_aff(cls, maff: "MultiAff") -> "Map":
        return _isl_map_from_multi_aff(maff)

    @classmethod
    def from_pw_aff(cls, paff: "PwAff") -> "Map":
        return _isl_map_from_pw_aff(paff)

    @classmethod
    def from_pw_multi_aff(cls, pmaff: "PwMultiAff") -> "Map":
        return _isl_map_from_pw_multi_aff(pmaff)

    @classmethod
    # MultiPwAff not wrapped; keep placeholder

    def compute_divs(self) -> "Map":
        return _isl_map_compute_divs(self)

    def remove_divs_involving_dims(self, dim_type: int, first: int, n: int) -> "Map":
        return _isl_map_remove_divs_involving_dims(self, dim_type, first, n)

    def make_disjoint(self) -> "Map":
        return _isl_map_make_disjoint(self)

    def n_basic_map(self) -> int:
        return _isl_map_n_basic_map(self)

    def get_basic_map_list(self) -> "BasicMap":
        return _isl_map_get_basic_map_list(self)

    @classmethod
    def read_from_file(cls, ctx: "Context", file_ptr: int) -> "Map":
        return _isl_map_read_from_file(ctx, file_ptr)

    # predicates
    def plain_is_empty(self) -> bool:
        return _isl_map_plain_is_empty(self)

    def plain_is_universe(self) -> bool:
        return _isl_map_plain_is_universe(self)

    def plain_is_single_valued(self) -> bool:
        return _isl_map_plain_is_single_valued(self)

    def is_single_valued(self) -> bool:
        return _isl_map_is_single_valued(self)

    def plain_is_injective(self) -> bool:
        return _isl_map_plain_is_injective(self)

    def is_injective(self) -> bool:
        return _isl_map_is_injective(self)

    def is_bijective(self) -> bool:
        return _isl_map_is_bijective(self)

    def is_identity(self) -> bool:
        return _isl_map_is_identity(self)

    def plain_get_val_if_fixed(self, dim_type: int, pos: int) -> "Set":
        return _isl_map_plain_get_val_if_fixed(self, dim_type, pos)

    def get_range_stride_info(self, pos: int) -> object:
        return _isl_map_get_range_stride_info(self, pos)

    def get_range_lattice_tile(self) -> object:
        return _isl_map_get_range_lattice_tile(self)

    def involves_dims(self, dim_type: int, first: int, n: int) -> bool:
        return _isl_map_involves_dims(self, dim_type, first, n)

    def domain_is_wrapping(self) -> bool:
        return _isl_map_domain_is_wrapping(self)

    def range_is_wrapping(self) -> bool:
        return _isl_map_range_is_wrapping(self)

    def is_product(self) -> bool:
        return _isl_map_is_product(self)

    def can_zip(self) -> bool:
        return _isl_map_can_zip(self)

    def can_curry(self) -> bool:
        return _isl_map_can_curry(self)

    def can_uncurry(self) -> bool:
        return _isl_map_can_uncurry(self)

    def can_range_curry(self) -> bool:
        return _isl_map_can_range_curry(self)

    def plain_is_equal(self, other: "Map") -> bool:
        return _isl_map_plain_is_equal(self, other)

    def is_disjoint(self, other: "Map") -> bool:
        return _isl_map_is_disjoint(self, other)

    def is_subset(self, other: "Map") -> bool:
        return _isl_map_is_subset(self, other)

    def is_strict_subset(self, other: "Map") -> bool:
        return _isl_map_is_strict_subset(self, other)

    def complement(self) -> "Map":
        return _isl_map_complement(self)

    def reverse(self) -> "Map":
        return _isl_map_reverse(self)

    def domain_reverse(self) -> "Map":
        return _isl_map_domain_reverse(self)

    def range_reverse(self) -> "Map":
        return _isl_map_range_reverse(self)

    def project_out_param_id(self, id: Id) -> "Map":
        return _isl_map_project_out_param_id(self, id)

    def project_out_all_params(self) -> "Map":
        return _isl_map_project_out_all_params(self)

    def params(self) -> Set:
        return _isl_map_params(self)

    def domain_map(self) -> "Map":
        return _isl_map_domain_map(self)

    def range_map(self) -> "Map":
        return _isl_map_range_map(self)

    def eliminate(self, dim_type: int, first: int, n: int) -> "Map":
        return _isl_map_eliminate(self, dim_type, first, n)

    @classmethod
    def from_domain(cls, set_: Set) -> "Map":
        return _isl_map_from_domain(set_)

    @classmethod
    def from_range(cls, set_: Set) -> "Map":
        return _isl_map_from_range(set_)

    def fix_si(self, dim_type: int, pos: int, value: int) -> "Map":
        return _isl_map_fix_si(self, dim_type, pos, value)

    def fix_val(self, dim_type: int, pos: int, value: Val) -> "Map":
        return _isl_map_fix_val(self, dim_type, pos, value)

    def lower_bound_si(self, dim_type: int, pos: int, value: int) -> "Map":
        return _isl_map_lower_bound_si(self, dim_type, pos, value)

    def upper_bound_si(self, dim_type: int, pos: int, value: int) -> "Map":
        return _isl_map_upper_bound_si(self, dim_type, pos, value)

    def lower_bound_val(self, dim_type: int, pos: int, value: Val) -> "Map":
        return _isl_map_lower_bound_val(self, dim_type, pos, value)

    def upper_bound_val(self, dim_type: int, pos: int, value: Val) -> "Map":
        return _isl_map_upper_bound_val(self, dim_type, pos, value)

    def to_union_map(self) -> None:  # NotImplemented: to avoid circular import
        raise NotImplementedError("isl_map_to_union_map not wired due to circular import")

    @classmethod
    def from_union_map(cls, umap: object) -> "Map":  # NotImplemented placeholder
        raise NotImplementedError("isl_map_from_union_map not wired due to circular import")

    def foreach_basic_map(self, fn: object) -> None:
        raise NotImplementedError("callback bridge not implemented")

    def bind_domain(self, multi_id: object) -> "Set":
        raise NotImplementedError("multi_id wrapper not implemented")

    def bind_range(self, multi_id: object) -> "Set":
        raise NotImplementedError("multi_id wrapper not implemented")

    def project_out_param_id_list(self, id_list: object) -> "Map":
        raise NotImplementedError("id_list wrapper not implemented")

    def equate(self, t1: int, pos1: int, t2: int, pos2: int) -> "Map":
        return _isl_map_equate(self, t1, pos1, t2, pos2)

    def order_le(self, t1: int, pos1: int, t2: int, pos2: int) -> "Map":
        return _isl_map_order_le(self, t1, pos1, t2, pos2)

    def order_ge(self, t1: int, pos1: int, t2: int, pos2: int) -> "Map":
        return _isl_map_order_ge(self, t1, pos1, t2, pos2)

    def order_lt(self, t1: int, pos1: int, t2: int, pos2: int) -> "Map":
        return _isl_map_order_lt(self, t1, pos1, t2, pos2)

    def order_gt(self, t1: int, pos1: int, t2: int, pos2: int) -> "Map":
        return _isl_map_order_gt(self, t1, pos1, t2, pos2)

    def deltas(self) -> Set:
        return _isl_map_deltas(self)

    def deltas_map(self) -> "Map":
        return _isl_map_deltas_map(self)


_isl_map_read_from_str = ISLFunction.create(
    _lib.isl_map_read_from_str,
    Context(),
    Param(str),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_copy = ISLFunction.create(
    _lib.isl_map_copy,
    Keep(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_free = ISLFunction.create(
    _lib.isl_map_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

_isl_map_empty = ISLFunction.create(
    _lib.isl_map_empty,
    Take(Space),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_universe = ISLFunction.create(
    _lib.isl_map_universe,
    Take(Space),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_from_basic_map = ISLFunction.create(
    _lib.isl_map_from_basic_map,
    Take(BasicMap),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_dim = ISLFunction.create(
    _lib.isl_map_dim,
    Keep(Map),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_map_is_empty = ISLFunction.create(
    _lib.isl_map_is_empty,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_equal = ISLFunction.create(
    _lib.isl_map_is_equal,
    Keep(Map),
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_add_constraint = ISLFunction.create(
    _lib.isl_map_add_constraint,
    Take(Map),
    Take(Constraint),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_intersect = ISLFunction.create(
    _lib.isl_map_intersect,
    Take(Map),
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_project_out = ISLFunction.create(
    _lib.isl_map_project_out,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_remove_divs = ISLFunction.create(
    _lib.isl_map_remove_divs,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_remove_unknown_divs = ISLFunction.create(
    _lib.isl_map_remove_unknown_divs,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_to_str = ISLFunction.create(
    _lib.isl_map_to_str,
    Keep(Map),
    return_=Param(str),
    lib=_lib,
)

_isl_map_has_tuple_name = ISLFunction.create(
    _lib.isl_map_has_tuple_name,
    Keep(Map),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_reset_user = ISLFunction.create(
    _lib.isl_map_reset_user,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_nat_universe = ISLFunction.create(
    _lib.isl_map_nat_universe,
    Take(Space),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_identity = ISLFunction.create(
    _lib.isl_map_identity,
    Take(Space),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_lex_lt = ISLFunction.create(
    _lib.isl_map_lex_lt,
    Take(Space),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_lex_le = ISLFunction.create(
    _lib.isl_map_lex_le,
    Take(Space),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_lex_gt = ISLFunction.create(
    _lib.isl_map_lex_gt,
    Take(Space),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_lex_ge = ISLFunction.create(
    _lib.isl_map_lex_ge,
    Take(Space),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_lex_lt_first = ISLFunction.create(
    _lib.isl_map_lex_lt_first,
    Take(Space),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_lex_le_first = ISLFunction.create(
    _lib.isl_map_lex_le_first,
    Take(Space),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_lex_gt_first = ISLFunction.create(
    _lib.isl_map_lex_gt_first,
    Take(Space),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_lex_ge_first = ISLFunction.create(
    _lib.isl_map_lex_ge_first,
    Take(Space),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_from_aff = ISLFunction.create(
    _lib.isl_map_from_aff,
    Take(Aff),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_from_multi_aff = ISLFunction.create(
    _lib.isl_map_from_multi_aff,
    Take(MultiAff),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_from_pw_aff = ISLFunction.create(
    _lib.isl_map_from_pw_aff,
    Take(PwAff),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_from_pw_multi_aff = ISLFunction.create(
    _lib.isl_map_from_pw_multi_aff,
    Take(PwMultiAff),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_compute_divs = ISLFunction.create(
    _lib.isl_map_compute_divs,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_remove_divs_involving_dims = ISLFunction.create(
    _lib.isl_map_remove_divs_involving_dims,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_make_disjoint = ISLFunction.create(
    _lib.isl_map_make_disjoint,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_n_basic_map = ISLFunction.create(
    _lib.isl_map_n_basic_map,
    Keep(Map),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_map_get_basic_map_list = ISLFunction.create(
    _lib.isl_map_get_basic_map_list,
    Keep(Map),
    return_=Give(BasicMap),
    lib=_lib,
)

_isl_map_read_from_file = ISLFunction.create(
    _lib.isl_map_read_from_file,
    Context(),
    Param(None, ctype=c_void_p),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_plain_is_empty = ISLFunction.create(
    _lib.isl_map_plain_is_empty,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_plain_is_universe = ISLFunction.create(
    _lib.isl_map_plain_is_universe,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_plain_is_single_valued = ISLFunction.create(
    _lib.isl_map_plain_is_single_valued,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_single_valued = ISLFunction.create(
    _lib.isl_map_is_single_valued,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_plain_is_injective = ISLFunction.create(
    _lib.isl_map_plain_is_injective,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_injective = ISLFunction.create(
    _lib.isl_map_is_injective,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_bijective = ISLFunction.create(
    _lib.isl_map_is_bijective,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_identity = ISLFunction.create(
    _lib.isl_map_is_identity,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_plain_get_val_if_fixed = ISLFunction.create(
    _lib.isl_map_plain_get_val_if_fixed,
    Keep(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Param(None, ctype=c_void_p),
    lib=_lib,
)

_isl_map_get_range_stride_info = ISLFunction.create(
    _lib.isl_map_get_range_stride_info,
    Keep(Map),
    Param(int, ctype=c_int),
    return_=Param(None, ctype=c_void_p),
    lib=_lib,
)

_isl_map_get_range_lattice_tile = ISLFunction.create(
    _lib.isl_map_get_range_lattice_tile,
    Keep(Map),
    return_=Param(None, ctype=c_void_p),
    lib=_lib,
)

_isl_map_involves_dims = ISLFunction.create(
    _lib.isl_map_involves_dims,
    Keep(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_domain_is_wrapping = ISLFunction.create(
    _lib.isl_map_domain_is_wrapping,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_range_is_wrapping = ISLFunction.create(
    _lib.isl_map_range_is_wrapping,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_product = ISLFunction.create(
    _lib.isl_map_is_product,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_can_zip = ISLFunction.create(
    _lib.isl_map_can_zip,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_can_curry = ISLFunction.create(
    _lib.isl_map_can_curry,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_can_uncurry = ISLFunction.create(
    _lib.isl_map_can_uncurry,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_can_range_curry = ISLFunction.create(
    _lib.isl_map_can_range_curry,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_plain_is_equal = ISLFunction.create(
    _lib.isl_map_plain_is_equal,
    Keep(Map),
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_disjoint = ISLFunction.create(
    _lib.isl_map_is_disjoint,
    Keep(Map),
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_subset = ISLFunction.create(
    _lib.isl_map_is_subset,
    Keep(Map),
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_strict_subset = ISLFunction.create(
    _lib.isl_map_is_strict_subset,
    Keep(Map),
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_complement = ISLFunction.create(
    _lib.isl_map_complement,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_reverse = ISLFunction.create(
    _lib.isl_map_reverse,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_domain_reverse = ISLFunction.create(
    _lib.isl_map_domain_reverse,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_range_reverse = ISLFunction.create(
    _lib.isl_map_range_reverse,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_project_out_param_id = ISLFunction.create(
    _lib.isl_map_project_out_param_id,
    Take(Map),
    Take(Id),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_project_out_all_params = ISLFunction.create(
    _lib.isl_map_project_out_all_params,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_params = ISLFunction.create(
    _lib.isl_map_params,
    Take(Map),
    return_=Give(Set),
    lib=_lib,
)

_isl_map_domain_map = ISLFunction.create(
    _lib.isl_map_domain_map,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_range_map = ISLFunction.create(
    _lib.isl_map_range_map,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_eliminate = ISLFunction.create(
    _lib.isl_map_eliminate,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_from_domain = ISLFunction.create(
    _lib.isl_map_from_domain,
    Take(Set),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_from_range = ISLFunction.create(
    _lib.isl_map_from_range,
    Take(Set),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_fix_si = ISLFunction.create(
    _lib.isl_map_fix_si,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_fix_val = ISLFunction.create(
    _lib.isl_map_fix_val,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Take(Val),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_lower_bound_si = ISLFunction.create(
    _lib.isl_map_lower_bound_si,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_upper_bound_si = ISLFunction.create(
    _lib.isl_map_upper_bound_si,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_lower_bound_val = ISLFunction.create(
    _lib.isl_map_lower_bound_val,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Take(Val),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_upper_bound_val = ISLFunction.create(
    _lib.isl_map_upper_bound_val,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Take(Val),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_foreach_basic_map = ISLFunction.create(
    _lib.isl_map_foreach_basic_map,
    Keep(Map),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_map_bind_domain = ISLFunction.create(
    _lib.isl_map_bind_domain,
    Take(Map),
    Param(None, ctype=c_void_p),
    return_=Give(Set),
    lib=_lib,
)

_isl_map_bind_range = ISLFunction.create(
    _lib.isl_map_bind_range,
    Take(Map),
    Param(None, ctype=c_void_p),
    return_=Give(Set),
    lib=_lib,
)

_isl_map_project_out_param_id_list = ISLFunction.create(
    _lib.isl_map_project_out_param_id_list,
    Take(Map),
    Param(None, ctype=c_void_p),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_equate = ISLFunction.create(
    _lib.isl_map_equate,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_order_le = ISLFunction.create(
    _lib.isl_map_order_le,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_order_ge = ISLFunction.create(
    _lib.isl_map_order_ge,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_order_lt = ISLFunction.create(
    _lib.isl_map_order_lt,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_order_gt = ISLFunction.create(
    _lib.isl_map_order_gt,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_deltas = ISLFunction.create(
    _lib.isl_map_deltas,
    Take(Map),
    return_=Give(Set),
    lib=_lib,
)

_isl_map_deltas_map = ISLFunction.create(
    _lib.isl_map_deltas_map,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)
_isl_map_get_space = ISLFunction.create(
    _lib.isl_map_get_space,
    Keep(Map),
    return_=Give(Space),
    lib=_lib,
)

_isl_map_domain_tuple_dim = ISLFunction.create(
    _lib.isl_map_domain_tuple_dim,
    Keep(Map),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_map_range_tuple_dim = ISLFunction.create(
    _lib.isl_map_range_tuple_dim,
    Keep(Map),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_map_set_dim_id = ISLFunction.create(
    _lib.isl_map_set_dim_id,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Take(Id),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_has_dim_id = ISLFunction.create(
    _lib.isl_map_has_dim_id,
    Keep(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_get_dim_id = ISLFunction.create(
    _lib.isl_map_get_dim_id,
    Keep(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Id),
    lib=_lib,
)

_isl_map_has_dim_name = ISLFunction.create(
    _lib.isl_map_has_dim_name,
    Keep(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_get_dim_name = ISLFunction.create(
    _lib.isl_map_get_dim_name,
    Keep(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Param(str),
    lib=_lib,
)

_isl_map_find_dim_by_id = ISLFunction.create(
    _lib.isl_map_find_dim_by_id,
    Keep(Map),
    Keep(Id),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_map_find_dim_by_name = ISLFunction.create(
    _lib.isl_map_find_dim_by_name,
    Keep(Map),
    Param(int, ctype=c_int),
    Param(str),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_map_set_domain_tuple_id = ISLFunction.create(
    _lib.isl_map_set_domain_tuple_id,
    Take(Map),
    Take(Id),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_set_range_tuple_id = ISLFunction.create(
    _lib.isl_map_set_range_tuple_id,
    Take(Map),
    Take(Id),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_set_tuple_id = ISLFunction.create(
    _lib.isl_map_set_tuple_id,
    Take(Map),
    Param(int, ctype=c_int),
    Take(Id),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_reset_tuple_id = ISLFunction.create(
    _lib.isl_map_reset_tuple_id,
    Take(Map),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_has_domain_tuple_id = ISLFunction.create(
    _lib.isl_map_has_domain_tuple_id,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_has_range_tuple_id = ISLFunction.create(
    _lib.isl_map_has_range_tuple_id,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_has_tuple_id = ISLFunction.create(
    _lib.isl_map_has_tuple_id,
    Keep(Map),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_get_domain_tuple_id = ISLFunction.create(
    _lib.isl_map_get_domain_tuple_id,
    Keep(Map),
    return_=Give(Id),
    lib=_lib,
)

_isl_map_get_range_tuple_id = ISLFunction.create(
    _lib.isl_map_get_range_tuple_id,
    Keep(Map),
    return_=Give(Id),
    lib=_lib,
)

_isl_map_get_tuple_id = ISLFunction.create(
    _lib.isl_map_get_tuple_id,
    Keep(Map),
    Param(int, ctype=c_int),
    return_=Give(Id),
    lib=_lib,
)

_isl_map_domain = ISLFunction.create(
    _lib.isl_map_domain,
    Take(Map),
    return_=Give(Set),
    lib=_lib,
)

_isl_map_range = ISLFunction.create(
    _lib.isl_map_range,
    Take(Map),
    return_=Give(Set),
    lib=_lib,
)

_isl_map_union = ISLFunction.create(
    _lib.isl_map_union,
    Take(Map),
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_subtract = ISLFunction.create(
    _lib.isl_map_subtract,
    Take(Map),
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_remove_divs_involving_dims = ISLFunction.create(
    _lib.isl_map_remove_divs_involving_dims,
    Take(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_make_disjoint = ISLFunction.create(
    _lib.isl_map_make_disjoint,
    Take(Map),
    return_=Give(Map),
    lib=_lib,
)

_isl_map_n_basic_map = ISLFunction.create(
    _lib.isl_map_n_basic_map,
    Keep(Map),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_map_get_basic_map_list = ISLFunction.create(
    _lib.isl_map_get_basic_map_list,
    Keep(Map),
    return_=Give(BasicMap),
    lib=_lib,
)

_isl_map_plain_is_empty = ISLFunction.create(
    _lib.isl_map_plain_is_empty,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_plain_is_universe = ISLFunction.create(
    _lib.isl_map_plain_is_universe,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_plain_is_single_valued = ISLFunction.create(
    _lib.isl_map_plain_is_single_valued,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_single_valued = ISLFunction.create(
    _lib.isl_map_is_single_valued,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_plain_is_injective = ISLFunction.create(
    _lib.isl_map_plain_is_injective,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_injective = ISLFunction.create(
    _lib.isl_map_is_injective,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_bijective = ISLFunction.create(
    _lib.isl_map_is_bijective,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_identity = ISLFunction.create(
    _lib.isl_map_is_identity,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_plain_get_val_if_fixed = ISLFunction.create(
    _lib.isl_map_plain_get_val_if_fixed,
    Keep(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Set),  # placeholder; actual type is isl_val*
    lib=_lib,
)

_isl_map_get_range_stride_info = ISLFunction.create(
    _lib.isl_map_get_range_stride_info,
    Keep(Map),
    Param(int, ctype=c_int),
    return_=Param(None, ctype=c_void_p),
    lib=_lib,
)

_isl_map_get_range_lattice_tile = ISLFunction.create(
    _lib.isl_map_get_range_lattice_tile,
    Keep(Map),
    return_=Param(None, ctype=c_void_p),
    lib=_lib,
)

_isl_map_involves_dims = ISLFunction.create(
    _lib.isl_map_involves_dims,
    Keep(Map),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_domain_is_wrapping = ISLFunction.create(
    _lib.isl_map_domain_is_wrapping,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_range_is_wrapping = ISLFunction.create(
    _lib.isl_map_range_is_wrapping,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_is_product = ISLFunction.create(
    _lib.isl_map_is_product,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_can_zip = ISLFunction.create(
    _lib.isl_map_can_zip,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_can_curry = ISLFunction.create(
    _lib.isl_map_can_curry,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_can_uncurry = ISLFunction.create(
    _lib.isl_map_can_uncurry,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_map_can_range_curry = ISLFunction.create(
    _lib.isl_map_can_range_curry,
    Keep(Map),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)
__all__ = ["Map"]
