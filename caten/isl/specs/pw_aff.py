from __future__ import annotations

from ctypes import c_char_p, c_int, c_uint, c_void_p
from typing import TYPE_CHECKING, Any

from ..ffi import load_libisl
from ..func import ISLFunction
from ..mixin import ISLObjectMixin
from ..obj import ISLObject
from ..qualifier import Give, Keep, Param, Take
from ..registry import register_type
from .context import Context

if TYPE_CHECKING:
    from .aff import Aff
    from .context import Context
    from .id import Id
    from .local_space import LocalSpace
    from .map import Map
    from .multi_aff import MultiAff
    from .multi_id import MultiId
    from .multi_pw_aff import MultiPwAff
    from .point import Point
    from .pw_multi_aff import PwMultiAff
    from .set import Set
    from .space import Space
    from .union_pw_aff import UnionPwAff
    from .val import Val

_lib = load_libisl()

class PwAff(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_pw_aff_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_pw_aff_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_pw_aff_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_pw_aff_free(handle)

    def __str__(self) -> str:
        return _isl_pw_aff_to_str(self)

    def __repr__(self) -> str:
        return f"PwAff({self.__str__()})"

    def cond(self, pwaff_true: "PwAff", pwaff_false: "PwAff") -> "PwAff":
        return _isl_pw_aff_cond(self, pwaff_true, pwaff_false)

    def get_ctx(self) -> "Context":
        return _isl_pw_aff_get_ctx(self)

    def get_domain_space(self) -> "Space":
        return _isl_pw_aff_get_domain_space(self)

    def get_space(self) -> "Space":
        return _isl_pw_aff_get_space(self)

    def dim(self, type: int) -> int:
        return _isl_pw_aff_dim(self, type)

    def set_dim_id(self, type: int, pos: int, id: "Id") -> "PwAff":
        return _isl_pw_aff_set_dim_id(self, type, pos, id)

    def has_dim_id(self, type: int, pos: int) -> bool:
        return _isl_pw_aff_has_dim_id(self, type, pos)

    def get_dim_id(self, type: int, pos: int) -> "Id":
        return _isl_pw_aff_get_dim_id(self, type, pos)

    def get_dim_name(self, type: int, pos: int) -> str:
        return _isl_pw_aff_get_dim_name(self, type, pos)

    def find_dim_by_name(self, type: int, name: str) -> int:
        return _isl_pw_aff_find_dim_by_name(self, type, name)

    def set_tuple_id(self, type: int, id: "Id") -> "PwAff":
        return _isl_pw_aff_set_tuple_id(self, type, id)

    def reset_tuple_id(self, type: int) -> "PwAff":
        return _isl_pw_aff_reset_tuple_id(self, type)

    def has_tuple_id(self, type: int) -> bool:
        return _isl_pw_aff_has_tuple_id(self, type)

    def get_tuple_id(self, type: int) -> "Id":
        return _isl_pw_aff_get_tuple_id(self, type)

    def reset_user(self) -> "PwAff":
        return _isl_pw_aff_reset_user(self)

    def as_map(self) -> "Map":
        return _isl_pw_aff_as_map(self)

    @classmethod
    def empty(cls, space: "Space") -> "PwAff":
        return _isl_pw_aff_empty(space)

    @classmethod
    def from_aff(cls, aff: "Aff") -> "PwAff":
        return _isl_pw_aff_from_aff(aff)

    def isa_aff(self) -> bool:
        return _isl_pw_aff_isa_aff(self)

    def as_aff(self) -> "Aff":
        return _isl_pw_aff_as_aff(self)

    @classmethod
    def alloc(cls, set: "Set", aff: "Aff") -> "PwAff":
        return _isl_pw_aff_alloc(set, aff)

    @classmethod
    def zero_on_domain(cls, ls: "LocalSpace") -> "PwAff":
        return _isl_pw_aff_zero_on_domain(ls)

    @classmethod
    def var_on_domain(cls, ls: "LocalSpace", type: int, pos: int) -> "PwAff":
        return _isl_pw_aff_var_on_domain(ls, type, pos)

    @classmethod
    def nan_on_domain_space(cls, space: "Space") -> "PwAff":
        return _isl_pw_aff_nan_on_domain_space(space)

    @classmethod
    def nan_on_domain(cls, ls: "LocalSpace") -> "PwAff":
        return _isl_pw_aff_nan_on_domain(ls)

    @classmethod
    def val_on_domain(cls, domain: "Set", v: "Val") -> "PwAff":
        return _isl_pw_aff_val_on_domain(domain, v)

    @classmethod
    def param_on_domain_id(cls, domain: "Set", id: "Id") -> "PwAff":
        return _isl_pw_aff_param_on_domain_id(domain, id)

    def is_empty(self) -> bool:
        return _isl_pw_aff_is_empty(self)

    def n_piece(self) -> int:
        return _isl_pw_aff_n_piece(self)

    def foreach_piece(self, fn: Any, aff: "Aff", user: Any, user_: Any = None) -> int:
        return _isl_pw_aff_foreach_piece(self, fn, aff, user, user_)

    def every_piece(self, test: Any, aff: "Aff", user: Any, user_: Any = None) -> bool:
        return _isl_pw_aff_every_piece(self, test, aff, user, user_)

    def to_union_pw_aff(self) -> "UnionPwAff":
        return _isl_pw_aff_to_union_pw_aff(self)

    def involves_param_id(self, id: "Id") -> bool:
        return _isl_pw_aff_involves_param_id(self, id)

    def involves_dims(self, type: int, first: int, n: int) -> bool:
        return _isl_pw_aff_involves_dims(self, type, first, n)

    def is_cst(self) -> bool:
        return _isl_pw_aff_is_cst(self)

    def involves_nan(self) -> bool:
        return _isl_pw_aff_involves_nan(self)

    def plain_is_equal(self, pwaff2: "PwAff") -> bool:
        return _isl_pw_aff_plain_is_equal(self, pwaff2)

    def is_equal(self, pa2: "PwAff") -> bool:
        return _isl_pw_aff_is_equal(self, pa2)

    def plain_cmp(self, pa2: "PwAff") -> int:
        return _isl_pw_aff_plain_cmp(self, pa2)

    def domain_reverse(self) -> "PwAff":
        return _isl_pw_aff_domain_reverse(self)

    def bind_domain(self, tuple: "MultiId") -> "PwAff":
        return _isl_pw_aff_bind_domain(self, tuple)

    def bind_domain_wrapped_domain(self, tuple: "MultiId") -> "PwAff":
        return _isl_pw_aff_bind_domain_wrapped_domain(self, tuple)

    def bind_id(self, id: "Id") -> "Set":
        return _isl_pw_aff_bind_id(self, id)

    def project_domain_on_params(self) -> "PwAff":
        return _isl_pw_aff_project_domain_on_params(self)

    def domain(self) -> "Set":
        return _isl_pw_aff_domain(self)

    def params(self) -> "Set":
        return _isl_pw_aff_params(self)

    def insert_domain(self, domain: "Space") -> "PwAff":
        return _isl_pw_aff_insert_domain(self, domain)

    def from_range(self) -> "PwAff":
        return _isl_pw_aff_from_range(self)

    def pos_set(self) -> "Set":
        return _isl_pw_aff_pos_set(self)

    def nonneg_set(self) -> "Set":
        return _isl_pw_aff_nonneg_set(self)

    def zero_set(self) -> "Set":
        return _isl_pw_aff_zero_set(self)

    def non_zero_set(self) -> "Set":
        return _isl_pw_aff_non_zero_set(self)

    def coalesce(self) -> "PwAff":
        return _isl_pw_aff_coalesce(self)

    def min_val(self) -> "Val":
        return _isl_pw_aff_min_val(self)

    def max_val(self) -> "Val":
        return _isl_pw_aff_max_val(self)

    def align_params(self, model: "Space") -> "PwAff":
        return _isl_pw_aff_align_params(self, model)

    def drop_unused_params(self) -> "PwAff":
        return _isl_pw_aff_drop_unused_params(self)

    def neg(self) -> "PwAff":
        return _isl_pw_aff_neg(self)

    def ceil(self) -> "PwAff":
        return _isl_pw_aff_ceil(self)

    def floor(self) -> "PwAff":
        return _isl_pw_aff_floor(self)

    def eval(self, pnt: "Point") -> "Val":
        return _isl_pw_aff_eval(self, pnt)

    def insert_dims(self, type: int, first: int, n: int) -> "PwAff":
        return _isl_pw_aff_insert_dims(self, type, first, n)

    def add_dims(self, type: int, n: int) -> "PwAff":
        return _isl_pw_aff_add_dims(self, type, n)

    def drop_dims(self, type: int, first: int, n: int) -> "PwAff":
        return _isl_pw_aff_drop_dims(self, type, first, n)

    def move_dims(self, dst_type: int, dst_pos: int, src_type: int, src_pos: int, n: int) -> "PwAff":
        return _isl_pw_aff_move_dims(self, dst_type, dst_pos, src_type, src_pos, n)

    def intersect_domain(self, set: "Set") -> "PwAff":
        return _isl_pw_aff_intersect_domain(self, set)

    def intersect_domain_wrapped_domain(self, set: "Set") -> "PwAff":
        return _isl_pw_aff_intersect_domain_wrapped_domain(self, set)

    def intersect_domain_wrapped_range(self, set: "Set") -> "PwAff":
        return _isl_pw_aff_intersect_domain_wrapped_range(self, set)

    def intersect_params(self, set: "Set") -> "PwAff":
        return _isl_pw_aff_intersect_params(self, set)

    def subtract_domain(self, set: "Set") -> "PwAff":
        return _isl_pw_aff_subtract_domain(self, set)

    def pullback_multi_aff(self, ma: "MultiAff") -> "PwAff":
        return _isl_pw_aff_pullback_multi_aff(self, ma)

    def pullback_pw_multi_aff(self, pma: "PwMultiAff") -> "PwAff":
        return _isl_pw_aff_pullback_pw_multi_aff(self, pma)

    def pullback_multi_pw_aff(self, mpa: "MultiPwAff") -> "PwAff":
        return _isl_pw_aff_pullback_multi_pw_aff(self, mpa)

    def eq_set(self, pwaff2: "PwAff") -> "Set":
        return _isl_pw_aff_eq_set(self, pwaff2)

    def ne_set(self, pwaff2: "PwAff") -> "Set":
        return _isl_pw_aff_ne_set(self, pwaff2)

    def le_set(self, pwaff2: "PwAff") -> "Set":
        return _isl_pw_aff_le_set(self, pwaff2)

    def lt_set(self, pwaff2: "PwAff") -> "Set":
        return _isl_pw_aff_lt_set(self, pwaff2)

    def ge_set(self, pwaff2: "PwAff") -> "Set":
        return _isl_pw_aff_ge_set(self, pwaff2)

    def gt_set(self, pwaff2: "PwAff") -> "Set":
        return _isl_pw_aff_gt_set(self, pwaff2)

    def eq_map(self, pa2: "PwAff") -> "Map":
        return _isl_pw_aff_eq_map(self, pa2)

    def le_map(self, pa2: "PwAff") -> "Map":
        return _isl_pw_aff_le_map(self, pa2)

    def lt_map(self, pa2: "PwAff") -> "Map":
        return _isl_pw_aff_lt_map(self, pa2)

    def ge_map(self, pa2: "PwAff") -> "Map":
        return _isl_pw_aff_ge_map(self, pa2)

    def gt_map(self, pa2: "PwAff") -> "Map":
        return _isl_pw_aff_gt_map(self, pa2)

    def gist_params(self, context: "Set") -> "PwAff":
        return _isl_pw_aff_gist_params(self, context)

    def gist(self, context: "Set") -> "PwAff":
        return _isl_pw_aff_gist(self, context)

    def add(self, pwaff2: "PwAff") -> "PwAff":
        return _isl_pw_aff_add(self, pwaff2)

    def add_constant_val(self, v: "Val") -> "PwAff":
        return _isl_pw_aff_add_constant_val(self, v)

    def min(self, pwaff2: "PwAff") -> "PwAff":
        return _isl_pw_aff_min(self, pwaff2)

    def max(self, pwaff2: "PwAff") -> "PwAff":
        return _isl_pw_aff_max(self, pwaff2)

    def sub(self, pwaff2: "PwAff") -> "PwAff":
        return _isl_pw_aff_sub(self, pwaff2)

    def union_add(self, pwaff2: "PwAff") -> "PwAff":
        return _isl_pw_aff_union_add(self, pwaff2)

    def union_min(self, pwaff2: "PwAff") -> "PwAff":
        return _isl_pw_aff_union_min(self, pwaff2)

    def union_max(self, pwaff2: "PwAff") -> "PwAff":
        return _isl_pw_aff_union_max(self, pwaff2)

    def mod_val(self, mod: "Val") -> "PwAff":
        return _isl_pw_aff_mod_val(self, mod)

    def scale_val(self, v: "Val") -> "PwAff":
        return _isl_pw_aff_scale_val(self, v)

    def scale_down_val(self, f: "Val") -> "PwAff":
        return _isl_pw_aff_scale_down_val(self, f)

    def mul(self, pwaff2: "PwAff") -> "PwAff":
        return _isl_pw_aff_mul(self, pwaff2)

    def div(self, pa2: "PwAff") -> "PwAff":
        return _isl_pw_aff_div(self, pa2)

    def tdiv_q(self, pa2: "PwAff") -> "PwAff":
        return _isl_pw_aff_tdiv_q(self, pa2)

    def tdiv_r(self, pa2: "PwAff") -> "PwAff":
        return _isl_pw_aff_tdiv_r(self, pa2)


register_type("PwAff", PwAff)

_isl_pw_aff_cond = ISLFunction.create(
    "isl_pw_aff_cond",
    Take("PwAff"),
    Take("PwAff"),
    Take("PwAff"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_get_ctx = ISLFunction.create(
    "isl_pw_aff_get_ctx",
    Keep("PwAff"),
    return_=Give("Context"),
    lib=_lib,
)

_isl_pw_aff_get_domain_space = ISLFunction.create(
    "isl_pw_aff_get_domain_space",
    Keep("PwAff"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_pw_aff_get_space = ISLFunction.create(
    "isl_pw_aff_get_space",
    Keep("PwAff"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_pw_aff_dim = ISLFunction.create(
    "isl_pw_aff_dim",
    Keep("PwAff"),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_set_dim_id = ISLFunction.create(
    "isl_pw_aff_set_dim_id",
    Take("PwAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Take("Id"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_has_dim_id = ISLFunction.create(
    "isl_pw_aff_has_dim_id",
    Keep("PwAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_get_dim_id = ISLFunction.create(
    "isl_pw_aff_get_dim_id",
    Keep("PwAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Give("Id"),
    lib=_lib,
)

_isl_pw_aff_get_dim_name = ISLFunction.create(
    "isl_pw_aff_get_dim_name",
    Keep("PwAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_pw_aff_find_dim_by_name = ISLFunction.create(
    "isl_pw_aff_find_dim_by_name",
    Keep("PwAff"),
    Param(int, ctype=c_int),
    Param(str, ctype=c_char_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_set_tuple_id = ISLFunction.create(
    "isl_pw_aff_set_tuple_id",
    Take("PwAff"),
    Param(int, ctype=c_int),
    Take("Id"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_reset_tuple_id = ISLFunction.create(
    "isl_pw_aff_reset_tuple_id",
    Take("PwAff"),
    Param(int, ctype=c_int),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_has_tuple_id = ISLFunction.create(
    "isl_pw_aff_has_tuple_id",
    Keep("PwAff"),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_get_tuple_id = ISLFunction.create(
    "isl_pw_aff_get_tuple_id",
    Keep("PwAff"),
    Param(int, ctype=c_int),
    return_=Give("Id"),
    lib=_lib,
)

_isl_pw_aff_reset_user = ISLFunction.create(
    "isl_pw_aff_reset_user",
    Take("PwAff"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_as_map = ISLFunction.create(
    "isl_pw_aff_as_map",
    Take("PwAff"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_pw_aff_empty = ISLFunction.create(
    "isl_pw_aff_empty",
    Take("Space"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_from_aff = ISLFunction.create(
    "isl_pw_aff_from_aff",
    Take("Aff"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_isa_aff = ISLFunction.create(
    "isl_pw_aff_isa_aff",
    Keep("PwAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_as_aff = ISLFunction.create(
    "isl_pw_aff_as_aff",
    Take("PwAff"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_pw_aff_alloc = ISLFunction.create(
    "isl_pw_aff_alloc",
    Take("Set"),
    Take("Aff"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_zero_on_domain = ISLFunction.create(
    "isl_pw_aff_zero_on_domain",
    Take("LocalSpace"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_var_on_domain = ISLFunction.create(
    "isl_pw_aff_var_on_domain",
    Take("LocalSpace"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_nan_on_domain_space = ISLFunction.create(
    "isl_pw_aff_nan_on_domain_space",
    Take("Space"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_nan_on_domain = ISLFunction.create(
    "isl_pw_aff_nan_on_domain",
    Take("LocalSpace"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_val_on_domain = ISLFunction.create(
    "isl_pw_aff_val_on_domain",
    Take("Set"),
    Take("Val"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_param_on_domain_id = ISLFunction.create(
    "isl_pw_aff_param_on_domain_id",
    Take("Set"),
    Take("Id"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_copy = ISLFunction.create(
    "isl_pw_aff_copy",
    Keep("PwAff"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_free = ISLFunction.create(
    "isl_pw_aff_free",
    Take("PwAff"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_is_empty = ISLFunction.create(
    "isl_pw_aff_is_empty",
    Keep("PwAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_n_piece = ISLFunction.create(
    "isl_pw_aff_n_piece",
    Keep("PwAff"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_foreach_piece = ISLFunction.create(
    "isl_pw_aff_foreach_piece",
    Keep("PwAff"),
    Param(None, ctype=c_void_p),
    Take("Aff"),
    Param(Any, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_every_piece = ISLFunction.create(
    "isl_pw_aff_every_piece",
    Keep("PwAff"),
    Param(None, ctype=c_void_p),
    Keep("Aff"),
    Param(Any, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_to_union_pw_aff = ISLFunction.create(
    "isl_pw_aff_to_union_pw_aff",
    Take("PwAff"),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_pw_aff_read_from_str = ISLFunction.create(
    "isl_pw_aff_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_to_str = ISLFunction.create(
    "isl_pw_aff_to_str",
    Keep("PwAff"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_pw_aff_involves_param_id = ISLFunction.create(
    "isl_pw_aff_involves_param_id",
    Keep("PwAff"),
    Keep("Id"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_involves_dims = ISLFunction.create(
    "isl_pw_aff_involves_dims",
    Keep("PwAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_is_cst = ISLFunction.create(
    "isl_pw_aff_is_cst",
    Keep("PwAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_involves_nan = ISLFunction.create(
    "isl_pw_aff_involves_nan",
    Keep("PwAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_plain_is_equal = ISLFunction.create(
    "isl_pw_aff_plain_is_equal",
    Keep("PwAff"),
    Keep("PwAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_is_equal = ISLFunction.create(
    "isl_pw_aff_is_equal",
    Keep("PwAff"),
    Keep("PwAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_plain_cmp = ISLFunction.create(
    "isl_pw_aff_plain_cmp",
    Keep("PwAff"),
    Keep("PwAff"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_domain_reverse = ISLFunction.create(
    "isl_pw_aff_domain_reverse",
    Take("PwAff"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_bind_domain = ISLFunction.create(
    "isl_pw_aff_bind_domain",
    Take("PwAff"),
    Take("MultiId"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_bind_domain_wrapped_domain = ISLFunction.create(
    "isl_pw_aff_bind_domain_wrapped_domain",
    Take("PwAff"),
    Take("MultiId"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_bind_id = ISLFunction.create(
    "isl_pw_aff_bind_id",
    Take("PwAff"),
    Take("Id"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_pw_aff_project_domain_on_params = ISLFunction.create(
    "isl_pw_aff_project_domain_on_params",
    Take("PwAff"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_domain = ISLFunction.create(
    "isl_pw_aff_domain",
    Take("PwAff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_pw_aff_params = ISLFunction.create(
    "isl_pw_aff_params",
    Take("PwAff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_pw_aff_insert_domain = ISLFunction.create(
    "isl_pw_aff_insert_domain",
    Take("PwAff"),
    Take("Space"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_from_range = ISLFunction.create(
    "isl_pw_aff_from_range",
    Take("PwAff"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_pos_set = ISLFunction.create(
    "isl_pw_aff_pos_set",
    Take("PwAff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_pw_aff_nonneg_set = ISLFunction.create(
    "isl_pw_aff_nonneg_set",
    Take("PwAff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_pw_aff_zero_set = ISLFunction.create(
    "isl_pw_aff_zero_set",
    Take("PwAff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_pw_aff_non_zero_set = ISLFunction.create(
    "isl_pw_aff_non_zero_set",
    Take("PwAff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_pw_aff_coalesce = ISLFunction.create(
    "isl_pw_aff_coalesce",
    Take("PwAff"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_min_val = ISLFunction.create(
    "isl_pw_aff_min_val",
    Take("PwAff"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_pw_aff_max_val = ISLFunction.create(
    "isl_pw_aff_max_val",
    Take("PwAff"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_pw_aff_align_params = ISLFunction.create(
    "isl_pw_aff_align_params",
    Take("PwAff"),
    Take("Space"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_drop_unused_params = ISLFunction.create(
    "isl_pw_aff_drop_unused_params",
    Take("PwAff"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_neg = ISLFunction.create(
    "isl_pw_aff_neg",
    Take("PwAff"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_ceil = ISLFunction.create(
    "isl_pw_aff_ceil",
    Take("PwAff"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_floor = ISLFunction.create(
    "isl_pw_aff_floor",
    Take("PwAff"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_eval = ISLFunction.create(
    "isl_pw_aff_eval",
    Take("PwAff"),
    Take("Point"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_pw_aff_insert_dims = ISLFunction.create(
    "isl_pw_aff_insert_dims",
    Take("PwAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_add_dims = ISLFunction.create(
    "isl_pw_aff_add_dims",
    Take("PwAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_drop_dims = ISLFunction.create(
    "isl_pw_aff_drop_dims",
    Take("PwAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_move_dims = ISLFunction.create(
    "isl_pw_aff_move_dims",
    Take("PwAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_intersect_domain = ISLFunction.create(
    "isl_pw_aff_intersect_domain",
    Take("PwAff"),
    Take("Set"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_intersect_domain_wrapped_domain = ISLFunction.create(
    "isl_pw_aff_intersect_domain_wrapped_domain",
    Take("PwAff"),
    Take("Set"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_intersect_domain_wrapped_range = ISLFunction.create(
    "isl_pw_aff_intersect_domain_wrapped_range",
    Take("PwAff"),
    Take("Set"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_intersect_params = ISLFunction.create(
    "isl_pw_aff_intersect_params",
    Take("PwAff"),
    Take("Set"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_subtract_domain = ISLFunction.create(
    "isl_pw_aff_subtract_domain",
    Take("PwAff"),
    Take("Set"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_pullback_multi_aff = ISLFunction.create(
    "isl_pw_aff_pullback_multi_aff",
    Take("PwAff"),
    Take("MultiAff"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_pullback_pw_multi_aff = ISLFunction.create(
    "isl_pw_aff_pullback_pw_multi_aff",
    Take("PwAff"),
    Take("PwMultiAff"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_pullback_multi_pw_aff = ISLFunction.create(
    "isl_pw_aff_pullback_multi_pw_aff",
    Take("PwAff"),
    Take("MultiPwAff"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_eq_set = ISLFunction.create(
    "isl_pw_aff_eq_set",
    Take("PwAff"),
    Take("PwAff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_pw_aff_ne_set = ISLFunction.create(
    "isl_pw_aff_ne_set",
    Take("PwAff"),
    Take("PwAff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_pw_aff_le_set = ISLFunction.create(
    "isl_pw_aff_le_set",
    Take("PwAff"),
    Take("PwAff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_pw_aff_lt_set = ISLFunction.create(
    "isl_pw_aff_lt_set",
    Take("PwAff"),
    Take("PwAff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_pw_aff_ge_set = ISLFunction.create(
    "isl_pw_aff_ge_set",
    Take("PwAff"),
    Take("PwAff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_pw_aff_gt_set = ISLFunction.create(
    "isl_pw_aff_gt_set",
    Take("PwAff"),
    Take("PwAff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_pw_aff_eq_map = ISLFunction.create(
    "isl_pw_aff_eq_map",
    Take("PwAff"),
    Take("PwAff"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_pw_aff_le_map = ISLFunction.create(
    "isl_pw_aff_le_map",
    Take("PwAff"),
    Take("PwAff"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_pw_aff_lt_map = ISLFunction.create(
    "isl_pw_aff_lt_map",
    Take("PwAff"),
    Take("PwAff"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_pw_aff_ge_map = ISLFunction.create(
    "isl_pw_aff_ge_map",
    Take("PwAff"),
    Take("PwAff"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_pw_aff_gt_map = ISLFunction.create(
    "isl_pw_aff_gt_map",
    Take("PwAff"),
    Take("PwAff"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_pw_aff_gist_params = ISLFunction.create(
    "isl_pw_aff_gist_params",
    Take("PwAff"),
    Take("Set"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_gist = ISLFunction.create(
    "isl_pw_aff_gist",
    Take("PwAff"),
    Take("Set"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_add = ISLFunction.create(
    "isl_pw_aff_add",
    Take("PwAff"),
    Take("PwAff"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_add_constant_val = ISLFunction.create(
    "isl_pw_aff_add_constant_val",
    Take("PwAff"),
    Take("Val"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_min = ISLFunction.create(
    "isl_pw_aff_min",
    Take("PwAff"),
    Take("PwAff"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_max = ISLFunction.create(
    "isl_pw_aff_max",
    Take("PwAff"),
    Take("PwAff"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_sub = ISLFunction.create(
    "isl_pw_aff_sub",
    Take("PwAff"),
    Take("PwAff"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_union_add = ISLFunction.create(
    "isl_pw_aff_union_add",
    Take("PwAff"),
    Take("PwAff"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_union_min = ISLFunction.create(
    "isl_pw_aff_union_min",
    Take("PwAff"),
    Take("PwAff"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_union_max = ISLFunction.create(
    "isl_pw_aff_union_max",
    Take("PwAff"),
    Take("PwAff"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_mod_val = ISLFunction.create(
    "isl_pw_aff_mod_val",
    Take("PwAff"),
    Take("Val"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_scale_val = ISLFunction.create(
    "isl_pw_aff_scale_val",
    Take("PwAff"),
    Take("Val"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_scale_down_val = ISLFunction.create(
    "isl_pw_aff_scale_down_val",
    Take("PwAff"),
    Take("Val"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_mul = ISLFunction.create(
    "isl_pw_aff_mul",
    Take("PwAff"),
    Take("PwAff"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_div = ISLFunction.create(
    "isl_pw_aff_div",
    Take("PwAff"),
    Take("PwAff"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_tdiv_q = ISLFunction.create(
    "isl_pw_aff_tdiv_q",
    Take("PwAff"),
    Take("PwAff"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_tdiv_r = ISLFunction.create(
    "isl_pw_aff_tdiv_r",
    Take("PwAff"),
    Take("PwAff"),
    return_=Give("PwAff"),
    lib=_lib,
)
