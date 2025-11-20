from __future__ import annotations

from ctypes import c_char_p, c_int, c_uint
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
    from .aff_list import AffList
    from .context import Context
    from .id import Id
    from .id_list import IdList
    from .map import Map
    from .multi_aff import MultiAff
    from .multi_id import MultiId
    from .multi_pw_aff import MultiPwAff
    from .multi_union_pw_aff import MultiUnionPwAff
    from .multi_val import MultiVal
    from .pw_aff_list import PwAffList
    from .pw_multi_aff import PwMultiAff
    from .set import Set
    from .union_pw_aff_list import UnionPwAffList
    from .val_list import ValList

_lib = load_libisl()

class Space(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_space_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_space_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_space_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_space_free(handle)

    def __str__(self) -> str:
        return _isl_space_to_str(self)

    def __repr__(self) -> str:
        return f"Space({self.__str__()})"

    @classmethod
    def unit(cls) -> "Space":
        return _isl_space_unit()

    @classmethod
    def alloc(cls, nparam: int, n_in: int, n_out: int) -> "Space":
        return _isl_space_alloc(nparam, n_in, n_out)

    @classmethod
    def params_alloc(cls, nparam: int) -> "Space":
        return _isl_space_params_alloc(nparam)

    @classmethod
    def set_alloc(cls, nparam: int, dim: int) -> "Space":
        return _isl_space_set_alloc(nparam, dim)

    def is_params(self) -> bool:
        return _isl_space_is_params(self)

    def is_set(self) -> bool:
        return _isl_space_is_set(self)

    def is_map(self) -> bool:
        return _isl_space_is_map(self)

    def is_equal(self, space2: "Space") -> bool:
        return _isl_space_is_equal(self, space2)

    def has_equal_params(self, space2: "Space") -> bool:
        return _isl_space_has_equal_params(self, space2)

    def has_equal_tuples(self, space2: "Space") -> bool:
        return _isl_space_has_equal_tuples(self, space2)

    def is_domain(self, space2: "Space") -> bool:
        return _isl_space_is_domain(self, space2)

    def is_range(self, space2: "Space") -> bool:
        return _isl_space_is_range(self, space2)

    def tuple_is_equal(self, type1: int, space2: "Space", type2: int) -> bool:
        return _isl_space_tuple_is_equal(self, type1, space2, type2)

    def dim(self, type: int) -> int:
        return _isl_space_dim(self, type)

    def add_param_id(self, id: "Id") -> "Space":
        return _isl_space_add_param_id(self, id)

    def drop_all_params(self) -> "Space":
        return _isl_space_drop_all_params(self)

    def set_dim_id(self, type: int, pos: int, id: "Id") -> "Space":
        return _isl_space_set_dim_id(self, type, pos, id)

    def has_dim_id(self, type: int, pos: int) -> bool:
        return _isl_space_has_dim_id(self, type, pos)

    def get_dim_id(self, type: int, pos: int) -> "Id":
        return _isl_space_get_dim_id(self, type, pos)

    def set_dim_name(self, type: int, pos: int, name: str) -> "Space":
        return _isl_space_set_dim_name(self, type, pos, name)

    def has_dim_name(self, type: int, pos: int) -> bool:
        return _isl_space_has_dim_name(self, type, pos)

    def get_dim_name(self, type: int, pos: int) -> str:
        return _isl_space_get_dim_name(self, type, pos)

    def find_dim_by_id(self, type: int, id: "Id") -> int:
        return _isl_space_find_dim_by_id(self, type, id)

    def find_dim_by_name(self, type: int, name: str) -> int:
        return _isl_space_find_dim_by_name(self, type, name)

    def set_domain_tuple_id(self, id: "Id") -> "Space":
        return _isl_space_set_domain_tuple_id(self, id)

    def set_range_tuple_id(self, id: "Id") -> "Space":
        return _isl_space_set_range_tuple_id(self, id)

    def set_tuple_id(self, type: int, id: "Id") -> "Space":
        return _isl_space_set_tuple_id(self, type, id)

    def reset_tuple_id(self, type: int) -> "Space":
        return _isl_space_reset_tuple_id(self, type)

    def has_domain_tuple_id(self) -> bool:
        return _isl_space_has_domain_tuple_id(self)

    def has_range_tuple_id(self) -> bool:
        return _isl_space_has_range_tuple_id(self)

    def has_tuple_id(self, type: int) -> bool:
        return _isl_space_has_tuple_id(self, type)

    def get_domain_tuple_id(self) -> "Id":
        return _isl_space_get_domain_tuple_id(self)

    def get_range_tuple_id(self) -> "Id":
        return _isl_space_get_range_tuple_id(self)

    def get_tuple_id(self, type: int) -> "Id":
        return _isl_space_get_tuple_id(self, type)

    def set_tuple_name(self, type: int, s: str) -> "Space":
        return _isl_space_set_tuple_name(self, type, s)

    def has_tuple_name(self, type: int) -> bool:
        return _isl_space_has_tuple_name(self, type)

    def get_tuple_name(self, type: int) -> str:
        return _isl_space_get_tuple_name(self, type)

    def reset_user(self) -> "Space":
        return _isl_space_reset_user(self)

    def universe_set(self) -> "Set":
        return _isl_space_universe_set(self)

    def universe_map(self) -> "Map":
        return _isl_space_universe_map(self)

    def zero_aff_on_domain(self) -> "Aff":
        return _isl_space_zero_aff_on_domain(self)

    def param_aff_on_domain_id(self, id: "Id") -> "Aff":
        return _isl_space_param_aff_on_domain_id(self, id)

    def zero_multi_val(self) -> "MultiVal":
        return _isl_space_zero_multi_val(self)

    def zero_multi_aff(self) -> "MultiAff":
        return _isl_space_zero_multi_aff(self)

    def zero_multi_pw_aff(self) -> "MultiPwAff":
        return _isl_space_zero_multi_pw_aff(self)

    def zero_multi_union_pw_aff(self) -> "MultiUnionPwAff":
        return _isl_space_zero_multi_union_pw_aff(self)

    def identity_multi_aff_on_domain(self) -> "MultiAff":
        return _isl_space_identity_multi_aff_on_domain(self)

    def identity_multi_pw_aff_on_domain(self) -> "MultiPwAff":
        return _isl_space_identity_multi_pw_aff_on_domain(self)

    def domain_map_multi_aff(self) -> "MultiAff":
        return _isl_space_domain_map_multi_aff(self)

    def range_map_multi_aff(self) -> "MultiAff":
        return _isl_space_range_map_multi_aff(self)

    def multi_id(self, list: "IdList") -> "MultiId":
        return _isl_space_multi_id(self, list)

    def multi_val(self, list: "ValList") -> "MultiVal":
        return _isl_space_multi_val(self, list)

    def multi_aff(self, list: "AffList") -> "MultiAff":
        return _isl_space_multi_aff(self, list)

    def multi_pw_aff(self, list: "PwAffList") -> "MultiPwAff":
        return _isl_space_multi_pw_aff(self, list)

    def multi_union_pw_aff(self, list: "UnionPwAffList") -> "MultiUnionPwAff":
        return _isl_space_multi_union_pw_aff(self, list)

    def multi_aff_on_domain_multi_val(self, mv: "MultiVal") -> "MultiAff":
        return _isl_space_multi_aff_on_domain_multi_val(self, mv)

    def identity_pw_multi_aff_on_domain(self) -> "PwMultiAff":
        return _isl_space_identity_pw_multi_aff_on_domain(self)

    def domain_map_pw_multi_aff(self) -> "PwMultiAff":
        return _isl_space_domain_map_pw_multi_aff(self)

    def range_map_pw_multi_aff(self) -> "PwMultiAff":
        return _isl_space_range_map_pw_multi_aff(self)

    def is_wrapping(self) -> bool:
        return _isl_space_is_wrapping(self)

    def domain_is_wrapping(self) -> bool:
        return _isl_space_domain_is_wrapping(self)

    def range_is_wrapping(self) -> bool:
        return _isl_space_range_is_wrapping(self)

    def is_product(self) -> bool:
        return _isl_space_is_product(self)

    def can_curry(self) -> bool:
        return _isl_space_can_curry(self)

    def can_uncurry(self) -> bool:
        return _isl_space_can_uncurry(self)

    def can_range_curry(self) -> bool:
        return _isl_space_can_range_curry(self)

    def reverse(self) -> "Space":
        return _isl_space_reverse(self)

    def wrapped_reverse(self) -> "Space":
        return _isl_space_wrapped_reverse(self)

    def domain_reverse(self) -> "Space":
        return _isl_space_domain_reverse(self)

    def range_reverse(self) -> "Space":
        return _isl_space_range_reverse(self)

    def domain(self) -> "Space":
        return _isl_space_domain(self)

    def range(self) -> "Space":
        return _isl_space_range(self)

    def params(self) -> "Space":
        return _isl_space_params(self)

    def domain_wrapped_domain(self) -> "Space":
        return _isl_space_domain_wrapped_domain(self)

    def domain_wrapped_range(self) -> "Space":
        return _isl_space_domain_wrapped_range(self)

    def range_wrapped_domain(self) -> "Space":
        return _isl_space_range_wrapped_domain(self)

    def range_wrapped_range(self) -> "Space":
        return _isl_space_range_wrapped_range(self)

    def domain_map(self) -> "Space":
        return _isl_space_domain_map(self)

    def range_map(self) -> "Space":
        return _isl_space_range_map(self)

    def add_unnamed_tuple_ui(self, dim: int) -> "Space":
        return _isl_space_add_unnamed_tuple_ui(self, dim)

    def add_named_tuple_id_ui(self, tuple_id: "Id", dim: int) -> "Space":
        return _isl_space_add_named_tuple_id_ui(self, tuple_id, dim)

    def set_from_params(self) -> "Space":
        return _isl_space_set_from_params(self)

    def from_domain(self) -> "Space":
        return _isl_space_from_domain(self)

    def from_range(self) -> "Space":
        return _isl_space_from_range(self)

    def map_from_set(self) -> "Space":
        return _isl_space_map_from_set(self)

    def map_from_domain_and_range(self, range: "Space") -> "Space":
        return _isl_space_map_from_domain_and_range(self, range)

    def wrap(self) -> "Space":
        return _isl_space_wrap(self)

    def unwrap(self) -> "Space":
        return _isl_space_unwrap(self)

    def flatten_domain(self) -> "Space":
        return _isl_space_flatten_domain(self)

    def flatten_range(self) -> "Space":
        return _isl_space_flatten_range(self)

    def zip(self) -> "Space":
        return _isl_space_zip(self)

    def curry(self) -> "Space":
        return _isl_space_curry(self)

    def uncurry(self) -> "Space":
        return _isl_space_uncurry(self)

    def range_curry(self) -> "Space":
        return _isl_space_range_curry(self)

    def align_params(self, space2: "Space") -> "Space":
        return _isl_space_align_params(self, space2)

    def add_dims(self, type: int, n: int) -> "Space":
        return _isl_space_add_dims(self, type, n)

    def insert_dims(self, type: int, pos: int, n: int) -> "Space":
        return _isl_space_insert_dims(self, type, pos, n)

    def drop_dims(self, type: int, first: int, n: int) -> "Space":
        return _isl_space_drop_dims(self, type, first, n)

    def move_dims(self, dst_type: int, dst_pos: int, src_type: int, src_pos: int, n: int) -> "Space":
        return _isl_space_move_dims(self, dst_type, dst_pos, src_type, src_pos, n)

    def join(self, right: "Space") -> "Space":
        return _isl_space_join(self, right)

    def product(self, space2: "Space") -> "Space":
        return _isl_space_product(self, space2)

    def domain_product(self, space2: "Space") -> "Space":
        return _isl_space_domain_product(self, space2)

    def range_product(self, space2: "Space") -> "Space":
        return _isl_space_range_product(self, space2)

    def factor_domain(self) -> "Space":
        return _isl_space_factor_domain(self)

    def factor_range(self) -> "Space":
        return _isl_space_factor_range(self)

    def domain_factor_domain(self) -> "Space":
        return _isl_space_domain_factor_domain(self)

    def domain_factor_range(self) -> "Space":
        return _isl_space_domain_factor_range(self)

    def range_factor_domain(self) -> "Space":
        return _isl_space_range_factor_domain(self)

    def range_factor_range(self) -> "Space":
        return _isl_space_range_factor_range(self)


register_type("Space", Space)

_isl_space_unit = ISLFunction.create(
    "isl_space_unit",
    Context(),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_alloc = ISLFunction.create(
    "isl_space_alloc",
    Context(),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_params_alloc = ISLFunction.create(
    "isl_space_params_alloc",
    Context(),
    Param(int, ctype=c_uint),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_set_alloc = ISLFunction.create(
    "isl_space_set_alloc",
    Context(),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_copy = ISLFunction.create(
    "isl_space_copy",
    Keep("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_free = ISLFunction.create(
    "isl_space_free",
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_is_params = ISLFunction.create(
    "isl_space_is_params",
    Keep("Space"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_space_is_set = ISLFunction.create(
    "isl_space_is_set",
    Keep("Space"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_space_is_map = ISLFunction.create(
    "isl_space_is_map",
    Keep("Space"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_space_is_equal = ISLFunction.create(
    "isl_space_is_equal",
    Keep("Space"),
    Keep("Space"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_space_has_equal_params = ISLFunction.create(
    "isl_space_has_equal_params",
    Keep("Space"),
    Keep("Space"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_space_has_equal_tuples = ISLFunction.create(
    "isl_space_has_equal_tuples",
    Keep("Space"),
    Keep("Space"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_space_is_domain = ISLFunction.create(
    "isl_space_is_domain",
    Keep("Space"),
    Keep("Space"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_space_is_range = ISLFunction.create(
    "isl_space_is_range",
    Keep("Space"),
    Keep("Space"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_space_tuple_is_equal = ISLFunction.create(
    "isl_space_tuple_is_equal",
    Keep("Space"),
    Param(int, ctype=c_int),
    Keep("Space"),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_space_dim = ISLFunction.create(
    "isl_space_dim",
    Keep("Space"),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_space_add_param_id = ISLFunction.create(
    "isl_space_add_param_id",
    Take("Space"),
    Take("Id"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_drop_all_params = ISLFunction.create(
    "isl_space_drop_all_params",
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_set_dim_id = ISLFunction.create(
    "isl_space_set_dim_id",
    Take("Space"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Take("Id"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_has_dim_id = ISLFunction.create(
    "isl_space_has_dim_id",
    Keep("Space"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_space_get_dim_id = ISLFunction.create(
    "isl_space_get_dim_id",
    Keep("Space"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Give("Id"),
    lib=_lib,
)

_isl_space_set_dim_name = ISLFunction.create(
    "isl_space_set_dim_name",
    Take("Space"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(str, ctype=c_char_p),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_has_dim_name = ISLFunction.create(
    "isl_space_has_dim_name",
    Keep("Space"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_space_get_dim_name = ISLFunction.create(
    "isl_space_get_dim_name",
    Keep("Space"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_space_find_dim_by_id = ISLFunction.create(
    "isl_space_find_dim_by_id",
    Keep("Space"),
    Param(int, ctype=c_int),
    Keep("Id"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_space_find_dim_by_name = ISLFunction.create(
    "isl_space_find_dim_by_name",
    Keep("Space"),
    Param(int, ctype=c_int),
    Param(str, ctype=c_char_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_space_set_domain_tuple_id = ISLFunction.create(
    "isl_space_set_domain_tuple_id",
    Take("Space"),
    Take("Id"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_set_range_tuple_id = ISLFunction.create(
    "isl_space_set_range_tuple_id",
    Take("Space"),
    Take("Id"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_set_tuple_id = ISLFunction.create(
    "isl_space_set_tuple_id",
    Take("Space"),
    Param(int, ctype=c_int),
    Take("Id"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_reset_tuple_id = ISLFunction.create(
    "isl_space_reset_tuple_id",
    Take("Space"),
    Param(int, ctype=c_int),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_has_domain_tuple_id = ISLFunction.create(
    "isl_space_has_domain_tuple_id",
    Keep("Space"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_space_has_range_tuple_id = ISLFunction.create(
    "isl_space_has_range_tuple_id",
    Keep("Space"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_space_has_tuple_id = ISLFunction.create(
    "isl_space_has_tuple_id",
    Keep("Space"),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_space_get_domain_tuple_id = ISLFunction.create(
    "isl_space_get_domain_tuple_id",
    Keep("Space"),
    return_=Give("Id"),
    lib=_lib,
)

_isl_space_get_range_tuple_id = ISLFunction.create(
    "isl_space_get_range_tuple_id",
    Keep("Space"),
    return_=Give("Id"),
    lib=_lib,
)

_isl_space_get_tuple_id = ISLFunction.create(
    "isl_space_get_tuple_id",
    Keep("Space"),
    Param(int, ctype=c_int),
    return_=Give("Id"),
    lib=_lib,
)

_isl_space_set_tuple_name = ISLFunction.create(
    "isl_space_set_tuple_name",
    Take("Space"),
    Param(int, ctype=c_int),
    Param(str, ctype=c_char_p),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_has_tuple_name = ISLFunction.create(
    "isl_space_has_tuple_name",
    Keep("Space"),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_space_get_tuple_name = ISLFunction.create(
    "isl_space_get_tuple_name",
    Keep("Space"),
    Param(int, ctype=c_int),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_space_reset_user = ISLFunction.create(
    "isl_space_reset_user",
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_universe_set = ISLFunction.create(
    "isl_space_universe_set",
    Take("Space"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_space_universe_map = ISLFunction.create(
    "isl_space_universe_map",
    Take("Space"),
    return_=Give("Map"),
    lib=_lib,
)

_isl_space_zero_aff_on_domain = ISLFunction.create(
    "isl_space_zero_aff_on_domain",
    Take("Space"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_space_param_aff_on_domain_id = ISLFunction.create(
    "isl_space_param_aff_on_domain_id",
    Take("Space"),
    Take("Id"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_space_zero_multi_val = ISLFunction.create(
    "isl_space_zero_multi_val",
    Take("Space"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_space_zero_multi_aff = ISLFunction.create(
    "isl_space_zero_multi_aff",
    Take("Space"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_space_zero_multi_pw_aff = ISLFunction.create(
    "isl_space_zero_multi_pw_aff",
    Take("Space"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_space_zero_multi_union_pw_aff = ISLFunction.create(
    "isl_space_zero_multi_union_pw_aff",
    Take("Space"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_space_identity_multi_aff_on_domain = ISLFunction.create(
    "isl_space_identity_multi_aff_on_domain",
    Take("Space"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_space_identity_multi_pw_aff_on_domain = ISLFunction.create(
    "isl_space_identity_multi_pw_aff_on_domain",
    Take("Space"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_space_domain_map_multi_aff = ISLFunction.create(
    "isl_space_domain_map_multi_aff",
    Take("Space"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_space_range_map_multi_aff = ISLFunction.create(
    "isl_space_range_map_multi_aff",
    Take("Space"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_space_multi_id = ISLFunction.create(
    "isl_space_multi_id",
    Take("Space"),
    Take("IdList"),
    return_=Give("MultiId"),
    lib=_lib,
)

_isl_space_multi_val = ISLFunction.create(
    "isl_space_multi_val",
    Take("Space"),
    Take("ValList"),
    return_=Give("MultiVal"),
    lib=_lib,
)

_isl_space_multi_aff = ISLFunction.create(
    "isl_space_multi_aff",
    Take("Space"),
    Take("AffList"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_space_multi_pw_aff = ISLFunction.create(
    "isl_space_multi_pw_aff",
    Take("Space"),
    Take("PwAffList"),
    return_=Give("MultiPwAff"),
    lib=_lib,
)

_isl_space_multi_union_pw_aff = ISLFunction.create(
    "isl_space_multi_union_pw_aff",
    Take("Space"),
    Take("UnionPwAffList"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_space_multi_aff_on_domain_multi_val = ISLFunction.create(
    "isl_space_multi_aff_on_domain_multi_val",
    Take("Space"),
    Take("MultiVal"),
    return_=Give("MultiAff"),
    lib=_lib,
)

_isl_space_identity_pw_multi_aff_on_domain = ISLFunction.create(
    "isl_space_identity_pw_multi_aff_on_domain",
    Take("Space"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_space_domain_map_pw_multi_aff = ISLFunction.create(
    "isl_space_domain_map_pw_multi_aff",
    Take("Space"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_space_range_map_pw_multi_aff = ISLFunction.create(
    "isl_space_range_map_pw_multi_aff",
    Take("Space"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_space_read_from_str = ISLFunction.create(
    "isl_space_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_to_str = ISLFunction.create(
    "isl_space_to_str",
    Keep("Space"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_space_is_wrapping = ISLFunction.create(
    "isl_space_is_wrapping",
    Keep("Space"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_space_domain_is_wrapping = ISLFunction.create(
    "isl_space_domain_is_wrapping",
    Keep("Space"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_space_range_is_wrapping = ISLFunction.create(
    "isl_space_range_is_wrapping",
    Keep("Space"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_space_is_product = ISLFunction.create(
    "isl_space_is_product",
    Keep("Space"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_space_can_curry = ISLFunction.create(
    "isl_space_can_curry",
    Keep("Space"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_space_can_uncurry = ISLFunction.create(
    "isl_space_can_uncurry",
    Keep("Space"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_space_can_range_curry = ISLFunction.create(
    "isl_space_can_range_curry",
    Keep("Space"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_space_reverse = ISLFunction.create(
    "isl_space_reverse",
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_wrapped_reverse = ISLFunction.create(
    "isl_space_wrapped_reverse",
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_domain_reverse = ISLFunction.create(
    "isl_space_domain_reverse",
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_range_reverse = ISLFunction.create(
    "isl_space_range_reverse",
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_domain = ISLFunction.create(
    "isl_space_domain",
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_range = ISLFunction.create(
    "isl_space_range",
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_params = ISLFunction.create(
    "isl_space_params",
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_domain_wrapped_domain = ISLFunction.create(
    "isl_space_domain_wrapped_domain",
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_domain_wrapped_range = ISLFunction.create(
    "isl_space_domain_wrapped_range",
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_range_wrapped_domain = ISLFunction.create(
    "isl_space_range_wrapped_domain",
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_range_wrapped_range = ISLFunction.create(
    "isl_space_range_wrapped_range",
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_domain_map = ISLFunction.create(
    "isl_space_domain_map",
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_range_map = ISLFunction.create(
    "isl_space_range_map",
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_add_unnamed_tuple_ui = ISLFunction.create(
    "isl_space_add_unnamed_tuple_ui",
    Take("Space"),
    Param(int, ctype=c_uint),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_add_named_tuple_id_ui = ISLFunction.create(
    "isl_space_add_named_tuple_id_ui",
    Take("Space"),
    Take("Id"),
    Param(int, ctype=c_uint),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_set_from_params = ISLFunction.create(
    "isl_space_set_from_params",
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_from_domain = ISLFunction.create(
    "isl_space_from_domain",
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_from_range = ISLFunction.create(
    "isl_space_from_range",
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_map_from_set = ISLFunction.create(
    "isl_space_map_from_set",
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_map_from_domain_and_range = ISLFunction.create(
    "isl_space_map_from_domain_and_range",
    Take("Space"),
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_wrap = ISLFunction.create(
    "isl_space_wrap",
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_unwrap = ISLFunction.create(
    "isl_space_unwrap",
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_flatten_domain = ISLFunction.create(
    "isl_space_flatten_domain",
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_flatten_range = ISLFunction.create(
    "isl_space_flatten_range",
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_zip = ISLFunction.create(
    "isl_space_zip",
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_curry = ISLFunction.create(
    "isl_space_curry",
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_uncurry = ISLFunction.create(
    "isl_space_uncurry",
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_range_curry = ISLFunction.create(
    "isl_space_range_curry",
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_align_params = ISLFunction.create(
    "isl_space_align_params",
    Take("Space"),
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_add_dims = ISLFunction.create(
    "isl_space_add_dims",
    Take("Space"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_insert_dims = ISLFunction.create(
    "isl_space_insert_dims",
    Take("Space"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_drop_dims = ISLFunction.create(
    "isl_space_drop_dims",
    Take("Space"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_move_dims = ISLFunction.create(
    "isl_space_move_dims",
    Take("Space"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_join = ISLFunction.create(
    "isl_space_join",
    Take("Space"),
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_product = ISLFunction.create(
    "isl_space_product",
    Take("Space"),
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_domain_product = ISLFunction.create(
    "isl_space_domain_product",
    Take("Space"),
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_range_product = ISLFunction.create(
    "isl_space_range_product",
    Take("Space"),
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_factor_domain = ISLFunction.create(
    "isl_space_factor_domain",
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_factor_range = ISLFunction.create(
    "isl_space_factor_range",
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_domain_factor_domain = ISLFunction.create(
    "isl_space_domain_factor_domain",
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_domain_factor_range = ISLFunction.create(
    "isl_space_domain_factor_range",
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_range_factor_domain = ISLFunction.create(
    "isl_space_range_factor_domain",
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_space_range_factor_range = ISLFunction.create(
    "isl_space_range_factor_range",
    Take("Space"),
    return_=Give("Space"),
    lib=_lib,
)
