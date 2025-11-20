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
    from .basic_set import BasicSet
    from .context import Context
    from .id import Id
    from .local_space import LocalSpace
    from .multi_aff import MultiAff
    from .multi_id import MultiId
    from .point import Point
    from .set import Set
    from .space import Space
    from .val import Val

_lib = load_libisl()

class Aff(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_aff_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_aff_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_aff_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_aff_free(handle)

    def __str__(self) -> str:
        return _isl_aff_to_str(self)

    def __repr__(self) -> str:
        return f"Aff({self.__str__()})"

    def get_ctx(self) -> "Context":
        return _isl_aff_get_ctx(self)

    def get_domain_space(self) -> "Space":
        return _isl_aff_get_domain_space(self)

    def get_space(self) -> "Space":
        return _isl_aff_get_space(self)

    def dim(self, type: int) -> int:
        return _isl_aff_dim(self, type)

    def set_dim_id(self, type: int, pos: int, id: "Id") -> "Aff":
        return _isl_aff_set_dim_id(self, type, pos, id)

    def set_dim_name(self, type: int, pos: int, s: str) -> "Aff":
        return _isl_aff_set_dim_name(self, type, pos, s)

    def get_dim_name(self, type: int, pos: int) -> str:
        return _isl_aff_get_dim_name(self, type, pos)

    def find_dim_by_name(self, type: int, name: str) -> int:
        return _isl_aff_find_dim_by_name(self, type, name)

    def set_tuple_id(self, type: int, id: "Id") -> "Aff":
        return _isl_aff_set_tuple_id(self, type, id)

    def get_domain_local_space(self) -> "LocalSpace":
        return _isl_aff_get_domain_local_space(self)

    def get_local_space(self) -> "LocalSpace":
        return _isl_aff_get_local_space(self)

    @classmethod
    def zero_on_domain_space(cls, space: "Space") -> "Aff":
        return _isl_aff_zero_on_domain_space(space)

    @classmethod
    def zero_on_domain(cls, ls: "LocalSpace") -> "Aff":
        return _isl_aff_zero_on_domain(ls)

    @classmethod
    def val_on_domain(cls, ls: "LocalSpace", val: "Val") -> "Aff":
        return _isl_aff_val_on_domain(ls, val)

    @classmethod
    def param_on_domain_space_id(cls, space: "Space", id: "Id") -> "Aff":
        return _isl_aff_param_on_domain_space_id(space, id)

    @classmethod
    def var_on_domain(cls, ls: "LocalSpace", type: int, pos: int) -> "Aff":
        return _isl_aff_var_on_domain(ls, type, pos)

    @classmethod
    def nan_on_domain_space(cls, space: "Space") -> "Aff":
        return _isl_aff_nan_on_domain_space(space)

    @classmethod
    def nan_on_domain(cls, ls: "LocalSpace") -> "Aff":
        return _isl_aff_nan_on_domain(ls)

    def get_constant_val(self) -> "Val":
        return _isl_aff_get_constant_val(self)

    def get_coefficient_val(self, type: int, pos: int) -> "Val":
        return _isl_aff_get_coefficient_val(self, type, pos)

    def coefficient_sgn(self, type: int, pos: int) -> int:
        return _isl_aff_coefficient_sgn(self, type, pos)

    def get_denominator_val(self) -> "Val":
        return _isl_aff_get_denominator_val(self)

    def get_div(self, pos: int) -> "Aff":
        return _isl_aff_get_div(self, pos)

    def set_constant_si(self, v: int) -> "Aff":
        return _isl_aff_set_constant_si(self, v)

    def set_constant_val(self, v: "Val") -> "Aff":
        return _isl_aff_set_constant_val(self, v)

    def set_coefficient_si(self, type: int, pos: int, v: int) -> "Aff":
        return _isl_aff_set_coefficient_si(self, type, pos, v)

    def set_coefficient_val(self, type: int, pos: int, v: "Val") -> "Aff":
        return _isl_aff_set_coefficient_val(self, type, pos, v)

    def add_constant_si(self, v: int) -> "Aff":
        return _isl_aff_add_constant_si(self, v)

    def add_constant_val(self, v: "Val") -> "Aff":
        return _isl_aff_add_constant_val(self, v)

    def add_constant_num_si(self, v: int) -> "Aff":
        return _isl_aff_add_constant_num_si(self, v)

    def add_coefficient_si(self, type: int, pos: int, v: int) -> "Aff":
        return _isl_aff_add_coefficient_si(self, type, pos, v)

    def add_coefficient_val(self, type: int, pos: int, v: "Val") -> "Aff":
        return _isl_aff_add_coefficient_val(self, type, pos, v)

    def involves_locals(self) -> bool:
        return _isl_aff_involves_locals(self)

    def involves_dims(self, type: int, first: int, n: int) -> bool:
        return _isl_aff_involves_dims(self, type, first, n)

    def is_cst(self) -> bool:
        return _isl_aff_is_cst(self)

    def is_nan(self) -> bool:
        return _isl_aff_is_nan(self)

    def plain_is_zero(self) -> bool:
        return _isl_aff_plain_is_zero(self)

    def plain_is_equal(self, aff2: "Aff") -> bool:
        return _isl_aff_plain_is_equal(self, aff2)

    def domain_reverse(self) -> "Aff":
        return _isl_aff_domain_reverse(self)

    def bind_id(self, id: "Id") -> "BasicSet":
        return _isl_aff_bind_id(self, id)

    def project_domain_on_params(self) -> "Aff":
        return _isl_aff_project_domain_on_params(self)

    def unbind_params_insert_domain(self, domain: "MultiId") -> "Aff":
        return _isl_aff_unbind_params_insert_domain(self, domain)

    def from_range(self) -> "Aff":
        return _isl_aff_from_range(self)

    def zero_basic_set(self) -> "BasicSet":
        return _isl_aff_zero_basic_set(self)

    def neg_basic_set(self) -> "BasicSet":
        return _isl_aff_neg_basic_set(self)

    def align_params(self, model: "Space") -> "Aff":
        return _isl_aff_align_params(self, model)

    def neg(self) -> "Aff":
        return _isl_aff_neg(self)

    def ceil(self) -> "Aff":
        return _isl_aff_ceil(self)

    def floor(self) -> "Aff":
        return _isl_aff_floor(self)

    def eval(self, pnt: "Point") -> "Val":
        return _isl_aff_eval(self, pnt)

    def insert_dims(self, type: int, first: int, n: int) -> "Aff":
        return _isl_aff_insert_dims(self, type, first, n)

    def add_dims(self, type: int, n: int) -> "Aff":
        return _isl_aff_add_dims(self, type, n)

    def drop_dims(self, type: int, first: int, n: int) -> "Aff":
        return _isl_aff_drop_dims(self, type, first, n)

    def move_dims(self, dst_type: int, dst_pos: int, src_type: int, src_pos: int, n: int) -> "Aff":
        return _isl_aff_move_dims(self, dst_type, dst_pos, src_type, src_pos, n)

    def pullback_aff(self, aff2: "Aff") -> "Aff":
        return _isl_aff_pullback_aff(self, aff2)

    def pullback_multi_aff(self, ma: "MultiAff") -> "Aff":
        return _isl_aff_pullback_multi_aff(self, ma)

    def eq_basic_set(self, aff2: "Aff") -> "BasicSet":
        return _isl_aff_eq_basic_set(self, aff2)

    def eq_set(self, aff2: "Aff") -> "Set":
        return _isl_aff_eq_set(self, aff2)

    def ne_set(self, aff2: "Aff") -> "Set":
        return _isl_aff_ne_set(self, aff2)

    def le_basic_set(self, aff2: "Aff") -> "BasicSet":
        return _isl_aff_le_basic_set(self, aff2)

    def le_set(self, aff2: "Aff") -> "Set":
        return _isl_aff_le_set(self, aff2)

    def lt_basic_set(self, aff2: "Aff") -> "BasicSet":
        return _isl_aff_lt_basic_set(self, aff2)

    def lt_set(self, aff2: "Aff") -> "Set":
        return _isl_aff_lt_set(self, aff2)

    def ge_set(self, aff2: "Aff") -> "Set":
        return _isl_aff_ge_set(self, aff2)

    def gt_basic_set(self, aff2: "Aff") -> "BasicSet":
        return _isl_aff_gt_basic_set(self, aff2)

    def gt_set(self, aff2: "Aff") -> "Set":
        return _isl_aff_gt_set(self, aff2)

    def gist_params(self, context: "Set") -> "Aff":
        return _isl_aff_gist_params(self, context)

    def gist(self, context: "Set") -> "Aff":
        return _isl_aff_gist(self, context)

    def add(self, aff2: "Aff") -> "Aff":
        return _isl_aff_add(self, aff2)

    def sub(self, aff2: "Aff") -> "Aff":
        return _isl_aff_sub(self, aff2)

    def mod_val(self, mod: "Val") -> "Aff":
        return _isl_aff_mod_val(self, mod)

    def scale_val(self, v: "Val") -> "Aff":
        return _isl_aff_scale_val(self, v)

    def scale_down_ui(self, f: int) -> "Aff":
        return _isl_aff_scale_down_ui(self, f)

    def scale_down_val(self, v: "Val") -> "Aff":
        return _isl_aff_scale_down_val(self, v)

    def mul(self, aff2: "Aff") -> "Aff":
        return _isl_aff_mul(self, aff2)

    def div(self, aff2: "Aff") -> "Aff":
        return _isl_aff_div(self, aff2)


register_type("Aff", Aff)

_isl_aff_get_ctx = ISLFunction.create(
    "isl_aff_get_ctx",
    Keep("Aff"),
    return_=Give("Context"),
    lib=_lib,
)

_isl_aff_get_domain_space = ISLFunction.create(
    "isl_aff_get_domain_space",
    Keep("Aff"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_aff_get_space = ISLFunction.create(
    "isl_aff_get_space",
    Keep("Aff"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_aff_dim = ISLFunction.create(
    "isl_aff_dim",
    Keep("Aff"),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_aff_set_dim_id = ISLFunction.create(
    "isl_aff_set_dim_id",
    Take("Aff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Take("Id"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_set_dim_name = ISLFunction.create(
    "isl_aff_set_dim_name",
    Take("Aff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(str, ctype=c_char_p),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_get_dim_name = ISLFunction.create(
    "isl_aff_get_dim_name",
    Keep("Aff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_aff_find_dim_by_name = ISLFunction.create(
    "isl_aff_find_dim_by_name",
    Keep("Aff"),
    Param(int, ctype=c_int),
    Param(str, ctype=c_char_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_aff_set_tuple_id = ISLFunction.create(
    "isl_aff_set_tuple_id",
    Take("Aff"),
    Param(int, ctype=c_int),
    Take("Id"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_get_domain_local_space = ISLFunction.create(
    "isl_aff_get_domain_local_space",
    Keep("Aff"),
    return_=Give("LocalSpace"),
    lib=_lib,
)

_isl_aff_get_local_space = ISLFunction.create(
    "isl_aff_get_local_space",
    Keep("Aff"),
    return_=Give("LocalSpace"),
    lib=_lib,
)

_isl_aff_zero_on_domain_space = ISLFunction.create(
    "isl_aff_zero_on_domain_space",
    Take("Space"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_zero_on_domain = ISLFunction.create(
    "isl_aff_zero_on_domain",
    Take("LocalSpace"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_val_on_domain = ISLFunction.create(
    "isl_aff_val_on_domain",
    Take("LocalSpace"),
    Take("Val"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_param_on_domain_space_id = ISLFunction.create(
    "isl_aff_param_on_domain_space_id",
    Take("Space"),
    Take("Id"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_var_on_domain = ISLFunction.create(
    "isl_aff_var_on_domain",
    Take("LocalSpace"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_nan_on_domain_space = ISLFunction.create(
    "isl_aff_nan_on_domain_space",
    Take("Space"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_nan_on_domain = ISLFunction.create(
    "isl_aff_nan_on_domain",
    Take("LocalSpace"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_copy = ISLFunction.create(
    "isl_aff_copy",
    Keep("Aff"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_free = ISLFunction.create(
    "isl_aff_free",
    Take("Aff"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_get_constant_val = ISLFunction.create(
    "isl_aff_get_constant_val",
    Keep("Aff"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_aff_get_coefficient_val = ISLFunction.create(
    "isl_aff_get_coefficient_val",
    Keep("Aff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give("Val"),
    lib=_lib,
)

_isl_aff_coefficient_sgn = ISLFunction.create(
    "isl_aff_coefficient_sgn",
    Keep("Aff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_aff_get_denominator_val = ISLFunction.create(
    "isl_aff_get_denominator_val",
    Keep("Aff"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_aff_get_div = ISLFunction.create(
    "isl_aff_get_div",
    Keep("Aff"),
    Param(int, ctype=c_int),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_set_constant_si = ISLFunction.create(
    "isl_aff_set_constant_si",
    Take("Aff"),
    Param(int, ctype=c_int),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_set_constant_val = ISLFunction.create(
    "isl_aff_set_constant_val",
    Take("Aff"),
    Take("Val"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_set_coefficient_si = ISLFunction.create(
    "isl_aff_set_coefficient_si",
    Take("Aff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_set_coefficient_val = ISLFunction.create(
    "isl_aff_set_coefficient_val",
    Take("Aff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Take("Val"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_add_constant_si = ISLFunction.create(
    "isl_aff_add_constant_si",
    Take("Aff"),
    Param(int, ctype=c_int),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_add_constant_val = ISLFunction.create(
    "isl_aff_add_constant_val",
    Take("Aff"),
    Take("Val"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_add_constant_num_si = ISLFunction.create(
    "isl_aff_add_constant_num_si",
    Take("Aff"),
    Param(int, ctype=c_int),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_add_coefficient_si = ISLFunction.create(
    "isl_aff_add_coefficient_si",
    Take("Aff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_add_coefficient_val = ISLFunction.create(
    "isl_aff_add_coefficient_val",
    Take("Aff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Take("Val"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_read_from_str = ISLFunction.create(
    "isl_aff_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_to_str = ISLFunction.create(
    "isl_aff_to_str",
    Keep("Aff"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_aff_involves_locals = ISLFunction.create(
    "isl_aff_involves_locals",
    Keep("Aff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_aff_involves_dims = ISLFunction.create(
    "isl_aff_involves_dims",
    Keep("Aff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_aff_is_cst = ISLFunction.create(
    "isl_aff_is_cst",
    Keep("Aff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_aff_is_nan = ISLFunction.create(
    "isl_aff_is_nan",
    Keep("Aff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_aff_plain_is_zero = ISLFunction.create(
    "isl_aff_plain_is_zero",
    Keep("Aff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_aff_plain_is_equal = ISLFunction.create(
    "isl_aff_plain_is_equal",
    Keep("Aff"),
    Keep("Aff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_aff_domain_reverse = ISLFunction.create(
    "isl_aff_domain_reverse",
    Take("Aff"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_bind_id = ISLFunction.create(
    "isl_aff_bind_id",
    Take("Aff"),
    Take("Id"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_aff_project_domain_on_params = ISLFunction.create(
    "isl_aff_project_domain_on_params",
    Take("Aff"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_unbind_params_insert_domain = ISLFunction.create(
    "isl_aff_unbind_params_insert_domain",
    Take("Aff"),
    Take("MultiId"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_from_range = ISLFunction.create(
    "isl_aff_from_range",
    Take("Aff"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_zero_basic_set = ISLFunction.create(
    "isl_aff_zero_basic_set",
    Take("Aff"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_aff_neg_basic_set = ISLFunction.create(
    "isl_aff_neg_basic_set",
    Take("Aff"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_aff_align_params = ISLFunction.create(
    "isl_aff_align_params",
    Take("Aff"),
    Take("Space"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_neg = ISLFunction.create(
    "isl_aff_neg",
    Take("Aff"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_ceil = ISLFunction.create(
    "isl_aff_ceil",
    Take("Aff"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_floor = ISLFunction.create(
    "isl_aff_floor",
    Take("Aff"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_eval = ISLFunction.create(
    "isl_aff_eval",
    Take("Aff"),
    Take("Point"),
    return_=Give("Val"),
    lib=_lib,
)

_isl_aff_insert_dims = ISLFunction.create(
    "isl_aff_insert_dims",
    Take("Aff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_add_dims = ISLFunction.create(
    "isl_aff_add_dims",
    Take("Aff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_drop_dims = ISLFunction.create(
    "isl_aff_drop_dims",
    Take("Aff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_move_dims = ISLFunction.create(
    "isl_aff_move_dims",
    Take("Aff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_pullback_aff = ISLFunction.create(
    "isl_aff_pullback_aff",
    Take("Aff"),
    Take("Aff"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_pullback_multi_aff = ISLFunction.create(
    "isl_aff_pullback_multi_aff",
    Take("Aff"),
    Take("MultiAff"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_eq_basic_set = ISLFunction.create(
    "isl_aff_eq_basic_set",
    Take("Aff"),
    Take("Aff"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_aff_eq_set = ISLFunction.create(
    "isl_aff_eq_set",
    Take("Aff"),
    Take("Aff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_aff_ne_set = ISLFunction.create(
    "isl_aff_ne_set",
    Take("Aff"),
    Take("Aff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_aff_le_basic_set = ISLFunction.create(
    "isl_aff_le_basic_set",
    Take("Aff"),
    Take("Aff"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_aff_le_set = ISLFunction.create(
    "isl_aff_le_set",
    Take("Aff"),
    Take("Aff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_aff_lt_basic_set = ISLFunction.create(
    "isl_aff_lt_basic_set",
    Take("Aff"),
    Take("Aff"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_aff_lt_set = ISLFunction.create(
    "isl_aff_lt_set",
    Take("Aff"),
    Take("Aff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_aff_ge_set = ISLFunction.create(
    "isl_aff_ge_set",
    Take("Aff"),
    Take("Aff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_aff_gt_basic_set = ISLFunction.create(
    "isl_aff_gt_basic_set",
    Take("Aff"),
    Take("Aff"),
    return_=Give("BasicSet"),
    lib=_lib,
)

_isl_aff_gt_set = ISLFunction.create(
    "isl_aff_gt_set",
    Take("Aff"),
    Take("Aff"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_aff_gist_params = ISLFunction.create(
    "isl_aff_gist_params",
    Take("Aff"),
    Take("Set"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_gist = ISLFunction.create(
    "isl_aff_gist",
    Take("Aff"),
    Take("Set"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_add = ISLFunction.create(
    "isl_aff_add",
    Take("Aff"),
    Take("Aff"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_sub = ISLFunction.create(
    "isl_aff_sub",
    Take("Aff"),
    Take("Aff"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_mod_val = ISLFunction.create(
    "isl_aff_mod_val",
    Take("Aff"),
    Take("Val"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_scale_val = ISLFunction.create(
    "isl_aff_scale_val",
    Take("Aff"),
    Take("Val"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_scale_down_ui = ISLFunction.create(
    "isl_aff_scale_down_ui",
    Take("Aff"),
    Param(int, ctype=c_uint),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_scale_down_val = ISLFunction.create(
    "isl_aff_scale_down_val",
    Take("Aff"),
    Take("Val"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_mul = ISLFunction.create(
    "isl_aff_mul",
    Take("Aff"),
    Take("Aff"),
    return_=Give("Aff"),
    lib=_lib,
)

_isl_aff_div = ISLFunction.create(
    "isl_aff_div",
    Take("Aff"),
    Take("Aff"),
    return_=Give("Aff"),
    lib=_lib,
)
