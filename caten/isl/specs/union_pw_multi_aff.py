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
    from .multi_aff import MultiAff
    from .multi_union_pw_aff import MultiUnionPwAff
    from .multi_val import MultiVal
    from .pw_multi_aff import PwMultiAff
    from .pw_multi_aff_list import PwMultiAffList
    from .set import Set
    from .space import Space
    from .union_map import UnionMap
    from .union_pw_aff import UnionPwAff
    from .union_set import UnionSet
    from .val import Val

_lib = load_libisl()

class UnionPwMultiAff(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_union_pw_multi_aff_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_union_pw_multi_aff_read_from_str(spec)

    def copy_handle(self) -> Any:
        return _isl_union_pw_multi_aff_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_union_pw_multi_aff_free(handle)

    def __str__(self) -> str:
        return _isl_union_pw_multi_aff_to_str(self)

    def __repr__(self) -> str:
        return f"UnionPwMultiAff({self.__str__()})"

    def get_ctx(self) -> "Context":
        return _isl_union_pw_multi_aff_get_ctx(self)

    def get_space(self) -> "Space":
        return _isl_union_pw_multi_aff_get_space(self)

    def dim(self, type: int) -> int:
        return _isl_union_pw_multi_aff_dim(self, type)

    def set_dim_name(self, type: int, pos: int, s: str) -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_set_dim_name(self, type, pos, s)

    def find_dim_by_name(self, type: int, name: str) -> int:
        return _isl_union_pw_multi_aff_find_dim_by_name(self, type, name)

    def reset_user(self) -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_reset_user(self)

    def as_union_map(self) -> "UnionMap":
        return _isl_union_pw_multi_aff_as_union_map(self)

    def as_multi_union_pw_aff(self) -> "MultiUnionPwAff":
        return _isl_union_pw_multi_aff_as_multi_union_pw_aff(self)

    @classmethod
    def from_multi_union_pw_aff(cls, mupa: "MultiUnionPwAff") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_from_multi_union_pw_aff(mupa)

    @classmethod
    def empty_ctx(cls) -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_empty_ctx()

    @classmethod
    def empty_space(cls, space: "Space") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_empty_space(space)

    @classmethod
    def empty(cls, space: "Space") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_empty(space)

    @classmethod
    def from_aff(cls, aff: "Aff") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_from_aff(aff)

    @classmethod
    def from_pw_multi_aff(cls, pma: "PwMultiAff") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_from_pw_multi_aff(pma)

    def isa_pw_multi_aff(self) -> bool:
        return _isl_union_pw_multi_aff_isa_pw_multi_aff(self)

    def as_pw_multi_aff(self) -> "PwMultiAff":
        return _isl_union_pw_multi_aff_as_pw_multi_aff(self)

    @classmethod
    def from_multi_aff(cls, ma: "MultiAff") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_from_multi_aff(ma)

    @classmethod
    def from_union_pw_aff(cls, upa: "UnionPwAff") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_from_union_pw_aff(upa)

    def get_union_pw_aff(self, pos: int) -> "UnionPwAff":
        return _isl_union_pw_multi_aff_get_union_pw_aff(self, pos)

    @classmethod
    def multi_val_on_domain(cls, domain: "UnionSet", mv: "MultiVal") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_multi_val_on_domain(domain, mv)

    def add_pw_multi_aff(self, pma: "PwMultiAff") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_add_pw_multi_aff(self, pma)

    def n_pw_multi_aff(self) -> int:
        return _isl_union_pw_multi_aff_n_pw_multi_aff(self)

    def foreach_pw_multi_aff(self, fn: Any, user: Any, user_: Any = None) -> int:
        return _isl_union_pw_multi_aff_foreach_pw_multi_aff(self, fn, user, user_)

    def every_pw_multi_aff(self, test: Any, user: Any, user_: Any = None) -> bool:
        return _isl_union_pw_multi_aff_every_pw_multi_aff(self, test, user, user_)

    def extract_pw_multi_aff(self, space: "Space") -> "PwMultiAff":
        return _isl_union_pw_multi_aff_extract_pw_multi_aff(self, space)

    def get_pw_multi_aff_list(self) -> "PwMultiAffList":
        return _isl_union_pw_multi_aff_get_pw_multi_aff_list(self)

    def plain_is_empty(self) -> bool:
        return _isl_union_pw_multi_aff_plain_is_empty(self)

    def involves_locals(self) -> bool:
        return _isl_union_pw_multi_aff_involves_locals(self)

    def involves_nan(self) -> bool:
        return _isl_union_pw_multi_aff_involves_nan(self)

    def plain_is_equal(self, upma2: "UnionPwMultiAff") -> bool:
        return _isl_union_pw_multi_aff_plain_is_equal(self, upma2)

    def domain(self) -> "UnionSet":
        return _isl_union_pw_multi_aff_domain(self)

    @classmethod
    def from_domain(cls, uset: "UnionSet") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_from_domain(uset)

    @classmethod
    def from_union_set(cls, uset: "UnionSet") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_from_union_set(uset)

    @classmethod
    def from_union_map(cls, umap: "UnionMap") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_from_union_map(umap)

    def coalesce(self) -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_coalesce(self)

    def align_params(self, model: "Space") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_align_params(self, model)

    def drop_unused_params(self) -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_drop_unused_params(self)

    def neg(self) -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_neg(self)

    def drop_dims(self, type: int, first: int, n: int) -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_drop_dims(self, type, first, n)

    def intersect_domain_space(self, space: "Space") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_intersect_domain_space(self, space)

    def intersect_domain_union_set(self, uset: "UnionSet") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_intersect_domain_union_set(self, uset)

    def intersect_domain(self, uset: "UnionSet") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_intersect_domain(self, uset)

    def intersect_domain_wrapped_domain(self, uset: "UnionSet") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_intersect_domain_wrapped_domain(self, uset)

    def intersect_domain_wrapped_range(self, uset: "UnionSet") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_intersect_domain_wrapped_range(self, uset)

    def intersect_params(self, set: "Set") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_intersect_params(self, set)

    def subtract_domain_union_set(self, set: "Set") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_subtract_domain_union_set(self, set)

    def subtract_domain_space(self, space: "Space") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_subtract_domain_space(self, space)

    def subtract_domain(self, uset: "UnionSet") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_subtract_domain(self, uset)

    def apply_union_pw_multi_aff(self, upma2: "UnionPwMultiAff") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_apply_union_pw_multi_aff(self, upma2)

    def preimage_domain_wrapped_domain_union_pw_multi_aff(self, upma2: "UnionPwMultiAff") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_preimage_domain_wrapped_domain_union_pw_multi_aff(self, upma2)

    def pullback_union_pw_multi_aff(self, upma2: "UnionPwMultiAff") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_pullback_union_pw_multi_aff(self, upma2)

    def range_product(self, upma2: "UnionPwMultiAff") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_range_product(self, upma2)

    def flat_range_product(self, upma2: "UnionPwMultiAff") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_flat_range_product(self, upma2)

    def range_factor_domain(self) -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_range_factor_domain(self)

    def range_factor_range(self) -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_range_factor_range(self)

    def gist_params(self, context: "Set") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_gist_params(self, context)

    def gist(self, context: "UnionSet") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_gist(self, context)

    def add(self, upma2: "UnionPwMultiAff") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_add(self, upma2)

    def sub(self, upma2: "UnionPwMultiAff") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_sub(self, upma2)

    def union_add(self, upma2: "UnionPwMultiAff") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_union_add(self, upma2)

    def scale_val(self, val: "Val") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_scale_val(self, val)

    def scale_down_val(self, val: "Val") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_scale_down_val(self, val)

    def scale_multi_val(self, mv: "MultiVal") -> "UnionPwMultiAff":
        return _isl_union_pw_multi_aff_scale_multi_val(self, mv)


register_type("UnionPwMultiAff", UnionPwMultiAff)

_isl_union_pw_multi_aff_get_ctx = ISLFunction.create(
    "isl_union_pw_multi_aff_get_ctx",
    Keep("UnionPwMultiAff"),
    return_=Give("Context"),
    lib=_lib,
)

_isl_union_pw_multi_aff_get_space = ISLFunction.create(
    "isl_union_pw_multi_aff_get_space",
    Keep("UnionPwMultiAff"),
    return_=Give("Space"),
    lib=_lib,
)

_isl_union_pw_multi_aff_dim = ISLFunction.create(
    "isl_union_pw_multi_aff_dim",
    Keep("UnionPwMultiAff"),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_multi_aff_set_dim_name = ISLFunction.create(
    "isl_union_pw_multi_aff_set_dim_name",
    Take("UnionPwMultiAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(str, ctype=c_char_p),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_find_dim_by_name = ISLFunction.create(
    "isl_union_pw_multi_aff_find_dim_by_name",
    Keep("UnionPwMultiAff"),
    Param(int, ctype=c_int),
    Param(str, ctype=c_char_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_multi_aff_reset_user = ISLFunction.create(
    "isl_union_pw_multi_aff_reset_user",
    Take("UnionPwMultiAff"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_as_union_map = ISLFunction.create(
    "isl_union_pw_multi_aff_as_union_map",
    Take("UnionPwMultiAff"),
    return_=Give("UnionMap"),
    lib=_lib,
)

_isl_union_pw_multi_aff_as_multi_union_pw_aff = ISLFunction.create(
    "isl_union_pw_multi_aff_as_multi_union_pw_aff",
    Take("UnionPwMultiAff"),
    return_=Give("MultiUnionPwAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_from_multi_union_pw_aff = ISLFunction.create(
    "isl_union_pw_multi_aff_from_multi_union_pw_aff",
    Take("MultiUnionPwAff"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_empty_ctx = ISLFunction.create(
    "isl_union_pw_multi_aff_empty_ctx",
    Context(),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_empty_space = ISLFunction.create(
    "isl_union_pw_multi_aff_empty_space",
    Take("Space"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_empty = ISLFunction.create(
    "isl_union_pw_multi_aff_empty",
    Take("Space"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_from_aff = ISLFunction.create(
    "isl_union_pw_multi_aff_from_aff",
    Take("Aff"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_from_pw_multi_aff = ISLFunction.create(
    "isl_union_pw_multi_aff_from_pw_multi_aff",
    Take("PwMultiAff"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_isa_pw_multi_aff = ISLFunction.create(
    "isl_union_pw_multi_aff_isa_pw_multi_aff",
    Keep("UnionPwMultiAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_multi_aff_as_pw_multi_aff = ISLFunction.create(
    "isl_union_pw_multi_aff_as_pw_multi_aff",
    Take("UnionPwMultiAff"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_from_multi_aff = ISLFunction.create(
    "isl_union_pw_multi_aff_from_multi_aff",
    Take("MultiAff"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_from_union_pw_aff = ISLFunction.create(
    "isl_union_pw_multi_aff_from_union_pw_aff",
    Take("UnionPwAff"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_get_union_pw_aff = ISLFunction.create(
    "isl_union_pw_multi_aff_get_union_pw_aff",
    Keep("UnionPwMultiAff"),
    Param(int, ctype=c_int),
    return_=Give("UnionPwAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_multi_val_on_domain = ISLFunction.create(
    "isl_union_pw_multi_aff_multi_val_on_domain",
    Take("UnionSet"),
    Take("MultiVal"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_add_pw_multi_aff = ISLFunction.create(
    "isl_union_pw_multi_aff_add_pw_multi_aff",
    Take("UnionPwMultiAff"),
    Take("PwMultiAff"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_copy = ISLFunction.create(
    "isl_union_pw_multi_aff_copy",
    Keep("UnionPwMultiAff"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_free = ISLFunction.create(
    "isl_union_pw_multi_aff_free",
    Take("UnionPwMultiAff"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_n_pw_multi_aff = ISLFunction.create(
    "isl_union_pw_multi_aff_n_pw_multi_aff",
    Keep("UnionPwMultiAff"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_multi_aff_foreach_pw_multi_aff = ISLFunction.create(
    "isl_union_pw_multi_aff_foreach_pw_multi_aff",
    Keep("UnionPwMultiAff"),
    Param(None, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_multi_aff_every_pw_multi_aff = ISLFunction.create(
    "isl_union_pw_multi_aff_every_pw_multi_aff",
    Keep("UnionPwMultiAff"),
    Param(None, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    Param(Any, ctype=c_void_p),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_multi_aff_extract_pw_multi_aff = ISLFunction.create(
    "isl_union_pw_multi_aff_extract_pw_multi_aff",
    Keep("UnionPwMultiAff"),
    Take("Space"),
    return_=Give("PwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_get_pw_multi_aff_list = ISLFunction.create(
    "isl_union_pw_multi_aff_get_pw_multi_aff_list",
    Keep("UnionPwMultiAff"),
    return_=Give("PwMultiAffList"),
    lib=_lib,
)

_isl_union_pw_multi_aff_read_from_str = ISLFunction.create(
    "isl_union_pw_multi_aff_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_to_str = ISLFunction.create(
    "isl_union_pw_multi_aff_to_str",
    Keep("UnionPwMultiAff"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_union_pw_multi_aff_plain_is_empty = ISLFunction.create(
    "isl_union_pw_multi_aff_plain_is_empty",
    Keep("UnionPwMultiAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_multi_aff_involves_locals = ISLFunction.create(
    "isl_union_pw_multi_aff_involves_locals",
    Keep("UnionPwMultiAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_multi_aff_involves_nan = ISLFunction.create(
    "isl_union_pw_multi_aff_involves_nan",
    Keep("UnionPwMultiAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_multi_aff_plain_is_equal = ISLFunction.create(
    "isl_union_pw_multi_aff_plain_is_equal",
    Keep("UnionPwMultiAff"),
    Keep("UnionPwMultiAff"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_union_pw_multi_aff_domain = ISLFunction.create(
    "isl_union_pw_multi_aff_domain",
    Take("UnionPwMultiAff"),
    return_=Give("UnionSet"),
    lib=_lib,
)

_isl_union_pw_multi_aff_from_domain = ISLFunction.create(
    "isl_union_pw_multi_aff_from_domain",
    Take("UnionSet"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_from_union_set = ISLFunction.create(
    "isl_union_pw_multi_aff_from_union_set",
    Take("UnionSet"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_from_union_map = ISLFunction.create(
    "isl_union_pw_multi_aff_from_union_map",
    Take("UnionMap"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_coalesce = ISLFunction.create(
    "isl_union_pw_multi_aff_coalesce",
    Take("UnionPwMultiAff"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_align_params = ISLFunction.create(
    "isl_union_pw_multi_aff_align_params",
    Take("UnionPwMultiAff"),
    Take("Space"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_drop_unused_params = ISLFunction.create(
    "isl_union_pw_multi_aff_drop_unused_params",
    Take("UnionPwMultiAff"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_neg = ISLFunction.create(
    "isl_union_pw_multi_aff_neg",
    Take("UnionPwMultiAff"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_drop_dims = ISLFunction.create(
    "isl_union_pw_multi_aff_drop_dims",
    Take("UnionPwMultiAff"),
    Param(int, ctype=c_int),
    Param(int, ctype=c_uint),
    Param(int, ctype=c_uint),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_intersect_domain_space = ISLFunction.create(
    "isl_union_pw_multi_aff_intersect_domain_space",
    Take("UnionPwMultiAff"),
    Take("Space"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_intersect_domain_union_set = ISLFunction.create(
    "isl_union_pw_multi_aff_intersect_domain_union_set",
    Take("UnionPwMultiAff"),
    Take("UnionSet"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_intersect_domain = ISLFunction.create(
    "isl_union_pw_multi_aff_intersect_domain",
    Take("UnionPwMultiAff"),
    Take("UnionSet"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_intersect_domain_wrapped_domain = ISLFunction.create(
    "isl_union_pw_multi_aff_intersect_domain_wrapped_domain",
    Take("UnionPwMultiAff"),
    Take("UnionSet"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_intersect_domain_wrapped_range = ISLFunction.create(
    "isl_union_pw_multi_aff_intersect_domain_wrapped_range",
    Take("UnionPwMultiAff"),
    Take("UnionSet"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_intersect_params = ISLFunction.create(
    "isl_union_pw_multi_aff_intersect_params",
    Take("UnionPwMultiAff"),
    Take("Set"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_subtract_domain_union_set = ISLFunction.create(
    "isl_union_pw_multi_aff_subtract_domain_union_set",
    Take("UnionPwMultiAff"),
    Take("Set"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_subtract_domain_space = ISLFunction.create(
    "isl_union_pw_multi_aff_subtract_domain_space",
    Take("UnionPwMultiAff"),
    Take("Space"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_subtract_domain = ISLFunction.create(
    "isl_union_pw_multi_aff_subtract_domain",
    Take("UnionPwMultiAff"),
    Take("UnionSet"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_apply_union_pw_multi_aff = ISLFunction.create(
    "isl_union_pw_multi_aff_apply_union_pw_multi_aff",
    Take("UnionPwMultiAff"),
    Take("UnionPwMultiAff"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_preimage_domain_wrapped_domain_union_pw_multi_aff = ISLFunction.create(
    "isl_union_pw_multi_aff_preimage_domain_wrapped_domain_union_pw_multi_aff",
    Take("UnionPwMultiAff"),
    Take("UnionPwMultiAff"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_pullback_union_pw_multi_aff = ISLFunction.create(
    "isl_union_pw_multi_aff_pullback_union_pw_multi_aff",
    Take("UnionPwMultiAff"),
    Take("UnionPwMultiAff"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_range_product = ISLFunction.create(
    "isl_union_pw_multi_aff_range_product",
    Take("UnionPwMultiAff"),
    Take("UnionPwMultiAff"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_flat_range_product = ISLFunction.create(
    "isl_union_pw_multi_aff_flat_range_product",
    Take("UnionPwMultiAff"),
    Take("UnionPwMultiAff"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_range_factor_domain = ISLFunction.create(
    "isl_union_pw_multi_aff_range_factor_domain",
    Take("UnionPwMultiAff"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_range_factor_range = ISLFunction.create(
    "isl_union_pw_multi_aff_range_factor_range",
    Take("UnionPwMultiAff"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_gist_params = ISLFunction.create(
    "isl_union_pw_multi_aff_gist_params",
    Take("UnionPwMultiAff"),
    Take("Set"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_gist = ISLFunction.create(
    "isl_union_pw_multi_aff_gist",
    Take("UnionPwMultiAff"),
    Take("UnionSet"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_add = ISLFunction.create(
    "isl_union_pw_multi_aff_add",
    Take("UnionPwMultiAff"),
    Take("UnionPwMultiAff"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_sub = ISLFunction.create(
    "isl_union_pw_multi_aff_sub",
    Take("UnionPwMultiAff"),
    Take("UnionPwMultiAff"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_union_add = ISLFunction.create(
    "isl_union_pw_multi_aff_union_add",
    Take("UnionPwMultiAff"),
    Take("UnionPwMultiAff"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_scale_val = ISLFunction.create(
    "isl_union_pw_multi_aff_scale_val",
    Take("UnionPwMultiAff"),
    Take("Val"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_scale_down_val = ISLFunction.create(
    "isl_union_pw_multi_aff_scale_down_val",
    Take("UnionPwMultiAff"),
    Take("Val"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)

_isl_union_pw_multi_aff_scale_multi_val = ISLFunction.create(
    "isl_union_pw_multi_aff_scale_multi_val",
    Take("UnionPwMultiAff"),
    Take("MultiVal"),
    return_=Give("UnionPwMultiAff"),
    lib=_lib,
)
