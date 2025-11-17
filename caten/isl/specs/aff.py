from __future__ import annotations

from ctypes import c_int, c_void_p

from ..ffi import FfiPointer, load_libisl
from ..func import ISLFunction
from ..obj import ISLObject
from ..qualifier import Give, Keep, Null, Param, Take
from .context import Context
from .id import Id
from .set import Set
from .space import Space
from .val import Val

_lib = load_libisl()


class Aff(ISLObject):
    """Wrapper for ``isl_aff``."""

    __slots__ = ()

    def __init__(self, handle_or_spec: str | FfiPointer) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_aff_read_from_str(handle_or_spec, return_raw_pointer=True)
        else:
            handle = handle_or_spec
        super().__init__(handle)

    @classmethod
    def from_str(cls, spec: str) -> "Aff":
        return _isl_aff_read_from_str(spec)

    def copy_handle(self) -> FfiPointer:
        return _isl_aff_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_aff_free(handle)

    def dim(self, dim_type: int) -> int:
        return _isl_aff_dim(self, dim_type)

    def get_space(self) -> Space:
        return _isl_aff_get_space(self)

    def get_domain_space(self) -> Space:
        return _isl_aff_get_domain_space(self)

    def plain_is_equal(self, other: "Aff") -> bool:
        return _isl_aff_plain_is_equal(self, other)

    def to_pw_aff(self) -> "PwAff":
        return _isl_pw_aff_from_aff(self)

    @classmethod
    def zero_on_domain_space(cls, space: Space) -> "Aff":
        return _isl_aff_zero_on_domain_space(space)

    def __str__(self) -> str:  # pragma: no cover
        return _isl_aff_to_str(self)

    def __repr__(self) -> str:  # pragma: no cover
        return f"Aff({self.__str__()})"


class PwAff(ISLObject):
    """Wrapper for ``isl_pw_aff``."""

    __slots__ = ()

    def __init__(self, handle_or_spec: str | FfiPointer) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_pw_aff_read_from_str(handle_or_spec, return_raw_pointer=True)
        else:
            handle = handle_or_spec
        super().__init__(handle)

    @classmethod
    def from_str(cls, spec: str) -> "PwAff":
        return _isl_pw_aff_read_from_str(spec)

    @classmethod
    def from_aff(cls, aff: Aff) -> "PwAff":
        return _isl_pw_aff_from_aff(aff)

    def copy_handle(self) -> FfiPointer:
        return _isl_pw_aff_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_pw_aff_free(handle)

    def dim(self, dim_type: int) -> int:
        return _isl_pw_aff_dim(self, dim_type)

    def get_space(self) -> Space:
        return _isl_pw_aff_get_space(self)

    def get_domain_space(self) -> Space:
        return _isl_pw_aff_get_domain_space(self)

    def get_ctx(self) -> Context:
        return _isl_pw_aff_get_ctx(self)

    def get_dim_id(self, dim_type: int, pos: int) -> Id:
        return _isl_pw_aff_get_dim_id(self, dim_type, pos)

    def get_dim_name(self, dim_type: int, pos: int) -> str:
        return _isl_pw_aff_get_dim_name(self, dim_type, pos)

    def get_tuple_id(self, dim_type: int) -> Id:
        return _isl_pw_aff_get_tuple_id(self, dim_type)

    def set_dim_id(self, dim_type: int, pos: int, id: Id) -> "PwAff":
        return _isl_pw_aff_set_dim_id(self, dim_type, pos, id)

    def find_dim_by_name(self, dim_type: int, name: str) -> int:
        return _isl_pw_aff_find_dim_by_name(self, dim_type, name)

    def set_tuple_id(self, dim_type: int, id: Id) -> "PwAff":
        return _isl_pw_aff_set_tuple_id(self, dim_type, id)

    def reset_tuple_id(self, dim_type: int) -> "PwAff":
        return _isl_pw_aff_reset_tuple_id(self, dim_type)

    def reset_user(self) -> "PwAff":
        return _isl_pw_aff_reset_user(self)

    def has_dim_id(self, dim_type: int, pos: int) -> bool:
        return _isl_pw_aff_has_dim_id(self, dim_type, pos)

    def has_tuple_id(self, dim_type: int) -> bool:
        return _isl_pw_aff_has_tuple_id(self, dim_type)

    # construction / transforms
    @classmethod
    def empty(cls, space: Space) -> "PwAff":
        return _isl_pw_aff_empty(space)

    def add(self, other: "PwAff") -> "PwAff":
        return _isl_pw_aff_add(self, other)

    def add_constant_val(self, val: Val) -> "PwAff":
        return _isl_pw_aff_add_constant_val(self, val)

    def add_dims(self, dim_type: int, n: int) -> "PwAff":
        return _isl_pw_aff_add_dims(self, dim_type, n)

    def align_params(self, space: Space) -> "PwAff":
        return _isl_pw_aff_align_params(self, space)

    def domain(self) -> Set:
        return _isl_pw_aff_domain(self)

    def domain_reverse(self) -> "PwAff":
        return _isl_pw_aff_domain_reverse(self)

    def drop_dims(self, dim_type: int, first: int, n: int) -> "PwAff":
        return _isl_pw_aff_drop_dims(self, dim_type, first, n)

    def drop_unused_params(self) -> "PwAff":
        return _isl_pw_aff_drop_unused_params(self)

    def coalesce(self) -> "PwAff":
        return _isl_pw_aff_coalesce(self)

    def cond(self, zero: "PwAff") -> "PwAff":
        return _isl_pw_aff_cond(self, zero)

    def div(self, aff: Aff) -> "PwAff":
        return _isl_pw_aff_div(self, aff)

    def floor(self) -> "PwAff":
        return _isl_pw_aff_floor(self)

    def ceil(self) -> "PwAff":
        return _isl_pw_aff_ceil(self)

    def mod_val(self, val: Val) -> "PwAff":
        return _isl_pw_aff_mod_val(self, val)

    def mul(self, aff: Aff) -> "PwAff":
        return _isl_pw_aff_mul(self, aff)

    def neg(self) -> "PwAff":
        return _isl_pw_aff_neg(self)

    def insert_dims(self, dim_type: int, first: int, n: int) -> "PwAff":
        return _isl_pw_aff_insert_dims(self, dim_type, first, n)

    def insert_domain(self, domain: Set) -> "PwAff":
        return _isl_pw_aff_insert_domain(self, domain)

    def intersect_domain(self, domain: Set) -> "PwAff":
        return _isl_pw_aff_intersect_domain(self, domain)

    def intersect_params(self, params: Set) -> "PwAff":
        return _isl_pw_aff_intersect_params(self, params)

    def intersect_domain_wrapped_domain(self, set_: Set) -> "PwAff":
        return _isl_pw_aff_intersect_domain_wrapped_domain(self, set_)

    def intersect_domain_wrapped_range(self, set_: Set) -> "PwAff":
        return _isl_pw_aff_intersect_domain_wrapped_range(self, set_)

    def subtract_domain(self, set_: Set) -> "PwAff":
        return _isl_pw_aff_subtract_domain(self, set_)

    def gist(self, context: Set) -> "PwAff":
        return _isl_pw_aff_gist(self, context)

    def gist_params(self, context: Set) -> "PwAff":
        return _isl_pw_aff_gist_params(self, context)

    def params(self) -> Set:
        return _isl_pw_aff_params(self)

    # predicates
    def involves_dims(self, dim_type: int, first: int, n: int) -> bool:
        return _isl_pw_aff_involves_dims(self, dim_type, first, n)

    def involves_nan(self) -> bool:
        return _isl_pw_aff_involves_nan(self)

    def involves_param_id(self, id_: Id) -> bool:
        return _isl_pw_aff_involves_param_id(self, id_)

    def is_cst(self) -> bool:
        return _isl_pw_aff_is_cst(self)

    def is_empty(self) -> bool:
        return _isl_pw_aff_is_empty(self)

    def is_equal(self, other: "PwAff") -> bool:
        return _isl_pw_aff_is_equal(self, other)

    # conversions / maps
    def as_map(self) -> "Map":
        from .map import Map

        return _isl_pw_aff_as_map(self)

    def isa_aff(self) -> bool:
        return _isl_pw_aff_isa_aff(self)

    def as_aff(self) -> Aff:
        return _isl_pw_aff_as_aff(self)

    @classmethod
    def alloc(cls, set_space: Space, aff: Aff) -> "PwAff":
        return _isl_pw_aff_alloc(set_space, aff)

    @classmethod
    def zero_on_domain(cls, set_space: Set) -> "PwAff":
        return _isl_pw_aff_zero_on_domain(set_space)

    @classmethod
    def var_on_domain(cls, space: Space, type_: int, pos: int) -> "PwAff":
        return _isl_pw_aff_var_on_domain(space, type_, pos)

    def nan_on_domain_space(self) -> "PwAff":
        return _isl_pw_aff_nan_on_domain_space(self)

    def nan_on_domain(self) -> "PwAff":
        return _isl_pw_aff_nan_on_domain(self)

    def val_on_domain(self, val: Val) -> "PwAff":
        return _isl_pw_aff_val_on_domain(self, val)

    def param_on_domain_id(self, id_: Id) -> "PwAff":
        return _isl_pw_aff_param_on_domain_id(self, id_)

    def n_piece(self) -> int:
        return _isl_pw_aff_n_piece(self)

    def foreach_piece(self, fn: object) -> int:
        return _isl_pw_aff_foreach_piece(self, fn, None)

    def every_piece(self, fn: object) -> bool:
        return bool(_isl_pw_aff_every_piece(self, fn, None))

    def to_union_pw_aff(self) -> "UnionPwAff":
        from .union_pw_aff import UnionPwAff

        return _isl_pw_aff_to_union_pw_aff(self)

    def move_dims(self, dst_type: int, dst_pos: int, src_type: int, src_pos: int, n: int) -> "PwAff":
        return _isl_pw_aff_move_dims(self, dst_type, dst_pos, src_type, src_pos, n)

    def pullback_multi_aff(self, ma: "MultiAff") -> "PwAff":
        return _isl_pw_aff_pullback_multi_aff(self, ma)

    def pullback_pw_multi_aff(self, pma: "PwMultiAff") -> "PwAff":
        return _isl_pw_aff_pullback_pw_multi_aff(self, pma)

    def pullback_multi_pw_aff(self, mpa: "MultiPwAff") -> "PwAff":
        return _isl_pw_aff_pullback_multi_pw_aff(self, mpa)

    def eq_set(self, set_: Set) -> Set:
        return _isl_pw_aff_eq_set(self, set_)

    def ne_set(self, set_: Set) -> Set:
        return _isl_pw_aff_ne_set(self, set_)

    def le_set(self, set_: Set) -> Set:
        return _isl_pw_aff_le_set(self, set_)

    def lt_set(self, set_: Set) -> Set:
        return _isl_pw_aff_lt_set(self, set_)

    def ge_set(self, set_: Set) -> Set:
        return _isl_pw_aff_ge_set(self, set_)

    def gt_set(self, set_: Set) -> Set:
        return _isl_pw_aff_gt_set(self, set_)

    def eq_map(self, other: "PwAff") -> "Map":
        from .map import Map

        return _isl_pw_aff_eq_map(self, other)

    def le_map(self, other: "PwAff") -> "Map":
        from .map import Map

        return _isl_pw_aff_le_map(self, other)

    def lt_map(self, other: "PwAff") -> "Map":
        from .map import Map

        return _isl_pw_aff_lt_map(self, other)

    def ge_map(self, other: "PwAff") -> "Map":
        from .map import Map

        return _isl_pw_aff_ge_map(self, other)

    def gt_map(self, other: "PwAff") -> "Map":
        from .map import Map

        return _isl_pw_aff_gt_map(self, other)

    def plain_is_equal(self, other: "PwAff") -> bool:
        return _isl_pw_aff_plain_is_equal(self, other)

    def __str__(self) -> str:  # pragma: no cover
        return _isl_pw_aff_to_str(self)

    def __repr__(self) -> str:  # pragma: no cover
        return f"PwAff({self.__str__()})"


class MultiAff(ISLObject):
    """Wrapper for ``isl_multi_aff``."""

    __slots__ = ()

    def __init__(self, handle_or_spec: str | FfiPointer) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_multi_aff_read_from_str(handle_or_spec, return_raw_pointer=True)
        else:
            handle = handle_or_spec
        super().__init__(handle)

    @classmethod
    def from_str(cls, spec: str) -> "MultiAff":
        return _isl_multi_aff_read_from_str(spec)

    def copy_handle(self) -> FfiPointer:
        return _isl_multi_aff_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_multi_aff_free(handle)

    def dim(self, dim_type: int) -> int:
        return _isl_multi_aff_dim(self, dim_type)

    def __str__(self) -> str:  # pragma: no cover
        return _isl_multi_aff_to_str(self)

    def __repr__(self) -> str:  # pragma: no cover
        return f"MultiAff({self.__str__()})"


class PwMultiAff(ISLObject):
    """Wrapper for ``isl_pw_multi_aff``."""

    __slots__ = ()

    def __init__(self, handle_or_spec: str | FfiPointer) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_pw_multi_aff_read_from_str(handle_or_spec, return_raw_pointer=True)
        else:
            handle = handle_or_spec
        super().__init__(handle)

    @classmethod
    def from_str(cls, spec: str) -> "PwMultiAff":
        return _isl_pw_multi_aff_read_from_str(spec)

    def copy_handle(self) -> FfiPointer:
        return _isl_pw_multi_aff_copy(self, return_raw_pointer=True)

    @classmethod
    def free_handle(cls, handle: FfiPointer) -> None:
        _isl_pw_multi_aff_free(handle)

    def dim(self, dim_type: int) -> int:
        return _isl_pw_multi_aff_dim(self, dim_type)

    def __str__(self) -> str:  # pragma: no cover
        return _isl_pw_multi_aff_to_str(self)

    def __repr__(self) -> str:  # pragma: no cover
        return f"PwMultiAff({self.__str__()})"


# --- Aff primitives ---

_isl_aff_read_from_str = ISLFunction.create(
    _lib.isl_aff_read_from_str,
    Context(),
    Param(str),
    return_=Give(Aff),
    lib=_lib,
)

_isl_aff_copy = ISLFunction.create(
    _lib.isl_aff_copy,
    Keep(Aff),
    return_=Give(Aff),
    lib=_lib,
)

_isl_aff_free = ISLFunction.create(
    _lib.isl_aff_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

_isl_aff_dim = ISLFunction.create(
    _lib.isl_aff_dim,
    Keep(Aff),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_aff_get_space = ISLFunction.create(
    _lib.isl_aff_get_space,
    Keep(Aff),
    return_=Give(Space),
    lib=_lib,
)

_isl_aff_get_domain_space = ISLFunction.create(
    _lib.isl_aff_get_domain_space,
    Keep(Aff),
    return_=Give(Space),
    lib=_lib,
)

_isl_aff_plain_is_equal = ISLFunction.create(
    _lib.isl_aff_plain_is_equal,
    Keep(Aff),
    Keep(Aff),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_aff_to_str = ISLFunction.create(
    _lib.isl_aff_to_str,
    Keep(Aff),
    return_=Param(str),
    lib=_lib,
)

_isl_aff_zero_on_domain_space = ISLFunction.create(
    _lib.isl_aff_zero_on_domain_space,
    Take(Space),
    return_=Give(Aff),
    lib=_lib,
)

# --- PwAff primitives ---

_isl_pw_aff_read_from_str = ISLFunction.create(
    _lib.isl_pw_aff_read_from_str,
    Context(),
    Param(str),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_copy = ISLFunction.create(
    _lib.isl_pw_aff_copy,
    Keep(PwAff),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_free = ISLFunction.create(
    _lib.isl_pw_aff_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

_isl_pw_aff_dim = ISLFunction.create(
    _lib.isl_pw_aff_dim,
    Keep(PwAff),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_get_space = ISLFunction.create(
    _lib.isl_pw_aff_get_space,
    Keep(PwAff),
    return_=Give(Space),
    lib=_lib,
)

_isl_pw_aff_get_domain_space = ISLFunction.create(
    _lib.isl_pw_aff_get_domain_space,
    Keep(PwAff),
    return_=Give(Space),
    lib=_lib,
)

_isl_pw_aff_plain_is_equal = ISLFunction.create(
    _lib.isl_pw_aff_plain_is_equal,
    Keep(PwAff),
    Keep(PwAff),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_is_equal = ISLFunction.create(
    _lib.isl_pw_aff_is_equal,
    Keep(PwAff),
    Keep(PwAff),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_from_aff = ISLFunction.create(
    _lib.isl_pw_aff_from_aff,
    Take(Aff),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_to_str = ISLFunction.create(
    _lib.isl_pw_aff_to_str,
    Keep(PwAff),
    return_=Param(str),
    lib=_lib,
)

_isl_pw_aff_get_ctx = ISLFunction.create(
    _lib.isl_pw_aff_get_ctx,
    Keep(PwAff),
    return_=Give(Context),
    lib=_lib,
)

_isl_pw_aff_get_dim_id = ISLFunction.create(
    _lib.isl_pw_aff_get_dim_id,
    Keep(PwAff),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Id),
    lib=_lib,
)

_isl_pw_aff_get_dim_name = ISLFunction.create(
    _lib.isl_pw_aff_get_dim_name,
    Keep(PwAff),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Param(str),
    lib=_lib,
)

_isl_pw_aff_get_tuple_id = ISLFunction.create(
    _lib.isl_pw_aff_get_tuple_id,
    Keep(PwAff),
    Param(int, ctype=c_int),
    return_=Give(Id),
    lib=_lib,
)

_isl_pw_aff_has_dim_id = ISLFunction.create(
    _lib.isl_pw_aff_has_dim_id,
    Keep(PwAff),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_has_tuple_id = ISLFunction.create(
    _lib.isl_pw_aff_has_tuple_id,
    Keep(PwAff),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_set_dim_id = ISLFunction.create(
    _lib.isl_pw_aff_set_dim_id,
    Take(PwAff),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Take(Id),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_find_dim_by_name = ISLFunction.create(
    _lib.isl_pw_aff_find_dim_by_name,
    Keep(PwAff),
    Param(int, ctype=c_int),
    Param(str),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_set_tuple_id = ISLFunction.create(
    _lib.isl_pw_aff_set_tuple_id,
    Take(PwAff),
    Param(int, ctype=c_int),
    Take(Id),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_reset_tuple_id = ISLFunction.create(
    _lib.isl_pw_aff_reset_tuple_id,
    Take(PwAff),
    Param(int, ctype=c_int),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_reset_user = ISLFunction.create(
    _lib.isl_pw_aff_reset_user,
    Take(PwAff),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_empty = ISLFunction.create(
    _lib.isl_pw_aff_empty,
    Take(Space),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_add = ISLFunction.create(
    _lib.isl_pw_aff_add,
    Take(PwAff),
    Take(PwAff),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_add_constant_val = ISLFunction.create(
    _lib.isl_pw_aff_add_constant_val,
    Take(PwAff),
    Take(Val),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_add_dims = ISLFunction.create(
    _lib.isl_pw_aff_add_dims,
    Take(PwAff),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_align_params = ISLFunction.create(
    _lib.isl_pw_aff_align_params,
    Take(PwAff),
    Take(Space),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_domain = ISLFunction.create(
    _lib.isl_pw_aff_domain,
    Take(PwAff),
    return_=Give(Set),
    lib=_lib,
)

_isl_pw_aff_domain_reverse = ISLFunction.create(
    _lib.isl_pw_aff_domain_reverse,
    Take(PwAff),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_drop_dims = ISLFunction.create(
    _lib.isl_pw_aff_drop_dims,
    Take(PwAff),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_drop_unused_params = ISLFunction.create(
    _lib.isl_pw_aff_drop_unused_params,
    Take(PwAff),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_coalesce = ISLFunction.create(
    _lib.isl_pw_aff_coalesce,
    Take(PwAff),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_cond = ISLFunction.create(
    _lib.isl_pw_aff_cond,
    Take(PwAff),
    Take(PwAff),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_div = ISLFunction.create(
    _lib.isl_pw_aff_div,
    Take(PwAff),
    Take(Aff),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_floor = ISLFunction.create(
    _lib.isl_pw_aff_floor,
    Take(PwAff),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_ceil = ISLFunction.create(
    _lib.isl_pw_aff_ceil,
    Take(PwAff),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_mod_val = ISLFunction.create(
    _lib.isl_pw_aff_mod_val,
    Take(PwAff),
    Take(Val),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_mul = ISLFunction.create(
    _lib.isl_pw_aff_mul,
    Take(PwAff),
    Take(Aff),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_neg = ISLFunction.create(
    _lib.isl_pw_aff_neg,
    Take(PwAff),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_insert_dims = ISLFunction.create(
    _lib.isl_pw_aff_insert_dims,
    Take(PwAff),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_insert_domain = ISLFunction.create(
    _lib.isl_pw_aff_insert_domain,
    Take(PwAff),
    Take(Set),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_intersect_domain = ISLFunction.create(
    _lib.isl_pw_aff_intersect_domain,
    Take(PwAff),
    Take(Set),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_intersect_params = ISLFunction.create(
    _lib.isl_pw_aff_intersect_params,
    Take(PwAff),
    Take(Set),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_intersect_domain_wrapped_domain = ISLFunction.create(
    _lib.isl_pw_aff_intersect_domain_wrapped_domain,
    Take(PwAff),
    Take(Set),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_intersect_domain_wrapped_range = ISLFunction.create(
    _lib.isl_pw_aff_intersect_domain_wrapped_range,
    Take(PwAff),
    Take(Set),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_subtract_domain = ISLFunction.create(
    _lib.isl_pw_aff_subtract_domain,
    Take(PwAff),
    Take(Set),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_gist = ISLFunction.create(
    _lib.isl_pw_aff_gist,
    Take(PwAff),
    Take(Set),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_gist_params = ISLFunction.create(
    _lib.isl_pw_aff_gist_params,
    Take(PwAff),
    Take(Set),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_params = ISLFunction.create(
    _lib.isl_pw_aff_params,
    Take(PwAff),
    return_=Give(Set),
    lib=_lib,
)

_isl_pw_aff_involves_dims = ISLFunction.create(
    _lib.isl_pw_aff_involves_dims,
    Keep(PwAff),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_involves_nan = ISLFunction.create(
    _lib.isl_pw_aff_involves_nan,
    Keep(PwAff),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_involves_param_id = ISLFunction.create(
    _lib.isl_pw_aff_involves_param_id,
    Keep(PwAff),
    Keep(Id),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_is_cst = ISLFunction.create(
    _lib.isl_pw_aff_is_cst,
    Keep(PwAff),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_is_empty = ISLFunction.create(
    _lib.isl_pw_aff_is_empty,
    Keep(PwAff),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_is_equal = ISLFunction.create(
    _lib.isl_pw_aff_is_equal,
    Keep(PwAff),
    Keep(PwAff),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_isa_aff = ISLFunction.create(
    _lib.isl_pw_aff_isa_aff,
    Keep(PwAff),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_as_aff = ISLFunction.create(
    _lib.isl_pw_aff_as_aff,
    Take(PwAff),
    return_=Give(Aff),
    lib=_lib,
)

_isl_pw_aff_alloc = ISLFunction.create(
    _lib.isl_pw_aff_alloc,
    Take(Set),
    Take(Aff),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_zero_on_domain = ISLFunction.create(
    _lib.isl_pw_aff_zero_on_domain,
    Take(Set),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_var_on_domain = ISLFunction.create(
    _lib.isl_pw_aff_var_on_domain,
    Take(Space),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_nan_on_domain_space = ISLFunction.create(
    _lib.isl_pw_aff_nan_on_domain_space,
    Take(Space),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_nan_on_domain = ISLFunction.create(
    _lib.isl_pw_aff_nan_on_domain,
    Take(Set),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_val_on_domain = ISLFunction.create(
    _lib.isl_pw_aff_val_on_domain,
    Take(Set),
    Take(Val),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_param_on_domain_id = ISLFunction.create(
    _lib.isl_pw_aff_param_on_domain_id,
    Take(Set),
    Take(Id),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_n_piece = ISLFunction.create(
    _lib.isl_pw_aff_n_piece,
    Keep(PwAff),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_foreach_piece = ISLFunction.create(
    _lib.isl_pw_aff_foreach_piece,
    Keep(PwAff),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_every_piece = ISLFunction.create(
    _lib.isl_pw_aff_every_piece,
    Keep(PwAff),
    Param(None, ctype=c_void_p),
    Param(None, ctype=c_void_p),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_to_union_pw_aff = ISLFunction.create(
    _lib.isl_pw_aff_to_union_pw_aff,
    Take(PwAff),
    return_=Give("UnionPwAff"),  # lazy import
    lib=_lib,
)

_isl_pw_aff_balance_dummy = None  # marker to keep section consistent
_isl_pw_aff_get_ctx = ISLFunction.create(
    _lib.isl_pw_aff_get_ctx,
    Keep(PwAff),
    return_=Give(Context),
    lib=_lib,
)

_isl_pw_aff_get_dim_id = ISLFunction.create(
    _lib.isl_pw_aff_get_dim_id,
    Keep(PwAff),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(Id),
    lib=_lib,
)

_isl_pw_aff_get_dim_name = ISLFunction.create(
    _lib.isl_pw_aff_get_dim_name,
    Keep(PwAff),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Param(str),
    lib=_lib,
)

_isl_pw_aff_get_tuple_id = ISLFunction.create(
    _lib.isl_pw_aff_get_tuple_id,
    Keep(PwAff),
    Param(int, ctype=c_int),
    return_=Give(Id),
    lib=_lib,
)

_isl_pw_aff_has_dim_id = ISLFunction.create(
    _lib.isl_pw_aff_has_dim_id,
    Keep(PwAff),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_has_tuple_id = ISLFunction.create(
    _lib.isl_pw_aff_has_tuple_id,
    Keep(PwAff),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_empty = ISLFunction.create(
    _lib.isl_pw_aff_empty,
    Take(Space),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_add = ISLFunction.create(
    _lib.isl_pw_aff_add,
    Take(PwAff),
    Take(PwAff),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_add_constant_val = ISLFunction.create(
    _lib.isl_pw_aff_add_constant_val,
    Take(PwAff),
    Take(Val),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_add_dims = ISLFunction.create(
    _lib.isl_pw_aff_add_dims,
    Take(PwAff),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_align_params = ISLFunction.create(
    _lib.isl_pw_aff_align_params,
    Take(PwAff),
    Take(Space),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_domain = ISLFunction.create(
    _lib.isl_pw_aff_domain,
    Take(PwAff),
    return_=Give(Set),
    lib=_lib,
)

_isl_pw_aff_domain_reverse = ISLFunction.create(
    _lib.isl_pw_aff_domain_reverse,
    Take(PwAff),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_drop_dims = ISLFunction.create(
    _lib.isl_pw_aff_drop_dims,
    Take(PwAff),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_drop_unused_params = ISLFunction.create(
    _lib.isl_pw_aff_drop_unused_params,
    Take(PwAff),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_coalesce = ISLFunction.create(
    _lib.isl_pw_aff_coalesce,
    Take(PwAff),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_cond = ISLFunction.create(
    _lib.isl_pw_aff_cond,
    Take(PwAff),
    Take(PwAff),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_div = ISLFunction.create(
    _lib.isl_pw_aff_div,
    Take(PwAff),
    Take(Aff),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_floor = ISLFunction.create(
    _lib.isl_pw_aff_floor,
    Take(PwAff),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_ceil = ISLFunction.create(
    _lib.isl_pw_aff_ceil,
    Take(PwAff),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_mod_val = ISLFunction.create(
    _lib.isl_pw_aff_mod_val,
    Take(PwAff),
    Take(Val),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_mul = ISLFunction.create(
    _lib.isl_pw_aff_mul,
    Take(PwAff),
    Take(Aff),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_neg = ISLFunction.create(
    _lib.isl_pw_aff_neg,
    Take(PwAff),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_insert_dims = ISLFunction.create(
    _lib.isl_pw_aff_insert_dims,
    Take(PwAff),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_insert_domain = ISLFunction.create(
    _lib.isl_pw_aff_insert_domain,
    Take(PwAff),
    Take(Set),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_intersect_domain = ISLFunction.create(
    _lib.isl_pw_aff_intersect_domain,
    Take(PwAff),
    Take(Set),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_intersect_params = ISLFunction.create(
    _lib.isl_pw_aff_intersect_params,
    Take(PwAff),
    Take(Set),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_gist = ISLFunction.create(
    _lib.isl_pw_aff_gist,
    Take(PwAff),
    Take(Set),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_gist_params = ISLFunction.create(
    _lib.isl_pw_aff_gist_params,
    Take(PwAff),
    Take(Set),
    return_=Give(PwAff),
    lib=_lib,
)

_isl_pw_aff_involves_dims = ISLFunction.create(
    _lib.isl_pw_aff_involves_dims,
    Keep(PwAff),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    Param(int, ctype=c_int),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_involves_nan = ISLFunction.create(
    _lib.isl_pw_aff_involves_nan,
    Keep(PwAff),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_involves_param_id = ISLFunction.create(
    _lib.isl_pw_aff_involves_param_id,
    Keep(PwAff),
    Keep(Id),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_is_cst = ISLFunction.create(
    _lib.isl_pw_aff_is_cst,
    Keep(PwAff),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_pw_aff_is_empty = ISLFunction.create(
    _lib.isl_pw_aff_is_empty,
    Keep(PwAff),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

# --- MultiAff primitives ---

_isl_multi_aff_read_from_str = ISLFunction.create(
    _lib.isl_multi_aff_read_from_str,
    Context(),
    Param(str),
    return_=Give(MultiAff),
    lib=_lib,
)

_isl_multi_aff_copy = ISLFunction.create(
    _lib.isl_multi_aff_copy,
    Keep(MultiAff),
    return_=Give(MultiAff),
    lib=_lib,
)

_isl_multi_aff_free = ISLFunction.create(
    _lib.isl_multi_aff_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

_isl_multi_aff_dim = ISLFunction.create(
    _lib.isl_multi_aff_dim,
    Keep(MultiAff),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_multi_aff_to_str = ISLFunction.create(
    _lib.isl_multi_aff_to_str,
    Keep(MultiAff),
    return_=Param(str),
    lib=_lib,
)

# --- PwMultiAff primitives ---

_isl_pw_multi_aff_read_from_str = ISLFunction.create(
    _lib.isl_pw_multi_aff_read_from_str,
    Context(),
    Param(str),
    return_=Give(PwMultiAff),
    lib=_lib,
)

_isl_pw_multi_aff_copy = ISLFunction.create(
    _lib.isl_pw_multi_aff_copy,
    Keep(PwMultiAff),
    return_=Give(PwMultiAff),
    lib=_lib,
)

_isl_pw_multi_aff_free = ISLFunction.create(
    _lib.isl_pw_multi_aff_free,
    Param(None, ctype=c_void_p),
    return_=Null(),
    lib=_lib,
)

_isl_pw_multi_aff_dim = ISLFunction.create(
    _lib.isl_pw_multi_aff_dim,
    Keep(PwMultiAff),
    Param(int, ctype=c_int),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_pw_multi_aff_to_str = ISLFunction.create(
    _lib.isl_pw_multi_aff_to_str,
    Keep(PwMultiAff),
    return_=Param(str),
    lib=_lib,
)

__all__ = ["Aff", "PwAff", "MultiAff", "PwMultiAff"]
