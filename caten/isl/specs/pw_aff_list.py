from __future__ import annotations

from ctypes import c_char_p
from typing import TYPE_CHECKING, Any

from ..ffi import load_libisl
from ..func import ISLFunction
from ..mixin import ISLObjectMixin
from ..obj import ISLObject
from ..qualifier import Give, Param, Take
from ..registry import register_type
from .context import Context

if TYPE_CHECKING:
    from .context import Context
    from .pw_aff import PwAff
    from .set import Set

_lib = load_libisl()

class PwAffList(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_pw_aff_list_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_pw_aff_list_read_from_str(spec)



    def min(self) -> "PwAff":
        return _isl_pw_aff_list_min(self)

    def max(self) -> "PwAff":
        return _isl_pw_aff_list_max(self)

    def eq_set(self, list2: "PwAffList") -> "Set":
        return _isl_pw_aff_list_eq_set(self, list2)

    def ne_set(self, list2: "PwAffList") -> "Set":
        return _isl_pw_aff_list_ne_set(self, list2)

    def le_set(self, list2: "PwAffList") -> "Set":
        return _isl_pw_aff_list_le_set(self, list2)

    def lt_set(self, list2: "PwAffList") -> "Set":
        return _isl_pw_aff_list_lt_set(self, list2)

    def ge_set(self, list2: "PwAffList") -> "Set":
        return _isl_pw_aff_list_ge_set(self, list2)

    def gt_set(self, list2: "PwAffList") -> "Set":
        return _isl_pw_aff_list_gt_set(self, list2)


register_type("PwAffList", PwAffList)

_isl_pw_aff_list_min = ISLFunction.create(
    "isl_pw_aff_list_min",
    Take("PwAffList"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_list_max = ISLFunction.create(
    "isl_pw_aff_list_max",
    Take("PwAffList"),
    return_=Give("PwAff"),
    lib=_lib,
)

_isl_pw_aff_list_eq_set = ISLFunction.create(
    "isl_pw_aff_list_eq_set",
    Take("PwAffList"),
    Take("PwAffList"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_pw_aff_list_ne_set = ISLFunction.create(
    "isl_pw_aff_list_ne_set",
    Take("PwAffList"),
    Take("PwAffList"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_pw_aff_list_le_set = ISLFunction.create(
    "isl_pw_aff_list_le_set",
    Take("PwAffList"),
    Take("PwAffList"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_pw_aff_list_lt_set = ISLFunction.create(
    "isl_pw_aff_list_lt_set",
    Take("PwAffList"),
    Take("PwAffList"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_pw_aff_list_ge_set = ISLFunction.create(
    "isl_pw_aff_list_ge_set",
    Take("PwAffList"),
    Take("PwAffList"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_pw_aff_list_gt_set = ISLFunction.create(
    "isl_pw_aff_list_gt_set",
    Take("PwAffList"),
    Take("PwAffList"),
    return_=Give("Set"),
    lib=_lib,
)

_isl_pw_aff_list_read_from_str = ISLFunction.create(
    "isl_pw_aff_list_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("PwAffList"),
    lib=_lib,
)
