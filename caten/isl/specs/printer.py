from __future__ import annotations

from ctypes import (
    c_char_p,
    c_double,
    c_int,
    c_void_p,
)
from typing import TYPE_CHECKING, Any

from ..ffi import load_libisl
from ..func import ISLFunction
from ..mixin import ISLObjectMixin
from ..obj import ISLObject
from ..qualifier import Give, Keep, Param, Take
from ..registry import register_type
from .context import Context

if TYPE_CHECKING:
    from .context import Context

_lib = load_libisl()

class Printer(ISLObject, ISLObjectMixin):
    __slots__ = ()

    def __init__(self, handle_or_spec: Any) -> None:
        if isinstance(handle_or_spec, str):
            handle = _isl_printer_read_from_str(handle_or_spec, return_raw_pointer=True)
            super().__init__(handle)
        else:
            super().__init__(handle_or_spec)

    @classmethod
    def from_str(cls, spec: str) -> Any:
        return _isl_printer_read_from_str(spec)


    @classmethod
    def free_handle(cls, handle: Any) -> None:
        _lib.isl_printer_free(handle)

    def __str__(self) -> str:
        return _isl_printer_to_str(self)

    def __repr__(self) -> str:
        return f"Printer({self.__str__()})"

    def print_id(self, id: "Id") -> "Printer":
        return _isl_printer_print_id(self, id)

    @classmethod
    def to_file(cls, file: None) -> "Printer":
        return _isl_printer_to_file(file)

    def get_str(self) -> str:
        return _isl_printer_get_str(self)

    def get_file(self) -> None:
        return _isl_printer_get_file(self)

    def get_output_format(self) -> int:
        return _isl_printer_get_output_format(self)

    def get_yaml_style(self) -> int:
        return _isl_printer_get_yaml_style(self)

    def set_output_format(self, output_format: int) -> "Printer":
        return _isl_printer_set_output_format(self, output_format)

    def set_indent(self, indent: int) -> "Printer":
        return _isl_printer_set_indent(self, indent)

    def set_indent_prefix(self, prefix: str) -> "Printer":
        return _isl_printer_set_indent_prefix(self, prefix)

    def indent(self, indent: int) -> "Printer":
        return _isl_printer_indent(self, indent)

    def set_prefix(self, prefix: str) -> "Printer":
        return _isl_printer_set_prefix(self, prefix)

    def set_suffix(self, suffix: str) -> "Printer":
        return _isl_printer_set_suffix(self, suffix)

    def set_yaml_style(self, yaml_style: int) -> "Printer":
        return _isl_printer_set_yaml_style(self, yaml_style)

    def print_double(self, d: float) -> "Printer":
        return _isl_printer_print_double(self, d)

    def print_val(self, v: "Val") -> "Printer":
        return _isl_printer_print_val(self, v)

    def print_multi_val(self, mv: "MultiVal") -> "Printer":
        return _isl_printer_print_multi_val(self, mv)

    def print_basic_set(self, bset: "BasicSet") -> "Printer":
        return _isl_printer_print_basic_set(self, bset)

    def print_set(self, set: "Set") -> "Printer":
        return _isl_printer_print_set(self, set)

    def print_basic_map(self, bmap: "BasicMap") -> "Printer":
        return _isl_printer_print_basic_map(self, bmap)

    def print_map(self, map: "Map") -> "Printer":
        return _isl_printer_print_map(self, map)

    def print_union_set(self, uset: "UnionSet") -> "Printer":
        return _isl_printer_print_union_set(self, uset)

    def print_union_map(self, umap: "UnionMap") -> "Printer":
        return _isl_printer_print_union_map(self, umap)

    def print_multi_id(self, mi: "MultiId") -> "Printer":
        return _isl_printer_print_multi_id(self, mi)

    def print_aff(self, aff: "Aff") -> "Printer":
        return _isl_printer_print_aff(self, aff)

    def print_multi_aff(self, maff: "MultiAff") -> "Printer":
        return _isl_printer_print_multi_aff(self, maff)

    def print_pw_aff(self, pwaff: "PwAff") -> "Printer":
        return _isl_printer_print_pw_aff(self, pwaff)

    def print_pw_multi_aff(self, pma: "PwMultiAff") -> "Printer":
        return _isl_printer_print_pw_multi_aff(self, pma)

    def print_multi_pw_aff(self, mpa: "MultiPwAff") -> "Printer":
        return _isl_printer_print_multi_pw_aff(self, mpa)

    def print_union_pw_aff(self, upa: "UnionPwAff") -> "Printer":
        return _isl_printer_print_union_pw_aff(self, upa)

    def print_union_pw_multi_aff(self, upma: "UnionPwMultiAff") -> "Printer":
        return _isl_printer_print_union_pw_multi_aff(self, upma)

    def print_multi_union_pw_aff(self, mupa: "MultiUnionPwAff") -> "Printer":
        return _isl_printer_print_multi_union_pw_aff(self, mupa)

    def print_qpolynomial(self, qp: "Qpolynomial") -> "Printer":
        return _isl_printer_print_qpolynomial(self, qp)

    def print_pw_qpolynomial(self, pwqp: "PwQpolynomial") -> "Printer":
        return _isl_printer_print_pw_qpolynomial(self, pwqp)

    def print_union_pw_qpolynomial(self, upwqp: "UnionPwQpolynomial") -> "Printer":
        return _isl_printer_print_union_pw_qpolynomial(self, upwqp)

    def print_pw_qpolynomial_fold(self, pwf: "PwQpolynomialFold") -> "Printer":
        return _isl_printer_print_pw_qpolynomial_fold(self, pwf)

    def print_union_pw_qpolynomial_fold(self, upwf: "UnionPwQpolynomialFold") -> "Printer":
        return _isl_printer_print_union_pw_qpolynomial_fold(self, upwf)

    def yaml_start_mapping(self) -> "Printer":
        return _isl_printer_yaml_start_mapping(self)

    def yaml_end_mapping(self) -> "Printer":
        return _isl_printer_yaml_end_mapping(self)

    def yaml_start_sequence(self) -> "Printer":
        return _isl_printer_yaml_start_sequence(self)

    def yaml_end_sequence(self) -> "Printer":
        return _isl_printer_yaml_end_sequence(self)

    def yaml_next(self) -> "Printer":
        return _isl_printer_yaml_next(self)

    def flush(self) -> "Printer":
        return _isl_printer_flush(self)

    def has_note(self, id: "Id") -> bool:
        return _isl_printer_has_note(self, id)

    def get_note(self, id: "Id") -> "Id":
        return _isl_printer_get_note(self, id)

    def set_note(self, id: "Id", note: "Id") -> "Printer":
        return _isl_printer_set_note(self, id, note)

    def print_fixed_box(self, box: "FixedBox") -> "Printer":
        return _isl_printer_print_fixed_box(self, box)

    def print_set_list(self, list: "SetList") -> "Printer":
        return _isl_printer_print_set_list(self, list)

    def print_id_to_ast_expr(self, id2expr: "IdToAstExpr") -> "Printer":
        return _isl_printer_print_id_to_ast_expr(self, id2expr)

    def print_schedule(self, schedule: "Schedule") -> "Printer":
        return _isl_printer_print_schedule(self, schedule)

    def print_schedule_node(self, node: "ScheduleNode") -> "Printer":
        return _isl_printer_print_schedule_node(self, node)

    def print_union_access_info(self, access: "UnionAccessInfo") -> "Printer":
        return _isl_printer_print_union_access_info(self, access)

    def print_union_flow(self, flow: "UnionFlow") -> "Printer":
        return _isl_printer_print_union_flow(self, flow)

    def print_schedule_constraints(self, sc: "ScheduleConstraints") -> "Printer":
        return _isl_printer_print_schedule_constraints(self, sc)

    def print_ast_expr(self, expr: "ASTExpr") -> "Printer":
        return _isl_printer_print_ast_expr(self, expr)

    def print_ast_node(self, node: "ASTNode") -> "Printer":
        return _isl_printer_print_ast_node(self, node)


register_type("Printer", Printer)

_isl_printer_print_id = ISLFunction.create(
    "isl_printer_print_id",
    Take("Printer"),
    Keep("Id"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_to_file = ISLFunction.create(
    "isl_printer_to_file",
    Context(),
    Param(None, ctype=c_void_p),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_to_str = ISLFunction.create(
    "isl_printer_to_str",
    Context(),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_free = ISLFunction.create(
    "isl_printer_free",
    Take("Printer"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_get_str = ISLFunction.create(
    "isl_printer_get_str",
    Keep("Printer"),
    return_=Param(str, ctype=c_char_p),
    lib=_lib,
)

_isl_printer_get_file = ISLFunction.create(
    "isl_printer_get_file",
    Keep("Printer"),
    return_=Param(None, ctype=c_void_p),
    lib=_lib,
)

_isl_printer_get_output_format = ISLFunction.create(
    "isl_printer_get_output_format",
    Keep("Printer"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_printer_get_yaml_style = ISLFunction.create(
    "isl_printer_get_yaml_style",
    Keep("Printer"),
    return_=Param(int, ctype=c_int),
    lib=_lib,
)

_isl_printer_set_output_format = ISLFunction.create(
    "isl_printer_set_output_format",
    Take("Printer"),
    Param(int, ctype=c_int),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_set_indent = ISLFunction.create(
    "isl_printer_set_indent",
    Take("Printer"),
    Param(int, ctype=c_int),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_set_indent_prefix = ISLFunction.create(
    "isl_printer_set_indent_prefix",
    Take("Printer"),
    Param(str, ctype=c_char_p),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_indent = ISLFunction.create(
    "isl_printer_indent",
    Take("Printer"),
    Param(int, ctype=c_int),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_set_prefix = ISLFunction.create(
    "isl_printer_set_prefix",
    Take("Printer"),
    Param(str, ctype=c_char_p),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_set_suffix = ISLFunction.create(
    "isl_printer_set_suffix",
    Take("Printer"),
    Param(str, ctype=c_char_p),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_set_yaml_style = ISLFunction.create(
    "isl_printer_set_yaml_style",
    Take("Printer"),
    Param(int, ctype=c_int),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_print_double = ISLFunction.create(
    "isl_printer_print_double",
    Take("Printer"),
    Param(float, ctype=c_double),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_print_val = ISLFunction.create(
    "isl_printer_print_val",
    Take("Printer"),
    Keep("Val"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_print_multi_val = ISLFunction.create(
    "isl_printer_print_multi_val",
    Take("Printer"),
    Keep("MultiVal"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_print_basic_set = ISLFunction.create(
    "isl_printer_print_basic_set",
    Take("Printer"),
    Keep("BasicSet"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_print_set = ISLFunction.create(
    "isl_printer_print_set",
    Take("Printer"),
    Keep("Set"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_print_basic_map = ISLFunction.create(
    "isl_printer_print_basic_map",
    Take("Printer"),
    Keep("BasicMap"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_print_map = ISLFunction.create(
    "isl_printer_print_map",
    Take("Printer"),
    Keep("Map"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_print_union_set = ISLFunction.create(
    "isl_printer_print_union_set",
    Take("Printer"),
    Keep("UnionSet"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_print_union_map = ISLFunction.create(
    "isl_printer_print_union_map",
    Take("Printer"),
    Keep("UnionMap"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_print_multi_id = ISLFunction.create(
    "isl_printer_print_multi_id",
    Take("Printer"),
    Keep("MultiId"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_print_aff = ISLFunction.create(
    "isl_printer_print_aff",
    Take("Printer"),
    Keep("Aff"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_print_multi_aff = ISLFunction.create(
    "isl_printer_print_multi_aff",
    Take("Printer"),
    Keep("MultiAff"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_print_pw_aff = ISLFunction.create(
    "isl_printer_print_pw_aff",
    Take("Printer"),
    Keep("PwAff"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_print_pw_multi_aff = ISLFunction.create(
    "isl_printer_print_pw_multi_aff",
    Take("Printer"),
    Keep("PwMultiAff"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_print_multi_pw_aff = ISLFunction.create(
    "isl_printer_print_multi_pw_aff",
    Take("Printer"),
    Keep("MultiPwAff"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_print_union_pw_aff = ISLFunction.create(
    "isl_printer_print_union_pw_aff",
    Take("Printer"),
    Keep("UnionPwAff"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_print_union_pw_multi_aff = ISLFunction.create(
    "isl_printer_print_union_pw_multi_aff",
    Take("Printer"),
    Keep("UnionPwMultiAff"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_print_multi_union_pw_aff = ISLFunction.create(
    "isl_printer_print_multi_union_pw_aff",
    Take("Printer"),
    Keep("MultiUnionPwAff"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_print_qpolynomial = ISLFunction.create(
    "isl_printer_print_qpolynomial",
    Take("Printer"),
    Keep("Qpolynomial"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_print_pw_qpolynomial = ISLFunction.create(
    "isl_printer_print_pw_qpolynomial",
    Take("Printer"),
    Keep("PwQpolynomial"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_print_union_pw_qpolynomial = ISLFunction.create(
    "isl_printer_print_union_pw_qpolynomial",
    Take("Printer"),
    Keep("UnionPwQpolynomial"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_print_pw_qpolynomial_fold = ISLFunction.create(
    "isl_printer_print_pw_qpolynomial_fold",
    Take("Printer"),
    Keep("PwQpolynomialFold"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_print_union_pw_qpolynomial_fold = ISLFunction.create(
    "isl_printer_print_union_pw_qpolynomial_fold",
    Take("Printer"),
    Keep("UnionPwQpolynomialFold"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_yaml_start_mapping = ISLFunction.create(
    "isl_printer_yaml_start_mapping",
    Take("Printer"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_yaml_end_mapping = ISLFunction.create(
    "isl_printer_yaml_end_mapping",
    Take("Printer"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_yaml_start_sequence = ISLFunction.create(
    "isl_printer_yaml_start_sequence",
    Take("Printer"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_yaml_end_sequence = ISLFunction.create(
    "isl_printer_yaml_end_sequence",
    Take("Printer"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_yaml_next = ISLFunction.create(
    "isl_printer_yaml_next",
    Take("Printer"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_flush = ISLFunction.create(
    "isl_printer_flush",
    Take("Printer"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_has_note = ISLFunction.create(
    "isl_printer_has_note",
    Keep("Printer"),
    Keep("Id"),
    return_=Param(bool, ctype=c_int),
    lib=_lib,
)

_isl_printer_get_note = ISLFunction.create(
    "isl_printer_get_note",
    Keep("Printer"),
    Take("Id"),
    return_=Give("Id"),
    lib=_lib,
)

_isl_printer_set_note = ISLFunction.create(
    "isl_printer_set_note",
    Take("Printer"),
    Take("Id"),
    Take("Id"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_print_fixed_box = ISLFunction.create(
    "isl_printer_print_fixed_box",
    Take("Printer"),
    Keep("FixedBox"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_print_set_list = ISLFunction.create(
    "isl_printer_print_set_list",
    Take("Printer"),
    Keep("SetList"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_print_id_to_ast_expr = ISLFunction.create(
    "isl_printer_print_id_to_ast_expr",
    Take("Printer"),
    Keep("IdToAstExpr"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_print_schedule = ISLFunction.create(
    "isl_printer_print_schedule",
    Take("Printer"),
    Keep("Schedule"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_print_schedule_node = ISLFunction.create(
    "isl_printer_print_schedule_node",
    Take("Printer"),
    Keep("ScheduleNode"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_print_union_access_info = ISLFunction.create(
    "isl_printer_print_union_access_info",
    Take("Printer"),
    Keep("UnionAccessInfo"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_print_union_flow = ISLFunction.create(
    "isl_printer_print_union_flow",
    Take("Printer"),
    Keep("UnionFlow"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_print_schedule_constraints = ISLFunction.create(
    "isl_printer_print_schedule_constraints",
    Take("Printer"),
    Keep("ScheduleConstraints"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_print_ast_expr = ISLFunction.create(
    "isl_printer_print_ast_expr",
    Take("Printer"),
    Keep("ASTExpr"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_print_ast_node = ISLFunction.create(
    "isl_printer_print_ast_node",
    Take("Printer"),
    Keep("ASTNode"),
    return_=Give("Printer"),
    lib=_lib,
)

_isl_printer_read_from_str = ISLFunction.create(
    "isl_printer_read_from_str",
    Context(),
    Param(str, ctype=c_char_p),
    return_=Give("Printer"),
    lib=_lib,
)
