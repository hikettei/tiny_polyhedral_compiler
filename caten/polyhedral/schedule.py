from __future__ import annotations

import abc
import re
from typing import TYPE_CHECKING, Any, Optional, Union, List, Tuple, Callable, Dict
import contextvars

import caten.isl as I
## ~~ ScheduleBuilder ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
class ScheduleBuilder:
    def __init__(self) -> None:
        self.current_node: Optional["I.ScheduleNode"] = None
        self.domain: Any = None
        self.params: str = "[]"

_builder_ctx: contextvars.ContextVar[Optional[ScheduleBuilder]] = contextvars.ContextVar("schedule_builder", default=None)

def get_builder() -> ScheduleBuilder:
    b = _builder_ctx.get()
    if b is None:
        b = ScheduleBuilder()
        _builder_ctx.set(b)
    return b
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
class ScheduleNodeBase(metaclass=abc.ABCMeta):
    """
    Abstract base class for all schedule nodes in the DSL.

    This class serves as a wrapper around ISL schedule nodes, providing a Pythonic
    interface for constructing and manipulating schedule trees. It handles the
    realization of the schedule node within the builder context and supports
    conversion to C code.
    """
    def __init__(self, node_type: str) -> None:
        self.node: Optional["I.ScheduleNode"] = None
        self.node_type: str = node_type

    @abc.abstractmethod
    def realize(self, parent: Optional["I.ScheduleNode"]) -> "ScheduleNode": pass

    def get_node(self) -> "I.ScheduleNode":
        if not self.node:
            raise RuntimeError("Schedule node has not been realized yet. Ensure you are accessing this node within the appropriate context (e.g., inside a 'with P.domain(...):' block).")
        return self.node
    
    def __enter__(self) -> "ScheduleNodeContext":
        builder = get_builder()
        # reset ctx
        if isinstance(self, domain):
            builder.current_node = None
            builder.domain = None
        node = None
        if builder.current_node:
            node = builder.current_node.child(builder.current_node.n_children()-1)
        if isinstance(self, filter) and node.get_type_name() == "filter":
            new_sequence = I.UnionSetList.alloc(0)
            new_sequence = new_sequence + node.filter_get_filter()
            new_sequence = new_sequence + self.filter_set

            node = node.delete()
            n1 = node
            node = node.copy().insert_sequence(new_sequence)
            node = node.child(node.n_children()-1)
            # new sequence element copies the structure of first children. delete them
            node = node.child(0).cut().parent()
            builder.current_node = node
        else:
            builder.current_node = self.realize(node)
        
        if isinstance(self, domain):
            builder.domain = self
        self.node = builder.current_node
        return self

    def __exit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        # Default exit behavior: move up to parent
        if isinstance(self, domain):
            self.node = get_builder().current_node.get_schedule().get_root()
        else:
            self.node = None
            builder = get_builder()
            builder.current_node = builder.current_node.parent()

    def __repr__(self) -> str:
        if self.node is not None:
            from caten.polyhedral.viz import print_schedule
            return f"{self.node_type}(\n{print_schedule(self.node)}\n)"
        else:
            return f"{self.node_type}(Not Realized)"

    def viz(self) -> str:
        if self.node is not None:
            from caten.polyhedral.viz import viz_schedule
            return viz_schedule(self.node)
        else:
            return "(Not Realized)"

    def __getitem__(self, idx: int) -> "I.ScheduleNode":
        return self.node.child(idx)
## ~~ Specs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
class parameter():
    def __init__(self, params: str) -> None:
        # TODO: Syntax Check
        self.params = f"[{params}]"

    def __enter__(self) -> None:
        get_builder().params = self.params

    def __exit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        get_builder().params = "[]"

class domain(ScheduleNodeBase):
    """
    Represents the domain of the schedule tree.

    The domain node is always the root of a schedule tree and defines the set of
    statement instances that are subject to scheduling. It encapsulates the iteration
    domain of the program.
    """
    def __init__(self, domain_set: Union[str, "I.Set", "I.UnionSet"]):
        super().__init__("ScheduleNodeDomain")
        # read/write access relation which updated by P.stmt
        self.reads_map: Optional["I.UnionMap"] = None
        self.writes_map: Optional["I.UnionMap"] = None
        self.stmt_lambdas: Dict[str, Callable] = {}
        match domain_set:
            case str():
                self.uset = I.UnionSet(f"{get_builder().params} -> {domain_set}")
            case I.Set():
                self.uset = I.UnionSet.from_set(domain_set)
            case I.UnionSet():
                self.uset = domain_set
            case _:
                raise TypeError(f"P.domain expected a string, Set, or UnionSet, but got {type(domain_set).__name__}.")

    def realize(self, parent: Optional["I.ScheduleNode"]) -> "ScheduleNode":
        if parent is not None:
             raise RuntimeError(f"P.domain must be the root of the schedule tree, but found a parent node: {parent}.")
        return I.Schedule.from_domain(self.uset).get_root()

    def __or__(self, other: domain) -> "domain":
        if self.reads_map is not None or self.writes_map is not None:
            raise RuntimeError("Cannot merge domains that already have access relations defined (stmt() calls). Merge domains before defining statements.")
        return domain(self.uset | other.uset)
    
    def to_c(self) -> str:
        from caten.polyhedral.codegen import schedule_to_c
        return schedule_to_c(self.node.get_schedule(), self.stmts)

    def finalize(self):
        assert self.node is not None, "Cannot finalize P.domain before finalizing schedules."
        from .transform import ConstraintedModel
        return ConstraintedModel.from_schedule(self.node.get_schedule(), self.reads_map, self.writes_map, self.stmt_lambdas)

class band(ScheduleNodeBase):
    """
    Represents a band node in the schedule tree.

    A band node contains a partial schedule, which is a multi-dimensional
    piecewise affine function assigning a tuple of values to each domain element.
    Band nodes effectively define the loops of the resulting code. They can be
    permutable (allowing loop interchange and tiling) or not.
    """
    def __init__(self, schedule: Union[str, "I.UnionMap", "I.MultiUnionPwAff"]) -> None:
        super().__init__("ScheduleNodeBand")
        match schedule:
            case str():
                self.schedule = I.MultiUnionPwAff.from_union_map(I.UnionMap(schedule))
            case I.UnionMap():
                self.schedule = I.MultiUnionPwAff.from_union_map(schedule)
            case I.MultiUnionPwAff():
                self.schedule = schedule
            case _:
                raise TypeError(f"P.band expected a string, UnionMap, or MultiUnionPwAff, but got {type(schedule).__name__}.")

    def realize(self, parent: Optional["I.ScheduleNode"]) -> "ScheduleNode":
        if parent is None:
            raise RuntimeError("P.band requires a parent node (e.g., inside a 'with P.domain(...):' block). Cannot create a band at the root level.")
        return parent.insert_partial_schedule(self.schedule)

class sequence(ScheduleNodeBase):
    def __init__(self, filter_set_list: I.UnionSetList) -> None:
        super().__init__("ScheduleNodeSequence")
        self.filter_set_list = filter_set_list
        
    def realize(self, parent: Optional["I.ScheduleNode"]) -> "ScheduleNode":
        if parent is None:
             raise RuntimeError("P.sequence requires a parent node (e.g., inside a 'with P.domain(...):' block).")
        return parent.insert_sequence(self.filter_set_list)

class filter(ScheduleNodeBase):
    """
    Represents a filter node in the schedule tree.

    A filter node restricts the set of domain instances that reach its children
    to those that satisfy a given union set of constraints. This is used to
    select subsets of the domain for specific sub-schedules.
    """
    def __init__(self, filter_set: Union[str, "I.UnionSet"]) -> None:
        super().__init__("ScheduleNodeFilter")
        match filter_set:
            case str():
                self.filter_set = I.UnionSet(f"{get_builder().params} -> {filter_set}")
            case I.UnionSet():
                self.filter_set = filter_set
            case _:
                raise TypeError(f"P.filter expected a string or UnionSet, but got {type(filter_set).__name__}.")

    def realize(self, parent: Optional["I.ScheduleNode"]) -> "ScheduleNode":
        if parent is None:
            raise RuntimeError("P.filter requires a parent node (e.g., inside a 'with P.domain(...):' block).")
        return parent.insert_filter(self.filter_set)

class StmtContext():
    def __init__(self, dom: "domain", stmt_name: str) -> None:
        self.dom = dom
        self.stmt_name = stmt_name

    def __getitem__(self, f: Callable) -> None:
        self.dom.stmt_lambdas[self.stmt_name] = f

def stmt(expr: str) -> StmtContext:
    """
    Defines a statement with read/write access relations in the schedule.

    This function identifies the active statement in the current schedule context
    (ensuring uniqueness) and parses the assignment expression to define access maps.
    
    Args:
        expr: A string string representing the statement assignment.
    """
    builder = get_builder()
    dom = builder.domain
    if dom is None:
        raise RuntimeError("stmt() is called outside of a domain context.")
        
    if "=" not in expr:
        raise ValueError(f"Invalid statement expression: {expr}")
    
    # 1. Get Universe Domain & Check Uniqueness
    univ = builder.current_node.get_universe_domain()
    if univ.n_set() != 1:
        raise ValueError(f"stmt() requires exactly one active statement, but found {univ.n_set()}.")
    
    # 2. Stringify and extract "S[i, j]" from "[P] -> { S[i, j] }"
    dom_tuple_str = str(univ).split(" -> ")[-1].strip("{} ")

    lhs_str, rhs_str = expr.split("=", 1)
    
    def add_accesses(s: str, is_write: bool) -> None:
        for name, indices in re.findall(r"([a-zA-Z_]\w*)\s*\[(.*?)\]", s):
            # Helper to build map: [params] -> { S[...] -> A[...] }
            m_str = f"{builder.params} -> {{ {dom_tuple_str} -> {name}[{indices}] }}"
            m = I.UnionMap(m_str)
            if is_write:
                dom.writes_map = dom.writes_map.union(m) if dom.writes_map else m
            else:
                dom.reads_map = dom.reads_map.union(m) if dom.reads_map else m

    add_accesses(lhs_str, True)
    add_accesses(rhs_str, False)
            
    return StmtContext(dom, univ.get_set_list().get_at(0).get_tuple_name())
