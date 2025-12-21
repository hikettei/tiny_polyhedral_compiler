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
        self.node = self.realize(builder.current_node)
        if isinstance(self, domain):
            builder.domain = self
        builder.current_node = self[0]
        return self

    def __exit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        # Default exit behavior: move up to parent
        builder = get_builder()
        if builder.current_node:
            builder.current_node = builder.current_node.parent()
            self.node = builder.current_node

    def __repr__(self) -> str:
        if self.node is not None:
            return f"{self.node_type}(\n{print_schedule(self.node)}\n)"
        else:
            return "ScheduleNode(Not Realized)"

    def to_c(self) -> str:
        from ctypes import CFUNCTYPE, c_void_p, cast, py_object
        
        ast_node = I.ASTBuild.alloc().node_from_schedule(self.get_node().get_schedule())
        lambdas = getattr(self, "stmt_lambdas", {})

        if lambdas:
            # Callback signature: isl_ast_node *(*fn)(__isl_take isl_ast_node *node, void *user)
            CALLBACK = CFUNCTYPE(c_void_p, c_void_p, c_void_p)
            
            def replace_cb(node_handle, user_data):
                node = I.ASTNode(node_handle)
                
                from caten.isl.specs.enums import isl_ast_node_type, isl_ast_expr_type, isl_ast_expr_op_type
                
                if node.get_type() == isl_ast_node_type.ISL_AST_NODE_USER: 
                    expr = node.user_get_expr()
                    if expr.get_type() == isl_ast_expr_type.ISL_AST_EXPR_OP: 
                        if expr.get_op_type() == isl_ast_expr_op_type.ISL_AST_EXPR_OP_CALL: 
                            call_id = expr.get_op_arg(0).id_get_id()
                            name = call_id.name()
                            if name in lambdas:
                                func = lambdas[name]
                                n_args = expr.get_op_n_arg()
                                # arg 0 is the function ID, actual args start from 1
                                args = [expr.get_op_arg(i) for i in range(1, n_args)]
                                new_node = func(*args)
                                return new_node.copy_handle()
                return node.copy_handle()

            c_cb = CALLBACK(replace_cb)
            cb_ptr = cast(c_cb, c_void_p)
            ast_node = ast_node.map_descendant_bottom_up(cb_ptr, None)

        # Print to C
        p = I.Printer.alloc_str()
        p.request_inplace()
        p = p.set_output_format(4) # ISL_FORMAT_C
        
        p.request_inplace()
        p = p.print_ast_node(ast_node)
        return p.get_str()

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
    # TODO: Loop Transformation
    def get_tiling_sizes(self, sizes: Union[int, List[int]]) -> "I.MultiVal":
        "Convert sizes into MultiVal, broadcast if sizes is integer."
        depth = (band := self.get_node()).band_get_space().dim(3)
        sizes = [sizes] * depth if isinstance(sizes, int) else sizes
        if not len(sizes) == depth:
            raise ValueError(f"Tiling size mismatch: Band depth is {depth}, but provided {len(sizes)} sizes: {sizes}. Please provide exactly {depth} sizes.")
        mv = I.MultiVal.zeros(band.band_get_space())
        for i, size in enumerate(sizes):
            mv[i] = size
        return mv

    @property
    def depth(self) -> int:
        return self.get_node().band_get_space().dim(3)
    
    def scale(self, sizes: Union[int, List[int]]) -> "band":
        self.node = self.node.band_scale(self.get_tiling_sizes(sizes))
        return self

    def scale_down(self, sizes: Union[int, List[int]]) -> "band":
        self.node = self.node.band_scale_down(self.get_tiling_sizes(sizes))
        return self
    
    def mod(self, sizes: Union[int, List[int]]) -> "band":
        self.node = self.node.band_mod(self.get_tiling_sizes(sizes))
        return self

    def shift(self, sizes: Union[int, List[int]]) -> "band":
        self.node = self.node.band_shift(self.get_tiling_sizes(sizes))
        return self

    def tile(self, sizes: Union[int, List[int]]) -> "band":
        """{[i] -> [i mod size, size]}"""
        self.node = self.node.band_tile(self.get_tiling_sizes(sizes))
        return self

    def split(self, pos: int) -> "band":
        self.node = self.node.band_split(pos)
        return self

    def sink(self) -> "band":
        self.node = self.node.bank_sink()
        return self

    def __mul__(self, other):      return self.scale(other)
    def __floordiv__(self, other): return self.scale_down(other)
    def __mod__(self, other):      return self.mod(other)
    def __add__(self, other):      return self.shift(other)
    def __sub__(self, other):      return self.shift([-x for x in other] if isinstance(other, list) else -other)
    def __matmul__(self, other):   return self.tile(other)

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
        return parent.insert_partial_schedule(self.schedule)

class StmtContext():
    def __init__(self, dom: "domain", stmt_name: str) -> None:
        self.dom = dom
        self.stmt_name = stmt_name

    def __getitem__(self, f: Callable) -> None:
        self.dom.stmt_lambdas[self.stmt_name] = f

def stmt(expr: str) -> StmtContext:
    """
    Defines a statement with read/write access relations in the schedule.

    This function parses a string representation of a statement (e.g., "S[i,j] = A[i, j], B[j, i]")
    to extract the statement's domain and its memory access patterns (reads and writes).
    It updates the current domain's access maps, which are crucial for dependence analysis.
    
    Args:
        expr: A string string representing the statement assignment.
    """
    dom = get_builder().domain
    if dom is None:
        raise RuntimeError("stmt() is called outside of a domain context. Ensure you are calling stmt() inside a 'with P.domain(...):' block.")
        
    if "=" not in expr:
        raise ValueError(f"Invalid statement expression (must contain assignment '='): {expr}")
    
    # Extract statement name from domain
    # Assuming single statement domain for now or taking the first one
    stmt_name = "unknown"
    dom_tuple_str = ""
    
    set_list = dom.uset.get_set_list()
    for i in range(set_list.n_set()):
        s = set_list.get_at(i)
        stmt_name = s.get_tuple_name()
        # Reconstruct tuple string: S[i, j, k]
        # We need variable names. get_dim_name might work if they are set.
        # But parsing from string representation is safer if names are not set in obj.
        # s.to_str() -> "{ S[i, j] : ... }"
        # Use regex to extract "S[...]"
        m = re.search(r"(\w+\[[^\]]+\])", str(s))
        if m:
            dom_tuple_str = m.group(1)
        break # Only process the first set for now

    lhs_str, rhs_str = expr.split("=", 1)
        
    def extract_accesses(s: str) -> List[Tuple[str, str]]:
        return re.findall(r"([a-zA-Z_]\w*)\s*\[(.*?)\]", s)
        
    writes = extract_accesses(lhs_str)
    reads = extract_accesses(rhs_str)
    
    new_reads: Optional["I.UnionMap"] = None
    new_writes: Optional["I.UnionMap"] = None
    
    # Helper to build map: { S[...] -> A[...] }
    def build_map(name: str, indices: str) -> "I.UnionMap":
        if dom_tuple_str:
            m_str = f"{{ {dom_tuple_str} -> {name}[{indices}] }}"
            return I.UnionMap(m_str)
        else:
            # Fallback to old behavior (incorrect but keeps code running if extraction fails)
            tuple_part = get_builder().params
            m_str = f"{{ {tuple_part} -> {name}[{indices}] }}"
            return I.UnionMap(m_str)

    for (name, indices) in writes:
        m = build_map(name, indices)
        if new_writes is None:
            new_writes = m
        else:
            new_writes = new_writes.union(m)
            
    for (name, indices) in reads:
        m = build_map(name, indices)
        if new_reads is None:
            new_reads = m
        else:
            new_reads = new_reads.union(m)

    if new_reads:
        if dom.reads_map:
            dom.reads_map = dom.reads_map.union(new_reads)
        else:
            dom.reads_map = new_reads
        
    if new_writes:
        if dom.writes_map:
            dom.writes_map = dom.writes_map.union(new_writes)
        else:
            dom.writes_map = new_writes
            
    return StmtContext(dom, stmt_name)

def print_schedule(node: "I.ScheduleNode") -> str:
    """
    Pretty prints the schedule tree using a refined tree traversal algorithm.
    """
    def _rec(n: "I.ScheduleNode", prefix: str = "", is_last: bool = True) -> list[str]:
        t_name = n.get_type_name()
        info_map = {
            "band": lambda x: str(x.band_get_partial_schedule()),
            "domain": lambda x: str(x.domain_get_domain()),
            "filter": lambda x: str(x.filter_get_filter()),
            "mark": lambda x: f'"{x.mark_get_id().name()}"',
            "context": lambda x: str(x.context_get_context()),
            "guard": lambda x: str(x.guard_get_guard()),
            "extension": lambda x: str(x.extension_get_extension())
        }
        info = info_map.get(t_name, lambda x: "")(n).replace("{ ", "").replace(" }", "")
        yield f"{prefix}{'┗' if is_last else '┣'} {t_name}({info})"
        
        children = [n.child(i) for i in range(n.n_children())]
        for i, c in enumerate(children):
            yield from _rec(c, prefix + ("  " if is_last else "┃ "), i == len(children)-1)

    return "\n".join(_rec(node))
