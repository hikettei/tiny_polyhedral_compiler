from __future__ import annotations

import abc
from typing import TYPE_CHECKING, Any, Optional, Union
import contextvars

if TYPE_CHECKING:
    import caten.isl as I
## ~~ ScheduleBuilder ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
class ScheduleBuilder:
    def __init__(self) -> None:
        self.current_node: Optional["I.ScheduleNode"] = None
        self.domain: Any = None

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
    A base class for ScheduleNode.
    with P.<schedule_node_type>(initargs) as node:
       pass
    """
    def __init__(self, node_type: str) -> None:
        self.node: Optional["I.ScheduleNode"] = None
        self.node_type: str = node_type

    @abc.abstractmethod
    def realize(self, parent: Optional["I.ScheduleNode"]) -> "ScheduleNode": pass

    def get_node(self) -> "I.ScheduleNode":
        if not self.node:
            raise RuntimeError("Cannot apply loop transformation before entering context.")
        if not self.node_type == self.node.get_type():
            raise RuntimeError(f"The schedule is asserted to be {self.node_type} but is {self.node.get_type()}")
        return self.node
    
    def __enter__(self) -> "ScheduleNodeContext":
        builder = get_builder()
        self.node = self.realize(builder.current_node)
        # TODO: schedule_node_sequence/set
        builder.current_node = self[0]
        if isinstance(self, domain):
            assert builder.domain is None, "Domain node cannot be nested!"
            builder.domain = self
        return self

    def __exit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        # Default exit behavior: move up to parent
        builder = get_builder()
        if builder.current_node:
            builder.current_node = builder.current_node.parent()

    def __repr__(self) -> str:
        if self.node is not None:
            return self.node.to_str()
        else:
            return "ScheduleNode(Not Realized)"

    def to_c(self) -> str:
        ast_node = I.ASTBuild.alloc().node_from_schedule(self.get_node().get_schedule())
        # Print to C
        p = I.Printer.alloc_str()
        p.request_inplace()
        p = p.set_output_format(4) # ISL_FORMAT_C
        p.request_inplace()
        p = p.print_ast_node(ast_node)
        return p.get_str()
    # TODO: child, n_len, etc
    # TODO: getitem setitem
    def __getitem__(self, idx: int) -> "I.ScheduleNode":
        return self.node.child(idx)
## ~~ Specs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
class domain(ScheduleNodeBase):
    """
    A root of schedule node.
    """
    def __init__(self, domain_set: Union[str, "I.Set", "I.UnionSet"]):
        super().__init__("ScheduleNodeDomain")
        # read/write access relation which updated by P.stmt
        self.reads_map: Optional["I.UnionMap"] = None
        self.writes_map: Optional["I.UnionMap"] = None
        match domain_set:
            case str():
                self.uset = I.UnionSet(domain_set)
            case I.Set():
                self.uset = I.UnionSet.from_set(domain_set)
            case I.UnionSet():
                self.uset = domain_set
            case _:
                raise TypeError("P.domain(domain_set) should be Set or UnionSet")
    # TODO compute_flow dependence_relation
    # Visualize dependence_relation in notebook
    def realize(self, parent: Optional["I.ScheduleNode"]) -> "ScheduleNode":
        assert parent is None, f"P.domain should be the root of the schedule tree."
        return I.Schedule.from_domain(self.uset)

    # TODO
    def __getitem__(self, transformation: str) -> "domain":
        """
        s["ijk -> ikj"] will reshape the domain space
        """
        pass

    def padding(self, transformation: str) -> "domain":
        pass
    # TODO
    def finalize(self):
        pass

class band(ScheduleNodeBase):
    """
    TODO: Decent docs?
    """
    def __init__(self, schedule: Union[str, "I.MultiUnionPwAff"]) -> None:
        super().__init__("ScheduleNodeBand")
        match schedule:
            case str():
                self.schedule = I.MultiUnionPwAff(schedule)
            case I.MultiUnionPwAff():
                self.schedule = schedule
            case _:
                raise TypeError("P.band: schedule should be MultiUnionPwAff.")

    def realize(self, parent: Optional["I.ScheduleNode"]) -> "ScheduleNode":
        assert parent is not None, "band"
        return parent.insert_partial_schedule(self.schedule)

    # TODO: Loop Transformation
    def get_tiling_sizes(self, sizes: Union[int, List[int]]) -> "I.MultiVal":
        "Convert sizes into MultiVal, broadcast if sizes is integer."
        depth = (band := self.get_node()).band_get_space().dim(3)
        sizes = [sizes] * depth if isinstance(sizes, int) else sizes
        if not len(sizes) == depth:
            raise RuntimeError(f"Cannot construct a tiling space, depth={depth} but getting {sizes}")
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
    def __init__(self, filter_set: Union[str, "I.UnionSet"]) -> None:
        super().__init__("ScheduleNodeFilter")
        match filter_set:
            case str():
                self.filter_set = I.UnionSet(filter_set)
            case I.UnionSet():
                self.filter_set = filter_set
            case _:
                raise TypeError("P.filter: filter should be union set")

    def realize(self, parent: Optional["I.ScheduleNode"]) -> "ScheduleNode":
        assert parent is not None, "band"
        return parent.insert_partial_schedule(self.schedule)

def stmt(expr: str) -> None:
    """
    TODO: DOCS
    """
    dom = get_builder().domain
    if dom is None:
        raise RuntimeError("stmt() must be used within a P.domain context")
        
    if "=" not in expr:
        raise ValueError(f"Invalid statement expression (must contain assignment '='): {expr}")
        
    lhs_str, rhs_str = expr.split("=", 1)
        
    def extract_accesses(s: str) -> List[Tuple[str, str]]:
        return re.findall(r"([a-zA-Z_]\w*)\s*\[(.*?)\]", s)
        
    writes = extract_accesses(lhs_str)
    reads = extract_accesses(rhs_str)
    
    uset = dom.domain_set
    if isinstance(uset, str):
        uset = I.UnionSet(uset)
    elif isinstance(uset, I.Set):
        uset = I.UnionSet.from_set(uset)
    
    new_reads: Optional["I.UnionMap"] = None
    new_writes: Optional["I.UnionMap"] = None
    
    def process_set(s: "I.Set") -> None:
        nonlocal new_reads, new_writes
        s_str = str(s)
        if ":" in s_str:
            tuple_part = s_str.split(":")[0].strip()
            if tuple_part.startswith("{"):
                tuple_part = tuple_part[1:].strip()
        else:
            tuple_part = s_str.strip()
            if tuple_part.startswith("{") and tuple_part.endswith("}"):
                tuple_part = tuple_part[1:-1].strip()
                
        for (name, indices) in writes:
            m_str = f"{{ {tuple_part} -> {name}[{indices}] }}"
            m = I.UnionMap(m_str)
            if new_writes is None:
                new_writes = m
            else:
                new_writes = new_writes.union(m)
            
        for (name, indices) in reads:
            m_str = f"{{ {tuple_part} -> {name}[{indices}] }}"
            m = I.UnionMap(m_str)
            if new_reads is None:
                new_reads = m
            else:
                new_reads = new_reads.union(m)

    set_list = uset.get_set_list()
    n = set_list.n_set()
    for i in range(n):
        process_set(set_list.get_at(i))
    
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
