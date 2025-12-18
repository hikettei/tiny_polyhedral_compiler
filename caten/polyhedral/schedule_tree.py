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
        self.schedule: Optional["I.Schedule"] = None
        self.current_domain: Any = None

_builder_ctx: contextvars.ContextVar[Optional[ScheduleBuilder]] = contextvars.ContextVar("schedule_builder", default=None)

def get_builder() -> ScheduleBuilder:
    b = _builder_ctx.get()
    if b is None:
        b = ScheduleBuilder()
        _builder_ctx.set(b)
    return b
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
class ScheduleNodeContext(metaclass=abc.ABCMeta):
    """
    Base class for schedule tree nodes that can be used as context managers.
    """
    def __init__(self, node_type: str) -> None:
        self.node: Optional["I.ScheduleNode"] = None
        self.node_type: str = node_type

    @abc.abstractmethod
    def realize(self, parent: Optional["I.ScheduleNode"]) -> "ScheduleNode":
        pass

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
    # TODO: getitem setitem
    def __getitem__(self, idx: int) -> "I.ScheduleNode":
        return self.node.child(idx)
    # TODO: setitem delitem
## ~~ Specs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ScheduleNodeBand TODO List
# - Force Isolation Option (Loop Reminder) GPU Guard or Reminder Generation
# - Insert Mark
# - Paddingができるようにする
# NotebookにTransformationの一覧を記述する
# Symbolic Tileどこ？
# https://dl.acm.org/doi/epdf/10.1145/3178372.3179509
# https://inria.hal.science/hal-02493164v2/document
# 後もう一本読むべき論文があったはず・・・
class band(ScheduleNodeContext):
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
